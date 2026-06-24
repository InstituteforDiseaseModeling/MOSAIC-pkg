"""
MOSAIC Dask Worker — Pure Python LASER simulation runner
=========================================================
Called by Dask workers to run individual LASER simulations.
No R dependency — uses laser.cholera.metapop.model directly.

This module is uploaded to all workers via client.upload_file() from the R
orchestrator (run_MOSAIC_dask), then imported on each worker with:
    import mosaic_dask_worker

Seed scheme (mirrors .mosaic_run_simulation_worker in R/run_MOSAIC.R):
    seed_ij = (sim_id - 1) * n_iterations + j
This ensures same sim_id always gets same parameters, and different
iterations within a sim get different stochastic LASER realizations.
"""

import copy
import datetime
import gc
import json
import os
import time
import traceback as tb

import numpy as np

# =============================================================================
# FIELD TYPE REGISTRIES
# =============================================================================

# Fields that LASER expects as 1-D numpy arrays (one value per location).
# After JSON deserialization these arrive as Python lists and must be converted.
_VECTOR_FIELDS = frozenset([
    "tau_i",
    "alpha_1",
    "beta_j0_hum",
    "beta_j0_env",
    "beta_j0_tot",
    "p_beta",
    "theta_j",
    "a_1_j",
    "a_2_j",
    "b_1_j",
    "b_2_j",
    "mu_j",
    "psi_star_a",
    "psi_star_b",
    "psi_star_z",
    "psi_star_k",
    "N_j_initial",
    "S_j_initial",
    "E_j_initial",
    "I_j_initial",
    "R_j_initial",
    "V1_j_initial",
    "V2_j_initial",
    "prop_S_initial",
    "prop_E_initial",
    "prop_I_initial",
    "prop_R_initial",
    "prop_V1_initial",
    "prop_V2_initial",
    "longitude",
    "latitude",
    "mu_j_baseline",
    "mu_j_slope",
    "mu_j_epidemic_factor",
])

# Engine fields coerced via laser.cholera.metapop.params.as_ndarray (NOT
# np.array). CRITICAL parity contract: as_ndarray() returns an incoming numpy
# array UNCHANGED (it skips the declared dtype cast) but casts an incoming
# *list* to the declared dtype. The integer-count fields below are declared
# np.uint32 and the float fields np.float32 in params.dict_to_propertysetex().
#
# The local PSOCK path ships these via reticulate::r_to_py() as Python LISTS, so
# the engine casts them to uint32 / float32 (its intended dtypes). If the Dask
# worker pre-converts them to float64 numpy arrays (as _VECTOR_FIELDS otherwise
# does), as_ndarray() returns them as float64 and the engine seeds the
# stochastic state in float64 instead of uint32 (and reads seasonality coeffs at
# float64 instead of float32) -- diverging the simulation from the local backend
# by ~1-2% in log-likelihood at config_default scale even with identical values
# and seed (root cause: float-vs-integer state seeding). These MUST stay as
# lists on the worker so the engine applies its own dtype cast, matching local.
# Fields coerced via np.array (e.g. beta_j0_tot, psi_star_*, prop_*_initial) are
# ALWAYS recast to float32 regardless of input type, so they are parity-safe and
# remain in _VECTOR_FIELDS.
_AS_NDARRAY_FIELDS = frozenset([
    "N_j_initial",
    "S_j_initial",
    "E_j_initial",
    "I_j_initial",
    "R_j_initial",
    "V1_j_initial",
    "V2_j_initial",
    "longitude",
    "latitude",
    "tau_i",
    "beta_j0_hum",
    "a_1_j",
    "a_2_j",
    "b_1_j",
    "b_2_j",
    "beta_j0_env",
    "theta_j",
])

# Fields in the base_config that LASER expects as 2-D numpy arrays
# (shape: n_locations × n_time_steps). These come pre-converted from R via
# reticulate::r_to_py() and are already numpy arrays — no conversion needed
# here. Listed for documentation/awareness only.
_MATRIX_FIELDS = frozenset([
    "b_jt",
    "d_jt",
    "mu_jt",
    "psi_jt",
    "nu_1_jt",
    "nu_2_jt",
    "reported_cases",
    "reported_deaths",
])


# =============================================================================
# HELPERS
# =============================================================================

def _apply_sampled_params(base_config: dict, sampled_params_json: str) -> dict:
    """
    Merge JSON-serialized sampled scalar/vector params into a deep copy of
    base_config.

    base_config already contains numpy arrays for most matrix fields (b_jt,
    etc.) because it was created via reticulate::r_to_py() and then scattered
    via client.scatter().

    psi_jt is a special case: apply_psi_star_calibration() in R recalculates
    it per-sim using the sampled psi_star_* params, so the updated psi_jt is
    sent via JSON per-sim and must override the stale broadcast copy.  It
    arrives as a list-of-lists (rows × cols) and is converted to a 2-D numpy
    array here.

    A deep copy of base_config is made to prevent mutation of the scattered
    object across simulations on the same worker.
    """
    config = copy.deepcopy(base_config)

    sampled = json.loads(sampled_params_json)

    # Convert to numpy 1-D array for known vector fields. EXCEPT the
    # _AS_NDARRAY_FIELDS, which MUST stay as Python lists so the engine's
    # as_ndarray() casts them to its declared dtype (uint32 for IC counts,
    # float32 for seasonality/transmission vectors) -- exactly as the local
    # PSOCK path (which ships lists). Converting them to float64 here would make
    # as_ndarray() return them unchanged as float64 and silently diverge the
    # backend from local (see _AS_NDARRAY_FIELDS docstring).
    # Single-location countries produce scalars (not lists) after JSON
    # deserialization, so we must handle both cases.
    for field, val in sampled.items():
        if field in _AS_NDARRAY_FIELDS:
            # Keep as a list; wrap a single-location scalar into a 1-element list
            # (mirrors as_ndarray's scalar handling and the local r_to_py list).
            if isinstance(val, (int, float)):
                sampled[field] = [val]
            elif isinstance(val, tuple):
                sampled[field] = list(val)
            # already a list -> leave as-is
        elif field in _VECTOR_FIELDS:
            if isinstance(val, (list, tuple)):
                sampled[field] = np.array(val, dtype=float)
            elif isinstance(val, (int, float)):
                sampled[field] = np.atleast_1d(np.array(val, dtype=float))

    # Convert list-of-lists → numpy 2-D array for matrix fields sent per-sim
    # (currently only psi_jt; other matrices are unchanged across sims)
    for field in _MATRIX_FIELDS:
        if field in sampled and isinstance(sampled[field], (list, tuple)):
            sampled[field] = np.array(sampled[field], dtype=float)

    # base_config carries N_j_initial / longitude / latitude as numpy float64
    # arrays (reticulate::r_to_py of .extract_base_config). Those are also
    # as_ndarray fields, so convert any deep-copied numpy array back to a list
    # for parity (only if not overridden by the sampled set above).
    for field in _AS_NDARRAY_FIELDS:
        v = config.get(field)
        if isinstance(v, np.ndarray):
            config[field] = v.tolist()

    config.update(sampled)
    return config


def _weights_obs_matrix_trivial(w, obs) -> bool:
    """True if a per-cell confidence-weight matrix is trivial.

    A matrix is trivial when it is ``None`` OR every cell with a finite
    observation has ``w == 1.0`` and is finite -- in which case the unweighted
    code path is bit-identical to passing the matrix, so the no-slice
    ``model.log_likelihood`` (which the engine analyzer computes WITHOUT
    weights_obs) is still correct. A non-finite weight on a finite-obs cell is
    a real "no confidence" signal and is therefore NOT trivial. Matrix-level
    analogue of R ``.weights_obs_row_trivial`` (R/calc_model_likelihood.R)
    applied across all rows at once.
    """
    if w is None:
        return True
    w = np.asarray(w, dtype=float)
    obs = np.asarray(obs, dtype=float)
    if w.ndim == 1:
        w = w[np.newaxis, :]
    if obs.ndim == 1:
        obs = obs[np.newaxis, :]
    fin = np.isfinite(obs)
    if not np.any(fin):
        return True
    wf = w[fin]
    return bool(np.all(np.isfinite(wf) & (wf == 1.0)))


def _score_window_likelihood(config: dict, model) -> float:
    """Recompute the log-likelihood with per-cell obs weights and/or a sliced window.

    Mirrors the R local-PSOCK scorer in .mosaic_run_simulation_worker
    (R/run_MOSAIC.R:387-529). Called whenever the engine analyzer's full-window,
    unweighted ``model.log_likelihood`` would diverge from the local R score --
    i.e. when EITHER:
      * ``score_idx_cases`` / ``score_idx_deaths`` (1-based) indicate a scored
        start later than 1: the IC-transient / deaths-era head is SLICED off the
        obs/est arrays (the shape terms do not honor weights_time, so
        down-weighting is insufficient), OR
      * the per-cell confidence-weight matrices (``reported_cases_weight`` /
        ``reported_deaths_weight``, config_default v4.1+) are non-trivial: the
        engine analyzer never sees weights_obs (analyzer.py does not pass them),
        so its ``model.log_likelihood`` is unweighted.

    laser-cholera v0.16.0+ accepts the ``weights_obs_cases`` / ``weights_obs_deaths``
    matrices, so we pass them directly (sliced + deaths-prefix-zeroed exactly as
    the R path does) instead of the prior NaN-on-obs emulation. The sliced
    ``date_start`` is passed so the peak date_seq length matches the sliced
    n_time_steps.
    """
    from laser.cholera.calc_model_likelihood import calc_model_likelihood
    import pandas as pd

    idx_cases = int(config.get("score_idx_cases", 1))
    idx_deaths = int(config.get("score_idx_deaths", 1))

    obs_cases = np.asarray(config["reported_cases"], dtype=float)
    obs_deaths = np.asarray(config["reported_deaths"], dtype=float)
    est_cases = np.asarray(model.results.reported_cases, dtype=float)
    est_deaths = np.asarray(model.results.reported_deaths, dtype=float)

    # Per-cell confidence-weight matrices (config_default v4.1+); None on older
    # configs (-> unweighted, matching the engine analyzer).
    wobs_cases = config.get("reported_cases_weight", None)
    wobs_deaths = config.get("reported_deaths_weight", None)
    if wobs_cases is not None:
        wobs_cases = np.asarray(wobs_cases, dtype=float)
    if wobs_deaths is not None:
        wobs_deaths = np.asarray(wobs_deaths, dtype=float)

    def _as2d(a):
        if a is not None and a.ndim == 1:
            return a[np.newaxis, :]
        return a

    obs_cases, obs_deaths = _as2d(obs_cases), _as2d(obs_deaths)
    est_cases, est_deaths = _as2d(est_cases), _as2d(est_deaths)
    wobs_cases, wobs_deaths = _as2d(wobs_cases), _as2d(wobs_deaths)

    # Match the engine analyzer's nreports trim (drop the initial-state column
    # mismatch) BEFORE slicing, so the columns align with obs.
    nreports = min(obs_cases.shape[1], est_cases.shape[1])
    obs_cases = obs_cases[:, :nreports]
    obs_deaths = obs_deaths[:, :nreports]
    est_cases = est_cases[:, :nreports]
    est_deaths = est_deaths[:, :nreports]
    if wobs_cases is not None:
        wobs_cases = wobs_cases[:, :nreports]
    if wobs_deaths is not None:
        wobs_deaths = wobs_deaths[:, :nreports]

    s = max(1, min(idx_cases, idx_deaths))   # shared slice start (1-based)
    keep0 = s - 1                            # 0-based slice index

    obs_cases = obs_cases[:, keep0:]
    obs_deaths = obs_deaths[:, keep0:]
    est_cases = est_cases[:, keep0:]
    est_deaths = est_deaths[:, keep0:]
    if wobs_cases is not None:
        wobs_cases = wobs_cases[:, keep0:].copy()
    if wobs_deaths is not None:
        wobs_deaths = wobs_deaths[:, keep0:].copy()

    # Zero the later-starting deaths channel on the residual prefix
    # s:(idx_deaths-1) via the WEIGHTS matrix (mirrors R run_MOSAIC.R:426-437).
    # Synthesize a ones matrix when no per-cell deaths weights exist so the
    # prefix can be zeroed (belt-and-suspenders; the deaths min_obs gate also
    # protects this region). This replaces the prior NaN-on-obs emulation now
    # that v0.16.0 calc_model_likelihood honors weights_obs_*.
    deaths_prefix = idx_deaths - s
    if deaths_prefix > 0:
        if wobs_deaths is None:
            wobs_deaths = np.ones((obs_deaths.shape[0], obs_deaths.shape[1]), dtype=float)
        wobs_deaths[:, :deaths_prefix] = 0.0

    # weights_time, sliced to the kept columns.
    wt = config.get("weights_time", None)
    if wt is not None:
        wt = np.asarray(wt, dtype=float)
        if wt.ndim == 0:
            wt = np.full(nreports, float(wt))
        wt = wt[:nreports][keep0:]

    # Sliced start date for the peak date_seq.
    date_start = config.get("date_start", None)
    if isinstance(date_start, str):
        date_start = datetime.datetime.strptime(date_start, "%Y-%m-%d")
    if isinstance(date_start, datetime.datetime) and s > 1:
        date_start = date_start + datetime.timedelta(days=int(s - 1))

    # Source epidemic_peaks from model.params -- NOT the raw config dict. The
    # engine's params.py (dict_to_propertysetex) appends the integer ``loc_idx``
    # column that maps each ``iso_code`` row to its 0-based position in
    # ``location_name``; the v0.16.0 calc_model_likelihood matches peaks by
    # ``loc_idx`` (getattr(row, "loc_idx", None)) and SILENTLY DROPS every row
    # that lacks it. The raw ``config["epidemic_peaks"]`` carries only
    # iso_code/peak_date (see .filter_epidemic_peaks in R), so reading it here
    # would zero out all peak/shape terms -- diverging from the local R scorer
    # (which dispatches by iso_code) whenever shape weights are on. Reading
    # model.params guarantees byte-parity with the engine analyzer's own peak set.
    epidemic_peaks = None
    try:
        if "epidemic_peaks" in model.params:
            epidemic_peaks = model.params["epidemic_peaks"]
    except (TypeError, KeyError):
        epidemic_peaks = None
    if epidemic_peaks is None:
        epidemic_peaks = config.get("epidemic_peaks", None)
    if epidemic_peaks is not None and not isinstance(epidemic_peaks, pd.DataFrame):
        epidemic_peaks = pd.DataFrame(epidemic_peaks)

    optional = {}
    for key in ("weight_cases", "weight_deaths", "weights_location",
                "nb_k_min_cases", "nb_k_min_deaths",
                "weight_peak_timing", "weight_peak_magnitude",
                "weight_cumulative_total", "weight_wis",
                "sigma_peak_time", "sigma_peak_log"):
        if key in config and config[key] is not None:
            optional[key] = config[key]
    if wt is not None:
        optional["weights_time"] = wt
    if epidemic_peaks is not None:
        optional["epidemic_peaks"] = epidemic_peaks
        optional["date_start"] = date_start
        optional["date_stop"] = config.get("date_stop", None)
    if wobs_cases is not None:
        optional["weights_obs_cases"] = wobs_cases
    if wobs_deaths is not None:
        optional["weights_obs_deaths"] = wobs_deaths

    return float(calc_model_likelihood(
        obs_cases=obs_cases,
        est_cases=est_cases,
        obs_deaths=obs_deaths,
        est_deaths=est_deaths,
        **optional,
    ))


# =============================================================================
# PUBLIC WORKER FUNCTION
# =============================================================================

def run_laser_sim(sim_id: int, n_iterations: int,
                  sampled_params_json: str, base_config: dict) -> dict:
    """
    Run a single MOSAIC simulation on a Dask worker.

    Parameters
    ----------
    sim_id : int
        1-based simulation ID. Determines parameter sampling seed and
        iteration seeds (see seed scheme above).
    n_iterations : int
        Number of stochastic LASER realizations per simulation.
        Each uses a unique seed; results are returned per-iteration so that
        the R orchestrator can compute calc_log_mean_exp() over likelihoods.
    sampled_params_json : str
        JSON string of scalar/vector parameters sampled for this simulation
        (output of R's .extract_sampled_params()). Does NOT include matrix
        fields — those live in base_config.
    base_config : dict
        Scattered base config dict (reticulate-converted from R). Contains
        numpy 2-D arrays for b_jt, d_jt, psi_jt, etc., plus fixed scalars
        like date_start, location_name, N_j_initial, etc.

    Returns
    -------
    dict
        On success (issue #101 schema)::

            {
                "sim_id": int,
                "success": True,
                "worker_elapsed_sec": float,
                "params": dict,        # sampled scalars/vectors echoed back,
                                       # minus _MATRIX_FIELDS. location_name
                                       # is NOT included — the R gather path
                                       # re-injects it before serialization.
                "iterations": [
                    {
                        "iter":       int,    # 1-based iteration index
                        "seed_iter":  int,    # seed used for this iter
                        "likelihood": float,  # model.log_likelihood
                    },
                    ...
                ]
            }

        On failure::

            {
                "sim_id": int,
                "success": False,
                "error": str,
                "traceback": str,
            }
    """
    # ------------------------------------------------------------------
    # Threading guard — set BEFORE any Python JIT/BLAS code is imported.
    # Dask may reuse worker processes, so this must be inside the function
    # (not at module level) to guarantee it runs per-task.
    # ------------------------------------------------------------------
    for _var in ("OMP_NUM_THREADS", "MKL_NUM_THREADS", "NUMBA_NUM_THREADS",
                 "TBB_NUM_THREADS", "OPENBLAS_NUM_THREADS", "NUMEXPR_NUM_THREADS"):
        os.environ[_var] = "1"

    try:
        import laser.cholera.metapop.model as lc
        import warnings as _warnings
        _warnings.filterwarnings(
            "ignore", message="invalid value encountered in divide"
        )

        config = _apply_sampled_params(base_config, sampled_params_json)

        # The sampled dict (minus matrix fields) is echoed back so the R
        # gather adapter can flatten it into parquet columns without
        # re-deserializing the JSON. Built once per sim because params are
        # constant across iterations.
        sampled = json.loads(sampled_params_json)
        params_for_return = {
            k: v for k, v in sampled.items() if k not in _MATRIX_FIELDS
        }

        t_start = time.monotonic()
        iterations = []

        for j in range(1, n_iterations + 1):
            # Seed scheme: mirrors .mosaic_run_simulation_worker in R/run_MOSAIC.R
            seed_ij = (sim_id - 1) * n_iterations + j
            config["seed"] = seed_ij

            model = lc.run_model(paramfile=config, quiet=True)

            # Engine-version guard: laser-cholera >= 0.13 populates
            # model.log_likelihood when the analyzer is enabled (Phase 2 /
            # laser-cholera#58). Older engines silently lack the attribute,
            # which would otherwise crash the worker with AttributeError on
            # the first sim. Fail this iteration cleanly with a clear error
            # so the gather adapter surfaces an actionable warning.
            ll = getattr(model, "log_likelihood", None)
            if ll is None:
                del model
                gc.collect()
                return {
                    "sim_id": sim_id,
                    "success": False,
                    "error": (
                        "model.log_likelihood is not available; "
                        "laser-cholera >= 0.13 is required for Dask "
                        "worker-side scoring (issue #101)."
                    ),
                    "traceback": "",
                }

            # Recompute on-worker (mirroring the local PSOCK R score) when the
            # engine analyzer's full-window, UNWEIGHTED model.log_likelihood
            # would diverge from the local R score, i.e. when EITHER:
            #   * the resolved start indices are > 1 (burn-in / deaths-era
            #     start) -- the analyzer scores the IC-transient head + deaths
            #     prefix it should slice off; OR
            #   * the per-cell confidence-weight matrices are non-trivial -- the
            #     analyzer never passes weights_obs_* to calc_model_likelihood,
            #     so model.log_likelihood is unweighted while the local R path
            #     applies the weights (parity fix, laser-cholera v0.16.0).
            # Default (idx == 1 AND trivial weights) keeps model.log_likelihood
            # untouched => bit-identical to the engine analyzer.
            idx_c = int(config.get("score_idx_cases", 1))
            idx_d = int(config.get("score_idx_deaths", 1))
            need_wobs = (
                not _weights_obs_matrix_trivial(
                    config.get("reported_cases_weight"), config.get("reported_cases"))
                or not _weights_obs_matrix_trivial(
                    config.get("reported_deaths_weight"), config.get("reported_deaths"))
            )
            if idx_c > 1 or idx_d > 1 or need_wobs:
                ll = _score_window_likelihood(config, model)

            iterations.append({
                "iter":       j,
                "seed_iter":  seed_ij,
                "likelihood": float(ll),
            })

            del model  # release Python LASER object immediately

        worker_elapsed_sec = time.monotonic() - t_start

        # Explicit GC after all iterations — prevents Python object build-up
        # across thousands of simulations (mirrors run_MOSAIC.R lines 278-279)
        gc.collect()

        return {
            "sim_id": sim_id,
            "success": True,
            "worker_elapsed_sec": worker_elapsed_sec,
            "params": params_for_return,
            "iterations": iterations,
        }

    except Exception as exc:  # noqa: BLE001
        gc.collect()
        return {
            "sim_id": sim_id,
            "success": False,
            "error": str(exc),
            "traceback": tb.format_exc(),
        }


def run_laser_postca(task_id: int, seed: int,
                     config_json: str, extra_fields: dict) -> dict:
    """Run a single LASER simulation for post-calibration (ensemble / stochastic).

    Unlike run_laser_sim(), this takes a complete config as JSON (not base +
    sampled params separately) and runs a single iteration with a given seed.

    Parameters
    ----------
    task_id : int
        Arbitrary task identifier (for tracking).
    seed : int
        Stochastic seed for this LASER run.
    config_json : str
        Full MOSAIC config as JSON string (from R's jsonlite::toJSON).
    extra_fields : dict
        Dict of numpy arrays (matrices) that are too large for JSON.
        Typically scattered once via client.scatter(broadcast=True).
        Keys are field names (e.g. "b_jt", "psi_jt"), values are numpy arrays.

    Returns
    -------
    dict with keys: task_id, seed, success, reported_cases, reported_deaths
    """
    for _var in ("OMP_NUM_THREADS", "MKL_NUM_THREADS", "NUMBA_NUM_THREADS",
                 "TBB_NUM_THREADS", "OPENBLAS_NUM_THREADS", "NUMEXPR_NUM_THREADS"):
        os.environ[_var] = "1"

    try:
        import laser.cholera.metapop.model as lc
        import warnings as _warnings
        _warnings.filterwarnings(
            "ignore", message="invalid value encountered in divide"
        )

        config = json.loads(config_json)

        # Merge in large matrix fields from scattered dict
        if extra_fields is not None:
            for key, val in extra_fields.items():
                config[key] = val

        # Keep the engine as_ndarray fields as LISTS so the engine casts them to
        # its declared dtype (uint32 IC / float32 vectors), matching the local
        # PSOCK path. Converting them to float64 numpy here would diverge the
        # post-calibration ensemble from a local rerun (see _AS_NDARRAY_FIELDS).
        # A scattered numpy array (from extra_fields) is converted back to a list.
        for key in _AS_NDARRAY_FIELDS:
            if key in config:
                v = config[key]
                if isinstance(v, np.ndarray):
                    config[key] = v.tolist()
                elif isinstance(v, (int, float)):
                    config[key] = [v]
                elif isinstance(v, tuple):
                    config[key] = list(v)

        # Convert vector fields to numpy arrays (same logic as _apply_sampled_params)
        for key in _VECTOR_FIELDS:
            if key in _AS_NDARRAY_FIELDS:
                continue
            if key in config and not isinstance(config[key], np.ndarray):
                val = config[key]
                if isinstance(val, list):
                    config[key] = np.array(val, dtype=float)
                elif isinstance(val, (int, float)):
                    config[key] = np.atleast_1d(np.array(val, dtype=float))

        # Convert matrix fields to numpy arrays
        for key in _MATRIX_FIELDS:
            if key in config and not isinstance(config[key], np.ndarray):
                val = config[key]
                if isinstance(val, list) and len(val) > 0:
                    config[key] = np.array(val, dtype=float)

        config["seed"] = seed

        model = lc.run_model(paramfile=config, quiet=True)

        result = {
            "task_id": task_id,
            "seed": seed,
            "success": True,
            "reported_cases": np.array(model.results.reported_cases).tolist(),
            "reported_deaths": np.array(model.results.reported_deaths).tolist(),
        }

        del model
        gc.collect()
        return result

    except Exception as exc:  # noqa: BLE001
        gc.collect()
        return {
            "task_id": task_id,
            "seed": seed,
            "success": False,
            "error": str(exc),
            "traceback": tb.format_exc(),
        }
