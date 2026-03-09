"""
MOSAIC Dask Worker — Pure Python LASER simulation runner
=========================================================
Called by Dask workers to run individual LASER simulations.
No R dependency — uses laser_cholera.metapop.model directly.

This module is uploaded to all workers via client.upload_file() from the R
orchestrator (run_MOSAIC_dask), then imported on each worker with:
    import mosaic_dask_worker

Seed scheme (mirrors .mosaic_run_simulation_worker in R/run_MOSAIC.R):
    seed_ij = (sim_id - 1) * n_iterations + j
This ensures same sim_id always gets same parameters, and different
iterations within a sim get different stochastic LASER realizations.
"""

import copy
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

    base_config already contains numpy arrays for matrix fields (b_jt, psi_jt,
    etc.) because it was created via reticulate::r_to_py() and then scattered
    via client.scatter(). Only the sampled scalar/vector params (which arrive
    as JSON) need explicit numpy conversion.

    A deep copy of base_config is made to prevent mutation of the scattered
    object across simulations on the same worker.
    """
    config = copy.deepcopy(base_config)

    sampled = json.loads(sampled_params_json)

    # Convert list → numpy 1-D array for known vector fields
    for field, val in sampled.items():
        if field in _VECTOR_FIELDS and isinstance(val, (list, tuple)):
            sampled[field] = np.array(val, dtype=float)

    config.update(sampled)
    return config


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
        On success::

            {
                "sim_id": int,
                "success": True,
                "worker_elapsed_sec": float,
                "iterations": [
                    {
                        "j": int,                       # 1-based iteration index
                        "seed": int,                    # seed used for this iter
                        "expected_cases": list[list],   # shape: n_locs × n_time
                        "disease_deaths": list[list],   # shape: n_locs × n_time
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
                 "TBB_NUM_THREADS", "OPENBLAS_NUM_THREADS"):
        os.environ[_var] = "1"

    try:
        import laser_cholera.metapop.model as lc
        import warnings as _warnings
        _warnings.filterwarnings(
            "ignore", message="invalid value encountered in divide"
        )

        config = _apply_sampled_params(base_config, sampled_params_json)

        t_start = time.monotonic()
        iterations = []

        for j in range(1, n_iterations + 1):
            # Seed scheme: mirrors .mosaic_run_simulation_worker in R/run_MOSAIC.R
            seed_ij = (sim_id - 1) * n_iterations + j
            config["seed"] = seed_ij

            model = lc.run_model(paramfile=config, quiet=True)

            iterations.append({
                "j": j,
                "seed": seed_ij,
                # Convert numpy → nested Python lists for JSON-safe transport
                # back to R via reticulate/Dask gather.
                "expected_cases": np.array(
                    model.results.expected_cases
                ).tolist(),
                "disease_deaths": np.array(
                    model.results.disease_deaths
                ).tolist(),
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
