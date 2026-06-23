# =============================================================================
# test-dask_worker_score_window_parity.R
#
# Engine-backed parity test for the Dask worker's _score_window_likelihood()
# against the local-PSOCK R scorer contract (R/run_MOSAIC.R:387-529 +
# R/calc_model_likelihood.R). The worker must reproduce the SAME log-likelihood
# the local path computes for the same model/config, including:
#   * per-cell confidence weights (reported_cases_weight / reported_deaths_weight),
#   * the burn-in / deaths-prefix scored-window slice (score_idx_cases/deaths),
#   * the peak/cumulative shape terms.
#
# Regression guard for the v0.48.0 review finding: the v0.16.0 Python
# calc_model_likelihood matches epidemic_peaks by an integer `loc_idx` column,
# which the raw config["epidemic_peaks"] (iso_code/peak_date only) lacks. The
# worker must source peaks from model.params (engine-enriched with loc_idx) or
# every peak row is silently dropped and the shape terms diverge from local R.
#
# Skips cleanly when the engine is unavailable (CI without laser-cholera).
# =============================================================================

# WIS is intentionally NOT exercised here: laser-cholera v0.16.0's
# compute_wis_parametric_row uses np.sum (not np.nansum) on the MAE/interval
# terms, so a single NA observation poisons the whole WIS row to NaN and the
# term is dropped engine-side, while R's na.rm=TRUE keeps it. That is an engine
# bug (read-only repo), unrelated to the worker wiring under test.

test_that("Dask worker _score_window_likelihood matches local R scorer (weights + slice + shape)", {
  skip_if_no_python_likelihood()
  fx <- skip_if_no_data()

  if (!reticulate::py_module_available("pandas")) testthat::skip("pandas not available")
  np <- reticulate::import("numpy", convert = FALSE)
  lc <- tryCatch(reticulate::import("laser.cholera.metapop.model", convert = FALSE),
                 error = function(e) NULL)
  if (is.null(lc)) testthat::skip("laser.cholera.metapop.model not importable")

  worker_path <- system.file("python", "mosaic_dask_worker.py", package = "MOSAIC")
  if (!nzchar(worker_path)) worker_path <- "../../inst/python/mosaic_dask_worker.py"
  if (!file.exists(worker_path)) testthat::skip("mosaic_dask_worker.py not found")
  reticulate::source_python(worker_path)

  cfg <- MOSAIC::get_location_config(iso = c("ETH", "KEN"), config = fx$config)
  # Per-cell weight matrices ship with config_default v4.1+. Require them so the
  # weighted path is genuinely exercised; skip if an older config is installed.
  if (is.null(cfg$reported_cases_weight) || is.null(cfg$reported_deaths_weight)) {
    testthat::skip("config has no per-cell weight matrices (pre-v4.1 config_default)")
  }

  pycfg <- reticulate::r_to_py(cfg)
  m <- lc$run_model(paramfile = pycfg, quiet = TRUE)

  est_cases  <- reticulate::py_to_r(np$asarray(m$results$reported_cases))
  est_deaths <- reticulate::py_to_r(np$asarray(m$results$reported_deaths))
  obs_cases  <- cfg$reported_cases
  obs_deaths <- cfg$reported_deaths
  ntime <- ncol(obs_cases)

  # Use the engine-enriched peak set (iso_code/peak_date) on the R side so both
  # paths score the identical peaks; the loc_idx column lives on model.params.
  iso_v <- reticulate::py_to_r(m$params[["epidemic_peaks"]]$iso_code$tolist())
  pkd_v <- reticulate::py_to_r(m$params[["epidemic_peaks"]]$peak_date$astype("str")$tolist())
  ep_eng <- data.frame(
    iso_code  = as.character(iso_v),
    peak_date = substr(as.character(pkd_v), 1, 10),
    stringsAsFactors = FALSE
  )

  # --- Realistic case: burn-in slice s=20, deaths-prefix to idx_d=40,
  #     per-cell weights, peak-timing + peak-magnitude + cumulative shape terms.
  idx_c <- 20L; idx_d <- 40L; s <- 20L; keep <- s:ntime
  oc <- obs_cases[, keep, drop = FALSE]; ec <- est_cases[, keep, drop = FALSE]
  od <- obs_deaths[, keep, drop = FALSE]; ed <- est_deaths[, keep, drop = FALSE]
  wcl <- cfg$reported_cases_weight[, keep, drop = FALSE]
  wdl <- cfg$reported_deaths_weight[, keep, drop = FALSE]
  wdl[, seq_len(idx_d - s)] <- 0

  ds0 <- as.Date(cfg$date_start)
  cfgR <- cfg
  cfgR$date_start <- as.character(ds0 + (s - 1L))
  cfgR$epidemic_peaks <- MOSAIC:::.filter_epidemic_peaks(
    ep_eng, ds0 + (s - 1L), cfg$date_stop, cfg$location_name
  )

  ll_R <- MOSAIC::calc_model_likelihood(
    config = cfgR,
    obs_cases = oc, est_cases = ec, obs_deaths = od, est_deaths = ed,
    weights_obs_cases = wcl, weights_obs_deaths = wdl,
    weight_peak_timing = 0.3, weight_peak_magnitude = 0.2,
    weight_cumulative_total = 0.25
  )

  wcfg <- pycfg
  wcfg["reported_cases_weight"]  <- reticulate::r_to_py(cfg$reported_cases_weight)
  wcfg["reported_deaths_weight"] <- reticulate::r_to_py(cfg$reported_deaths_weight)
  wcfg["score_idx_cases"]  <- 20L
  wcfg["score_idx_deaths"] <- 40L
  wcfg["weight_peak_timing"]      <- 0.3
  wcfg["weight_peak_magnitude"]   <- 0.2
  wcfg["weight_cumulative_total"] <- 0.25
  ll_PY <- reticulate::py$`_score_window_likelihood`(wcfg, m)

  expect_true(is.finite(ll_R))
  expect_true(is.finite(ll_PY))
  expect_equal(ll_PY, ll_R, tolerance = 1e-6)
})

test_that("Dask worker passes loc_idx-bearing peaks so peak terms are non-zero", {
  skip_if_no_python_likelihood()
  fx <- skip_if_no_data()

  if (!reticulate::py_module_available("pandas")) testthat::skip("pandas not available")
  np <- reticulate::import("numpy", convert = FALSE)
  lc <- tryCatch(reticulate::import("laser.cholera.metapop.model", convert = FALSE),
                 error = function(e) NULL)
  if (is.null(lc)) testthat::skip("laser.cholera.metapop.model not importable")

  worker_path <- system.file("python", "mosaic_dask_worker.py", package = "MOSAIC")
  if (!nzchar(worker_path)) worker_path <- "../../inst/python/mosaic_dask_worker.py"
  if (!file.exists(worker_path)) testthat::skip("mosaic_dask_worker.py not found")
  reticulate::source_python(worker_path)

  cfg <- MOSAIC::get_location_config(iso = c("ETH", "KEN"), config = fx$config)
  pycfg <- reticulate::r_to_py(cfg)
  m <- lc$run_model(paramfile = pycfg, quiet = TRUE)

  # Peak terms ON, no slice. If the worker sourced peaks from the raw config
  # (no loc_idx), every peak row drops and the shape contribution collapses to
  # zero -> ll equals the peak-OFF likelihood. They must differ.
  base <- pycfg
  base["score_idx_cases"] <- 1L; base["score_idx_deaths"] <- 1L
  base["weight_peak_timing"] <- 0
  ll_off <- reticulate::py$`_score_window_likelihood`(base, m)

  withpk <- pycfg
  withpk["score_idx_cases"] <- 1L; withpk["score_idx_deaths"] <- 1L
  withpk["weight_peak_timing"] <- 1.0
  ll_on <- reticulate::py$`_score_window_likelihood`(withpk, m)

  expect_true(is.finite(ll_off))
  expect_true(is.finite(ll_on))
  expect_false(isTRUE(all.equal(ll_off, ll_on)))
})

test_that("Dask worker _apply_sampled_params preserves engine as_ndarray dtypes (local parity)", {
  # Regression for the v0.48.0 review finding: the engine's as_ndarray() returns
  # an incoming numpy array UNCHANGED (skipping its declared dtype cast) but
  # casts an incoming LIST to the declared dtype (uint32 for IC counts, float32
  # for seasonality/transmission vectors). The local PSOCK path ships these as
  # lists (r_to_py), so the engine seeds uint32 state; if the worker pre-converts
  # them to float64 numpy the engine seeds float64 state and the simulation
  # silently diverges from local by ~1-2% LL at config_default scale even with
  # identical values + seed. The worker must keep _AS_NDARRAY_FIELDS as lists.
  skip_if_no_python_likelihood()
  fx <- skip_if_no_data()
  if (!reticulate::py_module_available("numpy")) testthat::skip("numpy not available")
  lc <- tryCatch(reticulate::import("laser.cholera.metapop.model", convert = FALSE),
                 error = function(e) NULL)
  if (is.null(lc)) testthat::skip("laser.cholera.metapop.model not importable")

  worker_path <- system.file("python", "mosaic_dask_worker.py", package = "MOSAIC")
  if (!nzchar(worker_path)) worker_path <- "../../inst/python/mosaic_dask_worker.py"
  if (!file.exists(worker_path)) testthat::skip("mosaic_dask_worker.py not found")
  reticulate::source_python(worker_path)

  cfg <- MOSAIC::get_location_config(iso = c("ETH", "KEN"), config = fx$config)

  # Build the base_config + per-sim JSON exactly as the production Dask path does.
  ctrl <- MOSAIC:::mosaic_control_defaults()
  lik  <- ctrl$likelihood
  lik$.score_window_resolved <- MOSAIC:::.mosaic_resolve_score_window(cfg, ctrl)
  lik$.weights_time_resolved <- NULL
  lik$weights_location       <- NULL
  cfg_inj <- MOSAIC:::.mosaic_inject_likelihood_settings(cfg, lik)
  base_py <- reticulate::r_to_py(MOSAIC:::.extract_base_config(cfg_inj))

  ser <- MOSAIC:::.mosaic_sample_and_serialize(
    sim_id = 1L, PATHS = MOSAIC::get_paths(), priors = fx$priors,
    config = cfg, sampling_args = NULL
  )
  testthat::expect_null(ser$error)

  # Worker config (numpy/list mix) and the local-path config (r_to_py lists).
  config_w  <- reticulate::py$`_apply_sampled_params`(base_py, ser$json)
  config_lp <- reticulate::r_to_py(
    MOSAIC:::.mosaic_prepare_config_for_python(ser$params)
  )

  # Run the engine through dict_to_propertysetex on BOTH and compare the dtype
  # the engine ends up assigning to each as_ndarray field. They must match.
  params_mod <- reticulate::import("laser.cholera.metapop.params", convert = FALSE)
  as_ndarray_fields <- c(
    "N_j_initial", "S_j_initial", "E_j_initial", "I_j_initial", "R_j_initial",
    "V1_j_initial", "V2_j_initial", "tau_i", "beta_j0_hum",
    "a_1_j", "a_2_j", "b_1_j", "b_2_j", "beta_j0_env", "theta_j"
  )
  pw <- params_mod$dict_to_propertysetex(reticulate::py_to_r(config_w))
  pl <- params_mod$dict_to_propertysetex(reticulate::py_to_r(config_lp))
  for (fld in as_ndarray_fields) {
    dt_w <- reticulate::py_to_r(reticulate::py_get_attr(pw, fld)$dtype$name)
    dt_l <- reticulate::py_to_r(reticulate::py_get_attr(pl, fld)$dtype$name)
    expect_identical(dt_w, dt_l,
                     info = sprintf("engine dtype mismatch for %s: worker=%s local=%s",
                                    fld, dt_w, dt_l))
  }

  # End-to-end: identical seed -> bit-identical engine output across backends.
  e_w <- reticulate::py_run_string(paste0(
    "import numpy as _np\n",
    "import laser.cholera.metapop.model as _lc\n",
    "def _run(cfg, seed):\n",
    "    cfg=dict(cfg); cfg['seed']=int(seed); cfg['calc_likelihood']=False\n",
    "    m=_lc.run_model(paramfile=cfg, quiet=True)\n",
    "    return _np.asarray(m.results.reported_cases, dtype=float)\n"
  ), convert = TRUE)
  cases_w <- e_w$`_run`(config_w, 1L)
  cases_l <- e_w$`_run`(config_lp, 1L)
  expect_equal(max(abs(cases_w - cases_l)), 0)
})
