# MOSAIC 1.4.0 red team round 1: pipeline, reporting and consumer fixes
# (claude/redteam/ROUND1_DECISIONS.md, the "swe" items). The stub runs use
# helper-stub-run.R and skip on CRAN.

test_that("a run with fewer than two valid draws gets a RED status and both diagnostics JSONs (C2)", {
  skip_on_cran()
  skip_if_not_installed("arrow")
  local_test_root()
  d <- withr::local_tempdir()
  .stub_run_mosaic(d, mode = "fixed", n_sims = 40L, ll_mode = "onegood")
  sm <- jsonlite::read_json(file.path(d, "3_results", "summary.json"))
  expect_identical(sm$convergence_status, "red")
  expect_identical(sm$convergence_label, "Run failed: no usable draws")   # round 3: never "more simulations"
  expect_match(sm$convergence_reasons[[1]], "^the run failed: only 1 of 40 simulations")
  expect_identical(sm$ensemble_members, 1L)                     # RN5: the members it has, not null
  expect_true("fixed_budget" %in% unlist(sm$convergence_flags))
  expect_equal(sm$coarsening_alpha, 10)
  expect_true(is.finite(sm$eta)); expect_true(sm$n_obs_scored > 0)
  expect_true(file.exists(file.path(d, "3_results", "calibration_diagnostics.json")))
  expect_true(file.exists(file.path(d, "2_calibration", "diagnostics", "convergence_diagnostics.json")))
  cd <- jsonlite::read_json(file.path(d, "3_results", "calibration_diagnostics.json"))
  expect_identical(cd$status$convergence_status, "red")
  expect_length(cd$parameters, 0L)
  # the report is the real report, not the pre-1.4.0 notice
  expect_message(MOSAIC:::.mosaic_render_calibration_report(d, file.path(d, "fig"), verbose = TRUE),
                 "Calibration report \\([0-9]+ pages\\)")
})

test_that("calibration_diagnostics.json is assembled when no parameter varies or there is no status (C2)", {
  set.seed(1); ll <- stats::rnorm(200, -100, 5)
  wh <- calc_weight_health(ll, params = data.frame(p = rep(1, 200)), n_obs = 3000)
  cfg <- list(location_name = "MOZ", date_start = "2020-01-01", date_stop = "2020-12-31")
  ctl <- mosaic_control_defaults()
  rp <- MOSAIC:::.mosaic_ess_replay(ll, 1:200, 10 / 3000)
  for (h in list(wh, NULL)) {
    cd <- MOSAIC:::.mosaic_calibration_diagnostics(h, list(mode = "fixed"), cfg, ctl, 3000, rp, NULL)
    expect_identical(nrow(cd$parameters), 0L)
    f <- withr::local_tempfile(fileext = ".json")
    MOSAIC:::.mosaic_write_calibration_diagnostics(cd, f)
    expect_true(file.exists(f))
  }
  deg <- MOSAIC:::.mosaic_degenerate_health(1L, 40L, 3000, MOSAIC:::.mosaic_validate_and_merge_control(ctl),
                                            fixed_mode = TRUE)
  expect_identical(deg$status, "red")
  expect_true(deg$degenerate)
  f <- MOSAIC:::.mosaic_weight_health_fields(deg)
  expect_identical(f$convergence_status, "red")
  expect_equal(f$eta, 10 / 3000)
  expect_true("fixed_budget" %in% unlist(f$convergence_flags))
})

test_that("the adaptive gate scores the members of the configured scheme (D11)", {
  skip_on_cran()
  skip_if_not(nzchar(Sys.getenv("MOSAIC_RUN_INTEGRATION")),
              "set MOSAIC_RUN_INTEGRATION=1 to run the auto-mode stub runs")
  skip_if_not_installed("arrow")
  local_test_root()
  for (sch in c("coarsened", "saturated")) {
    d <- withr::local_tempdir()
    .stub_run_mosaic(d, weighting = sch, mode = "auto")
    sm <- jsonlite::read_json(file.path(d, "3_results", "summary.json"))
    h <- jsonlite::read_json(file.path(d, "2_calibration", "state", "run_state.json"))$convergence_history
    last <- h[[length(h)]]
    # the last check saw the whole pool, so it scored the members the run built
    expect_identical(last$n, sm$sims_used, info = sch)
    expect_equal(last$ess_best, sm$ess_best, tolerance = 1e-6, info = sch)
    expect_identical(last$status, sm$convergence_status, info = sch)
    # saturated: the gate's ensemble target is the tier target ESS_best (100 unless set)
    if (sch == "saturated") expect_equal(sm$ess_best_target, 100)
  }
})

test_that("the optimiser note explains a non-green status after the stopping rule (D11)", {
  wh <- list(status = "yellow",
             convergence = list(reasons = "ensemble ESS 150 of 200: run more simulations"))
  a <- MOSAIC:::.mosaic_note_optimised_status(wh, "bestfit", adaptive = TRUE, stopping_rule_met = TRUE)
  expect_length(a$convergence$reasons, 2L)
  expect_match(a$convergence$reasons[2], "stopping rule was met on the \"bestfit\" ensemble members")
  f <- MOSAIC:::.mosaic_note_optimised_status(wh, "bestfit", adaptive = FALSE, stopping_rule_met = FALSE)
  expect_match(f$convergence$reasons[2], "describes the members chosen by the subset optimiser")
  g <- MOSAIC:::.mosaic_note_optimised_status(list(status = "green", convergence = list(reasons = character(0))),
                                              "bestfit", TRUE, TRUE)
  expect_length(g$convergence$reasons, 0L)                 # green keeps no reasons
})

test_that("a fixed run's outputs are consistent and pruned (D7, D13, P10, P13, R10, R16, R20, C12)", {
  skip_on_cran()
  skip_if_not_installed("arrow")
  local_test_root()
  d <- withr::local_tempdir()
  .stub_run_mosaic(d, mode = "fixed")
  sm <- jsonlite::read_json(file.path(d, "3_results", "summary.json"))
  cd <- jsonlite::read_json(file.path(d, "3_results", "calibration_diagnostics.json"))
  # D13: every output carries the installed package version
  expect_identical(cd$run$mosaic_version, as.character(utils::packageVersion("MOSAIC")))
  # D7 / D2 fields in both files
  for (nm in c("ess_untempered_frac", "sims_projected_for_green", "projection_reliable",
               "n_locations", "alpha_per_location")) {
    expect_true(nm %in% names(sm), info = nm)
    expect_true(nm %in% names(cd$sampling), info = nm)
  }
  expect_identical(sm$n_locations, 1L)
  expect_equal(sm$alpha_per_location, sm$coarsening_alpha)
  # R10: no null legacy fields under bestfit
  for (gone in c("n_retained", "A_best_optimized", "ess_pct_above_target", "ess_n_above_target",
                 "r2_cases_ensemble_tier", "n_best_subset_tier"))
    expect_false(gone %in% names(sm), info = gone)
  diag <- jsonlite::read_json(file.path(d, "2_calibration", "diagnostics", "convergence_diagnostics.json"))
  expect_identical(diag$settings$ess_method, "kish")
  expect_identical(diag$metrics$khat$khat_status, sm$khat_status)
  expect_null(diag$metrics$param_ess$n_pass)
  # P13 / R16: run_state.json and _README.md report summary.json's status
  rs <- jsonlite::read_json(file.path(d, "2_calibration", "state", "run_state.json"))
  expect_null(rs$stopping_rule_met)
  expect_identical(rs$convergence_status, sm$convergence_status)
  readme <- readLines(file.path(d, "_README.md"))
  expect_true(any(grepl(sprintf("Simulation convergence:** %s", toupper(sm$convergence_status)), readme,
                        fixed = TRUE)))
  # P10: a fixed run keeps its number; R20: priors metadata nested
  ctl <- jsonlite::read_json(file.path(d, "1_inputs", "control.json"))$control
  expect_identical(ctl$calibration$n_simulations, 150L)
  post <- jsonlite::read_json(file.path(d, "2_calibration", "posterior", "posteriors.json"))
  expect_true("priors_metadata" %in% names(post$metadata))
  expect_null(post$metadata$version)
  expect_false(is.null(post$metadata$priors_metadata$version))
  # C12: no partial week in fit.weekly. Since 1.5.1 it holds every complete week
  # of the window; weeks touching the burn-in carry no model values and are
  # not scored.
  wk <- do.call(rbind, lapply(cd$fit$weekly, function(r) data.frame(
    loc = r$location, met = r$metric, week = as.Date(r$week), scored = isTRUE(r$scored),
    modelled = !is.null(r$predicted))))
  expect_true(all(wk$week >= as.Date(cd$run$date_start) & wk$week + 6 <= as.Date(cd$run$date_stop)))
  start_scored <- as.Date(cd$run$date_start) + cd$run$burn_in_days
  first <- tapply(wk$week[wk$modelled | wk$scored], paste(wk$loc, wk$met)[wk$modelled | wk$scored], min)
  expect_true(all(as.Date(first, origin = "1970-01-01") >= start_scored))
  expect_false(any(wk$scored & wk$week < start_scored))
  # R9: run log
  log <- readLines(file.path(d, "run.log"))
  rs_line <- grep("[RUN_SUMMARY]", log, fixed = TRUE, value = TRUE)
  expect_false(grepl("posthoc_criteria_met", rs_line))
  expect_true(any(grepl("informativeness:", log, fixed = TRUE)))
  expect_false(any(grepl("subset_optimization: artifact not found", log, fixed = TRUE)))
})

test_that("an adaptive run writes n_simulations as JSON null in control.json (P10)", {
  d <- withr::local_tempdir()
  ctl <- MOSAIC:::.mosaic_control_for_json(mosaic_control_defaults())
  f <- file.path(d, "control.json")
  MOSAIC:::.mosaic_write_json(list(control = ctl), f, list(), na = "null")
  txt <- paste(readLines(f), collapse = "\n")
  expect_match(txt, "\"n_simulations\": null")
  expect_false(grepl("\"n_simulations\": \\{\\}", txt))
  expect_null(jsonlite::read_json(f)$control$calibration$n_simulations)
})

test_that("a partial batch after the last check is its own batch on resume (C14)", {
  skip_if_not_installed("arrow")
  control <- MOSAIC:::.mosaic_validate_and_merge_control(mosaic_control_defaults(
    calibration = list(batch_size_adaptive = 20L, min_batches_adaptive = 1L, min_draws = 0L,
                       max_simulations_total = 1000L)))
  dirs <- list(cal_samples = file.path(withr::local_tempdir(), "samples"))
  dirs$cal_state <- file.path(dirname(dirs$cal_samples), "state")
  dir.create(dirs$cal_samples, recursive = TRUE); dir.create(dirs$cal_state)
  set.seed(3)
  w <- function(ids) arrow::write_parquet(
    data.frame(sim = ids, likelihood = stats::rnorm(length(ids), -100, 3), a = stats::runif(length(ids))),
    file.path(dirs$cal_samples, sprintf("sim_%07d-%07d.parquet", min(ids), max(ids))))
  w(1:20); w(21:40)
  st <- MOSAIC:::.mosaic_init_state(control, "a", list(mode = "auto", fixed_target = NA_integer_))
  st$batch_number <- 2L; st$total_sims_run <- 40L; st$batch_sizes_used <- c(20L, 20L)
  pool <- MOSAIC:::.mosaic_gate_pool_update(NULL, dirs$cal_samples, "a")
  st <- MOSAIC:::.mosaic_convergence_check_update_state(st, dirs, "a", control, 1000, results = pool$data,
                                                        log = FALSE)
  MOSAIC:::.mosaic_save_checkpoint(st, file.path(dirs$cal_state, "resume_checkpoint.rds"))
  w(41:50)                                                      # a partial third batch, then killed
  st2 <- MOSAIC:::.mosaic_init_state(control, "a", list(mode = "auto", fixed_target = NA_integer_))
  st2 <- suppressMessages(MOSAIC:::.mosaic_reconstruct_state(st2, dirs, control, "a", 1000))
  b <- vapply(st2$convergence_history, function(e) e$batch, 1L)
  expect_false(anyDuplicated(b) > 0L)
  expect_identical(st2$batch_number, 3L)
})

test_that("the sensitivity and correlation figures leave out derived compartments (R14)", {
  nm <- c("beta_j0_tot_MOZ", "beta_j0_env_MOZ", "prop_S_initial_MOZ", "S_j_initial_MOZ",
          "V1_j_initial_MOZ", "rho", "sigma")
  keep <- MOSAIC:::.mosaic_weight_health_param_cols(nm)
  expect_identical(keep, c("beta_j0_tot_MOZ", "prop_S_initial_MOZ", "rho", "sigma"))
})

test_that("the backfill reproduces a coarsened run's own summary (round 3)", {
  skip_on_cran()
  skip_if_not_installed("arrow")
  local_test_root()
  d <- withr::local_tempdir()
  .stub_run_mosaic(d, weighting = "coarsened", mode = "fixed")
  sm <- jsonlite::read_json(file.path(d, "3_results", "summary.json"))
  bf <- calc_run_weight_health(d, write = FALSE)$fields
  for (k in c("convergence_status", "ess_all", "ess_all_target", "ess_best", "ensemble_members",
              "mce_p90", "sims_projected_for_green", "projection_reliable"))
    expect_equal(bf[[k]] %||% NA, sm[[k]] %||% NA, tolerance = 1e-6, info = k)
  expect_equal(sm$ess_all_target, sm$ess_best_target)        # coarsened: the scheme's own target
})
