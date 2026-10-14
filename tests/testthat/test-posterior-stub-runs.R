# End-to-end posterior regressions on a stubbed single-location run_MOSAIC()
# (helper-stub-run.R). Each run is ~30 s, so they skip on CRAN. The auto-mode
# run is longer and needs MOSAIC_RUN_INTEGRATION=1, like
# test-run_MOSAIC_integration.R.

test_that("best_subset_weighting = 'saturated' reproduces v1.2.1 exactly: posterior, ensembles, medoid", {
  skip_on_cran()
  skip_if_not_installed("arrow")
  local_test_root()
  # Produced by this exact run on MOSAIC 1.2.1 (ba71c4e7d), where "saturated"
  # was the default: claude/weights_investigation/scripts/19_golden_stub_fixture.R.
  golden <- readRDS(test_path("fixtures", "stub_posterior_golden_v121.rds"))
  d <- withr::local_tempdir()
  # v1.2.1 defaulted optimize_subset to TRUE (1.2.0); 1.3.0 reverts it, so the
  # reproduction asks for it explicitly.
  .stub_run_mosaic(d, weighting = "saturated", mode = "fixed", optimize_subset = TRUE)
  snap <- .stub_posterior_snapshot(d)
  for (k in c("samples", "quantiles", "posteriors", "ensemble_candidate",
              "ensemble_optimized", "medoid_seed"))
    expect_identical(snap[[k]], golden[[k]], info = k)
})

test_that("the default coarsened posterior runs end to end and is internally coherent (fixed mode)", {
  skip_on_cran()
  skip_if_not_installed("arrow")
  local_test_root()
  d <- withr::local_tempdir()
  .stub_run_mosaic(d, weighting = NULL, mode = "fixed")
  smp <- as.data.frame(arrow::read_parquet(file.path(d, "2_calibration", "samples.parquet")))
  sm <- jsonlite::read_json(file.path(d, "3_results", "summary.json"))
  ctl <- jsonlite::read_json(file.path(d, "1_inputs", "control.json"))$control
  expect_identical(ctl$targets$best_subset_weighting, "coarsened")
  expect_false(isTRUE(ctl$predictions$optimize_subset))          # default since 1.3.0
  expect_identical(sm$posterior_weighting, "coarsened")
  expect_identical(sm$weights_used_column, "weight_best")
  expect_false(isTRUE(any(smp$is_best_subset_opt)))               # no optimiser subset

  # weight_all is the coarsened posterior at eta = alpha / n_obs over every finite draw
  eta <- sm$coarsening_alpha / sm$n_obs_scored
  expect_equal(sm$eta_ref, eta, tolerance = 1e-12)
  expect_equal(smp$weight_all, MOSAIC:::.mosaic_tempered_weights(smp$likelihood, eta), tolerance = 1e-14)
  expect_equal(sum(smp$weight_all), 1, tolerance = 1e-12)
  expect_equal(sm$ess_coarsened, 1 / sum(smp$weight_all^2), tolerance = 1e-6)

  # members: systematic resample, count / K weights, duplicates collapsed
  K <- sm$resample_K
  expect_identical(K, as.integer(min(max(round(sm$ess_coarsened), 10L), 60L)))
  expect_identical(smp$is_best_subset, smp$weight_best > 0)
  expect_equal(sum(smp$weight_best), 1, tolerance = 1e-12)
  expect_equal(smp$weight_best * K, round(smp$weight_best * K), tolerance = 1e-9)
  expect_identical(sum(smp$is_best_subset), sm$resample_n_unique)
  cp <- MOSAIC:::.mosaic_coarsened_posterior(smp$likelihood, sm$n_obs_scored, sm$coarsening_alpha,
                                             10L, 60L, MOSAIC:::.mosaic_resample_seed(
                                               jsonlite::read_json(file.path(d, "1_inputs", "config.json"))))
  expect_equal(smp$weight_best, cp$member_weights)

  # the posterior is built from exactly the prediction members: the parameter
  # sets behind the posterior quantiles are the ensemble members, at their
  # count weights, not all finite draws
  ens <- readRDS(file.path(d, "2_calibration", "ensemble_candidate.rds"))
  mem <- smp[smp$is_best_subset, ]
  expect_identical(sort(as.numeric(ens$seeds)), sort(as.numeric(mem$seed_sim %||% mem$sim)))
  expect_equal(ens$parameter_weights, mem$weight_best / sum(mem$weight_best))
  q <- utils::read.csv(file.path(d, "2_calibration", "posterior", "posterior_quantiles.csv"),
                       stringsAsFactors = FALSE)
  qp <- q[q$type == "posterior" & q$parameter == "beta_j0_tot_MOZ", ]
  expect_equal(qp$q0.5, weighted_quantiles(mem$beta_j0_tot_MOZ, mem$weight_best, 0.5),
               tolerance = 1e-10)
  expect_equal(qp$mean, sum(mem$beta_j0_tot_MOZ * mem$weight_best) / sum(mem$weight_best),
               tolerance = 1e-10)
  fin <- is.finite(smp$likelihood)
  expect_false(isTRUE(all.equal(qp$q0.5,
                                weighted_quantiles(smp$beta_j0_tot_MOZ[fin], smp$weight_all[fin], 0.5))))
  pe <- utils::read.csv(file.path(d, "3_results", "posterior", "parameter_estimates.csv"))
  expect_equal(pe$median[pe$parameter == "beta_j0_tot_MOZ"], qp$q0.5)
  med_line <- grep("Medoid: param set", readLines(file.path(d, "run.log")), value = TRUE)
  expect_length(med_line, 1L)
  med_seed <- as.numeric(sub(".*seed=([0-9]+).*", "\\1", med_line))
  expect_true(med_seed %in% ens$seeds)

  # verdict: posthoc_criteria_met is the coarsened posterior's weight health
  expect_identical(sm$posthoc_criteria_met, identical(sm$weight_health_status, "ok"))
  diag <- jsonlite::read_json(file.path(d, "2_calibration", "diagnostics", "convergence_diagnostics.json"))
  expect_identical(diag$settings$weighting, "coarsened")
  expect_false(is.null(diag$metrics$ess_coarsened))
  expect_identical(diag$metrics$ess_best$status, "info")
  st <- utils::read.csv(file.path(d, "3_results", "figures", "diagnostics", "convergence_status.csv"),
                        stringsAsFactors = FALSE)
  expect_true(all(c("Posterior ESS", "Seed-batch ratio") %in% st$metric))
  # the per-parameter ESS is on the all-draws coarsened posterior and bounded by its Kish ESS
  pe_ess <- utils::read.csv(file.path(d, "2_calibration", "diagnostics", "parameter_ess.csv"))
  expect_true(all(pe_ess$ess_marginal <= sm$ess_coarsened + 1e-6))
})

test_that("coarsened + optimize_subset = TRUE: the posterior is the optimised subset the predictions use", {
  skip_on_cran()
  skip_if_not_installed("arrow")
  local_test_root()
  d <- withr::local_tempdir()
  .stub_run_mosaic(d, weighting = NULL, mode = "fixed", optimize_subset = TRUE)
  smp <- as.data.frame(arrow::read_parquet(file.path(d, "2_calibration", "samples.parquet")))
  sm <- jsonlite::read_json(file.path(d, "3_results", "summary.json"))
  expect_true(any(smp$is_best_subset_opt))
  expect_true(all(smp$sim[smp$is_best_subset_opt] %in% smp$sim[smp$is_best_subset]))
  expect_identical(sm$weights_used_column, "weight_best_opt")
  ens_opt <- readRDS(file.path(d, "2_calibration", "ensemble_optimized.rds"))
  opt <- smp[smp$is_best_subset_opt, ]
  expect_identical(sort(as.numeric(ens_opt$seeds)), sort(as.numeric(opt$seed_sim %||% opt$sim)))
  q <- utils::read.csv(file.path(d, "2_calibration", "posterior", "posterior_quantiles.csv"),
                       stringsAsFactors = FALSE)
  qp <- q[q$type == "posterior" & q$parameter == "beta_j0_tot_MOZ", ]
  expect_equal(qp$q0.5, weighted_quantiles(opt$beta_j0_tot_MOZ, opt$weight_best_opt, 0.5),
               tolerance = 1e-10)
  med_line <- grep("Medoid: param set", readLines(file.path(d, "run.log")), value = TRUE)
  med_seed <- as.numeric(sub(".*seed=([0-9]+).*", "\\1", med_line))
  expect_true(med_seed %in% ens_opt$seeds)
})

test_that("auto mode under the coarsened posterior runs end to end on the new stopping rule", {
  skip_on_cran()
  skip_if_not(nzchar(Sys.getenv("MOSAIC_RUN_INTEGRATION")),
              "set MOSAIC_RUN_INTEGRATION=1 to run the auto-mode stub run")
  skip_if_not_installed("arrow")
  local_test_root()
  d <- withr::local_tempdir()
  .stub_run_mosaic(d, weighting = NULL, mode = "auto")
  sm <- jsonlite::read_json(file.path(d, "3_results", "summary.json"))
  expect_identical(sm$mode, "auto")
  expect_true(isTRUE(sm$convergence_evaluated))
  expect_true(is.logical(sm$converged))
  rs <- jsonlite::read_json(file.path(d, "2_calibration", "state", "run_state.json"))
  expect_match(rs$gate_metric, "^fs_ess\\+batch_ratio/v1.3.0/coarsened/alpha=10$")
  log <- readLines(file.path(d, "run.log"))
  expect_true(any(grepl("Parameter ESS (function-specific, coarsened posterior)", log, fixed = TRUE)))
  expect_true(any(grepl("Seed-batch ratio:", log, fixed = TRUE)))
  expect_false(any(grepl("ESS = .* sqrt_sims", log)))
  # if it converged, both criteria held at the stop
  if (isTRUE(sm$converged)) expect_true(any(grepl("CONVERGENCE ACHIEVED", log, fixed = TRUE)))
  expect_true(file.exists(file.path(d, "2_calibration", "ensemble_optimized.rds")))
})
