# End-to-end posterior regressions on a stubbed single-location run_MOSAIC()
# (helper-stub-run.R). Each run is ~30 s, so they skip on CRAN. The auto-mode
# run is longer and needs MOSAIC_RUN_INTEGRATION=1, like
# test-run_MOSAIC_integration.R.

# Golden comparison. The member sets, seeds and every non-double value must be
# identical; doubles must agree to a relative 1e-8. The fixtures were made on
# macOS arm64, and other platforms' libm differs in the last bits (CI: up to
# ~1e-12 relative in the likelihoods, ~5e-12 in the weights), which no
# methodology change could hide inside. posteriors.json holds distributions
# fitted by an optimiser to the posterior quantiles, which magnifies those
# differences (CI Ubuntu: 5e-8 relative in Beta shapes), so it is compared to a
# relative 1e-6.
.expect_golden <- function(actual, expected, info = NULL, tolerance = 1e-8) {
  expect_equal(actual, expected, tolerance = tolerance, info = info)
  if (is.data.frame(expected) && "is_best_subset" %in% names(expected))
    expect_identical(actual$is_best_subset, expected$is_best_subset, info = info)
}

test_that("posterior_weighting = 'saturated' reproduces v1.2.1: posterior, ensembles, medoid", {
  skip_on_cran()
  skip_if_not_installed("arrow")
  local_test_root()
  # Produced by this exact run on MOSAIC 1.2.1 (ba71c4e7d), where "saturated"
  # was the default: claude/weights_investigation/scripts/19_golden_stub_fixture.R.
  golden <- readRDS(test_path("fixtures", "stub_posterior_golden_v121.rds"))
  # Both ways of asking for "saturated": through mosaic_control_defaults(targets =)
  # and by assignment into the full defaults (lesson #13, red team round 2 CN1).
  for (assign in c(FALSE, TRUE)) {
  d <- withr::local_tempdir()
  # v1.2.1 defaulted optimize_subset to TRUE (1.2.0); 1.3.0 reverts it, so the
  # reproduction asks for it explicitly.
  .stub_run_mosaic(d, weighting = "saturated", mode = "fixed", optimize_subset = TRUE, assign_targets = assign)
  snap <- .stub_posterior_snapshot(d)
  for (k in c("samples", "posteriors", "ensemble_candidate", "ensemble_optimized", "medoid_seed"))
    .expect_golden(snap[[k]], golden[[k]], info = paste(k, if (assign) "(assignment)" else "(targets =)"),
                   tolerance = if (k == "posteriors") 1e-6 else 1e-8)
  # Quantiles and moments match; the KL column is a descriptive whose estimator
  # changed in 1.4.0 (percentile scale replaced the weighted KDE).
  keep <- setdiff(names(golden$quantiles), "kl")
  .expect_golden(snap$quantiles[keep], golden$quantiles[keep])
  kl <- snap$quantiles$kl[snap$quantiles$type == "posterior"]
  expect_true(all(is.na(kl) | (kl >= 0 & kl <= log(20))))
  }
})

test_that("posterior_weighting = 'coarsened' reproduces v1.3.0: posterior, ensembles, medoid", {
  skip_on_cran()
  skip_if_not_installed("arrow")
  local_test_root()
  # Produced by this run on MOSAIC 1.3.0 (7ccdb0caf), where "coarsened" was the
  # default and K = clamp(round(ESS), min_best_subset 10, min(max_best_subset 60,
  # max_ensemble_members 300)): claude/plan_convergence/ws2_golden_v130.R. In
  # 1.4.0 K is bounded by min/max_ensemble_members only.
  golden <- readRDS(test_path("fixtures", "stub_posterior_golden_v130.rds"))
  d <- withr::local_tempdir()
  .stub_run_mosaic(d, weighting = "coarsened", mode = "fixed",
                   targets_extra = list(min_ensemble_members = 10L, max_ensemble_members = 60L))
  snap <- .stub_posterior_snapshot(d)
  for (k in c("samples", "posteriors", "ensemble_candidate", "ensemble_optimized", "medoid_seed"))
    .expect_golden(snap[[k]], golden[[k]], info = k, tolerance = if (k == "posteriors") 1e-6 else 1e-8)
  keep <- setdiff(names(golden$quantiles), "kl")
  .expect_golden(snap$quantiles[keep], golden$quantiles[keep])
  sm <- jsonlite::read_json(file.path(d, "3_results", "summary.json"))
  expect_identical(sm$posterior_weighting, "coarsened")
  # ensemble_members is the resample size K, ensemble_members_unique its unique draws
  expect_identical(sm$ensemble_members_unique, sum(golden$samples$is_best_subset))
  expect_gte(sm$ensemble_members, sm$ensemble_members_unique)
  K <- sm$ensemble_members
  expect_equal(golden$samples$weight_best * K, round(golden$samples$weight_best * K), tolerance = 1e-9)
  expect_true(file.exists(file.path(d, "2_calibration", "diagnostics", "subset_selection_summary.csv")))
})

test_that("the default best-fit ensemble runs end to end, coherent, schema 2 (fixed mode)", {
  skip_on_cran()
  skip_if_not_installed("arrow")
  local_test_root()
  d <- withr::local_tempdir()
  .stub_run_mosaic(d, weighting = NULL, mode = "fixed")
  smp <- as.data.frame(arrow::read_parquet(file.path(d, "2_calibration", "samples.parquet")))
  sm <- jsonlite::read_json(file.path(d, "3_results", "summary.json"))
  ctl <- jsonlite::read_json(file.path(d, "1_inputs", "control.json"))$control
  expect_identical(ctl$targets$posterior_weighting, "bestfit")
  expect_null(ctl$targets$best_subset_weighting)
  expect_false(isTRUE(ctl$predictions$optimize_subset))
  expect_identical(sm$schema_version, "2")
  expect_identical(sm$posterior_weighting, "bestfit")
  expect_true(sm$convergence_status %in% c("green", "yellow", "red"))
  expect_true("stopping_rule_met" %in% names(sm)); expect_null(sm$stopping_rule_met)   # fixed: null
  expect_true("fixed_budget" %in% unlist(sm$convergence_flags))
  expect_false(isTRUE(any(smp$is_best_subset_opt)))

  # weight_all is the coarsened posterior over every finite draw
  eta <- sm$coarsening_alpha / sm$n_obs_scored
  expect_equal(sm$eta, eta, tolerance = 1e-12)
  expect_equal(smp$weight_all, MOSAIC:::.mosaic_tempered_weights(smp$likelihood, eta), tolerance = 1e-14)
  expect_equal(sm$ess_all, 1 / sum(smp$weight_all^2), tolerance = 1e-6)

  # members: the best-fit selection, carrying their renormalised coarsened weights
  sel <- MOSAIC:::.mosaic_bestfit_select(smp$likelihood, eta, 0.2, 30L, 1000L, sim = smp$sim)
  expect_identical(sort(which(smp$is_best_subset)), sort(sel$idx))
  expect_equal(smp$weight_best[sel$idx], sel$weights, tolerance = 1e-14)
  expect_equal(sum(smp$weight_best), 1, tolerance = 1e-12)
  expect_identical(sm$ensemble_members, sel$n_members)
  expect_equal(sm$ess_best, sel$ess_best, tolerance = 1e-8)
  expect_identical(sm$ensemble_binding, sel$binding)
  # legacy computations do not run under the default
  expect_false("weight_retained" %in% names(smp))
  expect_identical(smp$is_retained, smp$is_valid)
  expect_false(any(smp$is_outlier))
  expect_false(file.exists(file.path(d, "2_calibration", "diagnostics", "subset_selection_summary.csv")))

  # the posterior is built from exactly the prediction members
  ens <- readRDS(file.path(d, "2_calibration", "ensemble_candidate.rds"))
  mem <- smp[smp$is_best_subset, ]
  expect_identical(sort(as.numeric(ens$seeds)), sort(as.numeric(mem$seed_sim)))
  expect_equal(ens$parameter_weights, mem$weight_best / sum(mem$weight_best))
  q <- utils::read.csv(file.path(d, "2_calibration", "posterior", "posterior_quantiles.csv"),
                       stringsAsFactors = FALSE)
  qp <- q[q$type == "posterior" & q$parameter == "beta_j0_tot_MOZ", ]
  expect_equal(qp$q0.5, weighted_quantiles(mem$beta_j0_tot_MOZ, mem$weight_best, 0.5), tolerance = 1e-10)
  expect_equal(qp$mean, sum(mem$beta_j0_tot_MOZ * mem$weight_best), tolerance = 1e-10)
  pe <- utils::read.csv(file.path(d, "3_results", "posterior", "parameter_estimates.csv"))
  expect_equal(pe$median[pe$parameter == "beta_j0_tot_MOZ"], qp$q0.5)
  med_line <- grep("Medoid: param set", readLines(file.path(d, "run.log")), value = TRUE)
  expect_length(med_line, 1L)
  expect_true(as.numeric(sub(".*seed=([0-9]+).*", "\\1", med_line)) %in% ens$seeds)

  # aliases (schema 2 section 1 and 5)
  expect_identical(sm$converged, FALSE)
  expect_identical(sm$convergence_evaluated, FALSE)
  expect_identical(sm$posthoc_criteria_met, identical(sm$convergence_status, "green"))
  expect_identical(sm$weight_health_status, if (identical(sm$convergence_status, "green")) "ok" else "warn")
  expect_equal(sm$ess_coarsened, sm$ess_all)
  expect_identical(sm$n_best_subset, sm$ensemble_members)
  expect_identical(sm$ess_min, sm$param_ess_min)
  expect_identical(sm$ess_median, sm$param_ess_median)
  expect_identical(sm$ess_target, sm$ess_best_target)
  for (gone in c("weights_used_column", "eta_ref", "batch_ratio_coarsened_max", "ess_coarsened_prop",
                 "weight_health_reasons", "likelihood_concentration_note"))
    expect_false(gone %in% names(sm), info = gone)

  # parameter_ess.csv: the members' weights, ess + deprecated ess_marginal
  pe_ess <- utils::read.csv(file.path(d, "2_calibration", "diagnostics", "parameter_ess.csv"),
                            stringsAsFactors = FALSE)
  expect_identical(names(pe_ess), c("parameter", "type", "iso_code", "ess", "ess_marginal"))
  expect_identical(pe_ess$ess, pe_ess$ess_marginal)
  ref <- calc_model_ess_parameter(smp, pe_ess$parameter, weights = smp$weight_best)
  expect_equal(pe_ess$ess, ref$ess_marginal, tolerance = 1e-10)
  expect_equal(sm$param_ess_min, round(min(pe_ess$ess), 1))

  # convergence_results.parquet: is_member replaces retained; no fence columns
  cr <- as.data.frame(arrow::read_parquet(file.path(d, "2_calibration", "diagnostics",
                                                    "convergence_results.parquet")))
  expect_true("is_member" %in% names(cr))
  expect_false(any(c("retained", "is_retained", "w_retained") %in% names(cr)))
  expect_identical(cr$is_member, smp$is_best_subset)

  # convergence_diagnostics.json keeps its summary block, mirroring summary.json
  diag <- jsonlite::read_json(file.path(d, "2_calibration", "diagnostics", "convergence_diagnostics.json"))
  expect_identical(diag$summary$convergence_status, sm$convergence_status)
  expect_identical(diag$settings$weighting, "bestfit")
  # the pre-1.4.0 status table is no longer written (calibration report instead)
  expect_false(file.exists(file.path(d, "3_results", "figures", "diagnostics", "convergence_status.csv")))

  # calibration_diagnostics.json (written with plots = FALSE), SCHEMA_v2 section 2
  cd <- jsonlite::read_json(file.path(d, "3_results", "calibration_diagnostics.json"))
  expect_identical(names(cd), c("schema_version", "schema_revision", "run", "status", "sampling", "info",
                                "parameters", "mce_summary", "kl_summary", "fit", "likelihood_concentration"))
  expect_identical(cd$schema_revision, "1.5.1")
  expect_true(all(c("mosaic_version", "locations", "n_locations", "mode", "posterior_weighting",
                    "coarsening_alpha", "eta", "n_obs_scored", "burn_in_days", "n_iterations",
                    "n_iter_ensemble", "date_start", "date_stop") %in% names(cd$run)))
  expect_identical(unlist(cd$run$locations), "MOZ")
  expect_true(all(c("convergence_status", "convergence_label", "reasons", "flags", "flags_text",
                    "flags_detail", "stopping_rule_met", "checks", "thresholds", "headline_display",
                    "palette") %in% names(cd$status)))
  expect_true(all(c("max_simulations_total", "min_ensemble_members", "max_ensemble_members",
                    "central_method", "glossary_version") %in% names(cd$run)))
  expect_identical(cd$status$convergence_status, sm$convergence_status)
  expect_identical(cd$status$flags_text, sm$convergence_flags_text)
  expect_null(cd$status$stopping_rule_met)
  expect_identical(vapply(cd$status$checks, function(x) x$check, ""), c("ess", "mce", "khat", "informativeness"))
  expect_identical(names(cd$status$checks[[1]]),
                   c("check", "label", "value", "threshold_green", "threshold_yellow", "colour", "reason",
                     "key", "operator", "operator_yellow", "value_display", "threshold_green_display",
                     "threshold_yellow_display", "target_display", "target_note", "status_display"))
  samp <- c("n_locations", "alpha_per_location", "ess_untempered_frac", "sims_projected_for_green",
            "projection_reliable", "ess_all", "ess_all_target", "ess_best", "ess_best_target", "best_subset_ratio",
            "best_subset_mass", "ensemble_members", "ensemble_members_unique", "ensemble_binding",
            "khat_tempered", "khat_status", "eta_star", "eta_star_n_eff", "flat_likelihood",
            "ess_growth_slope", "sims_used", "sims_projected_for_target", "budget_exhausted",
            "ess_path", "loglik_path", "details_display")
  expect_identical(names(cd$sampling), samp)
  expect_lte(length(cd$sampling$ess_path), 60L)
  expect_identical(names(cd$sampling$ess_path[[1]]), c("n", "ess_all"))
  expect_identical(names(cd$sampling$loglik_path[[1]]), c("n", "best_gap", "top100_gap"))
  expect_true(is.finite(sm$ess_growth_slope))                  # fixed run: slope from the replay
  expect_identical(names(cd$parameters[[1]]),
                   c("parameter", "base", "location", "prior_family", "mce", "kl", "kl_band"))
  expect_true(all(vapply(cd$parameters, function(x) x$kl_band %||% NA_character_, "") %in%
                    c("largely prior", "partly informed", "data-informed", NA)))
  expect_equal(cd$mce_summary$p90, sm$mce_p90, tolerance = 1e-3)   # 4 significant digits in the JSON
  expect_identical(unlist(cd$kl_summary$band_edges), c(0.05, 0.5))
  expect_identical(names(cd$fit), c("table", "per_location", "weekly", "weekly_meta", "table_display"))
  expect_identical(cd$fit$weekly_meta$source, "trajectories")
  expect_identical(names(cd$info), c("ess_growth", "budget", "ensemble_size"))
  expect_identical(vapply(cd$fit$table, function(x) x$metric, ""), c("cases", "deaths"))
  # D8: the primary rows use every scored day, as summary.json does (the stub
  # config carries no tier, so there is no tier 1-2 row)
  expect_identical(vapply(cd$fit$table, function(x) x$subset, ""), c("scored", "scored"))
  expect_equal(cd$fit$table[[1]]$r2, sm$r2_cases_ensemble, tolerance = 1e-3)
  expect_equal(cd$fit$table[[1]]$bias, sm$bias_ratio_cases_ensemble, tolerance = 1e-3)
  expect_equal(cd$fit$table[[2]]$bias, sm$bias_ratio_deaths_ensemble, tolerance = 1e-3)
  expect_identical(names(cd$fit$weekly[[1]]),
                   c("location", "metric", "week", "scored", "observed", "predicted", "lo50", "hi50", "lo95", "hi95"))
  expect_identical(weekdays(as.Date(cd$fit$weekly[[1]]$week)), weekdays(as.Date("2026-10-19")))  # Monday

  # the report re-renders from the JSON of this real run (plots = FALSE wrote none)
  expect_false(file.exists(file.path(d, "3_results", "figures", "diagnostics", "calibration_report.pdf")))
  pdf <- MOSAIC:::.mosaic_render_calibration_report(d, file.path(d, "3_results", "figures", "diagnostics"))
  expect_true(file.exists(pdf))
  expect_gt(file.size(pdf), 20000)

  # run log: one status, no legacy vocabulary
  log <- readLines(file.path(d, "run.log"))
  expect_true(any(grepl("Convergence status: ", log, fixed = TRUE)))
  expect_true(any(grepl("[RUN_SUMMARY] status=completed_fixed mode=fixed convergence_status=", log, fixed = TRUE)))
  # ("reported_tier", the surveillance tier, is unrelated to the legacy tier search)
  for (bad in c("Akaike", "\\btier\\b", "\\bretained\\b", "[Ff]unction-specific", "Converged: NO", "ESS gap",
                "posthoc_criteria_met", "subset_optimization: artifact not found"))
    expect_false(any(grepl(bad, log, perl = TRUE)), info = bad)
})

test_that("best-fit + optimize_subset = TRUE: the posterior and the status are the optimised subset", {
  skip_on_cran()
  skip_if_not_installed("arrow")
  local_test_root()
  d <- withr::local_tempdir()
  .stub_run_mosaic(d, weighting = NULL, mode = "fixed", optimize_subset = TRUE,
                   targets_extra = list(min_ensemble_members = 40L))
  smp <- as.data.frame(arrow::read_parquet(file.path(d, "2_calibration", "samples.parquet")))
  sm <- jsonlite::read_json(file.path(d, "3_results", "summary.json"))
  expect_true(any(smp$is_best_subset_opt))
  expect_true(all(smp$sim[smp$is_best_subset_opt] %in% smp$sim[smp$is_best_subset]))
  ens_opt <- readRDS(file.path(d, "2_calibration", "ensemble_optimized.rds"))
  opt <- smp[smp$is_best_subset_opt, ]
  expect_identical(sort(as.numeric(ens_opt$seeds)), sort(as.numeric(opt$seed_sim)))
  q <- utils::read.csv(file.path(d, "2_calibration", "posterior", "posterior_quantiles.csv"),
                       stringsAsFactors = FALSE)
  qp <- q[q$type == "posterior" & q$parameter == "beta_j0_tot_MOZ", ]
  expect_equal(qp$q0.5, weighted_quantiles(opt$beta_j0_tot_MOZ, opt$weight_best_opt, 0.5), tolerance = 1e-10)
  expect_identical(sm$ensemble_members, nrow(opt))
  expect_equal(sm$ess_best, calc_model_ess(opt$weight_best_opt / sum(opt$weight_best_opt), "kish"),
               tolerance = 1e-8)
  expect_identical(sm$n_best_subset_tier, sum(smp$is_best_subset))
  med_line <- grep("Medoid: param set", readLines(file.path(d, "run.log")), value = TRUE)
  expect_true(as.numeric(sub(".*seed=([0-9]+).*", "\\1", med_line)) %in% ens_opt$seeds)
})

test_that("auto mode under the best-fit default runs end to end on the convergence-status gate", {
  skip_on_cran()
  skip_if_not(nzchar(Sys.getenv("MOSAIC_RUN_INTEGRATION")),
              "set MOSAIC_RUN_INTEGRATION=1 to run the auto-mode stub runs")
  skip_if_not_installed("arrow")
  local_test_root()
  # (a) the default target is out of reach of the 420-draw budget: runs to the cap
  d <- withr::local_tempdir()
  .stub_run_mosaic(d, weighting = NULL, mode = "auto", plots = TRUE)
  sm <- jsonlite::read_json(file.path(d, "3_results", "summary.json"))
  expect_identical(sm$mode, "auto")
  # plots = TRUE: run_MOSAIC renders the calibration report from the JSON
  expect_true(file.exists(file.path(d, "3_results", "figures", "diagnostics", "calibration_report.pdf")))
  expect_false(file.exists(file.path(d, "3_results", "figures", "diagnostics", "convergence_status.pdf")))
  expect_identical(sm$stopping_rule_met, FALSE)
  expect_identical(sm$converged, FALSE); expect_true(sm$convergence_evaluated)
  expect_true(sm$budget_exhausted)
  expect_true("budget_cap_reached" %in% unlist(sm$convergence_flags))
  expect_identical(sm$n_simulations_total, 420L)
  rs <- jsonlite::read_json(file.path(d, "2_calibration", "state", "run_state.json"))
  expect_match(rs$gate_metric, "^convergence_status/v1\\.4\\.0/alpha=10/ratio=0.2/ess_best=200/")
  expect_identical(rs$schema_version, 2L)
  expect_identical(rs$status, "completed")
  h <- rs$convergence_history
  expect_identical(length(h), rs$batch)
  expect_identical(names(h[[1]]), c("batch", "n", "ess_all", "ess_best", "mce_p90", "khat", "slope", "status",
                                    "sampling_green"))
  expect_identical(h[[length(h)]]$n, 420L)
  expect_identical(h[[length(h)]]$status, sm$convergence_status)
  log <- readLines(file.path(d, "run.log"))
  expect_true(any(grepl("Convergence check (batch 1,", log, fixed = TRUE)))
  expect_true(any(grepl("-- projection:", log, fixed = TRUE) | grepl("geometric growth", log, fixed = TRUE)))
  expect_true(any(grepl("Budget cap reached", log, fixed = TRUE)))
  for (bad in c("Phase 1", "ESS regression", "function-specific", "ESS gap", "Akaike"))
    expect_false(any(grepl(bad, log, fixed = TRUE)), info = bad)
  expect_true(file.exists(file.path(d, "3_results", "calibration_diagnostics.json")))

  # (b) a reachable target stops at GREEN once the minimum run length is met
  d2 <- withr::local_tempdir()
  .stub_run_mosaic(d2, weighting = NULL, mode = "auto",
                   targets_extra = list(ESS_best = 20, min_ensemble_members = 10L,
                                        status_thresholds = list(mce_green = 0.5, mce_yellow = 0.6,
                                                                 flat_ess_frac = 0.5)))
  sm2 <- jsonlite::read_json(file.path(d2, "3_results", "summary.json"))
  expect_identical(sm2$stopping_rule_met, TRUE)
  expect_identical(sm2$convergence_status, "green")
  expect_identical(sm2$converged, TRUE)
  expect_false(sm2$budget_exhausted)
  expect_lt(sm2$n_simulations_total, 420L)
  expect_gte(sm2$n_simulations_total, 180L)                      # min_draws
  expect_gte(sm2$n_batches, 3L)                                   # min_batches_adaptive
  # D5: the stop needs the sampling checks green at the last two checks
  h2 <- jsonlite::read_json(file.path(d2, "2_calibration", "state", "run_state.json"))$convergence_history
  sg2 <- vapply(h2, function(e) isTRUE(e$sampling_green), TRUE)
  expect_identical(utils::tail(sg2, 2L), c(TRUE, TRUE))
})

test_that("a pre-1.4.0 run directory renders the legacy notice instead of failing", {
  d <- withr::local_tempdir()
  dir.create(file.path(d, "3_results"))
  jsonlite::write_json(list(location = "MOZ", converged = FALSE, posterior_weighting = "coarsened"),
                       file.path(d, "3_results", "summary.json"), auto_unbox = TRUE)
  out <- MOSAIC:::.mosaic_render_calibration_report(d, file.path(d, "fig"))
  expect_true(file.exists(out))
  expect_identical(MOSAIC:::.mosaic_calibration_report_legacy_notice(d),
                   MOSAIC:::.mosaic_calrep_legacy_text())
  # A 1.4.0 run (schema 2) without its diagnostics file is not called pre-1.4.0 (R8)
  jsonlite::write_json(list(schema_version = "2", location = "MOZ", convergence_status = "red"),
                       file.path(d, "3_results", "summary.json"), auto_unbox = TRUE)
  expect_message(out2 <- MOSAIC:::.mosaic_render_calibration_report(d, file.path(d, "fig2"), verbose = TRUE),
                 "missing notice")
  expect_true(file.exists(out2))
  expect_null(MOSAIC:::.mosaic_calibration_report_legacy_notice(d))
})
