# Regression tests from the production-readiness review (group "ensemble"):
# the R_eff posterior paths (calc_Reff.R, add_reproductive_numbers.R). The
# re-simulation worker is replaced by a deterministic stub so the posterior
# bookkeeping (member weights, subset, medoid, failure handling) is tested
# without running the engine.

# ---- quantile column names (reff-05) ----------------------------------------

test_that("quantile column names have no trailing dot for 1-ulp products", {
  expect_identical(MOSAIC:::.mosaic_reff_prob_colnames(c(0.07, 0.29, 0.55)),
                   c("q7", "q29", "q55"))
  expect_identical(MOSAIC:::.mosaic_reff_prob_colnames(c(0.025, 0.5, 0.975)),
                   c("q2.5", "q50", "q97.5"))
  expect_identical(MOSAIC:::.mosaic_reff_prob_colnames(c(0.001, 0.125)),
                   c("q0.1", "q12.5"))
})

# ---- faithfulness gate on constant series (reff-04) -------------------------

test_that("a bit-exact re-simulation of a constant series is a perfect match", {
  fd <- MOSAIC:::.mosaic_reff_faithfulness(rep(0, 20), rep(0, 20))
  expect_identical(fd$cc, 1)
  expect_identical(fd$max_abs, 0)
  expect_identical(fd$re, 0)
  # A constant saved series that is NOT reproduced keeps an undefined correlation.
  fd2 <- MOSAIC:::.mosaic_reff_faithfulness(c(rep(0, 19), 5), rep(0, 20))
  expect_true(is.na(fd2$cc))
  fd3 <- MOSAIC:::.mosaic_reff_faithfulness(c(1, 2, 3, 5), c(1, 2, 3, 4))
  expect_equal(fd3$cc, cor(c(1, 2, 3, 5), c(1, 2, 3, 4)))
  expect_equal(fd3$re, 1 / 10)
  expect_equal(fd3$max_abs, 1)
})

# ---- direct-path half-weight rule counts dropped members (reff-06) ----------

test_that("members dropped for a gap still count toward the half-weight rule", {
  local_mocked_bindings(
    .mosaic_reff_routes = function(ih, ie, delta, kern, floor, init = NULL, window = NULL)
      list(R_eff = ih, R_hum = ih, R_env = 0 * ie),
    .package = "MOSAIC")
  Tn <- 5L
  mk <- function(id, w, gap) {
    v <- rep(id, Tn)
    tt <- seq_len(Tn)
    if (gap) tt <- tt[-3]
    rbind(
      data.frame(location = "A", member_id = id, channel = "incidence_human",
                 t = tt, value = v[tt], weight = w),
      data.frame(location = "A", member_id = id, channel = "incidence_env",
                 t = tt, value = 0, weight = w))
  }
  # 80% of the weight sits on members with a gap; 20% on a complete member.
  lines <- rbind(mk(1L, 0.4, TRUE), mk(2L, 0.4, TRUE), mk(3L, 0.2, FALSE))
  q <- MOSAIC:::.mosaic_reff_member_quantiles(
    lines, kern = NULL, delta = matrix(0, 1, Tn), loc_names = "A",
    t_present = seq_len(Tn), nL = 1L, Tn = Tn, probs = 0.5)
  # Only 20% of the posterior weight is defined -> no CI reported.
  expect_true(all(is.na(q$R_eff[1, , 1])))

  # Once complete members hold >= half the weight, the CI is reported.
  lines2 <- rbind(mk(1L, 0.3, TRUE), mk(2L, 0.3, FALSE), mk(3L, 0.4, FALSE))
  q2 <- MOSAIC:::.mosaic_reff_member_quantiles(
    lines2, kern = NULL, delta = matrix(0, 1, Tn), loc_names = "A",
    t_present = seq_len(Tn), nL = 1L, Tn = Tn, probs = 0.5)
  expect_true(all(is.finite(q2$R_eff[1, , 1])))
})

# ---- re-simulation bookkeeping (reff-01, reff-03, x-artifacts-07) ------------

# Stub member: R series is the constant p (so a weighted quantile over members
# reads off which parameter sets carry the weight); faithful to its saved slice.
.stub_member_env <- new.env()
.stub_member <- function(task, ctx = NULL) {
  .stub_member_env$called <- c(.stub_member_env$called, sprintf("%d_%d", task$p, task$s))
  nL <- nrow(task$saved); Tn <- ncol(task$saved)
  ests <- MOSAIC:::.MOSAIC_REFF_ESTIMANDS
  reff <- stats::setNames(lapply(ests, function(e)
    lapply(seq_len(nL), function(i) rep(as.numeric(task$p), Tn))), ests)
  peak <- stats::setNames(lapply(ests, function(e) rep(as.numeric(task$p), nL)), ests)
  sv <- as.numeric(task$saved)
  list(p = task$p, s = task$s, reff = reff, peak = peak, re = 0, cc = 1,
       ssum = sum(sv), rsum = sum(sv), max_abs = 0,
       kernel_params = c(iota = 1, gamma_1 = 1, gamma_2 = 1, sigma = 1,
                         zeta_1 = 1, zeta_2 = 1))
}

.mk_ens <- function(nP = 3L, nS = 2L, Tn = 10L) {
  ca <- array(NA_real_, dim = c(1L, Tn, nP, nS))
  for (p in seq_len(nP)) for (s in seq_len(nS)) ca[1L, , p, s] <- 10 * p + seq_len(Tn)
  structure(list(seeds = 100L + seq_len(nP), parameter_weights = rep(1 / nP, nP),
                 cases_array = ca, n_param_sets = nP, n_simulations_per_config = nS,
                 location_names = "A", date_start = "2020-01-01",
                 cases_median = matrix(apply(ca[1, , , , drop = FALSE], 2, median), 1),
                 cases_mean = matrix(apply(ca[1, , , , drop = FALSE], 2, mean), 1)),
            class = "mosaic_ensemble")
}

test_that("members with no saved cases are skipped, not re-run, and carry no weight", {
  local_mocked_bindings(.mosaic_reff_resim_member = .stub_member, .package = "MOSAIC")
  .stub_member_env$called <- character(0)
  ens <- .mk_ens()
  ens$cases_array[, , 2L, 1L] <- NA_real_     # failed at calibration
  res <- MOSAIC:::.mosaic_reff_resim_ci(ens, base_config = list(), priors = list(),
                                        sampling_args = list(), PATHS = list(),
                                        probs = 0.5, verbose = FALSE)
  expect_false("2_1" %in% .stub_member_env$called)
  expect_setequal(.stub_member_env$called, c("1_1", "3_1", "1_2", "2_2", "3_2"))
  expect_identical(res$n_members, 5L)
  # Member weights: p1 = 1/3 (2 x 1/6), p2 = 1/6 (surviving rerun), p3 = 1/3.
  # peak_Rt q50 over values {1,1,2,3,3} with those weights -> 2 (cum 0.4 < 0.5 <= 0.6).
  expect_equal(res$peak_Rt$q50[res$peak_Rt$estimand == "R_eff"], 2)
})

test_that("a failing member WITH saved cases still aborts the CI", {
  failing <- function(task, ctx = NULL) {
    if (task$p == 1L && task$s == 1L) return(list(p = 1L, s = 1L, error = "boom"))
    .stub_member(task, ctx)
  }
  local_mocked_bindings(.mosaic_reff_resim_member = failing, .package = "MOSAIC")
  expect_error(MOSAIC:::.mosaic_reff_resim_ci(.mk_ens(), list(), list(), list(), list(),
                                              probs = 0.5, verbose = FALSE),
               "1 of 6 members failed to re-simulate; first: boom")
})

test_that("the final (optimized) posterior sets the weights, subset and medoid", {
  local_mocked_bindings(.mosaic_reff_resim_member = .stub_member, .package = "MOSAIC")
  ens <- .mk_ens()
  # Candidate posterior: equal weights over p = 1..3 -> per-day q50 = 2.
  .stub_member_env$called <- character(0)
  r0 <- MOSAIC:::.mosaic_reff_resim_ci(ens, list(), list(), list(), list(),
                                       probs = 0.5, verbose = FALSE)
  expect_equal(unique(as.numeric(r0$qmats$R_eff[1, , 1])), 2)
  expect_identical(r0$medoid_member$param_idx, 2L)

  # Optimized subset {1, 3} with weights 0.7 / 0.3; medoid target = p3's series.
  .stub_member_env$called <- character(0)
  r1 <- MOSAIC:::.mosaic_reff_resim_ci(
    ens, list(), list(), list(), list(), probs = 0.5, verbose = FALSE,
    member_param_weights = c(0.7, 0, 0.3), param_subset = c(1L, 3L),
    medoid_cases_central = matrix(ens$cases_array[1, , 3, 1], nrow = 1))
  expect_false(any(grepl("^2_", .stub_member_env$called)))
  expect_equal(unique(as.numeric(r1$qmats$R_eff[1, , 1])), 1)   # 0.7 on p1
  expect_identical(r1$medoid_member$param_idx, 3L)
  expect_identical(r1$medoid_member$seed, 103L)
  expect_equal(unique(as.numeric(r1$central$R_eff)), 3)
  expect_identical(r1$n_members, 4L)
})

test_that(".add_reff_final_posterior maps ensemble_optimized onto the candidate by seed", {
  d <- tempfile("reff_fin_"); dir.create(file.path(d, "2_calibration"), recursive = TRUE)
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  cand <- .mk_ens(nP = 4L)
  # No optimized file -> candidate.
  f0 <- MOSAIC:::.add_reff_final_posterior(d, cand, "mean", verbose = FALSE)
  expect_null(f0$weights); expect_identical(f0$source, "ensemble_candidate.rds")

  opt <- cand
  opt$seeds <- c(104L, 102L)                 # re-sorted, sliced
  opt$parameter_weights <- c(0.8, 0.2)
  opt$n_param_sets <- 2L
  opt$cases_mean <- matrix(1:10, 1)
  saveRDS(opt, file.path(d, "2_calibration", "ensemble_optimized.rds"))
  f1 <- MOSAIC:::.add_reff_final_posterior(d, cand, "mean", verbose = FALSE)
  expect_equal(f1$weights, c(0, 0.2, 0, 0.8))
  expect_identical(f1$subset, c(4L, 2L))
  expect_equal(f1$central, matrix(1:10, 1))
  expect_identical(f1$source, "ensemble_optimized.rds")

  # A seed that is not in the candidate -> warn and fall back.
  opt$seeds <- c(104L, 999L)
  saveRDS(opt, file.path(d, "2_calibration", "ensemble_optimized.rds"))
  expect_warning(f2 <- MOSAIC:::.add_reff_final_posterior(d, cand, "mean", verbose = FALSE),
                 "not a subset")
  expect_null(f2$weights)

  # run_MOSAIC's fallback copy (optimize_subset off): all members, same order
  # and weights -> reported as the candidate, not as an optimized posterior.
  cp <- cand
  cp$cases_mean <- matrix(1:10, 1)
  saveRDS(cp, file.path(d, "2_calibration", "ensemble_optimized.rds"))
  f3 <- MOSAIC:::.add_reff_final_posterior(d, cand, "mean", verbose = FALSE)
  expect_identical(f3$source, "ensemble_candidate.rds")
  expect_equal(f3$weights, as.numeric(cand$parameter_weights))

  # central_method 'mean' with no cases_mean: warn, then use the median.
  opt$seeds <- c(104L, 102L); opt$cases_mean <- NULL
  opt$cases_median <- matrix(11:20, 1)
  saveRDS(opt, file.path(d, "2_calibration", "ensemble_optimized.rds"))
  expect_warning(f4 <- MOSAIC:::.add_reff_final_posterior(d, cand, "mean", verbose = FALSE),
                 "no cases_mean")
  expect_equal(f4$central, matrix(11:20, 1))
  expect_silent(MOSAIC:::.add_reff_final_posterior(d, cand, "median", verbose = FALSE))
})

# ---- central_method read back from control.json (reff-02) -------------------

test_that("recompute_ci reads the run's central_method through .mosaic_run_central_method", {
  d <- tempfile("reff_cm_"); dir.create(file.path(d, "1_inputs"), recursive = TRUE)
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  ctl_path <- file.path(d, "1_inputs", "control.json")
  wctl <- function(cm) MOSAIC:::.mosaic_write_json(
    list(control = list(predictions = list(central_method = cm))), ctl_path)

  # Current writer: a named list keeps the channel labels on disk.
  wctl(list(cases = "mean", deaths = "median"))
  expect_identical(MOSAIC:::.add_reff_cases_central_method(d), "mean")
  wctl(list(cases = "median", deaths = "mean"))
  expect_identical(MOSAIC:::.add_reff_cases_central_method(d), "median")

  # Old writer: a names-dropped pair is read in the documented order.
  wctl(c(cases = "median", deaths = "mean"))
  expect_warning(cm <- MOSAIC:::.add_reff_cases_central_method(d), "documented order")
  expect_identical(cm, "median")

  # summary.json's resolved value wins when present.
  dir.create(file.path(d, "3_results"))
  jsonlite::write_json(list(central_method_cases = "mean", central_method_deaths = "mean"),
                       file.path(d, "3_results", "summary.json"), auto_unbox = TRUE)
  expect_identical(MOSAIC:::.add_reff_cases_central_method(d), "mean")
  unlink(file.path(d, "3_results"), recursive = TRUE)

  # A control without the setting predates it (v0.38.0): those runs used the median.
  MOSAIC:::.mosaic_write_json(list(control = list(predictions = list())), ctl_path)
  expect_identical(MOSAIC:::.add_reff_cases_central_method(d), "median")

  # An unresolvable value falls back to the package default with a warning
  # (the median for cases since v0.101.0).
  wctl("trimmed_mean")
  expect_warning(cm <- MOSAIC:::.add_reff_cases_central_method(d), "package default")
  expect_identical(cm, "median")
})

# ---- recompute_ci does not need the trajectory artifact (reff-07) -----------

test_that("recompute_ci = TRUE is not refused for a missing trajectory artifact", {
  d <- tempfile("reff_rc_")
  dir.create(file.path(d, "1_inputs"), recursive = TRUE)
  dir.create(file.path(d, "2_calibration"), recursive = TRUE)
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  jsonlite::write_json(list(date_start = "2020-01-01"), file.path(d, "1_inputs", "config.json"),
                       auto_unbox = TRUE)
  seen <- FALSE
  local_mocked_bindings(
    .add_reff_recompute_ci = function(output_dir, base_config, ...) {
      seen <<- TRUE
      stop("reached the re-simulation path")
    }, .package = "MOSAIC")
  st <- suppressWarnings(add_reproductive_numbers(d, recompute_ci = TRUE, burn_in_days = 0L,
                                                  plots = FALSE, verbose = FALSE))
  expect_true(seen)
  expect_identical(st$status, "error")
  expect_match(st$message, "reached the re-simulation path")

  # The point path still requires the artifact.
  st2 <- suppressWarnings(add_reproductive_numbers(d, recompute_ci = FALSE, burn_in_days = 0L,
                                                   plots = FALSE, verbose = FALSE))
  expect_identical(st2$status, "skipped_missing_trajectories")
})

test_that("the documented day-wise generation-time mass (reff-11) matches the file", {
  d <- file.path(tempdir(), "gt_reff11")
  dir.create(d, showWarnings = FALSE)
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  suppressMessages(get_generation_time_distribution(
    list(MODEL_INPUT = d, DOCS_TABLES = d), mean_generation_time = 5))
  days  <- utils::read.csv(file.path(d, "pred_generation_time_days.csv"))
  weeks <- utils::read.csv(file.path(d, "pred_generation_time_weeks.csv"))
  # Gamma(shape 0.5, rate 0.1) density summed over days 1..56 = 0.7424 (the
  # roxygen says "about 0.74"); only the weekly table is normalised.
  expect_equal(sum(days$y), 0.7423659, tolerance = 1e-6)
  expect_equal(sum(weeks$Probability), 1, tolerance = 1e-12)
})
