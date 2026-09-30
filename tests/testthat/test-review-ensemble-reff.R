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
})

# ---- central_method read back from control.json (reff-02) -------------------

test_that("a per-channel central_method survives the control.json round trip", {
  d <- tempfile("reff_cm_"); dir.create(file.path(d, "1_inputs"), recursive = TRUE)
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  ctl_path <- file.path(d, "1_inputs", "control.json")
  MOSAIC:::.mosaic_write_json(
    list(control = list(predictions = list(central_method = c(cases = "mean", deaths = "median")))),
    ctl_path)
  ctl <- jsonlite::fromJSON(ctl_path)$control
  expect_null(names(ctl$predictions$central_method))     # names are lost on disk
  expect_identical(MOSAIC:::.add_reff_cases_central_method(d, ctl), "mean")

  ctl2 <- list(predictions = list(central_method = c("median", "mean")))
  expect_identical(MOSAIC:::.add_reff_cases_central_method(d, ctl2), "median")

  # summary.json's resolved value wins when present.
  dir.create(file.path(d, "3_results"))
  jsonlite::write_json(list(central_method_cases = "median"),
                       file.path(d, "3_results", "summary.json"), auto_unbox = TRUE)
  expect_identical(MOSAIC:::.add_reff_cases_central_method(d, ctl), "median")

  # A control without the setting predates it: median.
  unlink(file.path(d, "3_results"), recursive = TRUE)
  expect_identical(MOSAIC:::.add_reff_cases_central_method(d, list()), "median")
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
