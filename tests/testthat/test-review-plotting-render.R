# Regression tests for render_MOSAIC_figures() fixes from the production-readiness
# review (plotting group): per-channel central_method round-trip, posterior
# mobility parameters in the spatial group, psi_star from the run's own psi_jt,
# no HSIC recompute over an existing parameter_sensitivity.csv, and column-pruned
# samples.parquet reads.

.review_run_dir <- function() {
  d <- tempfile("mosaic_review_render_")
  dir.create(file.path(d, "1_inputs"), recursive = TRUE)
  dir.create(file.path(d, "2_calibration"), recursive = TRUE)
  dir.create(file.path(d, "3_results", "posterior"), recursive = TRUE)
  d
}

# --- x-artifacts-02: per-channel central_method --------------------------------

test_that("a per-channel central_method written by jsonlite round-trips", {
  d <- withr::local_tempdir()
  # jsonlite drops the names of an atomic vector: c(cases=, deaths=) is stored
  # as a bare array, exactly as run_MOSAIC() writes control.json.
  jsonlite::write_json(
    list(control = list(predictions = list(
      central_method = c(cases = "mean", deaths = "median")))),
    file.path(d, "control.json"), auto_unbox = TRUE)
  expect_match(paste(readLines(file.path(d, "control.json")), collapse = ""),
               '\\["mean","median"\\]')
  expect_equal(MOSAIC:::.mosaic_run_central_method(d),
               c(cases = "mean", deaths = "median"))
})

test_that("an unreadable central_method warns instead of aborting every group", {
  d <- .review_run_dir()
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  jsonlite::write_json(
    list(control = list(predictions = list(central_method = c("mean", "median", "mean")))),
    file.path(d, "1_inputs", "control.json"), auto_unbox = TRUE)
  w <- character()
  res <- withCallingHandlers(
    render_MOSAIC_figures(d, which = "convergence", verbose = FALSE),
    warning = function(cnd) { w <<- c(w, conditionMessage(cnd)); invokeRestart("muffleWarning") })
  expect_true(isTRUE(res[["convergence"]]))
  expect_true(any(grepl("central_method", w)))
})

# --- plotting-02: spatial figures use posterior mobility parameters ------------

.spatial_cfg <- function(J = 3L) {
  cfg <- jsonlite::fromJSON(system.file("extdata", "config_default.json", package = "MOSAIC"),
                            simplifyVector = TRUE)
  idx <- seq_len(J)
  for (f in c("location_name", "longitude", "latitude", "N_j_initial", "tau_i"))
    cfg[[f]] <- cfg[[f]][idx]
  cfg
}

test_that(".mosaic_posterior_mobility_config swaps in posterior medians and CI", {
  cfg <- .spatial_cfg()
  loc <- cfg$location_name
  csv <- tempfile(fileext = ".csv")
  on.exit(unlink(csv), add = TRUE)
  utils::write.csv(data.frame(
    parameter = c(paste0("tau_i_", loc), "mobility_omega", "mobility_gamma", "phi_1"),
    median    = c(0.011, 0.022, 0.033, 0.77, 1.9, 0.5),
    Q2.5      = c(0.001, 0.002, 0.003, 0.5, 1.0, 0.1),
    Q97.5     = c(0.1, 0.2, 0.3, 1.0, 3.0, 0.9)), csv, row.names = FALSE)

  out <- MOSAIC:::.mosaic_posterior_mobility_config(cfg, csv)
  expect_identical(out$source, "posterior")
  expect_equal(out$config$tau_i, c(0.011, 0.022, 0.033))
  expect_equal(out$config$mobility_omega, 0.77)
  expect_equal(out$config$mobility_gamma, 1.9)
  expect_equal(out$tau_ci$lower, c(0.001, 0.002, 0.003))
  expect_equal(out$tau_ci$upper, c(0.1, 0.2, 0.3))
  expect_identical(out$tau_ci$location, loc)

  # No posterior -> input config, unchanged.
  none <- MOSAIC:::.mosaic_posterior_mobility_config(cfg, tempfile())
  expect_identical(none$source, "input config")
  expect_equal(none$config$tau_i, cfg$tau_i)
  expect_null(none$tau_ci)
})

.spatial_run <- function() {
  d <- .review_run_dir()
  cfg <- .spatial_cfg()
  loc <- cfg$location_name
  jsonlite::write_json(cfg, file.path(d, "1_inputs", "config.json"),
                       auto_unbox = TRUE, digits = NA)
  post_tau <- c(0.0123, 0.0456, 0.0789)
  utils::write.csv(data.frame(
    parameter = c(paste0("tau_i_", loc), "mobility_omega", "mobility_gamma"),
    median = c(post_tau, 0.61, 1.7), Q2.5 = c(post_tau / 2, 0.5, 1.5),
    Q97.5 = c(post_tau * 2, 0.7, 1.9)),
    file.path(d, "3_results", "posterior", "parameter_estimates.csv"), row.names = FALSE)
  list(dir = d, post_tau = post_tau)
}

test_that("spatial group computes the flux from posterior omega/gamma", {
  r <- .spatial_run()
  d <- r$dir
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  seen <- new.env()
  testthat::local_mocked_bindings(
    calc_mobility_flux = function(config) {
      seen$omega <- config$mobility_omega; seen$gamma <- config$mobility_gamma
      stop("stop after capture")
    },
    .package = "MOSAIC")
  suppressWarnings(render_MOSAIC_figures(d, which = "spatial", verbose = FALSE))
  expect_equal(seen$omega, 0.61)
  expect_equal(seen$gamma, 1.7)
})

test_that("spatial group plots posterior tau_i with its posterior interval", {
  skip_if_not_installed("ggplot2")
  r <- .spatial_run()
  d <- r$dir; post_tau <- r$post_tau
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  seen <- new.env()
  testthat::local_mocked_bindings(
    plot_departure_tau = function(tau, N, location_name, ci = NULL) {
      seen$tau <- tau; seen$ci <- ci; ggplot2::ggplot()
    },
    .package = "MOSAIC")
  suppressWarnings(render_MOSAIC_figures(d, which = "spatial", verbose = FALSE))
  expect_equal(unname(seen$tau), post_tau)
  expect_equal(seen$ci$lower, post_tau / 2)
})

# --- plotting-03 / x-artifacts-08 / plotting-14: psi_star from psi_jt ----------

.psi_star_run <- function(single = FALSE) {
  d <- .review_run_dir()
  loc <- if (single) "MOZ" else c("MOZ", "KEN")
  dates <- seq(as.Date("2023-01-01"), as.Date("2023-01-10"), by = "day")
  psi <- if (single) seq(0.1, 0.55, length.out = 10) else
    rbind(seq(0.1, 0.55, length.out = 10), seq(0.9, 0.45, length.out = 10))
  jsonlite::write_json(list(location_name = loc, date_start = "2023-01-01",
                            date_stop = "2023-01-10", psi_jt = psi),
                       file.path(d, "1_inputs", "config.json"),
                       auto_unbox = TRUE, digits = NA)
  pars <- unlist(lapply(loc, function(j) paste0("psi_star_", c("a", "b", "z", "k"), "_", j)))
  vals <- rep(c(1, 0, 1, 0), length(loc))
  utils::write.csv(data.frame(parameter = pars, median = vals, Q2.5 = vals, Q97.5 = vals),
                   file.path(d, "3_results", "posterior", "parameter_estimates.csv"),
                   row.names = FALSE)
  list(dir = d, dates = dates, psi = psi, loc = loc)
}

test_that("psi_star diagnostic plots the run's psi_jt, not the live MODEL_INPUT CSV", {
  skip_if_not_installed("ggplot2")
  r <- .psi_star_run()
  on.exit(unlink(r$dir, recursive = TRUE), add = TRUE)
  dirs <- MOSAIC:::.mosaic_ensure_dir_tree(r$dir, clean_output = FALSE)

  # A live CSV with a different psi must be ignored when psi_jt is present.
  mi <- withr::local_tempdir()
  utils::write.csv(data.frame(iso_code = rep(r$loc, each = 10),
                              date = rep(as.character(r$dates), 2), psi = 0.001),
                   file.path(mi, "pred_psi_suitability_day.csv"), row.names = FALSE)

  plots <- plot_psi_star_diagnostic(dirs, PATHS = list(MODEL_INPUT = mi),
                                    location_names = r$loc, verbose = FALSE)
  expect_equal(plots$MOZ$data$raw, r$psi[1, ])
  expect_equal(plots$KEN$data$raw, r$psi[2, ])
  expect_equal(plots$KEN$data$date, r$dates)

  # And it renders without any PATHS at all (post-hoc on another machine).
  plots2 <- plot_psi_star_diagnostic(dirs, location_names = r$loc, verbose = FALSE)
  expect_equal(plots2$MOZ$data$raw, r$psi[1, ])
})

test_that("psi_star diagnostic handles a single-location psi_jt vector", {
  skip_if_not_installed("ggplot2")
  r <- .psi_star_run(single = TRUE)
  on.exit(unlink(r$dir, recursive = TRUE), add = TRUE)
  dirs <- MOSAIC:::.mosaic_ensure_dir_tree(r$dir, clean_output = FALSE)
  plots <- plot_psi_star_diagnostic(dirs, location_names = "MOZ", verbose = FALSE)
  expect_equal(plots$MOZ$data$raw, r$psi)
})

test_that("render's psi_star group works with no MOSAIC root configured", {
  skip_if_not_installed("ggplot2")
  r <- .psi_star_run()
  on.exit(unlink(r$dir, recursive = TRUE), add = TRUE)
  withr::local_options(root_directory = NULL)
  expect_no_warning(render_MOSAIC_figures(r$dir, which = "psi_star", verbose = FALSE))
  expect_true(file.exists(file.path(r$dir, "3_results", "figures", "diagnostics",
                                    "psi_raw_vs_psi_star_MOZ.png")))
})

# --- plotting-07 / x-artifacts-09: no HSIC recompute over an existing CSV ------

test_that("sensitivity group renders from parameter_sensitivity.csv without recomputing", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("arrow")
  d <- .review_run_dir()
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  dirs <- MOSAIC:::.mosaic_ensure_dir_tree(d, clean_output = FALSE)
  arrow::write_parquet(data.frame(sim = 1:5, iter = 1L, likelihood = -(1:5), phi_1 = runif(5)),
                       file.path(d, "2_calibration", "samples.parquet"))
  csv <- file.path(dirs$res_fig_diag, "parameter_sensitivity.csv")
  utils::write.csv(data.frame(parameter = c("phi_1", "phi_2"), hsic_r2 = c(0.4, 0.1),
                              p_value = c(0.01, 0.3), sig = c("*", ""),
                              description = c("a", "b")), csv, row.names = FALSE)
  before <- tools::md5sum(csv)

  testthat::local_mocked_bindings(
    calc_model_parameter_sensitivity = function(...) stop("HSIC recomputed"),
    .package = "MOSAIC")
  w <- character()
  withCallingHandlers(
    render_MOSAIC_figures(d, which = "sensitivity", verbose = FALSE),
    warning = function(cnd) { w <<- c(w, conditionMessage(cnd)); invokeRestart("muffleWarning") })
  expect_false(any(grepl("HSIC recomputed", w)))
  expect_identical(unname(tools::md5sum(csv)), unname(before))
  expect_true(file.exists(file.path(dirs$res_fig_diag, "parameter_sensitivity.png")))
})

test_that("the fixed-seed wrapper restores the caller's RNG stream", {
  set.seed(1); ref <- runif(1)
  set.seed(1)
  a <- MOSAIC:::.mosaic_with_fixed_seed(99L, function() runif(1))
  b <- MOSAIC:::.mosaic_with_fixed_seed(99L, function() runif(1))
  expect_identical(a, b)
  expect_identical(runif(1), ref)
})

# --- plotting-13: column-pruned samples.parquet reads --------------------------

test_that("convergence group passes only likelihood/sim/iter to plot_model_likelihood", {
  skip_if_not_installed("arrow")
  d <- .review_run_dir()
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  arrow::write_parquet(data.frame(sim = 1:4, iter = 1L, likelihood = -(1:4),
                                  phi_1 = runif(4), is_best_subset_opt = TRUE),
                       file.path(d, "2_calibration", "samples.parquet"))
  seen <- new.env()
  testthat::local_mocked_bindings(
    plot_model_likelihood = function(results, output_dir, verbose) {
      seen$cols <- names(results); invisible(NULL)
    },
    .package = "MOSAIC")
  suppressWarnings(render_MOSAIC_figures(d, which = "convergence", verbose = FALSE))
  expect_setequal(seen$cols, c("sim", "iter", "likelihood"))
})
