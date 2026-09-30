# Regression tests: the R2/Bias/totals printed in plot_model_ensemble()'s
# captions must be scored on the same masked series run_MOSAIC() uses for
# summary.json (.mosaic_mask_central_for_scoring), i.e. with the cases warm-up
# and the scored-window burn-in removed. Before the fix the faceted captions used
# the raw central series and the per-location captions ignored the warm-up, so a
# warm-up transient dominated "Pred" and wrecked R2.

.caption_ensemble <- function(n_locs = 2L, n_times = 10L, mask = NULL) {
  cases <- matrix(0, n_locs, n_times)
  deaths <- matrix(0, n_locs, n_times)
  for (i in seq_len(n_locs)) {
    cases[i, ] <- 10 * i + seq_len(n_times)
    cases[i, 2] <- 9999                         # IC warm-up transient
    deaths[i, ] <- i + seq_len(n_times) / 2
  }
  obs_c <- cases; obs_c[, 2] <- 20              # the observation never saw the spike
  obs_d <- deaths + 0.5
  ci <- function(m) list(list(lower = m * 0.8, upper = m * 1.2))
  structure(list(
    cases_mean = cases, cases_median = cases,
    deaths_mean = deaths, deaths_median = deaths,
    ci_bounds = list(cases = ci(cases), deaths = ci(deaths)),
    obs_cases = obs_c, obs_deaths = obs_d,
    cases_array = NULL, deaths_array = NULL,
    parameter_weights = c(0.5, 0.5), seeds = c(1L, 2L),
    n_param_sets = 2L, n_simulations_per_config = 3L, n_successful = 6L,
    location_names = paste0("L", seq_len(n_locs)),
    n_locations = n_locs, n_time_points = n_times,
    date_start = "2024-01-01", date_stop = as.character(as.Date("2024-01-01") + n_times - 1L),
    envelope_quantiles = c(0.025, 0.975),
    artifact_mask = if (is.null(mask)) list(cases_warmup = 2L, deaths_final = FALSE,
                                            score_idx_cases = 1L, score_idx_deaths = 1L) else mask
  ), class = "mosaic_ensemble")
}

# summary.json-style metrics (run_MOSAIC.R, ensemble R2/bias block).
.summary_metrics <- function(ens, chan) {
  cen <- if (chan == "cases") ens$cases_mean else ens$deaths_mean
  obs <- if (chan == "cases") ens$obs_cases else ens$obs_deaths
  pred <- as.numeric(MOSAIC:::.mosaic_mask_central_for_scoring(cen, chan, ens$artifact_mask))
  c(r2 = round(calc_model_R2(as.numeric(obs), pred), 3L),
    bias = round(calc_bias_ratio(as.numeric(obs), pred), 2L))
}

.caption_nums <- function(caption, key) {
  m <- regmatches(caption, gregexpr(paste0(key, " = -?[0-9.]+"), caption))[[1]]
  as.numeric(sub(paste0(key, " = "), "", m))
}

test_that("faceted captions score the masked series (warm-up spike excluded)", {
  skip_if_not_installed("ggplot2")
  ens <- .caption_ensemble()
  out <- withr::local_tempdir()
  res <- suppressWarnings(plot_model_ensemble(ens, output_dir = out,
                                              central_method = "mean", verbose = FALSE))
  cap_c <- res$cases_faceted$labels$caption
  cap_d <- res$deaths_faceted$labels$caption
  exp_c <- .summary_metrics(ens, "cases")
  exp_d <- .summary_metrics(ens, "deaths")
  expect_equal(.caption_nums(cap_c, "R\u00b2"), unname(exp_c["r2"]))
  expect_equal(.caption_nums(cap_c, "Bias"),    unname(exp_c["bias"]))
  expect_equal(.caption_nums(cap_d, "R\u00b2"), unname(exp_d["r2"]))
  expect_equal(.caption_nums(cap_d, "Bias"),    unname(exp_d["bias"]))
  # The spike (9999 per location) must not be in the caption's Pred total.
  expect_false(grepl("9,999|19,998|2[0-9],[0-9]{3}", cap_c))
})

test_that("per-location captions apply the cases warm-up mask", {
  skip_if_not_installed("ggplot2")
  ens <- .caption_ensemble()
  out <- withr::local_tempdir()
  res <- suppressWarnings(plot_model_ensemble(ens, output_dir = out,
                                              central_method = "mean", verbose = FALSE))
  for (i in seq_len(ens$n_locations)) {
    cap <- res$individual[[ens$location_names[i]]]$labels$caption
    obs <- ens$obs_cases[i, ]
    pred <- as.numeric(MOSAIC:::.mosaic_mask_central_for_scoring(
      ens$cases_mean[i, , drop = FALSE], "cases", ens$artifact_mask))
    r2 <- .caption_nums(cap, "R\u00b2")
    expect_equal(r2[1], round(calc_model_R2(obs, pred), 3L))
    expect_equal(.caption_nums(cap, "Bias")[1], round(calc_bias_ratio(obs, pred), 2L))
  }
})

test_that("faceted captions also drop the scored-window burn-in", {
  skip_if_not_installed("ggplot2")
  ens <- .caption_ensemble(mask = list(cases_warmup = 2L, deaths_final = FALSE,
                                       score_idx_cases = 5L, score_idx_deaths = 4L))
  # A burn-in-era over-prediction the caption must not see.
  ens$cases_mean[, 3:4] <- 5000; ens$cases_median <- ens$cases_mean
  ens$deaths_mean[, 1:3] <- 800; ens$deaths_median <- ens$deaths_mean
  out <- withr::local_tempdir()
  res <- suppressWarnings(plot_model_ensemble(ens, output_dir = out,
                                              central_method = "mean", verbose = FALSE))
  expect_equal(.caption_nums(res$cases_faceted$labels$caption, "Bias"),
               unname(.summary_metrics(ens, "cases")["bias"]))
  expect_equal(.caption_nums(res$deaths_faceted$labels$caption, "Bias"),
               unname(.summary_metrics(ens, "deaths")["bias"]))
})
