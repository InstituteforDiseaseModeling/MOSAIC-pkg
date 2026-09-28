# Tests for add_reproductive_numbers() -- the post-hoc R_eff driver over a
# single MOSAIC output directory.
#
# Builds a tiny synthetic output dir in tempdir() with the real artifact schema
# (trajectories_ensemble.rds + config.json), then asserts the driver writes the
# csv/rds, returns the right status, and skips a missing-incidence artifact
# gracefully. Does NOT depend on the real model tree.

# -----------------------------------------------------------------------------
# Helpers: write a minimal MOSAIC output directory from one engine run of the
# packaged demo config (route incidence + stocks, and a config the engine can
# rebuild delta_jt from).
# -----------------------------------------------------------------------------
.reff_demo <- local({
  cache <- NULL
  function() {
    if (is.null(cache)) {
      cfg <- MOSAIC::config_simulation_epidemic
      cfg$zeta_1 <- 1e6; cfg$zeta_2 <- 2e5
      r <- run_simulation(config = cfg, seed = 7L, quiet = TRUE)$results
      cache <<- list(cfg = cfg, r = r)
    }
    cache
  }
})

write_traj_rds <- function(path, drop_incidence = FALSE) {
  d <- .reff_demo(); r <- d$r
  nL <- nrow(r$incidence); Tn <- ncol(r$incidence)
  ch <- c("incidence", "incidence_human", "incidence_env", "E", "Isym", "Iasym")
  summary <- stats::setNames(lapply(ch, function(x)
    list(median = matrix(as.numeric(r[[x]]), nL, Tn))), ch)
  if (drop_incidence) summary[c("incidence_human", "incidence_env")] <- NULL
  traj <- structure(list(
    schema         = "mosaic_trajectories",
    channels       = ch,
    location_names = d$cfg$location_name,
    n_locations    = nL,
    n_time_points  = Tn,
    date_start     = d$cfg$date_start,
    date_stop      = d$cfg$date_stop,
    summary        = summary,
    lines          = data.frame(member_id = integer(0), weight = numeric(0),
                                location = character(0), channel = character(0),
                                t = integer(0), value = numeric(0),
                                stringsAsFactors = FALSE)
  ), class = "mosaic_trajectories")
  saveRDS(traj, path)
}

write_config_json <- function(path, cfg = .reff_demo()$cfg) {
  jsonlite::write_json(cfg, path, auto_unbox = TRUE, digits = NA)
}

# Build a complete tiny output dir; returns the dir path.
make_output_dir <- function(root, drop_incidence = FALSE, write_config = TRUE,
                            write_medoid = TRUE) {
  dir.create(file.path(root, "1_inputs"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(root, "2_calibration"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(root, "3_results"), recursive = TRUE, showWarnings = FALSE)
  write_traj_rds(file.path(root, "2_calibration", "trajectories_ensemble.rds"),
                 drop_incidence = drop_incidence)
  if (write_config)
    write_config_json(file.path(root, "1_inputs", "config.json"))
  if (write_medoid) {
    dir.create(file.path(root, "2_calibration", "best_model"), showWarnings = FALSE)
    write_config_json(file.path(root, "2_calibration", "best_model", "config_medoid.json"))
  }
  root
}

test_that("add_reproductive_numbers writes csv/rds + plot and returns ok status", {
  d <- file.path(tempdir(), paste0("reff_ok_", as.integer(runif(1, 1, 1e6))))
  make_output_dir(d)
  on.exit(unlink(d, recursive = TRUE), add = TRUE)

  st <- add_reproductive_numbers(d, plots = TRUE, verbose = FALSE)

  expect_s3_class(st, "data.frame")
  expect_equal(nrow(st), 1L)
  expect_equal(st$status, "ok")
  expect_equal(st$n_locations, 3L)
  # Production-default strided/empty lines -> CI unavailable.
  expect_false(isTRUE(st$ci_available))

  csv <- file.path(d, "3_results", "posterior", "reproductive_numbers.csv")
  rds <- file.path(d, "3_results", "posterior", "reproductive_numbers.rds")
  expect_true(file.exists(csv))
  expect_true(file.exists(rds))
  expect_equal(normalizePath(st$csv), normalizePath(csv))

  # CSV has the reproductive_numbers schema.
  tab <- utils::read.csv(csv, stringsAsFactors = FALSE)
  expect_true(all(c("location", "date", "t", "estimand", "central") %in% names(tab)))
  expect_setequal(unique(tab$estimand), c("R_eff", "R_hum", "R_env"))

  # RDS round-trips the class.
  ro <- readRDS(rds)
  expect_s3_class(ro, "reproductive_numbers")

  # Plot written (PNG path returned).
  expect_true(!is.na(st$plot) && file.exists(st$plot))
  expect_true(file.exists(file.path(d, "3_results", "figures",
                                    "reproductive_number",
                                    paste0("reproductive_number_", basename(d), ".png"))))
})

test_that("add_reproductive_numbers can skip plotting", {
  d <- file.path(tempdir(), paste0("reff_noplot_", as.integer(runif(1, 1, 1e6))))
  make_output_dir(d)
  on.exit(unlink(d, recursive = TRUE), add = TRUE)

  st <- add_reproductive_numbers(d, plots = FALSE, verbose = FALSE)
  expect_equal(st$status, "ok")
  expect_true(is.na(st$plot))
  expect_false(dir.exists(file.path(d, "3_results", "figures",
                                    "reproductive_number")))
})

test_that("add_reproductive_numbers skips an artifact without route incidence gracefully", {
  d <- file.path(tempdir(), paste0("reff_noinc_", as.integer(runif(1, 1, 1e6))))
  make_output_dir(d, drop_incidence = TRUE)
  on.exit(unlink(d, recursive = TRUE), add = TRUE)

  expect_warning(st <- add_reproductive_numbers(d, verbose = FALSE),
                 "incidence")
  expect_equal(st$status, "skipped_no_incidence")
  expect_false(file.exists(file.path(d, "3_results", "posterior",
                                     "reproductive_numbers.csv")))
})

test_that("add_reproductive_numbers skips when trajectories are missing", {
  d <- file.path(tempdir(), paste0("reff_notraj_", as.integer(runif(1, 1, 1e6))))
  dir.create(file.path(d, "1_inputs"), recursive = TRUE, showWarnings = FALSE)
  write_config_json(file.path(d, "1_inputs", "config.json"))
  on.exit(unlink(d, recursive = TRUE), add = TRUE)

  expect_warning(st <- add_reproductive_numbers(d, verbose = FALSE),
                 "trajectories")
  expect_equal(st$status, "skipped_missing_trajectories")
})

test_that("add_reproductive_numbers skips when config is missing", {
  d <- file.path(tempdir(), paste0("reff_nocfg_", as.integer(runif(1, 1, 1e6))))
  make_output_dir(d, write_config = FALSE)
  on.exit(unlink(d, recursive = TRUE), add = TRUE)

  expect_warning(st <- add_reproductive_numbers(d, verbose = FALSE),
                 "config")
  expect_equal(st$status, "skipped_missing_config")
})

test_that("add_reproductive_numbers respects overwrite = FALSE", {
  d <- file.path(tempdir(), paste0("reff_ow_", as.integer(runif(1, 1, 1e6))))
  make_output_dir(d)
  on.exit(unlink(d, recursive = TRUE), add = TRUE)

  st1 <- add_reproductive_numbers(d, plots = FALSE, verbose = FALSE)
  expect_equal(st1$status, "ok")
  st2 <- add_reproductive_numbers(d, plots = FALSE, overwrite = FALSE,
                                  verbose = FALSE)
  expect_equal(st2$status, "skipped_exists")
})

test_that("add_reproductive_numbers errors when output_dir does not exist", {
  expect_warning(st <- add_reproductive_numbers(
    file.path(tempdir(), "definitely_not_here_reff"), verbose = FALSE))
  expect_equal(st$status, "error")
})

test_that("add_reproductive_numbers uses the medoid config and records it", {
  d <- file.path(tempdir(), paste0("reff_med_", as.integer(runif(1, 1, 1e6))))
  make_output_dir(d)
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  # A base config with different decay makes the two choices distinguishable.
  base <- .reff_demo()$cfg; base$decay_days_long <- base$decay_days_long * 3
  write_config_json(file.path(d, "1_inputs", "config.json"), base)

  st <- add_reproductive_numbers(d, plots = FALSE, verbose = FALSE)
  ro <- readRDS(st$rds)
  expect_equal(attr(ro, "config_source"), "2_calibration/best_model/config_medoid.json")
  ref <- calc_Reff(readRDS(file.path(d, "2_calibration", "trajectories_ensemble.rds")),
                   .reff_demo()$cfg, verbose = FALSE)
  late <- ro$t > attr(ro, "burn_in_days")
  expect_equal(ro$central[late], ref$central[late])
})

test_that("add_reproductive_numbers falls back to the input config with a warning", {
  d <- file.path(tempdir(), paste0("reff_nomed_", as.integer(runif(1, 1, 1e6))))
  make_output_dir(d, write_medoid = FALSE)
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  expect_warning(st <- add_reproductive_numbers(d, plots = FALSE, verbose = FALSE),
                 "config_medoid")
  expect_equal(st$status, "ok")
  expect_equal(attr(readRDS(st$rds), "config_source"), "1_inputs/config.json")
})

test_that("add_reproductive_numbers applies the burn-in on the direct path", {
  d <- file.path(tempdir(), paste0("reff_burn_", as.integer(runif(1, 1, 1e6))))
  make_output_dir(d)
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  st <- add_reproductive_numbers(d, plots = FALSE, burn_in_days = 40L, verbose = FALSE)
  ro <- readRDS(st$rds)
  expect_equal(attr(ro, "burn_in_days"), 40L)
  expect_true(all(is.na(ro$central[ro$t <= 40])))
  expect_true(any(is.finite(ro$central[ro$t > 40])))
  expect_true(all(is.na(attr(ro, "central_matrix")[, 1:40])))
  # Absent control.json and argument -> 30-day default.
  st2 <- add_reproductive_numbers(d, plots = FALSE, verbose = FALSE)
  expect_equal(attr(readRDS(st2$rds), "burn_in_days"), 30L)
  # An explicit 0 disables the burn-in rather than falling back to 30.
  st0 <- add_reproductive_numbers(d, plots = FALSE, burn_in_days = 0L, verbose = FALSE)
  ro0 <- readRDS(st0$rds)
  expect_equal(attr(ro0, "burn_in_days"), 0L)
  ref <- calc_Reff(readRDS(file.path(d, "2_calibration", "trajectories_ensemble.rds")),
                   .reff_demo()$cfg, verbose = FALSE)
  expect_equal(ro0$central, ref$central)
  expect_error(add_reproductive_numbers(d, plots = FALSE, burn_in_days = -1L,
                                        verbose = FALSE), "burn_in_days")
})

test_that("overwrite = FALSE recomputes an older total-only table", {
  d <- file.path(tempdir(), paste0("reff_old_", as.integer(runif(1, 1, 1e6))))
  make_output_dir(d)
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  post <- file.path(d, "3_results", "posterior")
  dir.create(post, recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(data.frame(location = "FOO", date = "2020-01-01", t = 1,
                              estimand = "R_eff", central = 1),
                   file.path(post, "reproductive_numbers.csv"), row.names = FALSE)
  st <- add_reproductive_numbers(d, plots = FALSE, overwrite = FALSE, verbose = FALSE)
  expect_equal(st$status, "ok")
})
