# Tests for add_reproductive_numbers() -- the post-hoc R_eff driver over a
# single MOSAIC output directory.
#
# Builds a tiny synthetic output dir in tempdir() with the real artifact schema
# (trajectories_ensemble.rds + config.json), then asserts the driver writes the
# csv/rds, returns the right status, and skips a missing-incidence artifact
# gracefully. Does NOT depend on the real model tree.

# -----------------------------------------------------------------------------
# Helpers: write a minimal MOSAIC output directory
# -----------------------------------------------------------------------------
write_traj_rds <- function(path, inc_med, lines = NULL,
                           date_start = "2023-01-01", drop_incidence = FALSE) {
  nL <- nrow(inc_med); Tn <- ncol(inc_med)
  loc_names <- if (nL == 1L) "MOZ" else paste0("LOC", seq_len(nL))
  if (is.null(lines))
    lines <- data.frame(member_id = integer(0), weight = numeric(0),
                        location = character(0), channel = character(0),
                        t = integer(0), value = numeric(0),
                        stringsAsFactors = FALSE)
  summary <- list(incidence = list(median = inc_med))
  if (drop_incidence) summary$incidence <- NULL
  traj <- structure(list(
    schema         = "mosaic_trajectories",
    channels       = "incidence",
    location_names = loc_names,
    n_locations    = nL,
    n_time_points  = Tn,
    date_start     = date_start,
    date_stop      = as.character(as.Date(date_start) + Tn - 1L),
    summary        = summary,
    lines          = lines
  ), class = "mosaic_trajectories")
  saveRDS(traj, path)
}

write_config_json <- function(path,
                              cfg = list(iota = 1 / 1.4, gamma_1 = 0.1,
                                         gamma_2 = 0.5, sigma = 0.25)) {
  jsonlite::write_json(cfg, path, auto_unbox = TRUE)
}

# Build a complete tiny output dir; returns the dir path.
make_output_dir <- function(root, inc_med = NULL, drop_incidence = FALSE,
                            write_config = TRUE) {
  if (is.null(inc_med))
    inc_med <- matrix(exp(0.03 * seq_len(60)), nrow = 1)
  dir.create(file.path(root, "1_inputs"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(root, "2_calibration"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(root, "3_results"), recursive = TRUE, showWarnings = FALSE)
  write_traj_rds(file.path(root, "2_calibration", "trajectories_ensemble.rds"),
                 inc_med, drop_incidence = drop_incidence)
  if (write_config)
    write_config_json(file.path(root, "1_inputs", "config.json"))
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
  expect_equal(st$n_locations, 1L)
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
  expect_equal(unique(tab$estimand), "R_eff")

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

test_that("add_reproductive_numbers skips a missing-incidence artifact gracefully", {
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
