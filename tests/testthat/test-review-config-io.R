# Regression tests for the config-group review fixes to make_simulation_config(),
# the JSON writers, get_location_priors() and inflate_priors().

msc_args <- function(cfg) {
  keep <- intersect(names(cfg), names(formals(make_simulation_config)))
  cfg[keep]
}

test_that("make_simulation_config() rejects a per-location alpha_2 that the engine would reject", {
  cfg  <- get_location_config(iso = c("ETH", "KEN"))
  args <- msc_args(cfg)
  args$alpha_2 <- c(0.5, 0.6)
  expect_error(suppressMessages(do.call(make_simulation_config, args)),
               "alpha_2 must be a single numeric value")
})

test_that("make_simulation_config() rejects an epidemic_threshold of the wrong length", {
  cfg  <- get_location_config(iso = c("ETH", "KEN"))
  args <- msc_args(cfg)
  args$epidemic_threshold <- c(1e-4, 2e-4, 3e-4)
  expect_error(suppressMessages(do.call(make_simulation_config, args)),
               "epidemic_threshold")
  args$epidemic_threshold <- 1e-4
  expect_type(suppressMessages(do.call(make_simulation_config, args)), "list")
})

test_that("make_simulation_config() returns the validated list when it writes a file", {
  cfg  <- get_location_config(iso = "ETH")
  args <- msc_args(cfg)
  out_path <- tempfile(fileext = ".json")
  on.exit(unlink(out_path), add = TRUE)
  args$output_file_path <- out_path
  res <- suppressMessages(do.call(make_simulation_config, args))
  expect_type(res, "list")
  expect_identical(res$location_name, "ETH")
  expect_true(file.exists(out_path))
})

test_that("make_simulation_config() warns that sigfigs is ignored", {
  cfg  <- get_location_config(iso = "ETH")
  args <- msc_args(cfg)
  args$sigfigs <- 4
  expect_warning(res <- suppressMessages(do.call(make_simulation_config, args)),
                 "sigfigs")
  expect_identical(res$gamma_1, cfg$gamma_1)
})

test_that("write_list_to_hdf5() accepts the .hdf5 extension make_simulation_config() advertises", {
  skip_if_not_installed("hdf5r")
  out_path <- tempfile(fileext = ".hdf5")
  on.exit(unlink(out_path), add = TRUE)
  expect_no_error(suppressMessages(write_list_to_hdf5(list(a = 1, b = c(2, 3)), out_path)))
  expect_true(file.exists(out_path))
})

test_that("write_list_to_json() round-trips doubles exactly and leaves no temp file", {
  x <- list(a = 0.1 + 0.2, b = 1 / 3, c = 3.3e-16, d = c(2.1e11, 4.7e-9))
  dir <- tempfile("json_rt_")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  for (compress in c(FALSE, TRUE)) {
    path <- file.path(dir, "x.json")
    suppressMessages(write_list_to_json(x, path, compress = compress))
    if (compress) path <- paste0(path, ".gz")
    y <- jsonlite::fromJSON(path)
    expect_identical(y$a, x$a)
    expect_identical(y$b, x$b)
    expect_identical(y$c, x$c)
    expect_identical(y$d, x$d)
  }
  # Overwrite in place, and nothing but the two targets remains in the directory
  suppressMessages(write_list_to_json(list(a = 2), file.path(dir, "x.json")))
  expect_identical(jsonlite::fromJSON(file.path(dir, "x.json"))$a, 2L)
  expect_setequal(list.files(dir, all.files = TRUE, no.. = TRUE), c("x.json", "x.json.gz"))
})

test_that("write_json_or_gz() and write_model_json() round-trip doubles exactly", {
  x <- list(metadata = list(v = "1"), parameters_global = list(g = 1 / 3, h = 0.1 + 0.2))
  p1 <- tempfile(fileext = ".json")
  p2 <- tempfile(fileext = ".json")
  on.exit(unlink(c(p1, p2)), add = TRUE)
  suppressMessages(write_json_or_gz(x, p1))
  write_model_json(x, p2, type = "priors")
  for (p in c(p1, p2)) {
    y <- jsonlite::fromJSON(p)
    expect_identical(y$parameters_global$g, 1 / 3)
    expect_identical(y$parameters_global$h, 0.1 + 0.2)
  }
})

test_that("get_location_priors() works when MOSAIC is loaded but not attached", {
  skip_on_cran()
  skip_if_not_installed("pkgload")
  pkg_root <- normalizePath(test_path("..", ".."), mustWork = FALSE)
  skip_if_not(file.exists(file.path(pkg_root, "DESCRIPTION")) &&
                dir.exists(file.path(pkg_root, "R")), "not running from a source tree")
  script <- tempfile(fileext = ".R")
  on.exit(unlink(script), add = TRUE)
  writeLines(c(
    sprintf("suppressMessages(pkgload::load_all('%s', attach = FALSE, quiet = TRUE))", pkg_root),
    "out <- MOSAIC::get_location_priors(iso = 'ETH')",
    "cat('ATTACHED=', 'package:MOSAIC' %in% search(), '\\n', sep = '')",
    "cat('HAS_ETH=', 'ETH' %in% names(out$parameters_location$beta_j0_tot$location), '\\n', sep = '')"
  ), script)
  res <- suppressWarnings(system2(file.path(R.home("bin"), "Rscript"), script,
                                  stdout = TRUE, stderr = TRUE))
  expect_true("ATTACHED=FALSE" %in% res, info = paste(res, collapse = "\n"))
  expect_true("HAS_ETH=TRUE" %in% res, info = paste(res, collapse = "\n"))
})

test_that("inflate_priors() logs empirical methods correctly and writes nothing to globalenv", {
  pri <- list(
    metadata = list(description = "test"),
    parameters_global = list(
      g = list(distribution = "gompertz", parameters = list(b = 0.5, eta = 2))
    ),
    parameters_location = list()
  )
  if (exists("method_used", envir = globalenv())) rm("method_used", envir = globalenv())
  msgs <- character()
  withCallingHandlers(
    suppressWarnings(inflate_priors(pri, inflation_factor = 2, n_samples = 2000L, verbose = TRUE)),
    message = function(m) { msgs <<- c(msgs, conditionMessage(m)); invokeRestart("muffleMessage") }
  )
  line <- grep("gompertz", msgs, value = TRUE)
  expect_length(line, 1L)
  expect_match(line, "empirical")
  expect_false(exists("method_used", envir = globalenv()))
})
