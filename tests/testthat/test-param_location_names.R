# Location names are not restricted to ISO codes: parameter names "<base>_<location>" are split by
# the config's location names (R/param_location_names.R), so any names that pass
# .mosaic_check_location_names() calibrate (before, only "_[A-Z]{3}$" suffixes were recognised and
# every location parameter of other names was recorded as NA in samples.parquet).

# Rename the locations everywhere in a config or priors object: list and vector names, matrix
# dimnames and location_name.
.rename_locations <- function(x, map) {
     if (is.list(x)) {
          at <- attributes(x)
          y <- lapply(x, .rename_locations, map = map)
          attributes(y) <- at
          if (!is.null(names(x))) names(y) <- ifelse(names(x) %in% names(map), map[names(x)], names(x))
          if (!is.null(y$location_name)) y$location_name <- unname(map[y$location_name])
          return(y)
     }
     if (!is.null(dimnames(x))) {
          dimnames(x) <- lapply(dimnames(x), function(d) if (is.null(d)) d else ifelse(d %in% names(map), map[d], d))
          return(x)
     }
     if (!is.null(names(x))) names(x) <- ifelse(names(x) %in% names(map), map[names(x)], names(x))
     x
}

test_that("ISO-coded names split exactly as the former three-letter rule did", {
     legacy <- function(nm) {
          loc <- grepl("_[A-Z]{3}$", nm)
          list(base = ifelse(loc, sub("_[A-Z]{3}$", "", nm), nm),
               location = ifelse(loc, sub(".*_([A-Z]{3})$", "\\1", nm), NA_character_))
     }
     cfg <- MOSAIC::config_default
     nm <- names(convert_config_to_matrix(cfg))
     L <- legacy(nm)
     for (locs in list(cfg$location_name, NULL)) {
          sp <- MOSAIC:::.mosaic_split_param_names(nm, locs)
          expect_identical(sp$base, L$base)
          expect_identical(sp$location, L$location)
     }
     expect_gt(sum(!is.na(L$location)), 1000L)
})

test_that("any location names split, the longest match wins, and bookkeeping columns stay global", {
     nm <- c("beta_j0_tot_MOZ_Cabo_Delgado", "beta_j0_tot_Gaza", "beta_j0_tot_MOZ_Gaza", "theta_j_Zambia.North",
             "N_j_initial_MOZ_Gaza", "mu_j_baseline_MOZ_Gaza", "phi_1", "seed_sim", "seed_iter", "likelihood",
             "psi_star_a_Lusaka-1")
     locs <- c("MOZ_Cabo_Delgado", "Gaza", "MOZ_Gaza", "Zambia.North", "Lusaka-1")
     for (given in list(locs, NULL)) {
          sp <- MOSAIC:::.mosaic_split_param_names(nm, given)
          expect_identical(sp$location, c("MOZ_Cabo_Delgado", "Gaza", "MOZ_Gaza", "Zambia.North", "MOZ_Gaza",
                                          "MOZ_Gaza", NA, NA, NA, NA, "Lusaka-1"))
          expect_identical(sp$base, c("beta_j0_tot", "beta_j0_tot", "beta_j0_tot", "theta_j", "N_j_initial",
                                      "mu_j_baseline", "phi_1", "seed_sim", "seed_iter", "likelihood",
                                      "psi_star_a"))
     }
     # a vector that is not per location is indexed "_1", "_2": not a location
     expect_true(all(is.na(MOSAIC:::.mosaic_split_param_names(c("tau_i_1", "tau_i_2"))$location)))
     expect_identical(MOSAIC:::.mosaic_param_label(c("beta_j0_tot_MOZ_Gaza", "phi_1")),
                      c("beta_j0_tot (MOZ_Gaza)", "phi_1"))
})

test_that("location names are checked before a calibration", {
     ok <- c("MOZ", "MOZ_Cabo_Delgado", "Zambia.North", "Lusaka-1", "MOZ_Zambézia", "KAA")
     expect_invisible(MOSAIC:::.mosaic_check_location_names(ok))
     expect_error(MOSAIC:::.mosaic_check_location_names(c("MOZ", "1abc")), "'1abc'")
     expect_error(MOSAIC:::.mosaic_check_location_names("Zambia North"), "'Zambia North'")
     expect_error(MOSAIC:::.mosaic_check_location_names(c("MOZ", NA)), "'NA'")
     expect_error(MOSAIC:::.mosaic_check_location_names(c("MOZ", "MOZ")), "duplicates \\(ignoring case\\): MOZ")
     expect_error(MOSAIC:::.mosaic_check_location_names(c("ETH", "eth")), "ignoring case")
     expect_error(MOSAIC:::.mosaic_check_location_names("tau_i"), "parameter or samples column names")
     expect_error(MOSAIC:::.mosaic_check_location_names("sim"), "parameter or samples column names")
     expect_error(MOSAIC:::.mosaic_check_location_names(character(0)), "non-empty")
     # a name that is the tail of a parameter name plus another location makes names ambiguous
     expect_error(MOSAIC:::.mosaic_check_location_names(c("X", "tot_X")), "would read as beta_j0 at 'tot_X'")
     expect_error(MOSAIC:::.mosaic_check_location_names(c("X", "initial_X")), "ambiguous")
     # samples bookkeeping columns are never split, so a location called "best" is harmless
     expect_invisible(MOSAIC:::.mosaic_check_location_names(c("MOZ", "best")))
     expect_true(all(is.na(MOSAIC:::.mosaic_split_param_names(c("weight_best", "seed_sim", "is_best_subset_opt"),
                                                               c("best", "sim", "opt"))$location)))
     # overlapping names that split cleanly are fine
     expect_invisible(MOSAIC:::.mosaic_check_location_names(c("Gaza", "MOZ_Gaza", "MOZ", "B_A", "A")))
})

test_that("a known location is stripped literally, and convert_matrix_to_config takes the longest location", {
     expect_identical(MOSAIC:::.mosaic_strip_location(c("beta_j0_tot_Zambia.North", "beta_j0_tot_ZambiaxNorth", "phi_1"),
                                                      c("Zambia.North", "Zambia.North", NA)),
                      c("beta_j0_tot", "beta_j0_tot_ZambiaxNorth", "phi_1"))
     cfg <- list(location_name = c("Gaza", "MOZ_Gaza"), beta_j0_tot = c(0, 0), epsilon = 0)
     out <- convert_matrix_to_config(c(beta_j0_tot_Gaza = 1, beta_j0_tot_MOZ_Gaza = 2, epsilon = 3), cfg)
     expect_identical(out$beta_j0_tot, c(1, 2))
     expect_identical(out$epsilon, 3)
})

test_that("run_MOSAIC rejects unusable location names before touching the output directory", {
     local_test_root()
     dir <- withr::local_tempdir()
     writeLines("earlier run", file.path(dir, "keep.txt"))
     cfg <- get_location_config(iso = "MOZ", config = MOSAIC::config_default)
     pri <- get_location_priors(iso = "MOZ", priors = MOSAIC::priors_default)
     bad <- .rename_locations(cfg, c(MOZ = "Cabo Delgado"))
     for (clean in c(TRUE, FALSE)) {
          ctrl <- mosaic_control_defaults(calibration = list(n_simulations = 10L, n_iterations = 1L),
                                          paths = list(clean_output = clean), logging = list(verbose = FALSE))
          expect_error(suppressMessages(run_MOSAIC(bad, pri, dir_output = dir, control = ctrl)), "'Cabo Delgado'")
          expect_identical(list.files(dir, recursive = TRUE), "keep.txt")
     }
})

test_that("run_MOSAIC records every location parameter whatever the locations are called", {
     local_test_root()
     cfg <- get_location_config(iso = c("MOZ", "ZMB"), config = MOSAIC::config_default)
     pri <- get_location_priors(iso = c("MOZ", "ZMB"), priors = MOSAIC::priors_default)
     map <- c(MOZ = "MOZ_Cabo_Delgado", ZMB = "Zambia.North")
     rows <- lapply(list(iso = NULL, renamed = map), function(m) {
          c1 <- if (is.null(m)) cfg else .rename_locations(cfg, m)
          p1 <- if (is.null(m)) pri else .rename_locations(pri, m)
          s <- sample_parameters(PATHS = get_paths(), priors = p1, config = c1, seed = 11, verbose = FALSE)
          nm <- names(convert_config_to_matrix(s))
          lk <- MOSAIC:::.mosaic_build_param_lookup(nm, c1$location_name)
          stats::setNames(MOSAIC:::.mosaic_extract_param_row(s, lk, length(nm)), nm)
     })
     expect_false(anyNA(rows$renamed))
     expect_equal(sum(grepl("_(MOZ_Cabo_Delgado|Zambia\\.North)$", names(rows$renamed))), 58L)
     # the same draws, under the new names
     renamed_back <- names(rows$renamed)
     for (k in names(map)) renamed_back <- sub(paste0("_", map[[k]], "$"), paste0("_", k), renamed_back)
     expect_identical(unname(rows$renamed), unname(rows$iso[renamed_back]))
})

test_that("a stubbed calibration with renamed locations equals the ISO-named one", {
     skip_if_not(nzchar(Sys.getenv("MOSAIC_RUN_INTEGRATION")),
                 "set MOSAIC_RUN_INTEGRATION=1 to run the renamed-location stub runs")
     skip_if_testthat_parallel()
     local_test_root()
     map <- c(MOZ = "MOZ_Gaza", ZMB = "Gaza")          # overlapping: "Gaza" must not capture "MOZ_Gaza"
     gc0 <- get_location_config
     gp0 <- get_location_priors
     a <- withr::local_tempdir()
     b <- withr::local_tempdir()
     .stub_run_mosaic(a, iso = c("MOZ", "ZMB"), n_sims = 120L)
     testthat::with_mocked_bindings(
          .stub_run_mosaic(b, iso = c("MOZ", "ZMB"), n_sims = 120L, plots = TRUE),
          get_location_config = function(...) .rename_locations(gc0(...), map),
          get_location_priors = function(...) .rename_locations(gp0(...), map),
          .package = "MOSAIC")
     sa <- as.data.frame(arrow::read_parquet(file.path(a, "2_calibration", "samples.parquet")))
     sb <- as.data.frame(arrow::read_parquet(file.path(b, "2_calibration", "samples.parquet")))
     for (k in names(map)) names(sa) <- sub(paste0("_", k, "$"), paste0("_", map[[k]]), names(sa))
     expect_setequal(names(sb), names(sa))
     expect_equal(sb, sa[, names(sb)], ignore_attr = TRUE)
     sp <- MOSAIC:::.mosaic_split_param_names(names(sb), unname(map))
     expect_equal(as.vector(table(sp$location)), c(29L, 29L))
     expect_false(anyNA(sb[, !is.na(sp$location)]))
     qb <- utils::read.csv(file.path(b, "2_calibration", "posterior", "posterior_quantiles.csv"))
     expect_setequal(setdiff(unique(qb$location), ""), unname(map))
     pb <- jsonlite::read_json(file.path(b, "2_calibration", "posterior", "posteriors.json"))
     expect_setequal(names(pb$parameters_location$beta_j0_tot$location), unname(map))
     figs <- list.files(file.path(b, "3_results", "figures"), recursive = TRUE)
     expect_true(any(grepl("MOZ_Gaza", figs, fixed = TRUE)))
     expect_true(any(grepl("(?<!MOZ)_Gaza", figs, perl = TRUE)))   # Gaza's own figures, not MOZ_Gaza's
     cd <- jsonlite::read_json(file.path(b, "3_results", "calibration_diagnostics.json"), simplifyVector = TRUE)
     expect_setequal(unlist(cd$run$locations), unname(map))
     pr <- cd$parameters
     if (is.data.frame(pr) && all(c("parameter", "base", "location") %in% names(pr))) {
          loc_rows <- !is.na(pr$location)
          expect_true(any(loc_rows))
          expect_identical(paste0(pr$base[loc_rows], "_", pr$location[loc_rows]), pr$parameter[loc_rows])
     }
})
