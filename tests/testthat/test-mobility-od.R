# Regression tests for the overland-mobility chain.
# Network-free: everything runs against tempdir().

test_that("run suffix is applied to EVERY artifact name, including uppercase stems", {
     # Regression: the original routing regex was [a-z_]+, so mobility_M.csv,
     # mobility_D.csv and mobility_N.csv were written UNSUFFIXED and a fused
     # run silently overwrote the production air-derived files.
     for (f in c("mobility_M.csv", "mobility_D.csv", "mobility_N.csv",
                 "mobility_pi.csv", "param_tau_departure.csv",
                 "fused_od_1_sources.png")) {
          out <- MOSAIC:::.mosaic_suffix_path(file.path("d", f), "_fused")
          expect_false(identical(out, file.path("d", f)), info = f)
          expect_true(grepl("_fused\\.[a-z]+$", out), info = f)
     }
     expect_identical(MOSAIC:::.mosaic_suffix_path("d/x.csv", ""), "d/x.csv")
})

test_that("a non-default run cannot write to production filenames", {
     expect_error(MOSAIC:::.mosaic_check_suffix("", FALSE), "must not write")
     expect_error(MOSAIC:::.mosaic_check_suffix(NA_character_, FALSE), "non-NA")
     expect_error(MOSAIC:::.mosaic_check_suffix(c("a", "b"), FALSE), "single")
     expect_silent(MOSAIC:::.mosaic_check_suffix("", TRUE))
     expect_silent(MOSAIC:::.mosaic_check_suffix("_fused", FALSE))
})

test_that("SCI reader keeps Namibia, whose ISO2 is the string 'NA'", {
     # Regression: read.csv's default na.strings = "NA" silently deleted all
     # 178 Namibian rows before countrycode() saw them, leaving NAM the only
     # all-zero row and column of the SCI component. Exercises the package
     # reader .od_sci(), not base read.csv.
     skip_if_not_installed("countrycode")
     d <- withr::local_tempdir()
     writeLines(c("user_country,friend_country,scaled_sci",
                  "NA,ZA,500", "ZA,NA,400", "BW,ZA,100"),
                file.path(d, "meta_sci_country.csv"))
     iso <- c("BWA", "NAM", "ZAF")

     M <- MOSAIC:::.od_sci(d, iso, verbose = FALSE, denormalise = FALSE)

     expect_identical(dimnames(M), list(iso, iso))
     expect_gt(M["NAM", "ZAF"], 0)
     expect_gt(M["ZAF", "NAM"], 0)
     expect_gt(M["BWA", "ZAF"], 0)
})

test_that("Beta prior warns at the real J-shape cliff, not a decorative one", {
     # shape1 = (1-mu)/CV^2, so the mode collapses to zero at CV >= 1
     # regardless of mu. The old variance clamp only fired above CV ~ 53.
     mu <- 3.571e-4
     expect_silent(MOSAIC:::.beta_from_mean_sd(mu, mu * 0.45))
     expect_warning(MOSAIC:::.beta_from_mean_sd(mu, mu * 1.2), "J-shaped")

     b <- MOSAIC:::.beta_from_mean_sd(mu, mu * 0.45)
     expect_gt(b$shape1, 1)                                   # unimodal
     # exact: shape1 = mu*k with k = mu(1-mu)/v - 1, v = (mu*CV)^2
     #        => shape1 = (1-mu)/CV^2 - mu
     expect_equal(b$shape1, (1 - mu) / 0.45^2 - mu, tolerance = 1e-10)
})

test_that("documented prior interval widths match what the code produces", {
     # The roxygen claimed CV 0.75 gives "roughly a factor of 10"; it gives
     # 29.3x. Lock both so doc and code cannot drift apart again.
     mu <- 3.571e-4
     span <- function(cv) {
          b <- MOSAIC:::.beta_from_mean_sd(mu, mu * cv)
          q <- stats::qbeta(c(0.025, 0.975), b$shape1, b$shape2)
          q[2] / q[1]
     }
     expect_equal(span(0.45), 6.4, tolerance = 0.1)
     expect_equal(span(0.75), 29.3, tolerance = 0.5)
})

test_that("rake_mobility_od_to_tau hits the tau margins and keeps the row structure", {
     # A row-only IPF has a closed form -- seed rows rescaled to the departure
     # targets -- so the rake must reproduce the targets exactly while leaving
     # every row's destination shares untouched.
     skip_if_not_installed("mipfp")
     d <- withr::local_tempdir()
     dir.create(file.path(d, "mobility"))
     iso <- c("AAA", "BBB", "CCC", "DDD")
     set.seed(1)
     M <- matrix(runif(16), 4, dimnames = list(iso, iso))
     diag(M) <- 0
     utils::write.csv(as.data.frame(M), file.path(d, "mobility", "M_structure_fused.csv"))
     tau <- stats::setNames(c(1e-3, 2e-3, 5e-4, 4e-3), iso)
     N   <- stats::setNames(c(1e6, 2e6, 5e5, 3e6), iso)

     R <- rake_mobility_od_to_tau(list(DATA_PROCESSED = d), iso_codes = iso,
                                  tau_daily = tau, N = N, verbose = FALSE)

     expect_equal(unname(rowSums(R)), unname(tau * N), tolerance = 1e-8)
     expect_equal(unname(R / rowSums(R)), unname(M / rowSums(M)), tolerance = 1e-8)
     expect_true(file.exists(file.path(d, "mobility", "M_fused_raked.csv")))
})

test_that("freshness scan drops docs but keeps data files named README_*", {
     root <- withr::local_tempdir()
     dd <- file.path(root, "MOSAIC-data", "processed", "mobility")
     dir.create(dd, recursive = TRUE)
     for (f in c("README.md", "README", "notes.md", "README_country_counts.csv", "data.csv")) {
          writeLines("x", file.path(dd, f))
     }

     out <- MOSAIC:::.freshness_outputs(root, stale_days = 30)

     expect_identical(out$directory, "mobility")
     expect_identical(out$n_files, 2L)
})

test_that("no roxygen block hand-escapes a percent sign", {
     # Under Roxygen markdown, a hand-written \% double-escapes to \\%, which
     # Rd reads as backslash + comment-start. That swallows the rest of the
     # line INCLUDING a closing brace, silently unbalancing the enclosing
     # macro and deleting the whole section from the manual. It bit
     # download_WB_data (dropped \details) and plot_mobility_fused (broke
     # \describe, producing an install-time WARNING).
     skip_on_cran()
     root <- tryCatch(rprojroot::find_package_root_file(), error = function(e) NA_character_)
     skip_if(is.na(root), "package source not available")
     rfiles <- list.files(file.path(root, "R"), pattern = "\\.R$", full.names = TRUE)
     skip_if(length(rfiles) == 0L, "no R/ sources")
     offenders <- Filter(function(f) {
          any(grepl("^#'", readLines(f, warn = FALSE)) &
              grepl("\\\\%", readLines(f, warn = FALSE)))
     }, rfiles)
     expect_equal(basename(offenders), character(0))
})
