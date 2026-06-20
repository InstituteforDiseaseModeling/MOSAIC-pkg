# Unit tests for .ic_select_epoch() -- the pure IC-seeding-epoch selector defined in
# data-raw/make_priors_default.R. The selector picks the broadest-data-coverage month
# (max distinct active-case countries) within [date_start, date_start+12mo], ties broken
# to the earliest month, falling back to date_start (with a warning) on a data-empty span.
#
# We extract ONLY the .ic_select_epoch <- function(...) definition from the build script
# (rather than source()-ing the whole thing, which would trigger the full priors build)
# so the test stays pinned to the single canonical implementation.

extract_ic_select_epoch <- function() {
     candidates <- c(
          "../../data-raw/make_priors_default.R",   # from tests/testthat/
          "data-raw/make_priors_default.R",         # from package root
          file.path(testthat::test_path(), "..", "..", "data-raw", "make_priors_default.R")
     )
     path <- candidates[file.exists(candidates)][1]
     if (is.na(path)) testthat::skip("make_priors_default.R not found (data-raw not shipped)")
     exprs <- parse(file = path)
     env <- new.env(parent = baseenv())
     for (e in exprs) {
          if (is.call(e) && (identical(e[[1]], as.name("<-")) || identical(e[[1]], as.name("="))) &&
              is.name(e[[2]]) && identical(as.character(e[[2]]), ".ic_select_epoch")) {
               eval(e, envir = env)
               break
          }
     }
     if (!exists(".ic_select_epoch", envir = env, inherits = FALSE))
          testthat::skip(".ic_select_epoch definition not found in make_priors_default.R")
     get(".ic_select_epoch", envir = env)
}

.ic_select_epoch <- extract_ic_select_epoch()

# Synthetic weekly surveillance fixture builder: weekly steps over [from, to], and a
# named list mapping iso_code -> the dates at which that country has cases > 0. Each
# requested active date is SNAPPED to the nearest fixture week so callers can specify
# approximate month anchors without matching the exact weekly grid. All other
# country-weeks are 0.
make_surv <- function(from, to, active) {
     weeks <- seq(as.Date(from), as.Date(to), by = "1 week")
     snap  <- function(d) weeks[which.min(abs(as.numeric(weeks - as.Date(d))))]
     isos  <- names(active)
     grid  <- expand.grid(iso_code = isos, date_start = weeks,
                          stringsAsFactors = FALSE)
     grid$cases <- 0
     for (iso in isos) {
          for (d in as.Date(active[[iso]])) {
               wk <- snap(d)
               grid$cases[grid$iso_code == iso & grid$date_start == wk] <- 100
          }
     }
     grid
}

test_that("2023 default resolves to 2023-02-01 (shipped-anchor invariant)", {
     # Reproduces the real-data plateau shape (the binding HARD CONSTRAINT: the shipped
     # 2023 priors_default must seed ICs from 2023-02-01). In the real combined weekly
     # file the breadth profile is Jan = 21, Feb-Oct = 22, Nov-Dec = 20; so 2023-02 is
     # the EARLIEST month at the plateau max. We mimic that: 21 countries active from
     # Jan, a 22nd joining only in mid-March. Jan-01's +/-8wk window reaches only to
     # ~Feb-26 so it sees just 21; Feb-01's window reaches to ~Mar-29 so it picks up the
     # 22nd -> Feb is the unique earliest plateau-max month.
     core <- sprintf("C%02d", 1:21)
     active <- setNames(
          lapply(core, function(x) seq(as.Date("2023-01-15"), as.Date("2023-10-15"), by = "1 month")),
          core
     )
     active[["C22"]] <- seq(as.Date("2023-03-15"), as.Date("2023-10-15"), by = "1 month")
     surv <- make_surv("2023-01-01", "2024-02-01", active)
     res  <- .ic_select_epoch(surv, as.Date("2023-01-01"), window_days = 56L)
     expect_s3_class(res$ic_t0, "Date")
     expect_equal(res$ic_t0, as.Date("2023-02-01"))
})

test_that("epoch tracks an early date_start", {
     # Single country active only mid-2015 (June); with a +/-8wk window the breadth peak
     # sits on 2015-06. The selected epoch must land in that mid-year window (tracking
     # the early date_start), NOT default to the 2023 floor or the first month.
     surv <- make_surv("2015-01-01", "2016-02-01",
                       list(AAA = as.Date("2015-06-15")))
     res  <- .ic_select_epoch(surv, as.Date("2015-01-01"), window_days = 56L)
     # The earliest candidate month whose +/-8wk window reaches the mid-June signal is
     # 2015-05 (May-01 is ~45d before Jun-15). Selection tracks the early date_start...
     expect_equal(format(res$ic_t0, "%Y-%m"), "2015-05")
     # ...and does NOT default to the shipped 2023 anchor or the first month of the span.
     expect_lt(res$ic_t0, as.Date("2016-01-01"))
     expect_gt(res$ic_t0, as.Date("2015-01-01"))
})

test_that("ties are broken to the earliest month", {
     # Two clusters >8 months apart so their +/-8wk windows never overlap. Cluster 1
     # (mid-April): X,Y active -> breadth 2. Cluster 2 (mid-December): X,Y active ->
     # breadth 2. Equal breadth -> the EARLIER cluster must win. The earliest candidate
     # month whose +/-8wk window reaches the mid-April signal is 2020-03.
     d_apr <- as.Date("2020-04-15"); d_dec <- as.Date("2020-12-15")
     surv  <- make_surv("2020-01-01", "2021-06-01",
                        list(X = c(d_apr, d_dec), Y = c(d_apr, d_dec)))
     res   <- .ic_select_epoch(surv, as.Date("2020-01-01"), window_days = 56L)
     expect_equal(format(res$ic_t0, "%Y-%m"), "2020-03")
     # the later (December) cluster, equal in breadth, must NOT win the tie
     expect_lt(res$ic_t0, as.Date("2020-07-01"))
})

test_that("data-empty span falls back to date_start and warns", {
     # All cases zero -> no active-case countries anywhere in the span.
     surv <- make_surv("2010-01-01", "2011-02-01", list(A = as.Date("2010-06-07")))
     surv$cases <- 0
     expect_warning(
          res <- .ic_select_epoch(surv, as.Date("2010-01-01"), window_days = 56L),
          "COLD-START"
     )
     expect_equal(res$ic_t0, as.Date("2010-01-01"))
     expect_true(all(res$nactive == 0L))
})

test_that("breadth, not volume, drives the choice", {
     # Two clusters spaced >8 months apart so their +/-8wk windows do not overlap.
     # Cluster A (2022-03): 5 countries, one active week each -> broad (breadth 5),
     #   low total volume. Cluster B (2022-11): 1 country active across MANY weeks ->
     #   narrow (breadth 1) but huge total volume. The breadth criterion must pick A;
     #   a volume criterion would (wrongly) pick B -- exactly the outbreak-peak over-
     #   seeding bias that breadth is chosen to avoid.
     wkA <- as.Date("2022-03-15")
     broad  <- setNames(lapply(sprintf("B%d", 1:5), function(x) wkA), sprintf("B%d", 1:5))
     narrow <- list(HOT = seq(as.Date("2022-10-15"), as.Date("2022-12-15"), by = "1 week"))
     surv <- make_surv("2022-01-03", "2023-06-30", c(broad, narrow))
     res  <- .ic_select_epoch(surv, as.Date("2022-01-01"), window_days = 56L)
     # earliest candidate month whose +/-8wk window reaches the broad mid-March cluster
     expect_equal(format(res$ic_t0, "%Y-%m"), "2022-02")
     # the narrow-but-high-volume cluster (Oct-Dec) must NOT be selected
     expect_false(format(res$ic_t0, "%Y-%m") %in% c("2022-09", "2022-10", "2022-11", "2022-12"))
})
