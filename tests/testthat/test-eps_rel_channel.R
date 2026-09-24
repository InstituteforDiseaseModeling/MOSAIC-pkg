# Regression tests for the per-channel relative epsilon floor (`eps_rel`).
#
# WHY THESE ARE VALUE TESTS, NOT PRESENCE TESTS (CLAUDE.md lesson #13):
# this package's single most-repeated bug is a control knob that is read,
# carried around, and then silently dropped -- a deprecation shim guarded by
# `is.null(<thing the defaults always fill>)` was dead for ~15 minor versions
# and nothing caught it, because every test asserted the FIELD was present
# rather than that the VALUE changed the answer. So every assertion below
# compares two log-likelihoods that must DIFFER, or compares one against a
# hand-computed density, and the top-level test drives `eps_rel` in from
# `mosaic_control_defaults()` the way a user actually sets it.

.eps_of <- function(obs, rel) max(1e-4, rel * mean(obs[is.finite(obs)], na.rm = TRUE))

test_that("eps_rel changes the returned negbin log-likelihood, and matches the hand density", {
     obs <- c(0, 5, 10, 20)
     est <- c(1, 0, 10, 20)          # cell 2 predicts zero against obs = 5

     lo <- MOSAIC::calc_log_likelihood_negbin(obs, est, k = 3, eps_rel = 0.02, verbose = FALSE)
     hi <- MOSAIC::calc_log_likelihood_negbin(obs, est, k = 3, eps_rel = 0.50, verbose = FALSE)

     # THE knob test: the value must move, and in the known direction -- a
     # bigger floor makes a zero prediction against a positive observation
     # cheaper, so the log-likelihood rises.
     expect_false(isTRUE(all.equal(lo, hi)))
     expect_gt(hi, lo)

     for (rel in c(0.02, 0.10, 0.25, 0.50)) {
          hand <- sum(stats::dnbinom(obs, mu = pmax(est, .eps_of(obs, rel)), size = 3, log = TRUE))
          expect_equal(
               MOSAIC::calc_log_likelihood_negbin(obs, est, k = 3, eps_rel = rel, verbose = FALSE),
               hand, tolerance = 1e-10)
     }

     # hand-computed fixture: mean(obs) = 8.75, so eps = 0.50 * 8.75 = 4.375 and
     # only cell 2 (est = 0) is floored. Cell 1 keeps est = 1 > 1e-4... but NOT
     # > 4.375, so it is floored too -- which is the point of pinning the
     # expected mu vector by hand rather than re-deriving it with pmax().
     expect_equal(round(hi, 6), round(sum(stats::dnbinom(
          c(0, 5, 10, 20), mu = c(4.375, 4.375, 10, 20), size = 3, log = TRUE)), 6))
     # and at the shipped deaths default (eps = 0.25 * 8.75 = 2.1875)
     expect_equal(
          round(MOSAIC::calc_log_likelihood_negbin(obs, est, k = 3, eps_rel = 0.25,
                                                   verbose = FALSE), 6),
          round(sum(stats::dnbinom(c(0, 5, 10, 20), mu = c(2.1875, 2.1875, 10, 20),
                                   size = 3, log = TRUE)), 6))
})

test_that("the default is 0.02, so pre-existing calls are unchanged", {
     obs <- c(0, 5, 10, 20); est <- c(1, 0, 10, 20)
     expect_equal(MOSAIC::calc_log_likelihood_negbin(obs, est, k = 3, verbose = FALSE),
                  MOSAIC::calc_log_likelihood_negbin(obs, est, k = 3, eps_rel = 0.02, verbose = FALSE))
     expect_equal(suppressWarnings(MOSAIC::calc_log_likelihood_poisson(obs, est, verbose = FALSE)),
                  suppressWarnings(MOSAIC::calc_log_likelihood_poisson(obs, est, eps_rel = 0.02,
                                                                       verbose = FALSE)))
})

test_that("eps_rel reaches the Poisson path too", {
     obs <- c(0, 5, 10, 20); est <- c(1, 0, 10, 20)
     # the fixture is overdispersed on purpose (it is the same one the negbin
     # tests use); the Poisson path's advisory warning is not what is under test
     lo <- suppressWarnings(
          MOSAIC::calc_log_likelihood_poisson(obs, est, eps_rel = 0.02, verbose = FALSE))
     hi <- suppressWarnings(
          MOSAIC::calc_log_likelihood_poisson(obs, est, eps_rel = 0.50, verbose = FALSE))
     expect_gt(hi, lo)
     expect_equal(hi, sum(stats::dpois(obs, pmax(est, .eps_of(obs, 0.50)), log = TRUE)),
                  tolerance = 1e-10)
})

test_that("eps_rel survives the calc_log_likelihood() dispatcher", {
     obs <- c(0, 5, 10, 20); est <- c(1, 0, 10, 20)
     expect_equal(
          MOSAIC::calc_log_likelihood(obs, est, family = "negbin", k = 3,
                                      eps_rel = 0.50, verbose = FALSE),
          MOSAIC::calc_log_likelihood_negbin(obs, est, k = 3, eps_rel = 0.50, verbose = FALSE))
     expect_equal(
          suppressWarnings(MOSAIC::calc_log_likelihood(obs, est, family = "poisson",
                                                       eps_rel = 0.50, verbose = FALSE)),
          suppressWarnings(MOSAIC::calc_log_likelihood_poisson(obs, est, eps_rel = 0.50,
                                                               verbose = FALSE)))
     # a family with no floor must REJECT the argument rather than eat it via `...`
     expect_error(MOSAIC::calc_log_likelihood(obs, est, family = "normal",
                                              eps_rel = 0.50, verbose = FALSE))
})

test_that("an unusable eps_rel is an error, never a silent fallback to the default", {
     obs <- c(0, 5, 10); est <- c(1, 0, 10)
     for (bad in list(0, -1, NA_real_, Inf, c(0.1, 0.2), "0.1", NULL)) {
          expect_error(MOSAIC::calc_log_likelihood_negbin(obs, est, k = 3,
                                                          eps_rel = bad, verbose = FALSE),
                       "eps_rel")
     }
})

# ---------------------------------------------------------------------------
# The knob must bite END TO END: control$likelihood -> calc_model_likelihood ->
# the scorer. A test that only checks the leaf function would not have caught
# lesson #13.
# ---------------------------------------------------------------------------

.fixture <- function() {
     set.seed(11)
     # one location, 60 steps; a low-count deaths channel with real structural
     # zeros in the PREDICTION, which is the cell type eps_rel prices.
     obs_c <- matrix(rpois(60, 30), nrow = 1)
     est_c <- matrix(rpois(60, 28), nrow = 1)
     est_c[1, c(3, 17, 41)] <- 0        # cases zeros are RARE (1.7% in production)
     obs_d <- matrix(rpois(60, 0.5), nrow = 1)
     est_d <- matrix(rpois(60, 0.4), nrow = 1)
     list(obs_c = obs_c, est_c = est_c, obs_d = obs_d, est_d = est_d)
}

test_that("calc_model_likelihood routes eps_rel per channel, and both channels bite", {
     f <- .fixture()
     stopifnot(any(f$est_d == 0 & f$obs_d > 0))   # the fixture exercises the floor

     base <- MOSAIC::calc_model_likelihood(
          f$obs_c, f$est_c, f$obs_d, f$est_d,
          nb_k_cases = 5, nb_k_deaths = 3,
          eps_rel_cases = 0.02, eps_rel_deaths = 0.02)
     dmoved <- MOSAIC::calc_model_likelihood(
          f$obs_c, f$est_c, f$obs_d, f$est_d,
          nb_k_cases = 5, nb_k_deaths = 3,
          eps_rel_cases = 0.02, eps_rel_deaths = 0.50)
     cmoved <- MOSAIC::calc_model_likelihood(
          f$obs_c, f$est_c, f$obs_d, f$est_d,
          nb_k_cases = 5, nb_k_deaths = 3,
          eps_rel_cases = 0.50, eps_rel_deaths = 0.02)

     expect_gt(dmoved, base)                      # deaths knob moved the total
     expect_false(isTRUE(all.equal(cmoved, base)))# cases knob moved the total
     expect_false(isTRUE(all.equal(cmoved, dmoved)))  # they are NOT the same knob

     # the deaths-channel move must equal the deaths block computed directly,
     # i.e. eps_rel_deaths touched deaths and ONLY deaths
     d_lo <- MOSAIC::calc_log_likelihood_negbin(as.numeric(f$obs_d), as.numeric(f$est_d),
                                                k = 3, eps_rel = 0.02, verbose = FALSE)
     d_hi <- MOSAIC::calc_log_likelihood_negbin(as.numeric(f$obs_d), as.numeric(f$est_d),
                                                k = 3, eps_rel = 0.50, verbose = FALSE)
     expect_equal(dmoved - base, d_hi - d_lo, tolerance = 1e-8)
})

test_that("the shipped defaults are cases 0.02 / deaths 0.25 and are actually asymmetric", {
     f <- .fixture()
     defaults <- MOSAIC::calc_model_likelihood(f$obs_c, f$est_c, f$obs_d, f$est_d,
                                               nb_k_cases = 5, nb_k_deaths = 3)
     explicit <- MOSAIC::calc_model_likelihood(f$obs_c, f$est_c, f$obs_d, f$est_d,
                                               nb_k_cases = 5, nb_k_deaths = 3,
                                               eps_rel_cases = 0.02, eps_rel_deaths = 0.25)
     expect_equal(defaults, explicit)

     # the old symmetric 0.02/0.02 behaviour must be measurably different, or the
     # whole change is inert
     symmetric <- MOSAIC::calc_model_likelihood(f$obs_c, f$est_c, f$obs_d, f$est_d,
                                                nb_k_cases = 5, nb_k_deaths = 3,
                                                eps_rel_cases = 0.02, eps_rel_deaths = 0.02)
     expect_gt(defaults - symmetric, 1)

     # NULL means "not supplied by an older control list" -> documented default,
     # not an error and not a dropped setting
     expect_equal(MOSAIC::calc_model_likelihood(f$obs_c, f$est_c, f$obs_d, f$est_d,
                                                nb_k_cases = 5, nb_k_deaths = 3,
                                                eps_rel_cases = NULL, eps_rel_deaths = NULL),
                  explicit)
     expect_error(MOSAIC::calc_model_likelihood(f$obs_c, f$est_c, f$obs_d, f$est_d,
                                                nb_k_cases = 5, nb_k_deaths = 3,
                                                eps_rel_deaths = -1), "eps_rel")
})

test_that("control$likelihood carries eps_rel and a user override reaches the scorer", {
     ctl <- MOSAIC::mosaic_control_defaults()
     expect_equal(ctl$likelihood$eps_rel_cases, 0.02)
     expect_equal(ctl$likelihood$eps_rel_deaths, 0.25)

     # partial override through the constructor must not wipe the sibling knob
     part <- MOSAIC::mosaic_control_defaults(likelihood = list(eps_rel_deaths = 0.50))
     expect_equal(part$likelihood$eps_rel_deaths, 0.50)
     expect_equal(part$likelihood$eps_rel_cases, 0.02)
     expect_equal(part$likelihood$weight_deaths, 1.0)

     # the usage pattern that lesson #13 broke: defaults() then override
     user <- MOSAIC::mosaic_control_defaults()
     user$likelihood$eps_rel_deaths <- 0.50
     merged <- MOSAIC:::.mosaic_validate_and_merge_control(user)
     expect_equal(merged$likelihood$eps_rel_deaths, 0.50)
     expect_equal(merged$likelihood$eps_rel_cases, 0.02)

     # and the merged value must CHANGE THE SCORE, not merely be present
     f <- .fixture()
     call_with <- function(cl) MOSAIC::calc_model_likelihood(
          f$obs_c, f$est_c, f$obs_d, f$est_d,
          nb_k_cases = 5, nb_k_deaths = 3,
          eps_rel_cases  = cl$likelihood$eps_rel_cases,
          eps_rel_deaths = cl$likelihood$eps_rel_deaths)
     expect_gt(call_with(merged),
               call_with(MOSAIC:::.mosaic_validate_and_merge_control(MOSAIC::mosaic_control_defaults())))
})

test_that("run_MOSAIC's worker forwards eps_rel from likelihood_settings", {
     # Guards the call site itself: a knob that exists in the control list but is
     # never passed to calc_model_likelihood() is exactly the lesson #13 failure.
     src <- deparse(MOSAIC:::.mosaic_run_simulation_worker)
     expect_true(any(grepl("eps_rel_cases\\s*=\\s*likelihood_settings\\$eps_rel_cases", src)))
     expect_true(any(grepl("eps_rel_deaths\\s*=\\s*likelihood_settings\\$eps_rel_deaths", src)))
})
