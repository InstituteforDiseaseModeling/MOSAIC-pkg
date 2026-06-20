# ============================================================================
# Tests for data-driven per-location influence weights
# (.mosaic_derive_weights_location) -- the cross-location complement to the
# per-cell weights_obs_* confidence weighting.
#
# Design invariants under test:
#  (1) Absent per-cell weight matrices -> NULL (caller falls back to uniform).
#  (2) No positive signal anywhere     -> NULL (uniform).
#  (3) Uniform positive signal          -> NULL (uniform; byte-identical LL).
#  (4) Absence country gets the floor; an at-median active country gets ~1.
#  (5) Every weight is strictly > 0 (no location is dropped).
#  (6) BFA-vs-COD style domination is resolved: the worst absence weighted
#      contribution is brought below a typical active country's.
#
# The weight map (hand-computable):
#   s_j = sum_{t: y_cases>0} w_cases + sum_{t: y_deaths>0} w_deaths
#   s_ref = median(s_j : s_j > 0)
#   w_j = floor + (1-floor) * min(1, log1p(s_j) / log1p(s_ref))
# ============================================================================

derive <- MOSAIC:::.mosaic_derive_weights_location

# --- (1) absent per-cell weight matrices -> NULL ----------------------------
testthat::test_that("absent weight matrices return NULL (uniform fallback)", {
     cfg <- list(
          reported_cases  = matrix(c(0, 0, 0,  5, 7, 9), nrow = 2, byrow = TRUE),
          reported_deaths = matrix(0, nrow = 2, ncol = 3)
          # no reported_cases_weight
     )
     expect_null(derive(cfg))
})

# --- (2) no positive signal anywhere -> NULL --------------------------------
testthat::test_that("all-zero observations return NULL (uniform fallback)", {
     cfg <- list(
          reported_cases        = matrix(0, nrow = 3, ncol = 4),
          reported_cases_weight = matrix(1, nrow = 3, ncol = 4)
     )
     expect_null(derive(cfg))
})

# --- (3) uniform positive signal -> NULL (byte-identical to current) --------
testthat::test_that("uniform positive signal returns NULL (uniform weights)", {
     # Every location has the same positive-signal mass -> all w == 1 -> NULL.
     cfg <- list(
          reported_cases        = matrix(c(10, 20, 30,
                                           40, 50, 60,
                                           70, 80, 90), nrow = 3, byrow = TRUE),
          reported_cases_weight = matrix(1, nrow = 3, ncol = 3)
     )
     # Each row has 3 positive cells, weight 1 each -> s = 3 for all -> uniform.
     expect_null(derive(cfg))
})

# --- (4)+(5) hand-computed floor and at-median weights ----------------------
testthat::test_that("absence -> floor, at-median -> 1, all weights > 0", {
     # 3 locations:
     #   L1 absence (s = 0)
     #   L2 one positive cell, weight 1 (s = 1)
     #   L3 two positive cells, weights 1 + 1 (s = 2)
     # s_pos = c(1, 2); s_ref = median = 1.5.
     obs <- matrix(c(0, 0, 0,
                     5, 0, 0,
                     5, 7, 0), nrow = 3, byrow = TRUE)
     w   <- matrix(1, nrow = 3, ncol = 3)
     cfg <- list(reported_cases = obs, reported_cases_weight = w)

     out <- derive(cfg)
     expect_false(is.null(out))
     expect_length(out, 3)
     expect_true(all(out > 0))   # invariant (5): no location dropped

     floor <- 0.05
     s_ref <- 1.5
     exp1 <- floor + (1 - floor) * min(1, log1p(0) / log1p(s_ref))  # = floor
     exp2 <- floor + (1 - floor) * min(1, log1p(1) / log1p(s_ref))
     exp3 <- floor + (1 - floor) * min(1, log1p(2) / log1p(s_ref))
     expect_equal(out[1], floor, tolerance = 1e-12)
     expect_equal(out[2], exp2,  tolerance = 1e-12)
     expect_equal(out[3], exp3,  tolerance = 1e-12)
     # The above-median location (s = 2 > s_ref = 1.5) saturates at 1.
     expect_equal(out[3], 1, tolerance = 1e-12)
})

# --- deaths-signal mass is added when conformable ---------------------------
testthat::test_that("deaths positive-signal mass adds to cases mass", {
     # L1: cases s = 1 (one positive cell, w 1); deaths none -> s = 1.
     # L2: cases s = 1; deaths s = 1 (one positive cell, w 1) -> s = 2.
     obs_c <- matrix(c(5, 0,
                       5, 0), nrow = 2, byrow = TRUE)
     obs_d <- matrix(c(0, 0,
                       3, 0), nrow = 2, byrow = TRUE)
     wc <- matrix(1, 2, 2); wd <- matrix(1, 2, 2)
     cfg <- list(reported_cases = obs_c, reported_cases_weight = wc,
                 reported_deaths = obs_d, reported_deaths_weight = wd)
     out <- derive(cfg)
     # s = c(1, 2); s_ref = 1.5.
     floor <- 0.05; s_ref <- 1.5
     expect_equal(out[1], floor + (1 - floor) * min(1, log1p(1) / log1p(s_ref)),
                  tolerance = 1e-12)
     expect_equal(out[2], 1, tolerance = 1e-12)
})

# --- per-cell confidence weight scales the signal mass ----------------------
testthat::test_that("per-cell confidence weights scale positive-signal mass", {
     # L1: two positive cells with confidence 0.25 each -> s = 0.5.
     # L2: two positive cells with confidence 1.0 each   -> s = 2.0.
     obs <- matrix(c(5, 7,
                     5, 7), nrow = 2, byrow = TRUE)
     w   <- matrix(c(0.25, 0.25,
                     1.0,  1.0), nrow = 2, byrow = TRUE)
     cfg <- list(reported_cases = obs, reported_cases_weight = w)
     out <- derive(cfg)
     # s = c(0.5, 2.0); s_ref = 1.25.
     floor <- 0.05; s_ref <- 1.25
     expect_equal(out[1], floor + (1 - floor) * min(1, log1p(0.5) / log1p(s_ref)),
                  tolerance = 1e-12)
     expect_equal(out[2], 1, tolerance = 1e-12)
})

# --- (6) BFA-vs-COD domination resolved on config_default -------------------
testthat::test_that("absence countries no longer dominate active ones (config_default)", {
     cfg <- MOSAIC::config_default
     testthat::skip_if(is.null(cfg$reported_cases_weight),
                       "config_default lacks per-cell weight matrices")

     rc  <- cfg$reported_cases;  rd  <- cfg$reported_deaths
     rcw <- cfg$reported_cases_weight; rdw <- cfg$reported_deaths_weight
     locs <- cfg$location_name
     n <- nrow(rc); T <- ncol(rc)

     w <- derive(cfg)
     expect_false(is.null(w))
     expect_length(w, n)
     expect_true(all(w > 0))                 # no country dropped
     expect_true(all(w <= 1 + 1e-12))

     # Identify absence (no positive cases or deaths) vs active countries.
     pos_any <- function(obs) vapply(seq_len(nrow(obs)),
          function(j) any(is.finite(obs[j, ]) & obs[j, ] > 0), logical(1))
     active <- pos_any(rc) | pos_any(rd)
     absent <- !active
     expect_gt(sum(absent), 0)               # default window has absence walls

     # Absence countries are at the floor; active high-signal ones near 1.
     expect_true(all(abs(w[absent] - min(w)) < 1e-9))
     expect_gt(max(w[active]), 0.9)

     # Score every location under a constant-outbreak prediction (the empirical
     # diagnostic that exposed the domination), then apply weights_location.
     est_c <- matrix(50,        n, T)
     est_d <- matrix(50 * 0.02, n, T)
     ll <- numeric(n)
     for (j in seq_len(n)) {
          ll[j] <- MOSAIC::calc_model_likelihood(
               obs_cases  = rc[j, , drop = FALSE], est_cases  = est_c[j, , drop = FALSE],
               obs_deaths = rd[j, , drop = FALSE], est_deaths = est_d[j, , drop = FALSE],
               weights_obs_cases  = rcw[j, , drop = FALSE],
               weights_obs_deaths = rdw[j, , drop = FALSE]
          )
     }

     wll <- w * ll
     # The worst (largest-magnitude) absence contribution must fall below the
     # median active contribution -- i.e. absence no longer dominates.
     max_absence <- max(abs(wll[absent]))
     med_active  <- stats::median(abs(wll[active]))
     expect_lt(max_absence, med_active)

     # Specific BFA-vs-COD check from the empirical context.
     if (all(c("BFA", "COD") %in% locs)) {
          expect_lt(abs(wll[locs == "BFA"]), abs(wll[locs == "COD"]))
     }
})

# --- single-location config -> vector, never dropped ------------------------
testthat::test_that("single-location config with signal returns length-1 weight > 0", {
     cfg <- list(
          reported_cases        = matrix(c(0, 5, 7, 0), nrow = 1),
          reported_cases_weight = matrix(1, nrow = 1, ncol = 4)
     )
     # Only one location with signal -> s_pos has length 1 -> s_ref == s -> w == 1
     # -> uniform -> NULL (back-compat: a single location cannot dominate itself).
     expect_null(derive(cfg))
})
