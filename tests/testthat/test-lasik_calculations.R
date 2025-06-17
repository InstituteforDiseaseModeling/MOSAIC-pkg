library(reticulate)

# Load default model and set baseline
mpm      <- reticulate::import("laser_cholera.metapop.model")
filename <- file.path(getwd(), "inst", "extdata", "default_parameters.json")
model    <- mpm$run_model(paramfile = filename)
baseline <- jsonlite::fromJSON(filename)

# Check human-human seasonality computation
# LASIK values in model.patches.beta_j_seasonality
testthat::test_that("beta_j_seasonality matches", {

     time_names <- seq(as.Date(model$params$date_start), as.Date(model$params$date_stop), 1)
     location_names <- model$params$location_name

     beta_j0_hum <- model$params$beta_j0_hum
     beta_jt_hum_model <- model$patches$beta_jt_human
     beta_jt_hum_expected <- matrix(NA_real_, nrow = length(time_names), ncol = length(location_names))

     # Baseline Fourier seasonality terms
     a1 <- baseline$a_1_j
     a2 <- baseline$a_2_j
     b1 <- baseline$a_1_j
     b2 <- baseline$b_2_j
     p <- baseline$p

     for (t in 1:length(time_names)) {
          for (j in 1:length(location_names)) {

               seasonal_term <-
                    a1[j] * cos(2 * pi * t / p) +
                    b1[j] * sin(2 * pi * t / p) +
                    a2[j] * cos(4 * pi * t / p) +
                    b2[j] * sin(4 * pi * t / p)

               beta_jt_hum_expected[t, j] <- beta_j0_hum[j] * (1 + seasonal_term)

          }
     }

     testthat::expect_equal(beta_jt_hum_expected, beta_jt_hum_model)

})

# Check environmental seasonality computation
# LASIK values in model.patches.beta_env
testthat::test_that("beta_env matches", {

     # psi_bar = params.psi_jt.mean(axis=0, keepdims=True)
     # model.patches.beta_env = params.beta_j0_env.T * (1.0 + (params.psi_jt - psi_bar) / psi_bar)

     expected <- NULL # ¡TODO!
     expect_equal(expected, model$patches$beta_env)
})

# Check environmental decay computation
# LASIK values in model.patches.delta_jt
testthat::test_that("delta_jt matches", {

     # model.patches.delta_jt = map_suitability_to_decay(
     #      fast = params.decay_days_short,
     #      slow = params.decay_days_long,
     #      suitability = params.psi_jt,
     #      beta_a = params.decay_shape_1,
     #      beta_b = params.decay_shape_2,
     # ) = 1.0 / fast + beta.cdf(suitability, beta_a, beta_b) * (1.0 / slow - 1.0 / fast)

     expected <- NULL # ¡TODO!
     expect_equal(expected, model$patches$delta_jt)
})

# Check OCV first dose schedule
# LASIK values in model.patches.dose_one_doses and model.patches.dose_two_doses
testthat::test_that("OCV dose one doses match", {

     # At a minimum, params$nu_1_jt != 0 where model$patches$dose_one_doses != 0
     # The converse _might_ not be true if there are no susceptibles to dose.

     # Transpose Python data from [time, patch] to [patch, time]
     sim_non_zero <- t(model$patches$dose_one_doses) != 0
     input_values <- baseline$nu_1_jt[sim_non_zero]
     good <- all(input_values != 0)

     testthat::expect_true(good)
})

# Check OCV second dose schedule
# LASIK values in model.patches.dose_one_doses and model.patches.dose_two_doses
testthat::test_that("OCV dose two doses match", {

     # At a minimum, params$nu_2_jt != 0 where model$patches$dose_two_doses != 0
     # The converse _might_ not be true if there are no susceptibles to dose.

     # Transpose Python data from [time, patch] to [patch, time]
     sim_non_zero <- t(model$patches$dose_two_doses) != 0
     input_values <- baseline$nu_2_jt[sim_non_zero]
     good <- all(input_values != 0)

     testthat::expect_true(good)
})

# Check pi_ij matrix
# LASIK values in model.patches.pi_ij
testthat::test_that("pi_ij calculations match", {

     # TODO - mobility::get_distance_matrix uses an underlying utility which
     # calculates _planar_ distance, not spherical. As a result, distances in
     # D will not match the distances used in the LASIK Python code which
     # calculates distances on a sphere using the Haversine formula
     D <- mobility::get_distance_matrix(
          x = baseline$longitude,
          y = baseline$latitude,
          id = baseline$location_name
     ) * 111.35
     expected <- MOSAIC::calc_diffusion_matrix_pi(
          D = D,
          N = baseline$N_j_initial,
          omega = baseline$mobility_omega,
          gamma = baseline$mobility_gamma
     )

     actual <- model$patches$pi_ij
     percentage <- 100 * abs(expected - actual) / expected

     testthat::expect_lt(max(percentage, na.rm = TRUE), 1.0)
})

# Check log_likelihood computation
# LASIK value in model.log_likelihood
testthat::test_that("log likelihood calculations match", {
     # TODO - try several different points in parameter space
     expected <- MOSAIC::get_model_likelihood(
          obs_cases=baseline$reported_cases,
          est_cases=t(model$patches$incidence[-1,]),
          obs_deaths=baseline$reported_deaths,
          est_deaths=t(model$patches$disease_deaths[-1,]))
     diff <- abs((expected - model$log_likelihood) / expected)

     testthat::expect_lt(diff, 0.01)
})

# Check spatial hazard computation
# LASIK values in model.patches.spatial_hazard
testthat::test_that("spatial hazard calculations match", {

     p      <- baseline$p
     nticks <- dim(model$people$S)[1] - 1
     t      <- ((0:(nticks-1)) %% p) + 1

     # Compute the seasonal component as a t x J matrix
     seasonal_component <- outer(t, baseline$a_1_j, function(t, a) a * cos(2 * pi * t / p)) +
          outer(t, baseline$b_1_j, function(t, b) b * sin(2 * pi * t / p)) +
          outer(t, baseline$a_2_j, function(t, a) a * cos(4 * pi * t / p)) +
          outer(t, baseline$b_2_j, function(t, b) b * sin(4 * pi * t / p))

     # Multiply baseline and add 1 — beta_j0_hum should be J-length vector
     beta <- t(t(1 + seasonal_component) * baseline$beta_j0_hum)
     expected <- MOSAIC::calc_spatial_hazard(
          beta,
          baseline$tau_i,
          t(model$patches$pi_ij),
          model$patches$N[-1,],
          model$people$S[-1,],
          model$people$V1sus[-1,],
          model$people$V2sus[-1,],
          I1 = model$people$Isym[-1,],
          I2 = model$people$Iasym[-1,],
          time_names = NULL,
          location_names = NULL
     )

     expect_equal(expected, t(model$patches$spatial_hazard))
})

# Check coupling computation
# LASIK values in model.patches.coupling
testthat::test_that("coupling calculations match", {
     expected <- NULL # ¡TODO!
     expect_equal(expected, model$patches$coupling)
})

# TODO: reproductive number
testthat::test_that("reproductive number calculations match", {})

# TODO: r-effective
testthat::test_that("r_effective calculations match", {})

# TODO? edge cases -
# Check birth and death rates (TBD)

# TODO? population trends -
# Check that total population tracks expected population sizes from UN WPP data
