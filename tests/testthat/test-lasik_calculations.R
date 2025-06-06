library(reticulate)
mpm <- reticulate::import("laser_cholera.metapop.model")
filename <- file.path(PATHS$ROOT, "inst", "extdata", "default_parameters.json")
model <- mpm$run_model(paramfile = filename)
baseline <- jsonlite::fromJSON(filename)

# Check human-human seasonality computation
# LASIK values in model.patches.beta_j_seasonality
testthat::test_that("beta_j_seasonality matches", {

     # t = np.arange(0, p)
     #
     # model.patches.beta_j_seasonality = (
     #      params.beta_j0_hum
     #      * (
     #           1.0
     #           + params.a_1_j[None, :] * np.cos(2 * np.pi * t / params.p)[:, None]
     #           + params.b_1_j[None, :] * np.sin(2 * np.pi * t / params.p)[:, None]
     #           + params.a_2_j[None, :] * np.cos(4 * np.pi * t / params.p)[:, None]
     #           + params.b_2_j[None, :] * np.sin(4 * np.pi * t / params.p)[:, None]
     #      )
     # )

     expect_equal(model$patches$beta_j_seasonality)
})

# Check environmental seasonality computation
# LASIK values in model.patches.beta_env
testthat::test_that("beta_env matches", {

     # psi_bar = params.psi_jt.mean(axis=0, keepdims=True)
     # model.patches.beta_env = params.beta_j0_env.T * (1.0 + (params.psi_jt - psi_bar) / psi_bar)

     expect_equal(model$patches$beta_env)
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

     expect_equal(model$patches$delta_jt)
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

     p <- baseline$p
     T <- dim(model$people$S)[1] - 1
     t <- (0:T-1 %% p) + 1

     # Compute the seasonal component as a t x J matrix
     seasonal_component <- outer(t, baseline$a_1_j, function(t, a) a * cos(2 * pi * t / p)) +
          outer(t, baseline$b_1_j, function(t, b) b * sin(2 * pi * t / p)) +
          outer(t, baseline$a_2_j, function(t, a) a * cos(4 * pi * t / p)) +
          outer(t, baseline$b_2_j, function(t, b) b * sin(4 * pi * t / p))

     # Multiply baseline and add 1 — beta_j0_hum should be J-length vector
     beta <- t(t(1 + seasonal_component) * baseline$beta_j0_hum)
     expected <- calc_spatial_hazard(
          beta,
          baseline$tau_i,
          t(model$patches$pi_ij),
          model$patches$N,
          model$people$S,
          model$people$V1_sus,
          model$people$V2_sus,
          I1 = model$people$Isym,
          I2 = model$people$Iasym,
          time_names = NULL,
          location_names = NULL
     )

     expect_equal(expected, t(model$patches$spatial_hazard))
})

# Check coupling computation
# LASIK values in model.patches.coupling
testthat::test_that("coupling calculations match", {
     expect_equal(model$patches$coupling)
})

# TODO: reproductive number
# TODO: r-effective

# TODO: edge cases -

# Check birth and death rates (TBD)
