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

     if (F) {

          library(ggplot2)
          library(grid)  # for unit()

          # 1. get dimensions
          T_model    <- nrow(beta_jt_hum_model)
          T_expected <- nrow(beta_jt_hum_expected)
          nloc       <- length(location_names)

          # 2. build long-form data.frames in base R
          df_model <- data.frame(
               time     = rep(seq_len(T_model),    times = nloc),
               location = rep(location_names, each = T_model),
               beta     = as.vector(beta_jt_hum_model),
               Series   = "Model",
               stringsAsFactors = FALSE
          )

          df_expected <- data.frame(
               time     = rep(seq_len(T_expected), times = nloc),
               location = rep(location_names,  each = T_expected),
               beta     = as.vector(beta_jt_hum_expected),
               Series   = "Expected",
               stringsAsFactors = FALSE
          )

          df_beta <- rbind(df_model, df_expected)

          ggplot(df_beta, aes(x = time, y = beta, color = Series)) +
               geom_line(size = 1.2) +
               scale_color_manual(
                    values = c("Model" = "dodgerblue", "Expected" = "darkorange")
               ) +
               facet_wrap(~ location, scales = "free_x", ncol = 4) +
               labs(
                    x     = NULL,
                    y     = expression(beta[j]^{human}),
                    title = "Human–human seasonality: Model vs Expected"
               ) +
               theme_minimal(base_size = 9) +
               theme(
                    panel.spacing    = unit(0.05, "lines"),
                    plot.margin      = unit(rep(1, 4), "mm"),
                    strip.text       = element_text(margin = margin(t = 1, b = 1, unit = "mm"),
                                                    size   = 8),
                    axis.text        = element_text(size = 6),
                    axis.title       = element_text(size = 8),
                    legend.position  = "bottom",
                    legend.key.size  = unit(4, "mm"),
                    legend.spacing.x = unit(1, "mm")
               )
     }


})

# Check environmental seasonality computation
# LASIK values in model.patches.beta_env
testthat::test_that("beta_env matches", {

     beta_jt_env_model <- model$patches$beta_jt_env

     # pull out the raw psi_jt matrix and beta0_env vector from the baseline JSON
     Psi       <- baseline$psi_jt            # dim: (time × locations)
     beta0_env <- baseline$beta_j0_env       # length = # locations
     psi_bar   <- rowMeans(Psi) # compute the time‐mean for each location

     # build expected exactly as LASIK does:
     beta_jt_env_expected <- matrix(NA_real_, nrow = ncol(Psi), ncol = nrow(Psi))

     for (t in 1:ncol(Psi)) {
          for (j in 1:nrow(Psi)) {

               beta_jt_env_expected[t, j] <- beta0_env[j] * (1 + (Psi[j, t] - psi_bar[j]) / psi_bar[j])

          }
     }

     # compare to what the Python model actually produced
     testthat::expect_equal(beta_jt_env_expected, beta_jt_env_model, tolerance = 1e-06)


     if(F) {

          library(ggplot2)
          library(grid)  # for unit()

          # build date and location vectors
          dates          <- seq(as.Date(model$params$date_start),
                                as.Date(model$params$date_stop), by = "day")
          location_names <- model$params$location_name

          # grab model vs expected matrices
          beta_model    <- model$patches$beta_jt_env
          beta_expected <- beta_jt_env_expected

          # long‐form data.frames without any temp T
          df_model <- data.frame(
               date     = rep(dates,    times = ncol(beta_model)),
               location = rep(location_names, each = nrow(beta_model)),
               beta     = as.vector(beta_model),
               Series   = "Model",
               stringsAsFactors = FALSE
          )
          df_expected <- data.frame(
               date     = rep(dates,    times = ncol(beta_expected)),
               location = rep(location_names, each = nrow(beta_expected)),
               beta     = as.vector(beta_expected),
               Series   = "Expected",
               stringsAsFactors = FALSE
          )

          df_plot <- rbind(df_model, df_expected)

          # facet‐wrapped comparison
          ggplot(df_plot, aes(x = date, y = beta, color = Series)) +
               geom_line(size = 1.2) +
               scale_color_manual(values = c(Model = "darkgreen", Expected = "red3")) +
               facet_wrap(~ location, scales = "free_x", ncol = 4) +
               labs(
                    x     = "Date",
                    y     = expression(beta[j]^{env}),
                    title = "Environmental seasonality: Model vs Expected"
               ) +
               theme_minimal(base_size = 9) +
               theme(
                    panel.spacing     = unit(0.05, "lines"),
                    plot.margin       = unit(rep(1, 4), "mm"),
                    strip.text        = element_text(margin = margin(t = 1, b = 1, unit = "mm"),
                                                     size   = 8),
                    axis.text         = element_text(size = 6),
                    axis.title        = element_text(size = 8),
                    legend.position   = "bottom",
                    legend.key.size   = unit(4, "mm"),
                    legend.spacing.x  = unit(1, "mm")
               )

     }


})

# Check environmental decay computation
# LASIK values in model.patches.delta_jt
testthat::test_that("delta_jt matches", {

     # model.patches.delta_jt = map_suitability_to_decay(
     #      decay_days_short = params.decay_days_short,
     #      decay_days_long = params.decay_days_long,
     #      suitability = params.psi_jt,
     #      decay_shape_1 = params.decay_shape_1,
     #      decay_shape_2 = params.decay_shape_2,
     # ) = 1.0 / decay_days_short + beta.cdf(suitability, decay_shape_1, decay_shape_2) * (1.0 / decay_days_long - 1.0 / decay_days_short)

     expected <- NULL # ¡TODO!
     expect_equal(expected, model$patches$delta_jt)

})


testthat::test_that("delta_jt matches", {

     # grab the pieces from baseline
     psi      <- baseline$psi_jt              # matrix, time × locations
     decay_days_short     <- baseline$decay_days_short    # vector, length = # locations
     decay_days_long     <- baseline$decay_days_long     # vector, length = # locations
     decay_shape_1   <- baseline$decay_shape_1       # vector, length = # locations
     decay_shape_2   <- baseline$decay_shape_2       # vector, length = # locations

     delta_jt_model <- model$patches$delta_jt
     delta_jt_expected <- matrix(NA_real_, nrow = ncol(psi), ncol = nrow(psi))

     for (t in 1:ncol(psi)) {
          for (j in 1:nrow(psi)) {

               delta_jt_expected[t, j] <-
                    1 / ( decay_days_short + pbeta(psi[j, t], decay_shape_1, decay_shape_2) * (decay_days_long - decay_days_short) )

          }
     }

     testthat::expect_equal(delta_jt_expected, delta_jt_model, tolerance = 1e-06)

     if (T) {

          library(ggplot2)
          library(grid)  # for unit()

          # 1. pull out model vs expected
          model_mat    <- model$patches$delta_jt
          expected_mat <- delta_jt_expected  # make sure this is the same dims: nrow=time, ncol=locations

          # 2. build date & location vectors
          dates     <- seq(as.Date(model$params$date_start),
                           as.Date(model$params$date_stop),
                           by = "day")
          locations <- model$params$location_name

          # 3. reshape into long form (base R)
          df_model <- data.frame(
               date     = rep(dates,    times = ncol(model_mat)),
               location = rep(locations, each  = nrow(model_mat)),
               delta    = as.vector(model_mat),
               Series   = "Model",
               stringsAsFactors = FALSE
          )
          df_expected <- data.frame(
               date     = rep(dates,    times = ncol(expected_mat)),
               location = rep(locations, each  = nrow(expected_mat)),
               delta    = as.vector(expected_mat),
               Series   = "Expected",
               stringsAsFactors = FALSE
          )
          df_plot <- rbind(df_model, df_expected)

          # 4. facet‐wrapped comparison
          ggplot(df_plot, aes(x = date, y = 1/delta, color = Series)) +
               geom_line(size = 1.2) +
               scale_color_manual(values = c("Model" = "purple", "Expected" = "green3")) +
               facet_wrap(~ location, scales = "free_x", ncol = 4) +
               labs(
                    x     = "Date",
                    y     = expression(delta[jt]),
                    title = "Decay rate (delta): Model vs Expected"
               ) +
               theme_minimal(base_size = 9) +
               theme(
                    panel.spacing     = unit(0.05, "lines"),
                    plot.margin       = unit(rep(1, 4), "mm"),
                    strip.text        = element_text(margin = margin(t = 1, b = 1, unit = "mm"),
                                                     size   = 8),
                    axis.text         = element_text(size = 6),
                    axis.title        = element_text(size = 8),
                    legend.position   = "bottom",
                    legend.key.size   = unit(4, "mm"),
                    legend.spacing.x  = unit(1, "mm")
               )


     }



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

