library(reticulate)
library(MOSAIC)

plot_diagnostics <- TRUE

# Load default model and set baseline
mpm      <- reticulate::import("laser_cholera.metapop.model")
filename <- file.path(getwd(), "inst", "extdata", "default_parameters.json")
model    <- mpm$run_model(paramfile = filename)
baseline <- jsonlite::fromJSON(filename)

# Check human-human seasonality computation
# LASIK values in model.patches.beta_j_seasonality
testthat::test_that("beta_j_seasonality matches", {

     time_names <- seq(as.Date(baseline$date_start), as.Date(baseline$date_stop), 1)
     location_names <- baseline$location_name

     beta_j0_hum <- baseline$beta_j0_hum
     beta_jt_hum_model <- model$results$beta_jt_human
     beta_jt_hum_expected <- matrix(NA_real_, nrow = length(location_names), ncol = length(time_names))

     # Baseline Fourier seasonality terms
     a1 <- baseline$a_1_j
     a2 <- baseline$a_2_j
     b1 <- baseline$b_1_j
     b2 <- baseline$b_2_j
     p <- baseline$p

     for (t in 1:length(time_names)) {
          for (j in 1:length(location_names)) {

               seasonal_term <-
                    a1[j] * cos(2 * pi * t / p) +
                    b1[j] * sin(2 * pi * t / p) +
                    a2[j] * cos(4 * pi * t / p) +
                    b2[j] * sin(4 * pi * t / p)

               beta_jt_hum_expected[j, t] <- beta_j0_hum[j] * (1 + seasonal_term)

          }
     }


     testthat::expect_equal(beta_jt_hum_expected, beta_jt_hum_model, tolerance = 1e-04)


     if (plot_diagnostics) {

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
               geom_line(linewidth = 1.2) +
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

     beta_jt_env_model <- model$results$beta_jt_env

     # pull out the raw psi_jt matrix and beta0_env vector from the baseline JSON
     Psi       <- baseline$psi_jt            # dim: (time × locations)
     beta0_env <- baseline$beta_j0_env       # length = # locations
     psi_bar   <- rowMeans(Psi) # compute the time‐mean for each location

     beta_jt_env_expected <- matrix(NA_real_, nrow = nrow(Psi), ncol = ncol(Psi))

     for (t in 1:ncol(Psi)) {
          for (j in 1:nrow(Psi)) {

               beta_jt_env_expected[j, t] <- beta0_env[j] * (1 + (Psi[j, t] - psi_bar[j]) / psi_bar[j])

          }
     }

     # compare to what the Python model actually produced
     testthat::expect_equal(beta_jt_env_expected, beta_jt_env_model, tolerance = 1e-04)


     if(plot_diagnostics) {

          library(ggplot2)
          library(grid)  # for unit()

          # build date and location vectors
          dates          <- seq(as.Date(baseline$date_start),
                                as.Date(baseline$date_stop), by = "day")
          location_names <- baseline$location_name

          # grab model vs expected matrices
          beta_model    <- model$results$beta_jt_env
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
testthat::test_that("delta_jt matches", {

     # grab the pieces from baseline
     psi      <- baseline$psi_jt              # matrix, time × locations
     decay_days_short     <- baseline$decay_days_short    # vector, length = # locations
     decay_days_long     <- baseline$decay_days_long     # vector, length = # locations
     decay_shape_1   <- baseline$decay_shape_1       # vector, length = # locations
     decay_shape_2   <- baseline$decay_shape_2       # vector, length = # locations

     delta_jt_model <- model$results$delta_jt
     delta_jt_expected <- matrix(NA_real_, nrow = nrow(psi), ncol = ncol(psi))

     for (t in 1:ncol(psi)) {
          for (j in 1:nrow(psi)) {

               delta_jt_expected[j, t] <-
                    1 / ( decay_days_short + pbeta(psi[j, t], decay_shape_1, decay_shape_2) * (decay_days_long - decay_days_short) )

          }
     }

     testthat::expect_equal(delta_jt_expected, delta_jt_model, tolerance = 1e-04)

     if (plot_diagnostics) {

          library(ggplot2)
          library(grid)  # for unit()

          # 1. pull out model vs expected
          model_mat    <- model$results$delta_jt
          expected_mat <- delta_jt_expected  # make sure this is the same dims: nrow=time, ncol=locations

          # 2. build date & location vectors
          dates     <- seq(as.Date(baseline$date_start),
                           as.Date(baseline$date_stop),
                           by = "day")
          locations <- baseline$location_name

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

     # At a minimum, params$nu_1_jt != 0 where model$results$dose_one_doses != 0
     # The converse _might_ not be true if there are no susceptibles to dose.

     # Transpose Python data from [time, patch] to [patch, time]
     sim_non_zero <- model$results$dose_one_doses != 0
     input_values <- baseline$nu_1_jt[sim_non_zero]
     good <- all(input_values != 0)

     testthat::expect_true(good)
})

# Check OCV second dose schedule
# LASIK values in model.patches.dose_one_doses and model.patches.dose_two_doses
testthat::test_that("OCV dose two doses match", {

     # At a minimum, params$nu_2_jt != 0 where model$results$dose_two_doses != 0
     # The converse _might_ not be true if there are no susceptibles to dose.

     # Transpose Python data from [time, patch] to [patch, time]
     sim_non_zero <- model$results$dose_two_doses != 0
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

     # Added get_distance_matrix to MOSAIC. Method='spherical' gives haversine formula
     # for spherical distance. Original mobility function reproduced by method='planar'.

     D <- MOSAIC::get_distance_matrix(
          x = baseline$longitude,
          y = baseline$latitude,
          id = baseline$location_name,
          method = 'spherical'
     )

     expected <- MOSAIC::calc_diffusion_matrix_pi(
          D = D,
          N = baseline$N_j_initial,
          omega = baseline$mobility_omega,
          gamma = baseline$mobility_gamma
     )

     actual <- model$results$pi_ij
     diag(actual) <- NA

     testthat::expect_equal(expected, actual, tolerance = 1e-04)

     if (plot_diagnostics) {

          library(ggplot2)
          library(reshape2)

          df_exp <- melt(expected,
                         varnames = c("origin", "destination"),
                         value.name = "value")
          df_exp$matrix <- "expected"

          df_act <- melt(actual,
                         varnames = c("origin", "destination"),
                         value.name = "value")
          df_act$matrix <- "actual"

          df_long <- rbind(df_exp, df_act)

          ggplot(df_long, aes(x = destination, y = origin, fill = value)) +
               geom_tile() +
               facet_wrap(~ matrix, ncol = 2) +
               scale_fill_gradient(low = "white", high = "steelblue") +
               labs(x = NULL, y = NULL, fill = "Value") +
               theme_minimal() +
               theme(
                    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
                    panel.grid = element_blank()
               )

     }

})


# Check log_likelihood computation
# LASIK value in model.log_likelihood
testthat::test_that("log likelihood calculations match", {
     # TODO - try several different points in parameter space
     expected <- MOSAIC::get_model_likelihood(
          obs_cases=baseline$reported_cases,
          est_cases=t(model$results$incidence),
          obs_deaths=baseline$reported_deaths,
          est_deaths=t(model$results$disease_deaths))
     diff <- abs((expected - model$log_likelihood) / expected)

     testthat::expect_lt(diff, 0.01)
})

# Check spatial hazard computation
# LASIK values in model.patches.spatial_hazard
testthat::test_that("spatial hazard calculations", {

     a1 <- baseline$a_1_j
     a2 <- baseline$a_2_j
     b1 <- baseline$b_1_j
     b2 <- baseline$b_2_j
     p <- baseline$p

     time_names <- seq(as.Date(baseline$date_start), as.Date(baseline$date_stop), 1)
     location_names <- baseline$location_name

     beta_j0_hum <- baseline$beta_j0_hum
     beta_jt_hum <- matrix(NA_real_, nrow = length(location_names), ncol = length(time_names))

     for (t in 1:length(time_names)) {
          for (j in 1:length(location_names)) {

               seasonal_term <-
                    a1[j] * cos(2 * pi * t / p) +
                    b1[j] * sin(2 * pi * t / p) +
                    a2[j] * cos(4 * pi * t / p) +
                    b2[j] * sin(4 * pi * t / p)

               beta_jt_hum[j, t] <- beta_j0_hum[j] * (1 + seasonal_term)

          }
     }

     expected <- MOSAIC::calc_spatial_hazard(
          beta_jt_hum,
          baseline$tau_i,
          model$results$pi_ij,
          model$results$N,
          model$results$S,
          model$results$V1sus,
          model$results$V2sus,
          I1 = model$results$Isym,
          I2 = model$results$Iasym,
          time_names = NULL,
          location_names = NULL
     )

     testthat::expect_equal(expected, model$results$spatial_hazard, tolerance = 1e-04)

})


# Check coupling computation
# LASIK values in model.patches.coupling
testthat::test_that("spatial coupling calculations", {

     I_sym <- model$results$Isym
     I_asym <- model$results$Iasym
     N <- model$results$N

     expected <- MOSAIC::calc_spatial_correlation_matrix(I_sym, I_asym, N)
     dimnames(expected) <- NULL

     actual <- model$results$coupling

     testthat::expect_equal(expected, actual, tolerance = 1e-4)

})


# Check that total population tracks expected population sizes from UN WPP data
testthat::test_that("UN population trends", {

     time_names <- seq(as.Date(baseline$date_start), as.Date(baseline$date_stop), 1)
     location_names <- baseline$location_name

     pop_UN <- read.csv(file.path(getwd(), 'model/input/param_N_population_size.csv'))
     pop_UN$t <- as.Date(pop_UN$t)
     pop_UN <- pop_UN[pop_UN$t %in% time_names & pop_UN$j %in% location_names,]

     pop_model <- model$results$N
     colnames(pop_model) <- location_names
     rownames(pop_model) <- as.character(time_names)

     actual <- pop_model
     expected <- pop_model; expected[,] <- NA

     for (j in 1:ncol(expected)) {

          tmp <- pop_UN[pop_UN$j == location_names[j],]
          tmp <- tmp[order(as.Date(tmp$t)),]
          expected[,j] <- tmp$parameter_value

     }

     actual_sums   <- colSums(actual)
     expected_sums <- colSums(expected)

     # Proportional‐nearness test
     tol    <- 0.01    # tolerance: 1% deviation allowed
     ratios <- actual_sums / expected_sums

     testthat::expect_true(
          all(abs(ratios - 1) < tol),
          info = paste0("Max proportional deviation = ",
                        round(max(abs(ratios - 1)), 4))
     )

     if (plot_diagnostics){

          par(mfrow=c(1,2))
          plot(expected_sums, actual_sums,
               xlab = "Expected population total",
               ylab = "Actual population total",
               main = "Actual vs Expected Population Totals",
               pch = 19)
          abline(0, 1, col = "red", lty = 2)  # 1:1 line

          hist(ratios, main="Histogram of ratios (actual:expected)")
          abline(v=1, col='red', lty=2)

          library(ggplot2)
          library(reshape2)

          # Melt the matrices into long format
          df_exp <- melt(expected,
                         varnames = c("date", "location"),
                         value.name = "population")
          df_exp$source <- "expected"

          df_act <- melt(actual,
                         varnames = c("date", "location"),
                         value.name = "population")
          df_act$source <- "actual"

          # Combine and convert date column
          df_long <- rbind(df_exp, df_act)
          df_long$date <- as.Date(df_long$date)

          # Plot with facets by location
          ggplot(df_long, aes(x = date, y = population, color = source)) +
               geom_line(linewidth=1.5) +
               facet_wrap(~ location, scales = "free_y") +
               labs(
                    x     = "Date",
                    y     = "Population size",
                    color = "Data type",
                    title = "Expected vs Actual Population Over Time"
               ) +
               theme_minimal() +
               theme(
                    axis.text.x = element_text(angle = 45, hjust = 1),
                    panel.grid.major = element_line(color = "grey80"),
                    panel.grid.minor = element_blank()
               )


     }

})


# TODO: reproductive number
# testthat::test_that("reproductive number calculations match", {})

# TODO: r-effective
# testthat::test_that("r_effective calculations match", {})


