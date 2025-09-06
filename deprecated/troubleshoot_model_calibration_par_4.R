#—————————————————————————————————————————————
# Parallel cholera‐model runs with expanded parameter space (~30 parameters)
#—————————————————————————————————————————————

library(MOSAIC)
library(dplyr)
library(reticulate)
library(parallel)
library(compiler)
library(withr)
# For Dirichlet distribution sampling
if (!requireNamespace("MCMCpack", quietly = TRUE)) {
     cat("MCMCpack not available, using gamma approximation for Dirichlet sampling\n")
}

#— 1) Start timer
script_start <- Sys.time()

#— 2) Project paths
set_root_directory(
     normalizePath("~/Library/CloudStorage/OneDrive-Bill&MelindaGatesFoundation/Projects/MOSAIC")
)
PATHS    <- MOSAIC::get_paths()
work_dir <- file.path(PATHS$ROOT, "MOSAIC-pkg", "local")

#— 3) Enhanced parameter sampling function with ~30 parameters
sample_parameter_space <- function(seed = 1) {
     with_seed(seed, {
          x <- MOSAIC::default_config
          x$seed <- as.integer(seed)
          n_locations <- length(x$location_name)

          # 1. INITIAL CONDITIONS (location-specific with Dirichlet distribution)
          # Use Dirichlet distribution for realistic proportion sampling with constraints
          prop_S <- numeric(n_locations)
          prop_E <- numeric(n_locations)
          prop_I <- numeric(n_locations)
          prop_R <- numeric(n_locations)
          prop_V1 <- numeric(n_locations)
          prop_V2 <- numeric(n_locations)

          # Generate proportions for each location using Dirichlet with constraints
          for (j in 1:n_locations) {
               valid_props <- FALSE
               attempts <- 0

               while (!valid_props && attempts < 100) {
                    attempts <- attempts + 1

                    # Dirichlet parameters (alpha values) - higher values = higher expected proportions
                    alpha_params <- c(
                         S = runif(1, 1, 8),    # Susceptible: moderate to high
                         E = runif(1, 0, 2),  # Exposed: low
                         I = runif(1, 0.01, 3),  # Infected: low to moderate
                         R = runif(1, 2, 30),   # Recovered: moderate to high
                         V1 = runif(1, 1, 4),   # First dose: low to moderate
                         V2 = 0  # Second dose: low
                    )

                    # Generate proportions using Dirichlet
                    if (requireNamespace("MCMCpack", quietly = TRUE)) {
                         props <- MCMCpack::rdirichlet(1, alpha_params)
                    } else {
                         # Fallback: use gamma distribution to approximate Dirichlet
                         gamma_samples <- rgamma(6, alpha_params)
                         props <- gamma_samples / sum(gamma_samples)
                    }

                    # Apply epidemiological constraints
                    prop_S_j <- props[1]
                    prop_E_j <- props[2]
                    prop_I_j <- props[3]
                    prop_R_j <- props[4]
                    prop_V1_j <- props[5]
                    prop_V2_j <- props[6]

                    # Constraint checks
                    active_infection <- prop_E_j + prop_I_j
                    immunity <- prop_R_j + prop_V1_j + prop_V2_j

                    if (prop_S_j >= 0.05 &&          # Minimum susceptibility
                        prop_S_j <= 0.8 &&           # Maximum susceptibility
                        active_infection <= 0.2 &&   # Active infection not too high
                        prop_I_j <= 0.15 &&          # Infected not too high
                        immunity >= 0.1 &&           # Some immunity present
                        immunity <= 0.9) {           # Not everyone immune

                         prop_S[j] <- prop_S_j
                         prop_E[j] <- prop_E_j
                         prop_I[j] <- prop_I_j
                         prop_R[j] <- prop_R_j
                         prop_V1[j] <- prop_V1_j
                         prop_V2[j] <- prop_V2_j
                         valid_props <- TRUE
                    }
               }

               # Fallback if constraints couldn't be met
               if (!valid_props) {
                    prop_S[j] <- runif(1, 0.2, 0.6)
                    prop_E[j] <- runif(1, 0, 0.02)
                    prop_I[j] <- runif(1, 0, 0.05)
                    prop_R[j] <- runif(1, 0.2, 0.5)
                    prop_V1[j] <- runif(1, 0.1, 0.3)
                    prop_V2[j] <- runif(1, 0, 0.1)

                    # Normalize fallback proportions
                    prop_sum <- prop_S[j] + prop_E[j] + prop_I[j] + prop_R[j] + prop_V1[j] + prop_V2[j]
                    prop_S[j] <- prop_S[j] / prop_sum
                    prop_E[j] <- prop_E[j] / prop_sum
                    prop_I[j] <- prop_I[j] / prop_sum
                    prop_R[j] <- prop_R[j] / prop_sum
                    prop_V1[j] <- prop_V1[j] / prop_sum
                    prop_V2[j] <- prop_V2[j] / prop_sum
               }
          }

          comp <- get_initial_compartment_values(
               N      = x$N_j_initial,
               prop_S = prop_S,
               prop_E = prop_E,
               prop_I = prop_I,
               prop_R = prop_R,
               prop_V1 = prop_V1,
               prop_V2 = prop_V2
          )
          x$N_j_initial  <- comp$N;   x$S_j_initial  <- comp$S
          x$E_j_initial  <- comp$E;   x$I_j_initial  <- comp$I
          x$R_j_initial  <- comp$R;   x$V1_j_initial <- comp$V1
          x$V2_j_initial <- comp$V2

          # 2. TRANSMISSION PARAMETERS (location-specific)
          x$beta_j0_hum <- runif(n_locations, 0.001, 0.20)  # Location-specific
          x$beta_j0_env <- runif(n_locations, 0.001, 0.50)  # Location-specific

          # 3. SEASONAL FORCING (location-specific)
          #x$a_1_j <- runif(n_locations, -0.5, 0.5)  # 1st harmonic sine
          #x$a_2_j <- runif(n_locations, -0.3, 0.3)  # 2nd harmonic sine
          #x$b_1_j <- runif(n_locations, -0.5, 0.5)  # 1st harmonic cosine
          #x$b_2_j <- runif(n_locations, -0.3, 0.3)  # 2nd harmonic cosine

          # 4. MIXING PARAMETERS
          x$alpha_1 <- runif(1, 0.1, 0.99)
          x$alpha_2 <- runif(1, 0.1, 0.99)

          # 5. ENVIRONMENTAL PARAMETERS
          x$kappa <- runif(1, 1e3, 1e10)  # 50% infection dose
          #x$zeta_1 <- runif(1, 5, 15)    # High shedding rate
          #x$zeta_2 <- runif(1, 1, 5)     # Low shedding rate (< zeta_1)
          # Ensure zeta_2 < zeta_1
          #if (x$zeta_2 >= x$zeta_1) {
          #     x$zeta_2 <- x$zeta_1 - 0.1
          #}

          # 6. ENVIRONMENTAL PERSISTENCE
          x$decay_days_short <- runif(1, 3/24, 7)
          x$decay_days_long <- runif(1, 30, 300)

          # Shape parameters for survival curve
          decay_shape_params <- list(
               Linear    = c(1, 1),
               Concave   = c(1, 5),
               Convex    = c(5, 1),
               Sigmoidal = c(5, 5),
               Arcsine   = c(0.5, 0.5)
          )
          shape_sel <- sample(names(decay_shape_params), 1)
          x$decay_shape_1 <- decay_shape_params[[shape_sel]][1]
          x$decay_shape_2 <- decay_shape_params[[shape_sel]][2]

          # 7. VACCINATION PARAMETERS
          #x$phi_1 <- runif(1, 0.4, 0.8)   # First-dose effectiveness
          #x$phi_2 <- runif(1, 0.7, 0.95)  # Second-dose effectiveness
          x$omega_1 <- runif(1, 0.0001, 0.002)  # First-dose waning
          x$omega_2 <- runif(1, 0.0001, 0.001)  # Second-dose waning

          # 8. CORE DISEASE DYNAMICS PARAMETERS
          x$iota <- runif(1, 0.5, 1.2)        # Incubation period (1/days)
          x$gamma_1 <- runif(1, 0.1, 0.4)     # Severe infection recovery rate
          x$gamma_2 <- runif(1, 0.05, 0.2)    # Mild infection recovery rate
          x$epsilon <- runif(1, 0.0001, 0.001) # Natural immunity waning rate

          # 9. OBSERVATION PARAMETERS
          #x$rho <- runif(1, 0.1, 0.8)         # Reporting rate
          #x$sigma <- runif(1, 0.1, 0.6)       # Symptomatic rate

          # 10. DEMOGRAPHIC PARAMETERS (location-specific)

          # 11. WASH COVERAGE (location-specific)
          #x$theta_j <- runif(n_locations, 0.3, 0.95)

          # 12. SPATIAL STRUCTURE/MOBILITY PARAMETERS
          #x$mobility_omega <- runif(1, 0.3, 1.0)    # Population attraction parameter
          #x$mobility_gamma <- runif(1, 0.8, 2.0)    # Distance decay parameter
          #x$tau_i <- runif(n_locations, 0.001, 0.1)  # Location-specific departure rates

          x
     })
}
sampler <- cmpfun(sample_parameter_space)

#— 4) Simulation settings & expanded result‐matrix specs
n_sim   <- 10000
n_cores <- detectCores() - 1

# Updated column names to accommodate ~40 parameters
col_nms <- c(
     "seed", "log_likelihood",
     # Initial conditions (averaged across locations)
     "prop_S_mean", "prop_E_mean", "prop_I_mean", "prop_R_mean", "prop_V1_mean", "prop_V2_mean",
     # Transmission parameters (averaged across locations)
     "beta_j0_hum_mean", "beta_j0_env_mean",
     "beta_j0_hum_sd", "beta_j0_env_sd",  # Add variability measures
     # Seasonal forcing (averaged)
     "a_1_j_mean", "a_2_j_mean", "b_1_j_mean", "b_2_j_mean",
     # Mixing parameters
     "alpha_1", "alpha_2",
     # Environmental parameters
     "kappa", "zeta_1", "zeta_2",
     "decay_days_short", "decay_days_long",
     "decay_shape_1", "decay_shape_2",
     # Vaccination parameters
     "phi_1", "phi_2", "omega_1", "omega_2",
     # Core disease dynamics parameters
     "iota", "gamma_1", "gamma_2", "epsilon",
     # Observation parameters
     "rho", "sigma",
     # Demographics and WASH
     "mu_jt_mean", "theta_j_mean", "theta_j_sd",
     # Spatial structure/mobility parameters
     "mobility_omega", "mobility_gamma", "tau_i_mean"
)

#— 5) Chunk seeds into blocks
seeds  <- seq_len(n_sim)
blocks <- split(seeds, cut(seq_along(seeds), n_cores, labels = FALSE))

#— 6) Block‐processing function with expanded parameter extraction
process_block <- function(block_seeds) {
     # ensure BLAS single-thread if available
     if (requireNamespace("RhpcBLASctl", quietly = TRUE)) {
          RhpcBLASctl::blas_set_num_threads(1)
     }
     # each fork/worker re-imports Python to be safe
     library(reticulate)
     lc <- import("laser_cholera")

     mat <- matrix(NA_real_, nrow = length(block_seeds), ncol = length(col_nms))
     colnames(mat) <- col_nms  # <— assign dimnames so mat[i, "seed"] works

     for (i in seq_along(block_seeds)) {
          seed <- block_seeds[i]
          mat[i, "seed"] <- seed

          params <- sampler(seed)
          try({
               model <- lc$metapop$model$run_model(
                    paramfile = params,
                    seed      = seed,
                    visualize = FALSE,
                    pdf       = FALSE,
                    outdir    = work_dir
               )
               ll <- calc_model_likelihood(
                    as.matrix(params$reported_cases),
                    t(model$patches$expected_cases)[, -ncol(model$patches$expected_cases)],
                    as.matrix(params$reported_deaths),
                    t(model$patches$disease_deaths)[, -ncol(model$patches$disease_deaths)],
                    verbose = FALSE
               )

               # Extract expanded parameter set
               mat[i, ] <- c(
                    seed, ll,
                    # Initial conditions
                    mean(params$S_j_initial / params$N_j_initial),
                    mean(params$E_j_initial / params$N_j_initial),
                    mean(params$I_j_initial / params$N_j_initial),
                    mean(params$R_j_initial / params$N_j_initial),
                    mean(params$V1_j_initial / params$N_j_initial),
                    mean(params$V2_j_initial / params$N_j_initial),
                    # Transmission parameters
                    mean(params$beta_j0_hum), mean(params$beta_j0_env),
                    sd(params$beta_j0_hum), sd(params$beta_j0_env),
                    # Seasonal forcing
                    mean(params$a_1_j), mean(params$a_2_j),
                    mean(params$b_1_j), mean(params$b_2_j),
                    # Mixing parameters
                    params$alpha_1, params$alpha_2,
                    # Environmental parameters
                    params$kappa, params$zeta_1, params$zeta_2,
                    params$decay_days_short, params$decay_days_long,
                    params$decay_shape_1, params$decay_shape_2,
                    # Vaccination parameters
                    params$phi_1, params$phi_2, params$omega_1, params$omega_2,
                    # Disease parameters
                    params$iota, params$gamma_1, params$gamma_2, params$epsilon,
                    # Observation parameters
                    params$rho, params$sigma,
                    # Demographics and WASH
                    mean(params$mu_jt), mean(params$theta_j), sd(params$theta_j),
                    # Mobility parameters
                    params$mobility_omega, params$mobility_gamma, mean(params$tau_i)
               )
          }, silent = TRUE)
     }
     mat
}

#— 7) OS‐aware parallel execution
use_fork <- .Platform$OS.type != "windows"
use_fork <- FALSE
parallel_start <- Sys.time()

if (use_fork) {
     # Unix-like: fork
     results_blocks <- mclapply(blocks, process_block, mc.cores = n_cores)
} else {
     # Windows: PSOCK
     cl <- makeCluster(n_cores, type = "PSOCK")
     clusterEvalQ(cl, {
          library(reticulate)
          library(MOSAIC)
          lc <- import("laser_cholera.metapop.model")
     })
     clusterExport(cl, c("sampler", "col_nms", "work_dir", "process_block", "with_seed"), envir = environment())
     results_blocks <- parLapply(cl, blocks, process_block)
     stopCluster(cl)
}
parallel_end <- Sys.time()

#— 8) Combine results into numeric matrix
results_mat <- do.call(rbind, results_blocks)
colnames(results_mat) <- col_nms

#— 9) Report timings & preview
cat("Parallel block runtime for", n_sim, "simulations: ",  format(parallel_end - parallel_start), "\n")
cat("Overall script runtime for", n_sim, "simulations: ", format(Sys.time() - script_start), "\n\n")
head(results_mat)

#— 10) Export key objects and save results
results_file <- file.path(work_dir, "results_par_4.csv")
write.csv(results_mat, results_file, row.names = FALSE)
cat("Results saved to", results_file, "\n")

#— 11) Basic analysis
results_df <- as.data.frame(results_mat)
valid_results <- results_df[!is.na(results_df$log_likelihood), ]
cat("Valid results:", nrow(valid_results), "out of", nrow(results_df), "\n")

if (nrow(valid_results) > 0) {
     best_idx <- which.max(valid_results$log_likelihood)
     cat("Best log-likelihood:", valid_results$log_likelihood[best_idx], "\n")
     cat("Best seed:", valid_results$seed[best_idx], "\n")
}

#— 12) Plotting functions for model visualization
library(ggplot2)
library(tidyr)

plot_all_locations <- function(seed, outcome = c("cases", "deaths")) {

     outcome <- match.arg(outcome)

     # Load required packages
     if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
     if (!requireNamespace("tidyr", quietly = TRUE)) install.packages("tidyr")

     library(ggplot2)
     library(tidyr)

     # Sample parameters and run model
     params <- sample_parameter_space(as.integer(seed))
     model  <- lc$run_model(paramfile = params)

     # Select observed & estimated matrices
     if (outcome == "cases") {
          obs_mat <- as.matrix(params$reported_cases)
          est_mat <- t(model$patches$expected_cases)[ , -ncol(model$patches$expected_cases)]
          y_lab   <- "Cases"
     } else {
          obs_mat <- as.matrix(params$reported_deaths)
          est_mat <- t(model$patches$disease_deaths)[ , -ncol(model$patches$disease_deaths)]
          y_lab   <- "Deaths"
     }

     # Build wide data.frames
     time      <- seq_len(ncol(est_mat))
     locations <- params$location_name

     est_df <- data.frame(location = locations, est_mat, check.names = FALSE)
     obs_df <- data.frame(location = locations, obs_mat, check.names = FALSE)
     colnames(est_df)[-1] <- time
     colnames(obs_df)[-1] <- time

     # Pivot to long format
     est_long <- pivot_longer(est_df, cols = -location,
                              names_to = "time", values_to = "estimated")
     obs_long <- pivot_longer(obs_df, cols = -location,
                              names_to = "time", values_to = "observed")

     df <- merge(est_long, obs_long, by = c("location", "time"))
     df$time <- as.integer(df$time)

     # Plot with facet wrap by location
     ggplot(df, aes(x = time)) +
          geom_line(aes(y = estimated), size = 0.5, color = "dodgerblue") +
          geom_point(aes(y = observed), size = 1) +
          facet_wrap(~ location, scales = "free_y") +
          labs(
               title = paste("Seed", seed, "- Observed vs Estimated", tools::toTitleCase(outcome), "by Location"),
               x     = "Time",
               y     = y_lab
          ) +
          theme_minimal()
}

plot_log_likelihood_params <- function(results_df) {
     if (!"log_likelihood" %in% names(results_df)) {
          stop("`results_df` must contain a `log_likelihood` column.")
     }

     params <- setdiff(
          names(results_df),
          c("sim", "iteration", "seed", "log_likelihood")
     )

     df_long <- tidyr::pivot_longer(
          results_df,
          cols      = params,
          names_to  = "parameter",
          values_to = "value"
     )

     p <- ggplot2::ggplot(df_long, ggplot2::aes(x = value, y = log_likelihood)) +
          ggplot2::geom_point(alpha = 0.7) +
          ggplot2::geom_smooth(method = "lm", se = FALSE) +
          ggplot2::facet_wrap(~ parameter, scales = "free_x") +
          ggplot2::labs(
               x = "Parameter value",
               y = "Log-likelihood"
          ) +
          ggplot2::theme_minimal()

     return(p)
}

#— 13) Visualization of results
if (nrow(valid_results) > 0) {
     # Import laser cholera for plotting
     lc <- reticulate::import("laser_cholera.metapop.model")

     # Get best seed
     best_seed <- valid_results$seed[which.max(valid_results$log_likelihood)]

     # Plot likelihood trajectory
     cat("Creating likelihood trajectory plot...\n")
     plot_df <- valid_results %>%
          arrange(log_likelihood) %>%
          mutate(run = row_number())

     # Find the index & value of the max likelihood
     max_idx <- which.max(plot_df$log_likelihood)
     max_row <- plot_df[max_idx, ]

     likelihood_plot <- ggplot(plot_df, aes(x = run, y = log_likelihood)) +
          geom_line() +
          geom_point(size = 2) +
          # horizontal dotted line at the max
          geom_hline(yintercept = max_row$log_likelihood,
                     linetype   = "dotted",
                     color      = "gray40") +
          # highlight the maximum point
          geom_point(data = max_row,
                     aes(x = run, y = log_likelihood),
                     size  = 4,
                     shape = 21,
                     fill  = "green2",
                     color = "black",
                     stroke = 1.5) +
          theme_minimal() +
          theme(
               axis.title.x = element_text(margin = margin(t = 10)),
               axis.title.y = element_text(margin = margin(r = 10))
          ) +
          labs(
               x = "Run index (sorted by log-likelihood ↑)",
               y = "Log-Likelihood",
               title = "Log-Likelihoods Across Runs",
               subtitle = paste0("Best fit at run ", max_row$run, " (Seed: ", max_row$seed, ")")
          )

     print(likelihood_plot)

     # Plot parameter relationships
     cat("Creating parameter relationship plots...\n")
     param_plot <- plot_log_likelihood_params(valid_results)
     print(param_plot)

     # Plot best-fit model results
     cat("Creating best-fit model visualization...\n")

     # Plot cases
     cases_plot <- plot_all_locations(seed = as.integer(best_seed), outcome = 'cases')
     print(cases_plot)

     # Plot deaths
     deaths_plot <- plot_all_locations(seed = as.integer(best_seed), outcome = 'deaths')
     print(deaths_plot)

     # Print best parameters
     cat("\nBest-fit parameters:\n")
     print(valid_results[which.max(valid_results$log_likelihood), ])
}

#— 14) Export key objects
#    • col_nms (expanded)
#    • results_mat (with ~30 parameters)
#    • work_dir
#    • results_file
#    • plotting functions: plot_all_locations, plot_log_likelihood_params
