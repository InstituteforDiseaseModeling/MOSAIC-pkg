library(MOSAIC)
library(reticulate)
library(parallel)
library(compiler)
library(withr)
library(pbapply)

# ───────────────────────────────────────────────────────────────────────────────
# New libraries for future-based parallelism with progress bar
# ───────────────────────────────────────────────────────────────────────────────
library(future.apply)
library(progressr)

script_start <- Sys.time()

set_root_directory(
     normalizePath("~/Library/CloudStorage/OneDrive-Bill&MelindaGatesFoundation/Projects/MOSAIC")
)
PATHS    <- MOSAIC::get_paths()
work_dir <- file.path(PATHS$ROOT, "MOSAIC-pkg", "local", "laser-cholera-output")
if (!dir.exists(work_dir)) dir.create(work_dir)

# Set number of simulations
n_sim   <- 1000                    # Number of parameter sets
n_iter  <- 1                       # Number of times to run each parameter set
n_cores <- detectCores() - 1       # Number of cores to use

# Func to supply sample from parameter space (will be a MOSAIC func soon)
sample_parameter_space <- function(seed = 1) {
     with_seed(seed, {
          x <- MOSAIC::default_config
          x$seed <- as.integer(seed)
          comp <- get_initial_compartment_values(
               N      = x$N_j_initial,
               prop_S = runif(1, 0.05, 0.95),
               prop_E = 0,
               prop_I = runif(1, 0, 0.1),
               prop_R = runif(1, 0.05, 0.95),
               prop_V1 = runif(1, 0.1, 0.3),
               prop_V2 = 0
          )
          x$N_j_initial  <- comp$N;   x$S_j_initial  <- comp$S
          x$E_j_initial  <- comp$E;   x$I_j_initial  <- comp$I
          x$R_j_initial  <- comp$R;   x$V1_j_initial <- comp$V1
          x$V2_j_initial <- comp$V2

          x$beta_j0_hum <- rep(runif(1, 0, 0.25), length(x$beta_j0_hum))
          x$beta_j0_env <- rep(runif(1, 0, 0.5),  length(x$beta_j0_env))
          x$alpha_1     <- runif(1, 0.01, 0.99)
          x$alpha_2     <- runif(1, 0.01, 0.99)

          decay_shape_params <- list(
               Linear    = c(1, 1),
               Concave   = c(1, 5),
               Convex    = c(5, 1),
               Sigmoidal = c(5, 5),
               Arcsine   = c(0.5, 0.5)
          )
          sel <- sample(names(decay_shape_params), 1)
          x$decay_shape_1 <- decay_shape_params[[sel]][1]
          x$decay_shape_2 <- decay_shape_params[[sel]][2]

          x$kappa <- runif(1, 1e3, 1e9)
          x
     })
}
sampler <- compiler::cmpfun(sample_parameter_space)

# Switching to numeric matrix for speed
col_nms <- c(
     "seed_sim", "sim_iter", "seed_unique", "log_likelihood",
     "prop_S", "prop_I", "prop_R", "prop_V1",
     "beta_j0_hum", "beta_j0_env",
     "alpha_1", "alpha_2",
     "decay_shape_1", "decay_shape_2",
     "kappa"
)

# Set up chunks for each core
seeds  <- seq_len(n_sim)
blocks <- split(seeds, cut(seq_along(seeds), n_cores, labels = FALSE))

# Function to collate results for set of simulation seeds
process_block <- function(block_seeds) {
     total_rows <- length(block_seeds) * n_iter
     mat <- matrix(NA_real_, nrow = total_rows, ncol = length(col_nms))
     colnames(mat) <- col_nms

     row_idx <- 1L
     for (seed in block_seeds) {
          params <- sampler(seed)
          for (iter in seq_len(n_iter)) {
               unique_seed <- (seed - 1) * n_iter + iter
               mat[row_idx, "seed_sim"]    <- seed
               mat[row_idx, "sim_iter"]    <- iter
               mat[row_idx, "seed_unique"] <- unique_seed

               try({
                    model <- lc$run_model(paramfile = params, quiet = TRUE)
                    ll <- calc_model_likelihood(
                         obs_cases = as.matrix(params$reported_cases),
                         est_cases = t(model$patches$expected_cases)[, -ncol(model$patches$expected_cases)],
                         obs_deaths = as.matrix(params$reported_deaths),
                         est_deaths = t(model$patches$disease_deaths)[, -ncol(model$patches$disease_deaths)],
                         verbose = FALSE
                    )
                    mat[row_idx, ] <- c(
                         seed,
                         iter,
                         unique_seed,
                         ll,
                         params$S_j_initial[1] / params$N_j_initial[1],
                         params$I_j_initial[1] / params$N_j_initial[1],
                         params$R_j_initial[1] / params$N_j_initial[1],
                         params$V1_j_initial[1] / params$N_j_initial[1],
                         params$beta_j0_hum[1],
                         params$beta_j0_env[1],
                         params$alpha_1,
                         params$alpha_2,
                         params$decay_shape_1,
                         params$decay_shape_2,
                         params$kappa
                    )
               }, silent = FALSE)
               row_idx <- row_idx + 1L
          }
     }
     mat
}

# ───────────────────────────────────────────────────────────────────────────────
# Replace PSOCK cluster with future.apply + progressr (fixed lc assignment)
# ───────────────────────────────────────────────────────────────────────────────

plan(multisession, workers = n_cores)
handlers("txtprogressbar")

parallel_start <- Sys.time()
with_progress({
     p <- progressor(along = blocks)
     results_blocks <- future_lapply(blocks, function(block_seeds) {
          library(reticulate)
          library(MOSAIC)
          library(RhpcBLASctl)
          RhpcBLASctl::blas_set_num_threads(1)

          # assign lc into global so process_block() can see it
          assign("lc",
                 reticulate::import("laser_cholera.metapop.model"),
                 envir = globalenv()
          )

          p()
          process_block(block_seeds)
     })
})
parallel_end <- Sys.time()

results_mat <- do.call(rbind, results_blocks)
colnames(results_mat) <- col_nms

script_stop <- Sys.time()

cat("Parallel block runtime for", n_sim, "sims ×", n_iter, "iters on", n_cores, "cores:", format(parallel_end - parallel_start), "\n\n")
cat("Overall script runtime on", n_cores, "cores:", format(script_stop - script_start), "\n\n")

head(results_mat)

results_file <- file.path(work_dir, "results.csv")
write.csv(results_mat, results_file, row.names = FALSE)
cat("Results saved to", results_file, "\n")
