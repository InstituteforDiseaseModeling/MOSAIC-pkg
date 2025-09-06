#—————————————————————————————————————————————
# Parallel cholera‐model runs with OS‐aware forking vs PSOCK, chunking, and fixed dimnames
#—————————————————————————————————————————————

library(MOSAIC)
library(dplyr)
library(reticulate)
library(parallel)
library(compiler)
library(withr)

#— 1) Start timer
script_start <- Sys.time()

#— 2) Project paths
set_root_directory(
     normalizePath("~/Library/CloudStorage/OneDrive-Bill&MelindaGatesFoundation/Projects/MOSAIC")
)
PATHS    <- MOSAIC::get_paths()
work_dir <- file.path(PATHS$ROOT, "MOSAIC-pkg", "local")

#— 3) Byte-compile the sampler
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
sampler <- cmpfun(sample_parameter_space)

#— 4) Simulation settings & result‐matrix specs
n_sim   <- 1000
n_cores <- detectCores() - 1

col_nms <- c(
     "seed", "log_likelihood",
     "prop_S", "prop_I", "prop_R", "prop_V1",
     "beta_j0_hum", "beta_j0_env",
     "alpha_1", "alpha_2",
     "decay_shape_1", "decay_shape_2",
     "kappa"
)

#— 5) Chunk seeds into blocks
seeds  <- seq_len(n_sim)
blocks <- split(seeds, cut(seq_along(seeds), n_cores, labels = FALSE))

#— 6) Block‐processing function (with dimnames fixed)
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
               mat[i, ] <- c(
                    seed,
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

#— 10) Export key objects
#    • col_nms
#    • results_mat
#    • work_dir
