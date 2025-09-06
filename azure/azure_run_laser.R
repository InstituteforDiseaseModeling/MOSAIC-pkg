#!/usr/bin/env Rscript
#———————————————————————————————————————
# Parallel cholera‐model runs with PSOCK, including n_iter, seed_sim, sim_iter & seed_unique
# and writing results & timing to CSV
#———————————————————————————————————————

library(MOSAIC)
library(reticulate)
library(parallel)
library(compiler)
library(withr)
library(pbapply)

#— 1) Parse command line args & start timer
args <- commandArgs(trailingOnly = TRUE)
if(length(args) >= 1) n_sim   <- as.integer(args[1])
if(length(args) >= 2) n_iter  <- as.integer(args[2])
if(length(args) >= 3) n_cores <- as.integer(args[3])
script_start <- Sys.time()

#— 2) Project paths
root <- Sys.getenv("MOSAIC_PROJECT_ROOT",
                   "~/Library/CloudStorage/OneDrive-Bill&MelindaGatesFoundation/Projects/MOSAIC")
set_root_directory(normalizePath(root))
PATHS    <- MOSAIC::get_paths()
work_dir <- file.path(PATHS$ROOT, "MOSAIC-pkg", "local", "laser-cholera-output")
if (!dir.exists(work_dir)) dir.create(work_dir, recursive = TRUE)

#— 3) Byte‐compile sampler
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
               Linear    = c(1,1), Concave = c(1,5), Convex    = c(5,1),
               Sigmoidal = c(5,5), Arcsine = c(0.5,0.5)
          )
          sel <- sample(names(decay_shape_params), 1)
          x$decay_shape_1 <- decay_shape_params[[sel]][1]
          x$decay_shape_2 <- decay_shape_params[[sel]][2]
          x$kappa <- runif(1, 1e3, 1e9)
          x
     })
}
sampler <- cmpfun(sample_parameter_space)

#— 4) Defaults if unset
if (!exists("n_sim"))   n_sim   <- 500
if (!exists("n_iter"))  n_iter  <- 2
if (!exists("n_cores")) n_cores <- detectCores() - 1

#— 5) Result matrix specs
col_nms <- c(
     "seed_sim","sim_iter","seed_unique","log_likelihood",
     "prop_S","prop_I","prop_R","prop_V1",
     "beta_j0_hum","beta_j0_env",
     "alpha_1","alpha_2",
     "decay_shape_1","decay_shape_2","kappa"
)

#— 6) Chunk seeds
seeds  <- seq_len(n_sim)
blocks <- split(seeds, cut(seq_along(seeds), n_cores, labels = FALSE))

#— 7) Block processor
process_block <- function(block_seeds) {
     rows <- length(block_seeds)*n_iter
     mat <- matrix(NA_real_, nrow=rows, ncol=length(col_nms))
     colnames(mat) <- col_nms
     idx <- 1L
     for(seed in block_seeds) {
          params <- sampler(seed)
          for(iter in seq_len(n_iter)){
               uniq <- (seed-1)*n_iter + iter
               mat[idx, c("seed_sim","sim_iter","seed_unique")] <- c(seed,iter,uniq)
               try({
                    model <- lc$metapop$model$run_model(
                         paramfile = params, seed = uniq, visualize=FALSE,
                         pdf=FALSE, outdir=work_dir)
                    ll <- calc_model_likelihood(
                         as.matrix(params$reported_cases),
                         t(model$patches$expected_cases)[,-ncol(model$patches$expected_cases)],
                         as.matrix(params$reported_deaths),
                         t(model$patches$disease_deaths)[,-ncol(model$patches$disease_deaths)], FALSE)
                    mat[idx, ] <- c(seed,iter,uniq,ll,
                                    params$S_j_initial[1]/params$N_j_initial[1],
                                    params$I_j_initial[1]/params$N_j_initial[1],
                                    params$R_j_initial[1]/params$N_j_initial[1],
                                    params$V1_j_initial[1]/params$N_j_initial[1],
                                    params$beta_j0_hum[1],params$beta_j0_env[1],
                                    params$alpha_1,params$alpha_2,
                                    params$decay_shape_1,params$decay_shape_2,params$kappa)
               },silent=TRUE)
               idx <- idx +1L
          }
     }
     mat
}

#— 8) Launch PSOCK & init
cl <- makeCluster(n_cores,type="PSOCK")
clusterEvalQ(cl,{
     library(reticulate);library(MOSAIC);library(withr);
     if(requireNamespace("RhpcBLASctl",quietly=TRUE))
          RhpcBLASctl::blas_set_num_threads(1)
     lc <<- import("laser_cholera")
})
clusterExport(cl,c("sampler","col_nms","work_dir","process_block","n_iter"),envir=environment())

#— 9) Run in parallel with progress
parallel_start <- Sys.time()
results_blocks <- pblapply(blocks, process_block, cl=cl)
parallel_end   <- Sys.time()
stopCluster(cl)

#—10) Combine results
results_mat <- do.call(rbind, results_blocks)
colnames(results_mat) <- col_nms

#—11) Write results & timing
res_file <- file.path(work_dir,"results.csv")
write.csv(results_mat,res_file,row.names=FALSE)
cat("Results saved to",res_file,"\n")

timings <- data.frame(
     n_sim=n_sim, n_iter=n_iter, n_cores=n_cores,
     parallel_sec=as.numeric(difftime(parallel_end,parallel_start,units="secs")),
     total_sec=as.numeric(difftime(Sys.time(),script_start,units="secs"))
)
time_file <- file.path(work_dir,"timings.csv")
write.csv(timings,time_file,row.names=FALSE)
cat("Timings saved to",time_file,"\n")

#—12) Summary print
cat("Parallel block runtime for",n_sim,"×",n_iter,"on",n_cores,"cores:",
    format(parallel_end-parallel_start),"\n")
cat("Overall runtime:",format(Sys.time()-script_start),"\n")

#—13) Peek
print(head(results_mat))

#—14) Export
#  * res_file * timings * results_mat * timings
