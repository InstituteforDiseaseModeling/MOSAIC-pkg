# =============================================================================
# compat.R -- API shim across MOSAIC engine generations
#
# This benchmark suite must produce comparable rows from three codebases that
# do not share an entry point:
#
#   v0.66.0 and earlier  run_LASER(config, seed, quiet, visualize, pdf, outdir,
#                        py_module) -> a reticulate handle imported with
#                        `convert = FALSE`
#   v0.66.0+ (R engine)  run_simulation(config, seed, quiet, components, rng,
#                        record) -> a plain R list
#
# The callable subset is (config, seed, quiet). Everything else is dispatched
# here so the workload definitions stay engine-agnostic.
#
# The `convert = FALSE` detail is not cosmetic. reticulate converts lazily, so
# timing run_LASER() alone charges the Python arm for the simulation and NOT
# for materialising the result -- which the calibration worker pays the moment
# it touches model$results$reported_cases. Timing the call without the
# extraction makes the Python engine look faster than it is. Every engine
# workload here therefore times call + extraction of the two channels the
# worker actually consumes.
# =============================================================================

.bench_engine_kind <- function() {
     if (exists("run_simulation", envir = asNamespace("MOSAIC"), inherits = FALSE)) "R" else "python"
}

.bench_engine_version <- function() {
     if (.bench_engine_kind() == "python") {
          v <- tryCatch({
               im <- reticulate::import("importlib.metadata", convert = TRUE)
               im$version("laser-cholera")
          }, error = function(e) NA_character_)
          paste0("laser-cholera ", v)
     } else {
          paste0("R engine ", as.character(utils::packageVersion("MOSAIC")))
     }
}

# Run one simulation and materialise the channels the calibration worker reads.
# Returns list(rc = <matrix>, rd = <matrix>) in both generations.
.bench_run <- function(config, seed = 1L) {
     if (.bench_engine_kind() == "R") {
          m  <- MOSAIC::run_simulation(config = config, seed = seed, quiet = TRUE)
          rc <- m$results$reported_cases
          rd <- m$results$reported_deaths
     } else {
          m  <- MOSAIC::run_LASER(config = config, seed = seed, quiet = TRUE)
          # Force the lazy conversion here, inside the timed region.
          rc <- reticulate::py_to_r(m$results$reported_cases)
          rd <- reticulate::py_to_r(m$results$reported_deaths)
     }
     list(rc = rc, rd = rd)
}

# Cross-engine results are statistically equivalent but NOT draw-for-draw
# identical (migrate-laser-r.md section 7), so an exact digest is only
# meaningful WITHIN an engine generation. Emit both: the digest for
# within-engine bit-parity, the sums for a cross-engine distributional band.
.bench_fingerprint <- function(res) {
     f <- tempfile(); on.exit(unlink(f), add = TRUE)
     saveRDS(list(rc = res$rc, rd = res$rd), f, compress = FALSE)
     list(digest     = unname(tools::md5sum(f)),
          cases_sum  = sum(as.numeric(res$rc), na.rm = TRUE),
          deaths_sum = sum(as.numeric(res$rd), na.rm = TRUE))
}
