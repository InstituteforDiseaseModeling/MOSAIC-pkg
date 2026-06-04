# =============================================================================
# Preset config helpers for run_MOSAIC()
#
# Returns named lists that drop into the corresponding `control` / `dask_spec`
# slot of `run_MOSAIC()`. Each helper encodes a small set of opinionated
# choices for a common use case so the call site stays terse.
# =============================================================================

#' Get Pre-configured I/O Settings
#'
#' @description
#' Returns pre-configured I/O settings for common use cases. Choose from
#' debug, fast, default, or archive presets to optimize for your workflow.
#'
#' @param preset Character. One of "default", "debug", "fast", or "archive"
#'
#' @return Named list with I/O settings (format, compression, compression_level)
#'
#' @details
#' Presets:
#' \itemize{
#'   \item \code{debug}: CSV format, no compression (easy inspection)
#'   \item \code{fast}: Parquet with low compression (fastest)
#'   \item \code{default}: Parquet with medium compression (balanced)
#'   \item \code{archive}: Parquet with high compression (smallest files)
#' }
#'
#' @export
mosaic_io_presets <- function(preset = c("default", "debug", "fast", "archive")) {
  preset <- match.arg(preset)

  switch(preset,
    debug = list(
      format = "csv",
      compression = "none",
      compression_level = NULL
    ),
    fast = list(
      format = "parquet",
      compression = "snappy",
      compression_level = NULL
    ),
    default = list(
      format = "parquet",
      compression = "zstd",
      compression_level = 3L
    ),
    archive = list(
      format = "parquet",
      compression = "zstd",
      compression_level = 9L
    )
  )
}


#' Get an Optimal Dask/Coiled Cluster Spec for a Given Core Budget
#'
#' @description
#' Returns a \code{dask_spec} list for \code{\link{run_MOSAIC}} sized to
#' any total core budget between 1 and 1,000 (the workspace cap on
#' \code{idm-coiled-idmad-r2}). Routes to the fastest worker/scheduler
#' configuration for LASER's single-threaded sim workload: every core
#' becomes a concurrent LASER worker, capped at the workspace limit.
#'
#' @param cores Integer between 1 and 1,000 inclusive. The total core
#'   budget for the cluster. Non-integer values are rounded.
#'
#' @return Named list ready to pass as the \code{dask_spec} argument of
#'   \code{\link{run_MOSAIC}}.
#'
#' @details
#' \strong{Routing rules:}
#' \itemize{
#'   \item \strong{Worker VM} — always \code{Standard_D2s_v6} (2 vCPU /
#'     8 GiB RAM). LASER runs at \code{nthreads = 1}, so cores beyond 2
#'     per worker idle. Smallest VM = maximum workers per core =
#'     maximum concurrent sims.
#'   \item \strong{Worker count} — \code{floor(cores / 2)}, with a
#'     minimum of 1 worker. So \code{cores = 100} → 50 workers (100
#'     cores), \code{cores = 99} → 49 workers (98 cores). A request of
#'     \code{cores = 1} rounds up to 1 worker = 2 cores (the smallest
#'     viable cluster).
#'   \item \strong{Scheduler VM} — \code{Standard_D8s_v6} (8 vCPU /
#'     32 GiB) for ≤ 200 workers, \code{Standard_D16s_v6} (16 vCPU /
#'     64 GiB) for > 200 workers. The 200-worker threshold is where
#'     scheduler task-throughput becomes the bottleneck on PR-#111's
#'     scalar-payload gather path.
#' }
#'
#' \strong{Rough sizing guide} (~0.7 s per LASER iteration measured on
#' ETH single-location, D2s_v6 worker):
#' \itemize{
#'   \item \code{cores = 50-100}: smoke tests, 1-5K sims, day-to-day.
#'   \item \code{cores = 250}: typical 10K-sim fixed-budget calibrations.
#'   \item \code{cores = 500}: 24K-sim predictive batches.
#'   \item \code{cores = 1000}: run_10-class jobs (60K+ sims). At the cap.
#' }
#'
#' \strong{Notes:}
#' \itemize{
#'   \item D2s_v6 has 8 GiB RAM per worker. Fine for ETH single-location
#'     and other small configs. For multi-country MOSAIC (>10 locations)
#'     you may need more headroom — override \code{vm_types} to
#'     \code{"Standard_D4s_v6"} (16 GiB), accepting that the worker count
#'     halves for the same core budget.
#'   \item Defaults to on-demand pricing. For spot/preemptible VMs
#'     (~70\% cheaper, with eviction risk), layer
#'     \code{spot_policy = "spot"} onto the returned list — most useful
#'     at \code{cores >= 500} where sim wall-time absorbs retries.
#' }
#'
#' @seealso \code{\link{mosaic_io_presets}} for I/O presets,
#'   \code{\link{run_MOSAIC}} for the calibration workflow.
#'
#' @examples
#' \dontrun{
#' # 10K-sim calibration on a 250-core cluster
#' result <- run_MOSAIC(
#'   config     = get_location_config(iso = "ETH"),
#'   priors     = get_location_priors(iso = "ETH"),
#'   dir_output = "output_eth_10k",
#'   control    = mosaic_control_defaults(
#'     calibration = list(n_simulations = 10000, n_iterations = 3)
#'   ),
#'   dask_spec  = mosaic_dask_presets(250)
#' )
#'
#' # Max cluster with spot VMs for a long adaptive run
#' spec <- mosaic_dask_presets(1000)
#' spec$spot_policy <- "spot"
#'
#' # Multi-country: override to D4s_v6 for RAM headroom
#' spec <- mosaic_dask_presets(500)
#' spec$vm_types <- c("Standard_D4s_v6")  # halves to 125 workers, 16 GiB each
#' }
#'
#' @export
mosaic_dask_presets <- function(cores) {
  if (missing(cores) || !is.numeric(cores) || length(cores) != 1L ||
      is.na(cores) || !is.finite(cores)) {
    stop("`cores` must be a single numeric value between 1 and 1000",
         call. = FALSE)
  }
  cores <- as.integer(round(cores))
  if (cores < 1L || cores > 1000L) {
    stop(sprintf("`cores` must be in [1, 1000]; got %d", cores),
         call. = FALSE)
  }

  # Optimal routing for LASER's nthreads=1 workload: max workers per core
  # via D2s_v6 (2 cores each). Scheduler scales at the ~200-worker knee.
  n_workers    <- max(1L, cores %/% 2L)
  scheduler_vm <- if (n_workers <= 200L) "Standard_D8s_v6" else "Standard_D16s_v6"

  list(
    type               = "coiled",
    workspace          = "idm-coiled-idmad-r2",
    software           = "mosaic-acr-workers",
    region             = "westus2",
    idle_timeout       = "30 minutes",
    timeout            = 1800,
    worker_options     = list(nthreads = 1L),
    n_workers          = n_workers,
    vm_types           = c("Standard_D2s_v6"),
    scheduler_vm_types = c(scheduler_vm)
  )
}
