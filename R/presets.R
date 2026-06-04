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
#'   \item \strong{Worker count} — \code{floor(cores / 2)}, with a
#'     minimum of 1 worker. So \code{cores = 100} → 50 workers (100
#'     cores), \code{cores = 99} → 49 workers (98 cores). A request of
#'     \code{cores = 1} rounds up to 1 worker = 2 cores (the smallest
#'     viable cluster).
#'   \item \strong{Worker VM family} — three equivalent 2-vCPU x86
#'     variants are passed to Coiled so it can grab whichever has
#'     capacity in westus2: \code{Standard_D2s_v6} (Intel Emerald
#'     Rapids), \code{Standard_D2ds_v6} (same Intel with local SSD),
#'     \code{Standard_D2ads_v6} (AMD Genoa with local SSD). All 2 vCPU
#'     / 8 GiB. Smallest VM = max workers per core = max concurrent
#'     sims at \code{nthreads = 1}.
#'   \item \strong{Scheduler VM} — 5-tier graduation by worker count:
#'     \tabular{ll}{
#'       \code{n_workers <= 50}   \tab \code{Standard_D2s_v6}   (2 vCPU /   8 GiB) \cr
#'       \code{n_workers <= 100}  \tab \code{Standard_D4s_v6}   (4 vCPU /  16 GiB) \cr
#'       \code{n_workers <= 200}  \tab \code{Standard_D8s_v6}   (8 vCPU /  32 GiB) \cr
#'       \code{n_workers <= 350}  \tab \code{Standard_D16s_v6} (16 vCPU /  64 GiB) \cr
#'       \code{n_workers >  350}  \tab \code{Standard_D32s_v6} (32 vCPU / 128 GiB) \cr
#'     }
#'     Scheduler is mostly connection / task-graph state, not CPU-bound.
#'     Finer tiers avoid over-provisioning at low worker counts while
#'     keeping RAM headroom at high counts.
#'   \item \strong{wait_for_workers} — fraction of the worker pool that
#'     must be up before tasks dispatch. Coiled's default 0.3 is too
#'     aggressive for short-task calibrations (first sims would run on
#'     a partial pool, then stall as new workers join). Set to 0.8 for
#'     \code{n_workers <= 250} and 0.9 above.
#' }
#'
#' \strong{Rough sizing guide} (~0.7 s per LASER iteration measured on
#' ETH single-location):
#' \itemize{
#'   \item \code{cores = 50-100}: smoke tests, 1-5K sims, day-to-day.
#'   \item \code{cores = 250}: typical 10K-sim fixed-budget calibrations.
#'   \item \code{cores = 500}: 24K-sim predictive batches.
#'   \item \code{cores = 1000}: run_10-class jobs (60K+ sims). At the cap.
#' }
#'
#' \strong{Notes:}
#' \itemize{
#'   \item 8 GiB RAM per worker is fine for ETH single-location and
#'     similar small configs. For multi-country MOSAIC (>10 locations)
#'     you may need more headroom — override \code{vm_types} to
#'     \code{"Standard_D4s_v6"} (16 GiB), accepting that the worker count
#'     halves for the same core budget.
#'   \item Defaults to on-demand pricing. For spot/preemptible VMs
#'     (~70\% cheaper, with eviction risk), layer
#'     \code{spot_policy = "spot_with_fallback"} (NOT \code{"spot"}) so
#'     the cluster falls back to on-demand if Azure spot capacity in
#'     westus2 runs out. Most useful at \code{cores >= 500} where sim
#'     wall-time absorbs the occasional eviction retry.
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
#' spec$spot_policy <- "spot_with_fallback"
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

  # n_workers = floor(cores / 2): smallest 2-vCPU VM = max workers per
  # core = max concurrent sims at LASER's nthreads=1.
  n_workers <- max(1L, cores %/% 2L)

  # 5-tier scheduler graduation. Scheduler is connection/task-graph
  # state, not CPU-bound — these sizes give RAM headroom for the
  # task graph (~5 KiB per task) without over-provisioning at low N.
  scheduler_vm <-
    if      (n_workers <=  50L) "Standard_D2s_v6"   #  2 vCPU /   8 GiB
    else if (n_workers <= 100L) "Standard_D4s_v6"   #  4 vCPU /  16 GiB
    else if (n_workers <= 200L) "Standard_D8s_v6"   #  8 vCPU /  32 GiB
    else if (n_workers <= 350L) "Standard_D16s_v6"  # 16 vCPU /  64 GiB
    else                        "Standard_D32s_v6"  # 32 vCPU / 128 GiB

  # wait_for_workers: hold submission until this fraction of the worker
  # pool is up. Coiled's 0.3 default penalizes short-task calibrations
  # (under-utilized cluster on the first batch). Bump to 0.8/0.9.
  wait_for <- if (n_workers <= 250L) 0.8 else 0.9

  list(
    type               = "coiled",
    workspace          = "idm-coiled-idmad-r2",
    software           = "mosaic-acr-workers",
    region             = "westus2",
    idle_timeout       = "30 minutes",
    timeout            = 1800,
    worker_options     = list(nthreads = 1L),
    n_workers          = n_workers,
    # Three equivalent 2-vCPU x86 variants. Coiled picks whichever has
    # capacity in westus2 — materially speeds provision under regional
    # contention (D2s_v6 is the most-requested 2-vCPU on Azure).
    vm_types           = c(
      "Standard_D2s_v6",   # Intel Emerald Rapids
      "Standard_D2ds_v6",  # Intel Emerald Rapids w/ local SSD
      "Standard_D2ads_v6"  # AMD Genoa w/ local SSD
    ),
    scheduler_vm_types = c(scheduler_vm),
    wait_for_workers   = wait_for
  )
}
