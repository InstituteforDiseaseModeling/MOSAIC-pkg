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


#' Get an Optimal Dask/Coiled Cluster Spec for a Given Worker Count
#'
#' @description
#' Returns a \code{dask_spec} list for \code{\link{run_MOSAIC}} sized for
#' a given number of LASER workers (1 to 500). Each worker is one D2s_v6
#' VM running one cholera sim at a time, so \code{n_workers} is also the
#' number of \strong{concurrent simulations} and the number of subnet
#' IPs the cluster will consume. The workspace cap (1,000 total vCPUs at
#' 2 per worker = 500 workers) sets the upper bound.
#'
#' @param n_workers Integer between 1 and 500 inclusive. The number of
#'   LASER worker VMs to provision. Each runs one cholera sim
#'   concurrently. Non-integer values are rounded.
#' @param n_sims Optional positive integer: the number of simulations
#'   submitted per Dask batch (the largest \code{n_simulations} the run
#'   will use). When supplied, the scheduler VM is sized to the LARGER of
#'   the worker-count tier and the sim-volume tier (see Details). This
#'   matters because the Dask path submits ALL \code{n_sims} futures and
#'   gathers them in a single blocking call, so the scheduler holds the
#'   whole batch's task graph + results in RAM at once — a memory load
#'   driven by \code{n_sims}, not \code{n_workers}. Leave \code{NULL}
#'   (default) to size the scheduler by \code{n_workers} alone, assuming
#'   the typical ~120 sims/worker batch. Non-integer values are rounded.
#'
#' @return Named list ready to pass as the \code{dask_spec} argument of
#'   \code{\link{run_MOSAIC}}.
#'
#' @details
#' \strong{Routing rules:}
#' \itemize{
#'   \item \strong{Worker VM family} — always 2-vCPU x86 v6. Three
#'     equivalent variants are passed to Coiled so it can grab whichever
#'     has capacity in westus2: \code{Standard_D2s_v6} (Intel Emerald
#'     Rapids), \code{Standard_D2ds_v6} (same Intel with local SSD),
#'     \code{Standard_D2ads_v6} (AMD Genoa with local SSD). All 2 vCPU
#'     / 8 GiB. The cholera sim is single-threaded GIL-bound Python, so
#'     larger VMs would idle cores; the second vCPU absorbs Dask
#'     plumbing (network I/O, nanny, heartbeats).
#'   \item \strong{Scheduler VM} — memory-optimized \strong{Ev6} family
#'     (Emerald Rapids, 8 GiB/vCPU vs Dsv6's 4), sized to the LARGER of two
#'     5-tier ladders: one keyed on \code{n_workers}, one on \code{n_sims}
#'     (only when supplied). The scheduler is RAM-bound, not CPU-bound: the
#'     Dask calibration path submits every one of the batch's \code{n_sims}
#'     futures, keeps them all alive, and gathers them in a SINGLE blocking
#'     call, so the scheduler holds the whole batch's task graph (the
#'     per-sim JSON params are embedded in each task spec) plus the
#'     in-transit results at once. That memory load scales with
#'     \code{n_sims} (result/graph volume), only weakly with
#'     \code{n_workers} (heartbeats/comms) — hence the two ladders.
#'     Scheduler-VM cost is negligible next to a 50-500 worker fleet, so
#'     the tiers err toward headroom; an under-sized scheduler that OOMs
#'     (exit 137) kills the entire multi-hour run. A 200-worker run OOM'd a
#'     general-purpose \code{Standard_D8s_v6} (32 GiB) under fast-turnover
#'     calibration, which is why the ladder is on the memory-optimized
#'     family.
#'
#'     Shared SKU ladder (tier -> VM -> RAM):
#'     \tabular{lll}{
#'       tier 1 \tab \code{Standard_E4s_v6}  \tab ( 4 vCPU /  32 GiB) \cr
#'       tier 2 \tab \code{Standard_E8s_v6}  \tab ( 8 vCPU /  64 GiB) \cr
#'       tier 3 \tab \code{Standard_E16s_v6} \tab (16 vCPU / 128 GiB) \cr
#'       tier 4 \tab \code{Standard_E32s_v6} \tab (32 vCPU / 256 GiB) \cr
#'       tier 5 \tab \code{Standard_E64s_v6} \tab (64 vCPU / 512 GiB) \cr
#'     }
#'     \code{n_workers} -> tier: \code{<=50} t1; \code{<=100} t2;
#'     \code{<=200} t3; \code{<=350} t4; \code{>350} t5. \code{n_sims} ->
#'     tier (when set): \code{<=10000} t1; \code{<=25000} t2;
#'     \code{<=50000} t3; \code{<=100000} t4; \code{>100000} t5. The floor
#'     is 32 GiB (never the old 8 GiB D2s, which is indefensible for a
#'     gather-everything design). Note: Coiled/Dask schedulers do NOT spill
#'     to disk, so a bigger VM — not a memory-limit option — is the only
#'     OOM guard; that is why this helper sizes the SKU rather than passing
#'     \code{scheduler_options}. Standard Esv6 quota in westus2 (IDM
#'     Research 2) is ample; no quota increase needed for these tiers.
#'   \item \strong{wait_for_workers} — absolute number of workers that
#'     must be up before tasks dispatch. Equivalent to 80\% of the
#'     pool at \code{n_workers <= 250}, 90\% above, floored at 1.
#'     Passed as an integer (not a fraction) so Coiled has no
#'     ambiguity to resolve at small \code{n_workers}. Coiled's
#'     fraction default of 0.3 is too aggressive for short-task
#'     calibrations: first sims dispatch onto a partial pool, then
#'     stall as remaining workers join.
#'   \item \strong{timeout} — cluster-creation timeout in seconds.
#'     Scales as \code{max(1800, 600 + 4 * n_workers)}. At
#'     \code{n_workers = 500} this is ~43 minutes, large enough for the
#'     long tail of Azure VM provisioning in westus2 (per-VM p99 ~7
#'     min, parallelized but not free) plus the 90\% wait threshold.
#'   \item \strong{workspace} — defaults to \code{"idm-coiled-idmad-r2"}.
#'     Override globally without forking the helper by setting
#'     \code{options(mosaic.coiled_workspace = "your-workspace")}.
#' }
#'
#' \strong{Sizing guide} (~0.7 s per LASER iteration measured on ETH
#' single-location). Pass \code{n_sims} to size the scheduler to the
#' batch you actually submit:
#' \itemize{
#'   \item \code{n_workers = 25-50}: smoke tests, 1-5K sims, day-to-day.
#'   \item \code{n_workers = 125}: typical 10K-sim fixed-budget calibrations.
#'   \item \code{n_workers = 250}: 24K-sim predictive batches.
#'   \item \code{n_workers = 500}: run_10-class jobs (60K+ sims). At the
#'     workspace 1,000-core cap.
#' }
#' If a batch runs many sims on relatively few workers (e.g. a 30K-sim
#' predictive batch on 40 workers), the \code{n_workers} tier alone
#' under-sizes the scheduler and it can OOM; pass
#' \code{mosaic_dask_presets(40, n_sims = 30000)} so the scheduler is
#' sized to the 30K-sim tier (64 GiB) instead of the 40-worker tier
#' (16 GiB).
#'
#' \strong{Notes:}
#' \itemize{
#'   \item Each worker consumes one Azure subnet IP. The
#'     \code{idm-coiled-idmad-r2} workspace is on a \code{/23} subnet
#'     (~507 usable IPs); under contention from other tenants, large
#'     clusters can fail at provisioning with \code{SubnetIsFull}. If
#'     that happens, drop \code{n_workers} or coordinate with the
#'     workspace admin to expand the subnet.
#'   \item 8 GiB RAM per worker is fine for ETH single-location and
#'     similar small configs. For multi-country MOSAIC (>10 locations)
#'     you may need more headroom — override \code{vm_types} to
#'     \code{"Standard_D4s_v6"} (16 GiB), accepting that the cluster's
#'     cost-per-sim doubles (idle cores) and the IP footprint is
#'     unchanged.
#'   \item Defaults to on-demand pricing. For spot/preemptible VMs
#'     (~70\% cheaper, with eviction risk), layer
#'     \code{spot_policy = "spot_with_fallback"} (NOT \code{"spot"}) so
#'     the cluster falls back to on-demand if Azure spot capacity in
#'     westus2 runs out.
#' }
#'
#' @seealso \code{\link{mosaic_io_presets}} for I/O presets,
#'   \code{\link{run_MOSAIC}} for the calibration workflow.
#'
#' @examples
#' \dontrun{
#' # 10K-sim calibration on 125 workers
#' result <- run_MOSAIC(
#'   config     = get_location_config(iso = "ETH"),
#'   priors     = get_location_priors(iso = "ETH"),
#'   dir_output = "output_eth_10k",
#'   control    = mosaic_control_defaults(
#'     calibration = list(n_simulations = 10000, n_iterations = 3)
#'   ),
#'   dask_spec  = mosaic_dask_presets(n_workers = 125)
#' )
#'
#' # Many sims on few workers: size the scheduler to the batch, not the
#' # worker count, to avoid a scheduler OOM (exit 137).
#' spec <- mosaic_dask_presets(n_workers = 40, n_sims = 30000)
#'
#' # Max cluster with spot VMs for a long adaptive run
#' spec <- mosaic_dask_presets(500)
#' spec$spot_policy <- "spot_with_fallback"
#'
#' # Multi-country: override to D4s_v6 for RAM headroom
#' spec <- mosaic_dask_presets(250)
#' spec$vm_types <- c("Standard_D4s_v6")  # 16 GiB / worker, ~2x cost-per-sim
#' }
#'
#' @export
mosaic_dask_presets <- function(n_workers, n_sims = NULL) {
  if (missing(n_workers) || !is.numeric(n_workers) || length(n_workers) != 1L ||
      is.na(n_workers) || !is.finite(n_workers)) {
    stop("`n_workers` must be a single numeric value between 1 and 500",
         call. = FALSE)
  }
  n_workers <- as.integer(round(n_workers))
  if (n_workers < 1L || n_workers > 500L) {
    stop(sprintf("`n_workers` must be in [1, 500]; got %d", n_workers),
         call. = FALSE)
  }
  if (!is.null(n_sims)) {
    if (!is.numeric(n_sims) || length(n_sims) != 1L || is.na(n_sims) ||
        !is.finite(n_sims) || n_sims < 1) {
      stop("`n_sims` must be NULL or a single positive integer", call. = FALSE)
    }
    n_sims <- as.integer(round(n_sims))
  }

  # Scheduler VM sizing on the memory-optimized Ev6 family (Emerald Rapids,
  # 8 GiB/vCPU vs Dsv6's 4). The scheduler is RAM-bound, not CPU-bound: the
  # Dask calibration path submits every one of the batch's n_sims futures,
  # keeps them all alive, and gathers them in a SINGLE blocking call, so the
  # scheduler holds the whole batch's task graph (per-sim JSON params are
  # embedded in each task spec) plus the in-transit results at once. That
  # load scales with n_sims (result/graph volume), only weakly with
  # n_workers (heartbeats/comms). We size to the LARGER of a worker-count
  # tier and a sim-volume tier. Floor is 32 GiB (never the old 8 GiB D2s,
  # indefensible for a gather-everything design); a 200-worker run OOM'd a
  # D8s_v6 (32 GiB) under fast turnover, which is why we moved to the
  # memory-optimized family. Coiled/Dask schedulers do NOT spill to disk,
  # so a bigger VM is the only OOM guard.
  scheduler_ladder <- c(
    "Standard_E4s_v6",   # tier 1:  4 vCPU /  32 GiB
    "Standard_E8s_v6",   # tier 2:  8 vCPU /  64 GiB
    "Standard_E16s_v6",  # tier 3: 16 vCPU / 128 GiB
    "Standard_E32s_v6",  # tier 4: 32 vCPU / 256 GiB
    "Standard_E64s_v6"   # tier 5: 64 vCPU / 512 GiB
  )

  # Worker-count tier. This is the safe default that fixed the observed
  # 200-worker OOM (tier 3 = 128 GiB). n_sims (below) can push the tier
  # higher for a many-sims/few-workers batch, but never lower.
  tier_workers <-
    if      (n_workers <=  50L) 1L
    else if (n_workers <= 100L) 2L
    else if (n_workers <= 200L) 3L
    else if (n_workers <= 350L) 4L
    else                        5L

  # Sim-volume tier (0L => not supplied; contributes nothing to the max).
  tier_sims <- 0L
  if (!is.null(n_sims)) {
    tier_sims <-
      if      (n_sims <=  10000L) 1L
      else if (n_sims <=  25000L) 2L
      else if (n_sims <=  50000L) 3L
      else if (n_sims <= 100000L) 4L
      else                        5L
  }

  scheduler_vm <- scheduler_ladder[[max(tier_workers, tier_sims)]]

  # wait_for_workers: hold submission until this many workers are up.
  # Coiled's 0.3-default penalizes short-task calibrations (first sims
  # dispatch onto a partial pool, then stall). Target 80% for typical
  # cluster sizes, 90% at the high end. Compute as an absolute integer
  # rather than a fraction so Coiled has no ambiguity to resolve when
  # n_workers is small. Floored at 1.
  wait_frac        <- if (n_workers <= 250L) 0.8 else 0.9
  wait_for_workers <- max(1L, as.integer(ceiling(wait_frac * n_workers)))

  # timeout: cluster-creation timeout in seconds. Scales with n_workers
  # because Azure VM provisioning has a long tail in westus2 (p99 ~7
  # min/VM, parallelized but not free). 1800 s floor for the smallest
  # clusters; ~4 s per worker added on top. At n_workers=500 this is
  # ~43 min, comfortable for the 450-worker wait at 90%.
  timeout <- max(1800L, as.integer(600 + 4 * n_workers))

  list(
    type               = "coiled",
    # Workspace override via R option lets users plug in a different
    # Coiled workspace without forking this helper:
    #   options(mosaic.coiled_workspace = "my-workspace")
    workspace          = getOption("mosaic.coiled_workspace",
                                   "idm-coiled-idmad-r2"),
    software           = "mosaic-acr-workers",
    region             = "westus2",
    # 90 min covers the R-only post-calibration window between the
    # posterior-ensemble dispatch and the medoid dispatch (optimize_subset,
    # posterior quantiles + distributions, sensitivity + correlation plots,
    # sampling of config_medoid). 30 min was too tight for large-country runs
    # with optimize_subset = TRUE and risked the cluster scaling to zero
    # between the dispatches.
    idle_timeout       = "90 minutes",
    timeout            = timeout,
    worker_options     = list(nthreads = 1L),
    n_workers          = n_workers,
    # Three equivalent 2-vCPU x86 variants + one 4-vCPU fallback. Coiled
    # honours LIST ORDER as decreasing priority (not cost-based auto-select),
    # so the D2 variants are tried first and D4s_v6 (4 vCPU / 16 GB) is only
    # used as a last-resort backup when NO D2 SKU is available in westus2
    # (D2s_v6 is the most-requested 2-vCPU on Azure and most likely to hit
    # regional-contention shortages). The 1 thread/worker config
    # (worker_options below) means the extra cores aren't used for
    # parallelism — D4 is purely an availability (and RAM) hedge, not a
    # capacity bump, and because it is last it never displaces an available
    # D2. If a run consistently OOMs on D2, set vm_types explicitly to the
    # D4 list rather than relying on this fallback.
    vm_types           = c(
      "Standard_D2s_v6",   # Intel Emerald Rapids               (cheapest)
      "Standard_D2ds_v6",  # Intel Emerald Rapids w/ local SSD
      "Standard_D2ads_v6", # AMD Genoa w/ local SSD
      "Standard_D4s_v6"    # Intel Emerald Rapids 4-vCPU/16 GB  (fallback)
    ),
    scheduler_vm_types = c(scheduler_vm),
    wait_for_workers   = wait_for_workers
  )
}
