#' Check Coiled Workspace Capacity and Activity
#'
#' @description
#' Prints a snapshot of a Coiled workspace's current resource usage:
#' core limit, cores in use (account-wide and by you), free cores, and
#' any active clusters. Useful before launching a large
#' \code{mosaic_dask_presets()} cluster to confirm there's headroom.
#'
#' @param workspace Character. The Coiled workspace slug. Defaults to
#'   \code{getOption("mosaic.coiled_workspace", "idm-coiled-idmad-r2")}.
#' @param subnet_ip_estimate Integer. Estimated usable IP count for the
#'   workspace's Azure subnet. Defaults to 507, the usable capacity of a
#'   \code{/23} subnet (the layout of \code{idm-coiled-idmad-r2}'s
#'   \code{snet-coiled}). Used only to compute a heuristic SubnetIsFull
#'   warning when active clusters' IP footprint exceeds 50\% of this.
#'   Pass a different value for workspaces backed by a different subnet
#'   size.
#'
#' @return Invisibly, a list with \code{workspace}, \code{core_limit},
#'   \code{cores_used_account}, \code{cores_used_user},
#'   \code{free_cores}, \code{active_clusters} (a list with \code{id},
#'   \code{name}, \code{state}, \code{n_workers} per cluster),
#'   \code{mosaic_ips_used} (IPs consumed by mosaic-dask clusters),
#'   \code{total_ips_used} (IPs consumed by all visible clusters), and
#'   \code{subnet_ip_estimate}.
#'
#' @details
#' This shows the \strong{Coiled-enforced} core limit and the clusters
#' Coiled can see in your workspace. The Azure subnet IP heuristic is
#' a best-effort warning: Coiled's API doesn't expose subnet usage
#' from OTHER tenants on the same VNet, so the IP count is a lower
#' bound on actual consumption. A clean Coiled view here does NOT
#' guarantee that the next large cluster will provision — a noisy
#' tenant in the same subnet could still cause SubnetIsFull. For the
#' authoritative view, query Azure directly (requires Azure CLI +
#' resource-group read access):
#'
#' \preformatted{
#' az network vnet subnet show \
#'   --resource-group rg-coiled-tting \
#'   --vnet-name vnet-coiled-tting \
#'   --name snet-coiled
#' }
#'
#' @seealso \code{\link{mosaic_dask_presets}} for cluster spec presets.
#'
#' @examples
#' \dontrun{
#' check_coiled_workspace()
#' # === Coiled workspace: idm-coiled-idmad-r2 ===
#' #   Core limit:        1000
#' #   Cores in use:      0 (0%) account-wide, 0 yours
#' #   Free cores:        1000
#' #   Active clusters:   1
#' #     - portfolio-workers     state=ready      workers=0
#'
#' # Override workspace globally:
#' options(mosaic.coiled_workspace = "ws-idm-coiled-azure")
#' check_coiled_workspace()
#'
#' # Or per-call:
#' check_coiled_workspace("ws-idm-coiled-azure")
#' }
#'
#' @export
check_coiled_workspace <- function(workspace = NULL,
                                   subnet_ip_estimate = 507L) {
  if (is.null(workspace)) {
    workspace <- getOption("mosaic.coiled_workspace", "idm-coiled-idmad-r2")
  }

  if (!requireNamespace("reticulate", quietly = TRUE)) {
    stop("`reticulate` package is required.", call. = FALSE)
  }
  if (!reticulate::py_available(initialize = TRUE)) {
    stop("Python is not available via reticulate. ",
         "Run `MOSAIC::check_dependencies()`.", call. = FALSE)
  }
  if (!reticulate::py_module_available("coiled")) {
    stop("`coiled` Python module is not installed. ",
         "Run `MOSAIC::install_dependencies()`.", call. = FALSE)
  }

  coiled <- reticulate::import("coiled", delay_load = FALSE)

  # Look up the membership for this workspace.
  ui <- coiled$list_user_information()
  membership <- NULL
  for (m in ui$membership_set) {
    if (identical(m$account$slug, workspace)) {
      membership <- m
      break
    }
  }
  if (is.null(membership)) {
    stop(sprintf("You are not a member of Coiled workspace '%s'.",
                 workspace), call. = FALSE)
  }

  limit <- as.integer(membership$core_limit)

  usage <- coiled$list_core_usage(account = workspace)
  used_account <- as.integer(usage$num_running_cores_account)
  used_user    <- as.integer(usage$num_running_cores_user)
  free_cores   <- limit - used_account

  # Active clusters (anything not stopped/closed).
  clusters <- coiled$list_clusters(workspace = workspace, max_pages = 1L)
  active <- list()
  for (cl in clusters) {
    state <- tryCatch(cl$current_state$state,
                      error = function(e) NA_character_)
    if (!is.na(state) &&
        state %in% c("scaling", "ready", "pending", "starting")) {
      wks <- if (is.null(cl$workers)) 0L else length(cl$workers)
      active[[length(active) + 1L]] <- list(
        id        = as.integer(cl$id),
        name      = as.character(cl$name),
        state     = state,
        n_workers = as.integer(wks)
      )
    }
  }

  # Estimate subnet IP pressure. Each active cluster consumes one IP per
  # worker plus one per scheduler. We can't see other Azure tenants on
  # the same /23 — only mosaic-* clusters in OUR workspace — but a
  # surprisingly-high count here is at least a leading indicator.
  # subnet_ip_estimate defaults to ~507 (a /23 subnet on Azure has
  # 512 addresses, ~5 reserved). Pass a different value for workspaces
  # backed by a different subnet size.
  mosaic_ips_used <- sum(vapply(active, function(a) {
    if (grepl("^mosaic-dask", a$name)) a$n_workers + 1L else 0L
  }, integer(1)))
  total_ips_used <- sum(vapply(active,
    function(a) a$n_workers + 1L, integer(1)))

  # Print summary.
  pct <- if (limit > 0L) 100 * used_account / limit else 0
  cat(sprintf("=== Coiled workspace: %s ===\n", workspace))
  cat(sprintf("  Core limit:        %d\n", limit))
  cat(sprintf("  Cores in use:      %d (%.0f%%) account-wide, %d yours\n",
              used_account, pct, used_user))
  cat(sprintf("  Free cores:        %d\n", free_cores))
  cat(sprintf("  Active clusters:   %d\n", length(active)))
  for (a in active) {
    cat(sprintf("    - %-40s state=%-10s workers=%d\n",
                a$name, a$state, a$n_workers))
  }
  cat(sprintf("  Active subnet IPs: %d total (~%d est. cap)",
              total_ips_used, subnet_ip_estimate))
  if (mosaic_ips_used > 0L && mosaic_ips_used != total_ips_used) {
    cat(sprintf(", %d from mosaic-dask clusters", mosaic_ips_used))
  }
  cat("\n")
  if (total_ips_used > 0.5 * subnet_ip_estimate) {
    cat(sprintf(
      "  WARNING: %d/%d IPs in use — provisioning a new large cluster\n",
      total_ips_used, subnet_ip_estimate))
    cat(
      "           may hit SubnetIsFull. Reduce n_workers or wait for\n")
    cat(
      "           other tenants' clusters to drain.\n")
  }
  cat("\nNote: Coiled API does not expose Azure subnet IP usage from",
      "other tenants;\nthe IP count above includes only clusters in",
      "this workspace. Regional VM\ncapacity is also invisible from",
      "this view.\n")

  invisible(list(
    workspace           = workspace,
    core_limit          = limit,
    cores_used_account  = used_account,
    cores_used_user     = used_user,
    free_cores          = free_cores,
    active_clusters     = active,
    mosaic_ips_used     = mosaic_ips_used,
    total_ips_used      = total_ips_used,
    subnet_ip_estimate  = as.integer(subnet_ip_estimate)
  ))
}
