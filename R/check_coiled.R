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
#'
#' @return Invisibly, a list with \code{workspace}, \code{core_limit},
#'   \code{cores_used_account}, \code{cores_used_user},
#'   \code{free_cores}, and \code{active_clusters} (a list with
#'   \code{id}, \code{name}, \code{state}, \code{n_workers} per cluster).
#'
#' @details
#' This shows the \strong{Coiled-enforced} core limit and the clusters
#' Coiled can see in your workspace. It does NOT show the underlying
#' Azure subnet IP availability, which is the constraint that bites
#' large clusters under shared-tenancy. For that, query Azure directly
#' (requires Azure CLI + resource-group read access):
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
check_coiled_workspace <- function(workspace = NULL) {
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
  cat("\nNote: this is Coiled's view only. Azure-level constraints",
      "(subnet IPs,\nregional VM capacity) can still cause cluster",
      "provisioning to fail.\n")

  invisible(list(
    workspace           = workspace,
    core_limit          = limit,
    cores_used_account  = used_account,
    cores_used_user     = used_user,
    free_cores          = free_cores,
    active_clusters     = active
  ))
}
