#' Refresh every MOSAIC data input that can be refreshed automatically
#'
#' Single entry point for the data-preparation pipeline: syncs the external
#' scraper repos, re-runs every \code{download_*} / \code{process_*} /
#' \code{est_*} step in dependency order, and returns a per-step status table.
#' Replaces hand-running \code{model/LAUNCH.R}.
#'
#' Unlike \code{LAUNCH.R}, a failing step does \strong{not} abort the run: it is
#' recorded, its dependents are marked \code{blocked}, and the remaining
#' independent steps still execute. One run therefore surfaces every problem at
#' once instead of one per invocation.
#'
#' @param root Path to the MOSAIC parent directory (the one holding
#'   \code{MOSAIC-pkg}, \code{MOSAIC-data}, and the scraper repos as siblings).
#'   Defaults to \code{get_paths()$ROOT} if a root has already been set.
#' @param steps Character vector of step ids or group ids (\code{"1A"},
#'   \code{"2"}, \code{"process_OAG_data"}) to run. \code{NULL} (default) runs
#'   everything eligible. See \code{\link{list_mosaic_data_steps}}.
#' @param skip Character vector of step or group ids to exclude.
#' @param refresh_repos If \code{TRUE} (default), \code{git pull --ff-only} the
#'   five scraper repos first via \code{\link{refresh_data_repos}}.
#' @section Scope — data building, not model fitting:
#' This function builds \strong{data}. It does not fit models. Group 4A
#' (\code{compile_suitability_data}) assembles the LSTM training panel from its
#' 13 upstream producers — climate, ENSO, demographics, surveillance, mobility,
#' epidemic peaks, EM-DAT, the four World Bank indicators, WASH and elevation —
#' and is in the default plan, so the suitability \emph{data} stays in step with
#' its inputs on an ordinary run.
#'
#' Fitting the suitability model is a separate concern and is
#' \strong{deliberately not reachable from here}. Call
#' \code{\link{est_suitability}} directly, or use the calibration workflow: it
#' needs the TensorFlow/keras Python environment, budgets ~6 GB per seed worker,
#' and runs for hours, so it belongs on its own schedule with its own failure
#' handling. \code{est_suitability} was a registry step (group 4B) up to
#' v0.91.13 and was removed in v0.91.14.
#' @param date_stop Upper date bound passed to \code{est_vaccination_rate()}.
#'   Defaults to \strong{today plus 540 days}, NOT today.
#'
#'   This must cover the psi forecast horizon, because
#'   \code{data-raw/make_config_default.R} derives the config's
#'   \code{date_stop} from the per-country minimum of the psi prediction
#'   dates and then requires every time-varying matrix to span exactly that
#'   window. A \code{Sys.Date()} default silently produced a vaccination
#'   matrix 139 days shorter than the psi horizon, and the config build failed
#'   validation with \dQuote{nu_1_jt must be a matrix with ... columns equal
#'   to the daily sequence from date_start to date_stop}. Over-covering is
#'   harmless (rates are zero beyond the data); under-covering is fatal and
#'   the error names the wrong culprit.
#' @param dry_run If \code{TRUE}, print the preflight report and the execution
#'   plan, then stop without running anything or touching any file.
#' @param stop_on_error If \code{TRUE}, abort at the first failure instead of
#'   continuing. \code{FALSE} by default.
#' @param verbose Print progress and the closing summary.
#'
#' @return Invisibly, a \code{data.frame} with one row per step:
#'   \code{step}, \code{group}, \code{status}, \code{seconds},
#'   \code{message}. \code{status} is one of \code{"ok"}, \code{"failed"},
#'   \code{"blocked"} (a dependency failed), \code{"not_run"} (aborted before
#'   this step under \code{stop_on_error}) or \code{"pending"} (returned by
#'   \code{dry_run}, where nothing executes). Attributes \code{"manual_inputs"}
#'   and \code{"repo_sync"} carry the preflight frame and any sync warning.
#'
#' @section Manual inputs:
#' Some sources have no automated route and must be refreshed by hand. Every
#' run begins with a preflight that checks each one and prints its age and
#' refresh instructions; \code{dry_run = TRUE} prints the preflight alone. See
#' \code{\link{check_mosaic_manual_inputs}} for the manifest. Nothing here
#' blocks the run -- a stale manual input degrades the relevant outputs, it
#' does not stop the pipeline.
#'
#' @section What this does NOT do:
#' \itemize{
#'   \item \strong{Package data objects.} Rebuilding \code{priors_default} /
#'     \code{config_default} requires \code{devtools::install(".")} \emph{between}
#'     \code{data-raw/make_priors_default.R} and
#'     \code{data-raw/make_config_default.R}, so it cannot run in one session.
#'     The summary flags when a rebuild looks warranted.
#'   \item \strong{Plots.} Visualisation is not a data step; use the
#'     \code{plot_*} functions directly.
#'   \item \strong{Calibration.} See \code{\link{run_MOSAIC}}.
#' }
#'
#' @seealso \code{\link{list_mosaic_data_steps}},
#'   \code{\link{check_mosaic_manual_inputs}}, \code{\link{refresh_data_repos}}
#'
#' @examples
#' \dontrun{
#' # What would run, and what needs manual attention?
#' update_mosaic_data("~/MOSAIC", dry_run = TRUE)
#'
#' # Full automated refresh (no suitability)
#' res <- update_mosaic_data("~/MOSAIC")
#' subset(res, status != "ok")
#'
#' # Resume after fixing a failure
#' update_mosaic_data("~/MOSAIC", steps = c("3A", "3D", "3E"), refresh_repos = FALSE)
#' }
#'
#' @export
update_mosaic_data <- function(root                = NULL,
                               steps               = NULL,
                               skip                = NULL,
                               refresh_repos       = TRUE,
                               date_stop           = Sys.Date() + 540,
                               dry_run             = FALSE,
                               stop_on_error       = FALSE,
                               verbose             = TRUE) {

     if (is.null(root)) {
          root <- tryCatch(MOSAIC::get_paths()$ROOT, error = function(e) NULL)
          if (is.null(root)) {
               stop("No MOSAIC root. Pass root = \"~/MOSAIC\" or call ",
                    "set_root_directory() first.", call. = FALSE)
          }
     }
     root <- normalizePath(path.expand(root), mustWork = TRUE)
     MOSAIC::set_root_directory(root)
     PATHS <- MOSAIC::get_paths()

     registry <- .mosaic_data_steps(date_stop = as.Date(date_stop))
     .mosaic_validate_registry(registry)
     plan     <- .mosaic_select_steps(registry, steps, skip)

     if (!length(plan)) stop("No steps selected.", call. = FALSE)

     if (verbose) {
          cat("\n", strrep("=", 78), "\n", sep = "")
          cat("MOSAIC data update\n")
          cat(sprintf("  root      : %s\n", root))
          cat(sprintf("  steps     : %d\n", length(plan)))
          cat(sprintf("  started   : %s\n", format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
          cat(strrep("=", 78), "\n", sep = "")
     }
     # Always run the preflight (the roxygen promises "every run begins with"
     # one) and always attach it, so a verbose = FALSE / cron caller still gets
     # the manual-input state back.
     manual <- check_mosaic_manual_inputs(root, verbose = verbose)

     if (dry_run) {
          cat("\n-- Execution plan (dry run; nothing was run) -------------------\n")
          for (s in plan) {
               cat(sprintf("  [%-3s] %-34s %s\n", s$group, s$id,
                           if (length(s$deps)) paste("<-", paste(s$deps, collapse = ", ")) else ""))
          }
          cat("\n")
          out <- .mosaic_empty_results(plan)
          attr(out, "manual_inputs") <- manual
          return(invisible(out))
     }

     sync_note <- NA_character_
     if (refresh_repos) {
          if (verbose) cat("\n-- Syncing scraper repos ---------------------------------------\n")
          sync <- tryCatch(MOSAIC::refresh_data_repos(root = root, stale_days = 14L,
                                                      verbose = verbose),
                           error = function(e) e)
          if (inherits(sync, "error")) {
               sync_note <- paste("repo sync FAILED:", conditionMessage(sync))
               warning(sync_note, call. = FALSE)
               if (verbose) cat("  !! ", sync_note, "\n", sep = "")
          } else {
               n_fail <- sum(!vapply(sync, function(x) isTRUE(x$ok), logical(1)))
               if (n_fail) {
                    sync_note <- sprintf("repo sync: %d of %d pulls failed",
                                         n_fail, length(sync))
                    warning(sync_note, call. = FALSE)
                    if (verbose) cat("  !! ", sync_note, "\n", sep = "")
               }
          }
     }

     res <- .mosaic_empty_results(plan)
     attr(res, "repo_sync") <- sync_note
     attr(res, "manual_inputs") <- manual
     done_ok <- character(0)

     if (verbose) cat("\n-- Running steps -----------------------------------------------\n")

     for (i in seq_along(plan)) {
          s <- plan[[i]]

          unmet <- setdiff(intersect(s$deps, vapply(plan, `[[`, "", "id")), done_ok)
          if (length(unmet)) {
               res$status[i]  <- "blocked"
               res$message[i] <- paste("upstream failed:", paste(unmet, collapse = ", "))
               if (verbose) cat(sprintf("  [%-3s] %-34s BLOCKED (%s)\n", s$group, s$id, res$message[i]))
               next
          }

          if (verbose) cat(sprintf("  [%-3s] %-34s ... ", s$group, s$id))
          t0 <- Sys.time()
          out <- tryCatch({
               s$run(PATHS)
               list(ok = TRUE, msg = NA_character_)
          }, error = function(e) {
               list(ok = FALSE, msg = conditionMessage(e))
          })
          el <- as.numeric(difftime(Sys.time(), t0, units = "secs"))

          res$seconds[i] <- round(el, 1)
          if (isTRUE(out$ok)) {
               res$status[i] <- "ok"
               done_ok <- c(done_ok, s$id)
               if (verbose) cat(sprintf("ok (%.1fs)\n", el))
          } else {
               res$status[i]  <- "failed"
               res$message[i] <- .mosaic_one_line(out$msg)
               if (verbose) cat(sprintf("FAILED (%.1fs)\n        %s\n", el, res$message[i]))
               if (stop_on_error) {
                    res$status[res$status == "pending"] <- "not_run"
                    if (verbose) .mosaic_print_summary(res, root)
                    message("Aborting: step '", s$id, "' failed (stop_on_error = TRUE). ",
                            "Returning the status table.")
                    return(invisible(res))
               }
          }
     }

     if (verbose) .mosaic_print_summary(res, root)
     invisible(res)
}


#' List the steps `update_mosaic_data()` knows about
#'
#' @return A \code{data.frame} of \code{step}, \code{group}, \code{depends_on},
#'   \code{description}. Every listed step runs in the default plan.
#' @seealso \code{\link{update_mosaic_data}}
#' @export
list_mosaic_data_steps <- function() {
     reg <- .mosaic_data_steps(date_stop = Sys.Date())
     data.frame(
          step        = vapply(reg, `[[`, "", "id"),
          group       = vapply(reg, `[[`, "", "group"),
          depends_on  = vapply(reg, function(s) paste(s$deps, collapse = ", "), ""),
          description = vapply(reg, `[[`, "", "desc"),
          stringsAsFactors = FALSE, row.names = NULL
     )
}


# ---------------------------------------------------------------------------
# Step registry
# ---------------------------------------------------------------------------

#' The ordered data-pipeline step registry
#'
#' Mirrors \code{model/LAUNCH.R} groups 1-4. Each entry carries an id, its
#' LAUNCH group, a one-line description, the ids it depends on, and a closure
#' taking \code{PATHS}. Arguments are pinned to the canonical LAUNCH.R values;
#' change them here, not at the call site, so the pipeline stays reproducible.
#'
#' @keywords internal
#' @noRd
.mosaic_data_steps <- function(date_stop = Sys.Date()) {

     S <- function(id, group, desc, deps = character(0), run) {
          list(id = id, group = group, desc = desc, deps = deps, run = run)
     }

     list(
          # ---- GROUP 1: independent sources --------------------------------
          S("download_africa_shapefile", "1A", "Africa shapefile", character(0),
            function(P) MOSAIC::download_africa_shapefile(P)),
          S("download_all_country_shapefiles", "1A", "Per-country shapefiles", character(0),
            function(P) MOSAIC::download_all_country_shapefiles(P)),
          S("process_country_similarity_data", "1A", "Country similarity matrix", character(0),
            function(P) MOSAIC::process_country_similarity_data(P)),

          S("process_open_meteo_data", "1B", "ERA5 historical + CMIP6 climate", character(0),
            # force = TRUE is REQUIRED for the DA-01 (v0.90.6) week-relabelling to
            # take effect: process_open_meteo_data() caches on source-vs-output
            # mtimes, and a code-only fix never invalidates that cache, so a plain
            # call silently keeps mislabelled parquets. Same class as the
            # soil-moisture schema change recorded in NEWS.md. Revert to a plain
            # call once every consumer has been rebuilt at least once.
            function(P) MOSAIC::process_open_meteo_data(P, force = TRUE)),

          S("download_country_DEM", "1C", "Country DEMs", "download_all_country_shapefiles",
            function(P) MOSAIC::download_country_DEM(P, iso_codes = MOSAIC::iso_codes_africa)),
          S("get_elevation", "1C", "Mean + median elevation",
            c("download_country_DEM", "download_all_country_shapefiles"),
            function(P) {
                 MOSAIC::get_elevation(P, iso_codes = MOSAIC::iso_codes_africa, "mean")
                 MOSAIC::get_elevation(P, iso_codes = MOSAIC::iso_codes_africa, "median")
            }),

          S("process_enso_data", "1D", "ENSO/IOD (NMME)", character(0),
            function(P) MOSAIC::process_enso_data(P, source = "nmme")),

          # Degrades rather than blocks: .wpp_newest_raw() resolves whatever raw
          # file is on disk, so a network failure must not gate the 8 steps
          # downstream of demographics. Same contract as the EM-DAT step.
          S("download_UN_WPP_data", "1E", "UN WPP bulk pull (pop/CBR/CDR)", character(0),
            function(P) tryCatch(MOSAIC::download_UN_WPP_data(P),
                                 error = function(e) {
                                      message("WPP download failed (", conditionMessage(e),
                                              "); using newest raw file already on disk.")
                                      invisible(NULL)
                                 })),
          S("process_UN_demographics_data", "1E", "UN WPP demographics", "download_UN_WPP_data",
            function(P) MOSAIC::process_UN_demographics_data(P)),

          # Same contract: .wb_newest_raw() falls back to the portal export.
          # download_WB_data() already catches per-indicator errors, but a
          # total outage (DNS, 5xx) still throws from the first fetch.
          S("download_WB_data", "1F", "World Bank API pull (4 indicators)", character(0),
            function(P) tryCatch(MOSAIC::download_WB_data(P),
                                 error = function(e) {
                                      message("World Bank download failed (", conditionMessage(e),
                                              "); using newest raw files already on disk.")
                                      invisible(NULL)
                                 })),
          S("process_WB_GDP_data", "1F", "World Bank GDP", "download_WB_data",
            function(P) MOSAIC::process_WB_GDP_data(P)),
          S("process_WB_poverty_ratio_data", "1F", "World Bank poverty ratio", "download_WB_data",
            function(P) MOSAIC::process_WB_poverty_ratio_data(P)),
          S("process_WB_population_density_data", "1F", "World Bank pop density", "download_WB_data",
            function(P) MOSAIC::process_WB_population_density_data(P)),
          S("process_WB_urban_population_data", "1F", "World Bank urban pop", "download_WB_data",
            function(P) MOSAIC::process_WB_urban_population_data(P)),

          S("process_UNICEF_malnutrition_data", "1G", "UNICEF/JME malnutrition", character(0),
            function(P) MOSAIC::process_UNICEF_malnutrition_data(P)),

          S("get_WASH_data", "1H", "WASH coverage (Sikder 2023)", character(0),
            function(P) MOSAIC::get_WASH_data(P)),

          # Reads AFRICA_ADM0.shp via an unguarded sf::st_read (process_OAG_data.R:67).
          S("process_OAG_data", "1I", "OAG flight mobility", "download_africa_shapefile",
            function(P) MOSAIC::process_OAG_data(P)),

          S("get_symptomatic_prop_data", "1J", "Symptomatic proportion (lit)", character(0),
            function(P) MOSAIC::get_symptomatic_prop_data(P)),
          S("get_immune_decay_data", "1J", "Immune decay (lit)", character(0),
            function(P) MOSAIC::get_immune_decay_data(P)),
          S("get_vaccine_effectiveness_data", "1J", "Vaccine effectiveness (lit)", character(0),
            function(P) MOSAIC::get_vaccine_effectiveness_data(P)),
          S("get_rho_care_seeking_params", "1J", "Care-seeking rho (lit)", character(0),
            function(P) MOSAIC::get_rho_care_seeking_params(P)),

          S("download_EMDAT_data", "1K", "EM-DAT pull (only if EMDAT_API_KEY set)", character(0),
            function(P) {
                 if (!nzchar(Sys.getenv("EMDAT_API_KEY"))) {
                      message("no EMDAT_API_KEY; using newest extract already in raw/EMDAT/")
                      return(invisible(NULL))
                 }
                 MOSAIC::download_EMDAT_data(P)
            }),
          S("process_EMDAT_data", "1K", "EM-DAT flood + cyclone panels", "download_EMDAT_data",
            function(P) MOSAIC::process_EMDAT_data(P)),

          S("download_IDMC_data", "1L", "IDMC IDU events from HDX", character(0),
            function(P) MOSAIC::download_IDMC_data(P)),
          S("process_IDMC_data", "1L", "IDMC displacement panels", "download_IDMC_data",
            function(P) MOSAIC::process_IDMC_data(P)),

          # ---- 1M. Overland mobility OD sources ---------------------------
          # Degrades rather than blocks: process_mobility_od_data() resolves
          # the newest snapshot already on disk.
          S("download_mobility_od_sources", "1M",
            "Bilateral mobility sources (DESA + Abel-Cohen + Meta SCI)", character(0),
            function(P) tryCatch(MOSAIC::download_mobility_od_sources(P),
                                 error = function(e) {
                                      message("mobility OD download failed (", conditionMessage(e),
                                              "); using newest snapshot on disk.")
                                      invisible(NULL)
                                 })),
          S("process_mobility_od_data", "1M", "Fuse 4-source overland OD structure",
            c("download_mobility_od_sources", "download_all_country_shapefiles"),
            function(P) MOSAIC::process_mobility_od_data(P)),
          S("est_overland_tau_prior", "1M", "Overland departure-rate prior (E3 evidence)",
            character(0),
            function(P) MOSAIC::est_overland_tau_prior(P)),
          S("rake_mobility_od_to_tau", "1M", "IPF-rake fused OD to departure margins",
            c("process_mobility_od_data", "est_overland_tau_prior"),
            function(P) MOSAIC::rake_mobility_od_to_tau(P)),

          # ---- GROUP 2: surveillance + derived -----------------------------
          S("process_WHO_annual_data", "2A", "WHO annual cholera", character(0),
            function(P) MOSAIC::process_WHO_annual_data(P)),
          S("process_CFR_data", "2A", "CFR from WHO annual", "process_WHO_annual_data",
            function(P) MOSAIC::process_CFR_data(P, min_obs = 3 / 0.02)),
          S("process_WHO_weekly_data", "2A", "WHO weekly surveillance", character(0),
            function(P) MOSAIC::process_WHO_weekly_data(P)),
          S("process_JHU_weekly_data", "2A", "JHU weekly surveillance", character(0),
            function(P) MOSAIC::process_JHU_weekly_data(P)),
          S("process_SUPP_weekly_data", "2A", "Supplemental weekly surveillance", character(0),
            function(P) MOSAIC::process_SUPP_weekly_data(P)),
          S("process_AI_cholera_data", "2A", "AI-mined weekly surveillance", character(0),
            function(P) MOSAIC::process_AI_cholera_data(P)),
          S("downscale_weekly_cholera_data", "2A", "WHO weekly -> daily", "process_WHO_weekly_data",
            function(P) MOSAIC::downscale_weekly_cholera_data(P)),
          S("process_cholera_surveillance_data", "2A", "Multi-source combine (include_ai)",
            c("process_WHO_weekly_data", "process_JHU_weekly_data",
              "process_SUPP_weekly_data", "process_AI_cholera_data"),
            function(P) MOSAIC::process_cholera_surveillance_data(P, include_ai = TRUE)),

          S("est_CFR_hierarchical", "2B", "Hierarchical Bayesian CFR", "process_WHO_annual_data",
            function(P) MOSAIC::est_CFR_hierarchical(
                 P, min_cases = 3, k_year = 15, include_country_trends = TRUE,
                 population_weighted = FALSE, save_diagnostics = FALSE, verbose = TRUE)),

          S("est_demographic_rates", "2C", "Birth/death/population rates",
            c("process_UN_demographics_data", "process_WHO_annual_data"),
            function(P) MOSAIC::est_demographic_rates(
                 P, date_start = "2000-01-01", date_stop = "2030-12-31",
                 smooth_method = "none")),

          S("get_WHO_vaccination_data", "2D", "WHO ICG OCV (hardcoded, <=2024)", character(0),
            function(P) MOSAIC::get_WHO_vaccination_data(P)),
          S("process_WHO_vaccination_data", "2D", "Process WHO OCV", "get_WHO_vaccination_data",
            function(P) MOSAIC::process_WHO_vaccination_data(P)),
          # NO demographics edge: this reads processed/demographics/
          # demographics_africa_2000_2023.csv (2024-09-20), which NO function in
          # the package writes -- process_UN_demographics_data() emits
          # UN_world_population_prospects_*.csv instead. Declaring the edge
          # implied a refresh path that does not exist. Same for est_mobility,
          # est_vaccination_rate and process_WHO_vaccination_data.
          # UNRESOLVED: that orphan file needs either a producer or repointed
          # consumers; until then the OCV coverage denominator is frozen.
          S("process_GTFCC_vaccination_data", "2D", "GTFCC OCV requests", character(0),
            function(P) MOSAIC::process_GTFCC_vaccination_data(P)),
          S("combine_vaccination_data", "2D", "Combine WHO + GTFCC",
            c("process_WHO_vaccination_data", "process_GTFCC_vaccination_data"),
            function(P) MOSAIC::combine_vaccination_data(P)),
          S("est_vaccination_rate", "2D", "Vaccination rate", "combine_vaccination_data",
            function(P) MOSAIC::est_vaccination_rate(
                 P, max_rate_per_day = 20000, date_start = "2000-01-01",
                 date_stop = date_stop, data_source = "BOTH")),

          S("est_vaccine_effectiveness", "2E", "Vaccine effectiveness model",
            "get_vaccine_effectiveness_data",
            function(P) MOSAIC::est_vaccine_effectiveness(P)),

          # ---- GROUP 3: derived parameters ---------------------------------
          S("est_seasonal_dynamics", "3A", "Seasonal transmission",
            c("process_open_meteo_data", "process_cholera_surveillance_data",
              "download_africa_shapefile"),
            function(P) MOSAIC::est_seasonal_dynamics(
                 P, date_start = "2010-09-01", date_stop = "2025-09-01", min_obs = 10,
                 clustering_method = "ward.D2", k = 4,
                 data_sources = c("WHO", "JHU", "SUPP"))),

          S("est_symptomatic_prop", "3B", "Symptomatic proportion", "get_symptomatic_prop_data",
            function(P) MOSAIC::est_symptomatic_prop(P)),
          S("est_immune_decay_vaccine", "3B", "Vaccine immune decay", "get_immune_decay_data",
            function(P) MOSAIC::est_immune_decay_vaccine(P)),

          S("get_suspected_cases", "3C", "Suspected-case multiplier", character(0),
            function(P) MOSAIC::get_suspected_cases(P)),
          S("get_generation_time_distribution", "3C", "Generation time", character(0),
            function(P) MOSAIC::get_generation_time_distribution(P, mean_generation_time = 5)),

          S("est_WASH_coverage", "3D", "WASH coverage index theta_j",
            c("get_WASH_data", "process_country_similarity_data",
              "process_cholera_surveillance_data"),
            function(P) MOSAIC::est_WASH_coverage(P)),

          S("est_mobility", "3E", "Mobility tau_i / pi_ij",
            c("process_OAG_data", "download_all_country_shapefiles"),
            function(P) MOSAIC::est_mobility(P)),

          S("est_epidemic_peaks", "3F", "Epidemic peaks", "process_cholera_surveillance_data",
            function(P) MOSAIC::est_epidemic_peaks(P)),

          # ---- GROUP 4: suitability (opt-in) -------------------------------
          # NOTE: est_vaccination_rate is deliberately NOT a dependency.
          # compile_suitability_data() has read no vaccination data since
          # v0.30.26 (removal documented at compile_suitability_data.R:582) --
          # declaring the edge would gate the most expensive step in the
          # pipeline on an input it does not consume.
          # The World Bank / WASH / elevation reads below are each wrapped in
          # `if (file.exists())` with no else branch, so a missing producer
          # yields a valid-looking panel with columns silently absent.
          S("compile_suitability_data", "4A", "Compile LSTM training panel",
            c("process_open_meteo_data", "process_enso_data", "est_demographic_rates",
              "process_cholera_surveillance_data",
              "est_mobility", "est_epidemic_peaks", "process_EMDAT_data",
              "process_WB_GDP_data", "process_WB_population_density_data",
              "process_WB_urban_population_data", "process_WB_poverty_ratio_data",
              "get_WASH_data", "get_elevation"),
            function(P) MOSAIC::compile_suitability_data(
                 P, cutoff = NULL, use_epidemic_peaks = TRUE, date_start = "2000-01-01",
                 date_stop = NULL, forecast_mode = TRUE, forecast_horizon = 9,
                 include_lags = TRUE))

          # NB no est_suitability step. This driver builds DATA; fitting the
          # LSTM is model fitting and lives with the calibration workflow (call
          # est_suitability() directly). It was group 4B until v0.91.14.
     )
}


# ---------------------------------------------------------------------------
# Manual-input preflight
# ---------------------------------------------------------------------------

#' Report on the MOSAIC data sources that must be refreshed by hand
#'
#' Checks each source with no automated route, reports its age, and prints the
#' refresh recipe for anything stale. Called automatically at the top of
#' \code{\link{update_mosaic_data}}; run it standalone to answer "what do I
#' need to download?" without starting a pipeline.
#'
#' Nothing here is fatal. A stale manual input degrades the outputs that
#' depend on it; it does not stop the pipeline.
#'
#' @param root MOSAIC parent directory. Defaults to \code{get_paths()$ROOT}.
#' @param verbose Print the report. Set \code{FALSE} for the data frame only.
#'
#' @return Invisibly, a \code{data.frame}: \code{source}, \code{path},
#'   \code{found}, \code{modified}, \code{age_days}, \code{stale},
#'   \code{instructions}.
#'
#' @seealso \code{\link{update_mosaic_data}}
#' @examples
#' \dontrun{ check_mosaic_manual_inputs("~/MOSAIC") }
#' @export
check_mosaic_manual_inputs <- function(root = NULL, verbose = TRUE) {

     if (is.null(root)) root <- MOSAIC::get_paths()$ROOT
     raw <- file.path(root, "MOSAIC-data", "raw")

     # stale_days: how old before the source is worth re-pulling. Set from how
     # often the publisher actually releases, not from a uniform default.
     man <- list(
          list(src = "EM-DAT disasters", stale = 90,
               # BOTH extensions: process_EMDAT_data() accepts .xlsx and .csv,
               # and download_EMDAT_data(source="api") writes .csv. A single
               # character-class glob cannot express this -- the extensions are
               # different lengths -- so pass a vector and Sys.glob each.
               glob = file.path(raw, "EMDAT",
                                c("public_emdat_*.xlsx", "public_emdat_*.csv")),
               inst = paste0("Sign in at https://public.emdat.be, submit a custom request ",
                             "(Africa, 2000-present, all natural disasters), download the xlsx ",
                             "into MOSAIC-data/raw/EMDAT/ and add a PROVENANCE.md row. ",
                             "The portal filename already matches the discovery pattern. ",
                             "With a key: download_EMDAT_data(PATHS).")),
          list(src = "OAG flight mobility", stale = 365,
               # case-insensitive: *.CSV alone fails on Linux/CI
               glob = file.path(raw, "OAG", "*.[cC][sS][vV]"),
               inst = paste0("Commercial OAG extract; no automated route. Request a refreshed ",
                             "Africa origin-destination job from OAG and place the CSV in ",
                             "MOSAIC-data/raw/OAG/. NOTE: process_OAG_data() hardcodes the ",
                             "filename 'oag_africa_JobId3062750.CSV' -- update it for a new job.")),
          list(src = "UNICEF/WHO/WB JME malnutrition", stale = 400,
               glob = file.path(raw, "UNICEF", "child_malnutrition", "*.xlsx"),
               inst = paste0("Download the current JME country estimates from ",
                             "https://data.unicef.org/resources/jme-report/ into ",
                             "MOSAIC-data/raw/UNICEF/child_malnutrition/. NOTE: ",
                             "process_UNICEF_malnutrition_data() hardcodes ",
                             "'JME_Country_Estimates_May_2023.xlsx' -- update it for a new edition."))
     )

     rows <- lapply(man, function(m) {
          hits <- unique(unlist(lapply(m$glob, Sys.glob)))
          # Drop unreadable entries (dangling symlinks give mtime NA). Without
          # this, which.max() returns integer(0) and the data.frame() below
          # dies -- aborting the whole run from a preflight that is documented
          # as non-fatal.
          if (length(hits)) hits <- hits[!is.na(file.info(hits)$mtime)]
          if (!length(hits)) {
               return(data.frame(source = m$src, path = paste(m$glob, collapse = " | "),
                                 found = FALSE,
                                 modified = NA_character_, age_days = NA_integer_,
                                 stale = TRUE, instructions = m$inst,
                                 stringsAsFactors = FALSE))
          }
          mt  <- file.info(hits)$mtime
          new <- hits[which.max(mt)]
          # as.Date.POSIXct defaults to tz = "UTC", which rolls an evening
          # local mtime into tomorrow and yields a negative age. Use local time.
          mdate <- as.Date(max(mt), tz = Sys.timezone())
          age <- as.integer(Sys.Date() - mdate)
          data.frame(source = m$src, path = new, found = TRUE,
                     modified = format(mdate), age_days = age,
                     stale = age > m$stale, instructions = m$inst,
                     stringsAsFactors = FALSE)
     })
     out <- do.call(rbind, rows)

     # Code-level staleness: this one cannot be detected from a file mtime.
     out <- rbind(out, data.frame(
          source = "WHO ICG OCV requests", path = "R/get_WHO_vaccine_data.R (hardcoded)",
          found = TRUE, modified = "2024 (in source)", age_days = NA_integer_,
          stale = TRUE,
          instructions = paste0("get_WHO_vaccination_data() holds the ICG dashboard as ",
                                "tab-separated string literals covering 2016-2024 ONLY. ",
                                "Refreshing means EDITING R SOURCE: add a raw_text_<year> block ",
                                "from https://app.powerbi.com (WHO ICG OCV dashboard). ",
                                "GTFCC (auto-updating) partially covers recent campaigns."),
          stringsAsFactors = FALSE))

     if (verbose) {
          cat("\n-- Manual inputs (no automated route) --------------------------\n")
          for (i in seq_len(nrow(out))) {
               r <- out[i, ]
               flag <- if (!r$found) "MISSING" else if (r$stale) "STALE  " else "ok     "
               age  <- if (is.na(r$age_days)) "" else sprintf(" (%d d)", r$age_days)
               cat(sprintf("  [%s] %-32s %s%s\n", flag, r$source,
                           if (is.na(r$modified)) "-" else r$modified, age))
          }
          bad <- out[out$stale, , drop = FALSE]
          if (nrow(bad)) {
               cat("\n  Refresh instructions:\n")
               for (i in seq_len(nrow(bad))) {
                    cat(sprintf("\n  * %s\n", bad$source[i]))
                    cat(strwrap(bad$instructions[i], width = 74, prefix = "      "), sep = "\n")
               }
               cat("\n  (None of these block the run.)\n")
          }
     }
     invisible(out)
}


# ---------------------------------------------------------------------------
# Internals
# ---------------------------------------------------------------------------

#' @keywords internal
#' @noRd
.mosaic_select_steps <- function(registry, steps, skip) {
     keep <- registry
     # No group is excluded by default any more: every step here is a data
     # build. The old include_suitability gate existed only to hold back the
     # multi-hour est_suitability fit, which was removed from the registry in
     # v0.91.14 (it is model fitting, not data building).
     match_any <- function(s, sel) {
          s$id %in% sel || s$group %in% sel || substr(s$group, 1, 1) %in% sel
     }
     if (!is.null(steps)) keep <- Filter(function(s) match_any(s, steps), keep)
     if (!is.null(skip))  keep <- Filter(function(s) !match_any(s, skip), keep)

     # skip= silently severs edges: the driver only checks deps that are in the
     # plan, so a skipped producer lets its consumer run against the old file.
     # Correct for the resume workflow, but it must not be silent.
     if (!is.null(skip) && length(keep)) {
          ids  <- vapply(keep, `[[`, "", "id")
          gone <- setdiff(unique(unlist(lapply(keep, `[[`, "deps"))), ids)
          gone <- intersect(gone, vapply(registry, `[[`, "", "id"))
          if (length(gone)) {
               warning("skip= removed step(s) that remaining steps depend on: ",
                       paste(gone, collapse = ", "),
                       ". Those consumers will run against whatever is already ",
                       "on disk.", call. = FALSE)
          }
     }
     keep
}

#' @keywords internal
#' @noRd
.mosaic_empty_results <- function(plan) {
     data.frame(
          step    = vapply(plan, `[[`, "", "id"),
          group   = vapply(plan, `[[`, "", "group"),
          status  = rep("pending", length(plan)),
          seconds = rep(NA_real_, length(plan)),
          message = rep(NA_character_, length(plan)),
          stringsAsFactors = FALSE, row.names = NULL
     )
}

#' @keywords internal
#' @noRd
.mosaic_one_line <- function(x) {
     x <- gsub("[\r\n]+", " ", paste(x, collapse = " "))
     if (nchar(x) > 160L) paste0(substr(x, 1, 157L), "...") else x
}

#' @keywords internal
#' @noRd
.mosaic_print_summary <- function(res, root) {
     n_ok <- sum(res$status == "ok")
     n_f  <- sum(res$status == "failed")
     n_b  <- sum(res$status == "blocked")
     n_n  <- sum(res$status %in% c("not_run", "pending"))
     cat("\n", strrep("=", 78), "\n", sep = "")
     cat(sprintf("Summary: %d ok, %d failed, %d blocked%s (%d steps, %.1f min)\n",
                 n_ok, n_f, n_b,
                 if (n_n) sprintf(", %d not run", n_n) else "",
                 nrow(res), sum(res$seconds, na.rm = TRUE) / 60))
     if (n_f || n_b || n_n) {
          cat("\n  STEP                                 STATUS   NOTE\n")
          # Must include not_run/pending: after an abort those steps never
          # executed, and omitting them from the hint lets an operator re-run
          # only the failure, see green, and believe the pipeline is complete.
          bad <- res[res$status %in% c("failed", "blocked", "not_run", "pending"), , drop = FALSE]
          for (i in seq_len(nrow(bad))) {
               cat(sprintf("  %-36s %-8s %s\n", bad$step[i], bad$status[i],
                           ifelse(is.na(bad$message[i]), "", bad$message[i])))
          }
          cat("\n  Re-run just these once fixed:\n")
          cat(sprintf("    update_mosaic_data(\"%s\", steps = c(%s), refresh_repos = FALSE)\n",
                      root, paste0('"', bad$step, '"', collapse = ", ")))
     }
     if (n_ok > 0) {
          cat("\n  Package data objects (priors_default / config_default) are NOT rebuilt\n",
              "  by this function. If surveillance, CFR, or demographics changed, rebuild:\n",
              "    export MOSAIC_BUILD_DATE_START=<start>\n",
              "    Rscript data-raw/make_priors_default.R\n",
              "    Rscript -e 'devtools::install(\".\")'\n",
              "    Rscript data-raw/make_config_default.R\n", sep = "")
     }
     cat(strrep("=", 78), "\n\n", sep = "")
}


#' Assert the step registry is internally consistent
#'
#' Two failures the driver would otherwise hide: a typo'd dependency id is
#' silently dropped by \code{intersect()}, and a step placed above its own
#' dependency yields a permanent bogus \code{blocked} (the driver executes in
#' list order and does not sort).
#'
#' @keywords internal
#' @noRd
.mosaic_validate_registry <- function(reg) {
     ids <- vapply(reg, `[[`, "", "id")
     if (anyDuplicated(ids)) {
          stop("Duplicate step id(s) in registry: ",
               paste(unique(ids[duplicated(ids)]), collapse = ", "), call. = FALSE)
     }
     deps <- unique(unlist(lapply(reg, `[[`, "deps")))
     bad  <- setdiff(deps, ids)
     if (length(bad)) {
          stop("Registry dependency names no such step: ",
               paste(bad, collapse = ", "), call. = FALSE)
     }
     for (i in seq_along(reg)) {
          late <- intersect(reg[[i]]$deps, ids[seq_along(ids) > i])
          if (length(late)) {
               stop("Step '", ids[i], "' is listed before its dependency(ies): ",
                    paste(late, collapse = ", "),
                    ". Registry order must be topological.", call. = FALSE)
          }
     }
     invisible(TRUE)
}
