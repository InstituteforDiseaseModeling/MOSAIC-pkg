#' Process Combined Weekly and Daily Cholera Surveillance Data with Truly Square Data Structure
#'
#' This function reads the processed weekly cholera data from WHO, JHU, and supplemental (SUPP) sources,
#' labels each record by its source, combines them (removing duplicate country-week entries according to
#' the specified source preference), creates a truly square data structure with all country-week combinations
#' from min to max date across the entire dataset, and downscales the combined weekly totals to a daily time series 
#' using \code{MOSAIC::downscale_weekly_values} with integer allocation.
#'
#' @param PATHS A list of file paths. Must include:
#' \itemize{
#'   \item \strong{DATA_WHO_WEEKLY}: Directory containing \code{cholera_country_weekly_processed.csv} from WHO.
#'   \item \strong{DATA_JHU_WEEKLY}: Directory containing \code{cholera_country_weekly_processed.csv} from JHU.
#'   \item \strong{DATA_SUPP_WEEKLY}: Directory containing \code{cholera_country_weekly_processed.csv} from supplemental source (may include extra columns).
#'   \item \strong{DATA_CHOLERA_WEEKLY}: Directory where the combined weekly output will be saved.
#'   \item \strong{DATA_CHOLERA_DAILY}: Directory where the combined daily output will be saved.
#'   \item \strong{DATA_WHO_ANNUAL} (optional): Directory containing \code{who_afro_annual.csv},
#'     the WHO annual totals that imputed rows are reconciled against (see Details).
#' }
#' @details Duplicate country-week entries across sources are resolved by
#'   selecting one whole row per week: a row carrying a count beats an empty one;
#'   then the trust tier decides, whatever the sources -- an observed row
#'   (WHO/JHU/SUPP, or AI \code{observed}/\code{documented_zero}) beats a
#'   reconstructed one (a WHO multi-week report spread over the weeks it covers,
#'   \code{who_catchup_*}, see \code{\link{process_WHO_weekly_data}}), which beats
#'   an imputed one (AI \code{fourier_*} or any other modelled method); a row with
#'   a case count beats a deaths-only row; and the fixed priority
#'   \strong{WHO > JHU > AI > SUPP} decides the rest. When the selected row has no
#'   death count, the deaths of the highest-priority other observed row reporting
#'   the same case count that week are used (the same report, compared after
#'   half-up rounding) and the week keeps the lower of the two rows'
#'   \code{confidence_weight}; \code{source_deaths} names the source of each death
#'   count.
#'
#'   Four cross-source rules keep one outbreak from being counted twice:
#'   \enumerate{
#'     \item \strong{AI aggregates.} An AI \code{observed} week of at least 20 cases,
#'       in a week no direct source (WHO/JHU/SUPP) reports a case count for, is an
#'       aggregate mislabelled as one week, and is dropped, when it is at least
#'       five times every direct-source count in the four weeks either side (two
#'       or more of them) -- Nigeria 2023 week 21 carries 1,851 cases, the
#'       year-to-date total of the WHO weeks before it -- or when it is within 15%
#'       of a WHO weekly cumulative of its year (the year-to-date total before it,
#'       or the year's total) with a WHO week reporting cases within four weeks of
#'       it: Congo 2023 week 29 carries 63 cases, the running total of the outbreak
#'       whose 69 cases the WHO dashboard reports in weeks 30 and 34.
#'     \item \strong{WHO multi-week windows.} Inside the window of a WHO multi-week
#'       report the report accounts for every week. A non-WHO observed row that
#'       repeats the dashboard's positive as-published value for its week, or the
#'       report total, is a copy of the dashboard and is dropped. When another
#'       source reports a positive count for every week of the window, the WHO
#'       total is redistributed in proportion to those counts
#'       (\code{who_catchup_shaped}, confidence 0.9); otherwise the even spread
#'       stands. A window shaped by a curated epidemic curve
#'       (\code{who_catchup_curated_shaped}) keeps its curve. All non-WHO rows in
#'       the window are then dropped, so a window never mixes the WHO total with
#'       another source's partial weeks.
#'     \item \strong{Curated corrections.} The surveillance curation table
#'       (\code{inst/extdata/surveillance_curation.csv}) lists corrections the
#'       rules cannot derive, each with its evidence and source: WHO windows dated
#'       by an outbreak report, and timed by its documented epidemic curve where
#'       there is one (South Africa 2023; anchors in
#'       \code{inst/extdata/surveillance_curation_shapes.csv}), applied by
#'       \code{\link{process_WHO_weekly_data}};
#'       documented absences of cholera, whose imputed weeks are emptied
#'       (\code{drop_imputed}: Angola 2023, South Sudan May 2023 - September
#'       2024); and contested imputed years that are kept but listed
#'       (\code{flag_imputed}: Burkina Faso 2025).
#'     \item \strong{Imputed rows against the WHO account.} AI \code{fourier_*}
#'       rows spread an annual (or other multi-week) total over every week of its
#'       span, and the observed weeks that later overwrite part of that span are
#'       not netted out, so the surviving imputed rows can duplicate cases those
#'       weeks already report (Ghana 2024: 937 imputed cases in April-August beside
#'       the 4,618 WHO-reported cases of an outbreak that began on 4 October), or
#'       spread a total the WHO account contradicts. Per country and ISO year with a
#'       WHO account \eqn{A}, with observed plus reconstructed cases \eqn{O} and
#'       imputed cases \eqn{I}, the imputed rows may only fill the gap to the
#'       account: they keep \eqn{\min(I, \max(0, A - O))} cases, cases and deaths
#'       scaled by the same factor. Rows are emptied (NA) when less than one case
#'       is left in all, and individually when rescaled below half a case (the
#'       integer daily downscale would make them zero-case weeks that keep their
#'       weight: Cote d'Ivoire 2025 had 8 cases left over 40 weeks). The account is
#'       the WHO annual total (\code{DATA_WHO_ANNUAL/who_afro_annual.csv}) or,
#'       for a country-year without an AFRO annual row, the positive year-to-date
#'       total of its WHO weekly rows, which accounts only for the weeks up to WHO's
#'       last report of the year: it reconciles those weeks and any imputed AI run
#'       that touches a WHO-reported week (Somalia 2026 in the 2026-09-18 AI
#'       snapshot: the AI spread the WHO epidemiological update's 233 cases, which
#'       the three WHO weekly rows already report), and leaves alone an imputed run
#'       that starts after WHO's last report (Somalia's Africa CDC weeks of
#'       February-September 2026). AI \code{inferred_zero} weeks are never emptied.
#'     \item \strong{Inferred zeros next to a positive count.} An AI
#'       \code{inferred_zero} week within 28 days of another row of the country
#'       that reports cases is dropped before selection: the absence it infers is
#'       contradicted nearby.
#'       Skipped, with a message, when
#'       \code{PATHS$DATA_WHO_ANNUAL} is not set.
#'   }
#'   Every week these rules (or the WHO spreading) change is listed, with its
#'   before and after values and the evidence, in
#'   \code{DATA_CHOLERA_WEEKLY/cholera_surveillance_weekly_adjustments.csv},
#'   including imputed rows a WHO window supersedes and the flagged weeks of
#'   \code{flag_imputed} (listed unchanged).
#' @param include_ai Logical (default \code{FALSE}). When \code{TRUE}, reads the
#'   AI-mined processed file (\code{DATA_AI_WEEKLY/cholera_country_weekly_processed.csv},
#'   produced by \code{\link{process_AI_cholera_data}}) as a fourth source, using
#'   its per-row \code{confidence_weight} and \code{disaggregation_method}. When
#'   \code{FALSE}, only WHO/JHU/SUPP are merged. If \code{include_ai = TRUE} but the
#'   AI file is missing/empty, a warning is emitted and the merge proceeds with the
#'   three direct sources.
#'
#'   The output always carries a \code{confidence_weight} and a
#'   \code{disaggregation_method} column (stable schema regardless of this flag):
#'   direct-source observations (WHO/JHU/SUPP) get \code{confidence_weight = 1.0}
#'   (fully trusted) and \code{disaggregation_method = NA}; AI rows keep their
#'   per-row values; and square-grid cells with no observation get
#'   \code{confidence_weight = NA}.
#'
#' @return Invisibly returns \code{NULL}. Side effects:
#' \itemize{
#'   \item Reads weekly CSVs from all three sources and adds a \code{source} column.
#'   \item Cleans rows with missing key grouping fields (iso_code, year, week).
#'   \item Harmonizes columns by taking the union across all sources, NA-filling any column missing from a given source (so source-specific columns are never silently dropped).
#'   \item Applies the cross-source rules (AI aggregates, WHO multi-week windows) and deduplicates by \code{iso_code} and \code{date_start} (the actual week Monday, robust to year-boundary week-1 collisions): observed beats reconstructed beats imputed, then the fixed priority WHO > JHU > AI > SUPP (see Details), and adds \code{source_deaths}; then applies the curated corrections and limits imputed rows to the gap left by the WHO account.
#'   \item Saves the adjustment log to
#'     \code{PATHS$DATA_CHOLERA_WEEKLY/cholera_surveillance_weekly_adjustments.csv}.
#'   \item Creates truly square data structure with all country-week combinations from min to max date (missing data = NA).
#'   \item Saves the combined weekly data to
#'     \code{PATHS$DATA_CHOLERA_WEEKLY/cholera_surveillance_weekly_combined.csv}.
#'   \item Applies the trust-tier gate before downscaling: only weeks tagged
#'     \code{disaggregation_method} \code{assumed_zero} (surveillance silence, a pure
#'     assumption) are NA-blanked. \code{observed}, \code{documented_zero},
#'     \code{inferred_zero}, direct WHO/JHU/SUPP rows, AND \code{fourier_*} (synthetic reconstructions of real
#'     annual/quarterly totals) all reach the daily fit target carrying their
#'     per-week \code{confidence_weight} (lower for fourier, ~0.4-0.5), which
#'     \code{calc_model_likelihood()} consumes as a per-observation weight -- so
#'     low-confidence reconstructed weeks inform the fit at reduced weight rather
#'     than being dropped.
#'   \item Downscales weekly \code{cases} and \code{deaths} to daily counts,
#'     preserving square structure (keeping days with NA), and carries \code{source},
#'     \code{source_deaths}, \code{disaggregation_method}, and \code{confidence_weight} to each daily row
#'     (the weight is replicated constant across the week, never divided).
#'   \item Saves the combined daily data to
#'     \code{PATHS$DATA_CHOLERA_DAILY/cholera_surveillance_daily_combined.csv}.
#' }
#'
#' @importFrom ISOweek ISOweek2date
#' @importFrom lubridate month wday floor_date
#' @export
#'

process_cholera_surveillance_data <- function(PATHS, include_ai = FALSE) {

     # create output dirs
     dirs <- c(PATHS$DATA_CHOLERA_WEEKLY, PATHS$DATA_CHOLERA_DAILY)
     lapply(dirs, function(d) if (!dir.exists(d)) dir.create(d, recursive = TRUE))

     # helper: read and clean a source (union-safe; returns NULL if absent/empty)
     read_source <- function(path, src) {
          if (!file.exists(path)) return(NULL)
          df <- utils::read.csv(path, stringsAsFactors = FALSE)
          df <- df[rowSums(is.na(df)) < ncol(df), ]
          df <- subset(df, !is.na(iso_code) & !is.na(year) & !is.na(week))
          if (nrow(df) == 0) return(NULL)
          df$source <- src
          df
     }

     # file paths
     who_path <- file.path(PATHS$DATA_WHO_WEEKLY, "cholera_country_weekly_processed.csv")
     jhu_path <- file.path(PATHS$DATA_JHU_WEEKLY, "cholera_country_weekly_processed.csv")
     sup_path <- file.path(PATHS$DATA_SUPP_WEEKLY, "cholera_country_weekly_processed.csv")
     ai_path  <- file.path(PATHS$DATA_AI_WEEKLY,  "cholera_country_weekly_processed.csv")

     # read each
     d_who <- read_source(who_path, "WHO")
     d_jhu <- read_source(jhu_path, "JHU")
     d_sup <- read_source(sup_path, "SUPP")
     d_ai  <- if (isTRUE(include_ai)) read_source(ai_path, "AI") else NULL

     if (isTRUE(include_ai) && is.null(d_ai)) {
          warning(sprintf(
               "include_ai=TRUE but no usable AI data at %s; proceeding with WHO/JHU/SUPP only.",
               ai_path), immediate. = TRUE)
     }

     # Interlock note: AI rows carry confidence_weight + disaggregation_method.
     # The LSTM suitability path consumes the weight (loss_suitability.R
     # apply_cw_overlay, use_confidence_weight = TRUE -- production default), so AI /
     # synthetic-fourier rows are down-weighted there, NOT at parity with direct
     # observations. As of v0.47.1 the daily combined file applies a minimal
     # TRUST-TIER GATE (below): AI `observed`, `documented_zero`, `inferred_zero`
     # (since v1.0.1) AND `fourier_*` (synthetic reconstructions of real
     # annual/quarterly totals) all reach the calibration fit target
     # (reported_cases/reported_deaths) carrying their per-week confidence_weight
     # (lower for fourier and inferred zeros); only `assumed_zero` (a pure
     # assumption) is NA-blanked. calc_model_likelihood() consumes the per-cell confidence_weight,
     # so low-confidence reconstructed weeks inform the fit at reduced weight.
     if (isTRUE(include_ai) && !is.null(d_ai)) {
          warning(sprintf(
               paste0("include_ai=TRUE: %d AI rows merged. AI `observed`/`documented_zero`/`inferred_zero`/`fourier_*` ",
                      "weeks enter the fit target carrying their confidence_weight (fourier ~0.4-0.5, inferred zeros 0.2-0.6, ",
                      "down-weighted via calc_model_likelihood per-observation weights); only ",
                      "`assumed_zero` is NA-blanked."),
               nrow(d_ai)), immediate. = TRUE)
     }

     # List order sets the rbind order; AI is placed before SUPP to match the
     # WHO > JHU > AI > SUPP priority. When include_ai=FALSE, d_ai is NULL and
     # this is byte-identical to the previous 3-source behavior.
     sources <- Filter(Negate(is.null), list(d_who, d_jhu, d_ai, d_sup))
     if (length(sources) == 0) stop("No surveillance sources available.")

     # UNION harmonization: take the union of all columns across sources, NA-fill
     # missing ones. Replaces the previous Reduce(intersect, ...) which silently
     # dropped any column not present in every source CSV.
     all_cols <- unique(unlist(lapply(sources, names)))
     sources <- lapply(sources, function(df) {
          for (cc in setdiff(all_cols, names(df))) df[[cc]] <- NA
          df[, all_cols]
     })

     # combine and dedupe by iso_code/year/week
     all_df <- do.call(rbind, sources)
     n_before <- nrow(all_df)
     key_cols <- c("iso_code", "year", "week")
     
     # FIX: Remove rows with NA in key columns before deduplication to prevent all-NA rows
     all_df <- all_df[complete.cases(all_df[, key_cols]), ]
     message(sprintf("Removed %d rows with missing key fields (iso_code, year, week)", n_before - nrow(all_df)))

     # Centralized confidence metadata (always present in the output schema):
     #   - direct-source observations (WHO/JHU/SUPP) are fully trusted -> 1.0
     #   - AI rows keep their per-row confidence_weight (from process_AI_cholera_data)
     #   - empty square-grid cells (added below) stay NA = "no observation"
     # disaggregation_method is NA for non-AI rows (not applicable). These columns
     # exist regardless of include_ai so the combined schema is stable.
     if (!"confidence_weight"     %in% names(all_df)) all_df$confidence_weight     <- NA_real_
     if (!"disaggregation_method" %in% names(all_df)) all_df$disaggregation_method <- NA_character_
     trusted <- all_df$source %in% c("WHO", "JHU", "SUPP")
     all_df$confidence_weight[trusted & is.na(all_df$confidence_weight)] <- 1.0

     # Deduplicate by (iso_code, date_start) -- the actual ISO-week Monday -- NOT
     # (iso_code, year, week). The (year, week) key collides at year boundaries:
     # ISO week 1 can belong to two different calendar Mondays (e.g. 2012-01-02
     # and 2012-12-31 both carry year=2012, week=1 across sources), which are 51
     # weeks apart. Keying on (year, week) collapses those two genuine weeks into
     # one and silently drops a real observation when only one of the pair carries
     # data. Keying on the Monday (date_start) keeps every distinct week.
     all_df$date_start <- as.Date(all_df$date_start)
     # Guard: dedup keys on date_start, so a row with NA date_start would collide on a
     # shared "iso_NA" key and could drop a real observation. All current sources carry
     # complete date_start; this protects against a future malformed source.
     n_bad_ds <- sum(is.na(all_df$date_start))
     if (n_bad_ds > 0) {
          warning(sprintf("Dropped %d row(s) with NA date_start before dedup.", n_bad_ds))
          all_df <- all_df[!is.na(all_df$date_start), ]
     }
     all_df$key <- paste(all_df$iso_code, as.character(all_df$date_start), sep = "_")

     # Trust tier of every row: 1 = observed (a direct WHO/JHU/SUPP row, or an AI
     # `observed` / `documented_zero` row); 2 = reconstructed (a WHO multi-week report
     # spread over the weeks it covers by process_WHO_weekly_data(): its total is
     # reported, only the timing within the window is not); 3 = imputed (AI
     # `fourier_*` or any other modelled method).
     all_df$.tier <- .surveillance_tier(all_df$disaggregation_method)
     adjustments <- list()

     # An inferred zero next to another row's positive count is contradicted
     # nearby, so the week is left to the other sources (see Details).
     izg <- .drop_inferred_zeros_near_positive(all_df)
     all_df <- izg$data
     adjustments <- c(adjustments, izg$log)

     # Cross-source rules applied before selection (see Details): inside a WHO
     # multi-week window the WHO report accounts for every week ...
     win <- .reconcile_who_catchup_windows(all_df)
     all_df <- win$data
     adjustments <- c(adjustments, win$log)
     # ... and an AI weekly count far above every direct-source count around it, or
     # repeating a WHO cumulative total, is an aggregate mislabelled as a week, not
     # a week of incidence.
     ai_bad <- .flag_inconsistent_ai_rows(all_df)
     ai_cum <- .flag_ai_cumulative_rows(all_df)
     ai_cum$flag <- ai_cum$flag & !ai_bad
     if (any(ai_bad))
          adjustments$ai_aggregate <- .surveillance_adjustment_log(
               all_df[ai_bad, ], "ai_aggregate_dropped",
               detail = "AI weekly count >= 5x every direct-source count within 4 weeks")
     if (any(ai_cum$flag))
          adjustments$ai_cumulative <- .surveillance_adjustment_log(
               all_df[ai_cum$flag, ], "ai_cumulative_dropped", detail = ai_cum$detail[ai_cum$flag])
     all_df <- all_df[!(ai_bad | ai_cum$flag), ]

     # Source selection within a key (same country + same Monday across sources).
     # ONE row supplies the week (whole-row selection), chosen by, in order:
     #   (1) a row carrying a count beats an empty row (both fields NA);
     #   (2) the lower trust tier wins, whatever the sources and whichever fields
     #       they carry: observed beats reconstructed beats imputed -- so an
     #       observed deaths-only row beats an imputed row with cases, and imputed
     #       values never fill a field of an observed week;
     #   (3) among rows of the same tier, a row with a case count beats a
     #       deaths-only row (cases are the primary fit target);
     #   (4) source priority WHO > JHU > AI > SUPP.
     # This replaces a completeness tie-break (rows with cases AND deaths first,
     # then priority) inherited from the original keep_source merge, which assumed
     # every source reports both fields. Once process_JHU_weekly_data() kept a
     # missing JHU death count as NA (v0.100.0), that rule handed every JHU week
     # without deaths to any AI row that had both fields, including fourier
     # interpolations.
     #
     # Deaths completion: when the selected row has cases but no deaths, its deaths
     # are taken from the highest-priority OTHER observed row reporting the same
     # case count for that week -- the same report, one source having dropped the
     # deaths field. Counts are compared after half-up rounding (floor(x + 0.5)),
     # because JHU carries half-integer counts. A row with a different case count
     # is a different report and is not mixed in, and reconstructed or imputed
     # deaths are never used. `source_deaths` records the source of the deaths
     # value (NA when deaths is NA); it differs from `source` only for completed
     # weeks. A completed week carries the lower of the two rows' confidence_weight,
     # since the weight scores both channels (make_config_default builds
     # reported_cases_weight and reported_deaths_weight from it).
     # Vectorized: O(n log n).
     all_df$.run_overlaps_who <- .imputed_runs_touching_who(all_df)
     all_df$.priority <- .SURVEILLANCE_PRIORITY[all_df$source]
     all_df$.empty    <- is.na(all_df$cases) & is.na(all_df$deaths)
     all_df$.no_cases <- is.na(all_df$cases)
     all_df <- all_df[order(all_df$iso_code, all_df$date_start, all_df$.empty,
                            all_df$.tier, all_df$.no_cases, all_df$.priority), ]
     is_winner <- !duplicated(all_df$key)
     dedup <- all_df[is_winner, ]
     dedup$source_deaths <- ifelse(is.na(dedup$deaths), NA_character_, dedup$source)

     half_up <- function(x) floor(x + 0.5)
     donors <- all_df[!is_winner & all_df$.tier == 1L &
                      !is.na(all_df$cases) & !is.na(all_df$deaths), ]
     wi <- match(donors$key, dedup$key)
     donors <- donors[is.na(dedup$deaths[wi]) & !is.na(dedup$cases[wi]) &
                      half_up(donors$cases) == half_up(dedup$cases[wi]), ]
     donors <- donors[!duplicated(donors$key), ]  # already priority-sorted
     wi <- match(donors$key, dedup$key)
     dedup$deaths[wi]            <- donors$deaths
     dedup$source_deaths[wi]     <- donors$source
     dedup$confidence_weight[wi] <- pmin(dedup$confidence_weight[wi],
                                         donors$confidence_weight, na.rm = TRUE)
     if (nrow(donors) > 0)
          message(sprintf("Completed deaths for %d week(s) from another observed row reporting the same case count",
                          nrow(donors)))

     # Documented corrections the rules cannot derive: imputed weeks inside a
     # documented absence of cholera are emptied, contested ones are flagged.
     curated <- .apply_imputed_curation(dedup, .surveillance_curation(c("drop_imputed", "flag_imputed")))
     dedup <- curated$data
     adjustments <- c(adjustments, curated$log)

     # Imputed allocations may only fill the gap between the observed weeks and the
     # WHO account of the same country-year (see Details).
     annual <- .read_who_annual_account(PATHS)
     if (!is.null(annual)) {
          cap <- .cap_imputed_to_annual_account(dedup, annual, .who_weekly_year_totals(d_who))
          dedup <- cap$data
          adjustments <- c(adjustments, cap$log)
     }

     # Provenance: one row per week whose value differs from what a source reported
     # (spread WHO reports, absorbed or dropped rows, rescaled imputations).
     adj <- if (length(adjustments) > 0) do.call(rbind, adjustments) else .surveillance_adjustment_log(dedup[0, ], character(0))
     adj <- adj[order(adj$iso_code, adj$date_start, adj$rule), ]
     adj_out <- file.path(PATHS$DATA_CHOLERA_WEEKLY, "cholera_surveillance_weekly_adjustments.csv")
     utils::write.csv(adj, adj_out, row.names = FALSE)
     if (nrow(adj) > 0) {
          tab <- table(adj$rule)
          message(sprintf("Surveillance adjustments (%s) logged to: %s",
                          paste(sprintf("%s=%d", names(tab), as.integer(tab)), collapse = ", "), adj_out))
     }

     dedup$key <- NULL
     dedup$.priority <- NULL
     dedup$.empty <- NULL
     dedup$.tier <- NULL
     dedup$.run_overlaps_who <- NULL
     dedup$.no_cases <- NULL

     removed <- n_before - nrow(dedup)
     message(if (removed > 0) sprintf("Removed %d duplicate or superseded weekly entries (observed > reconstructed > imputed, then priority WHO>JHU>AI>SUPP)", removed)
             else "No duplicate weekly entries found")
     
     # Create square data structure by filling missing country-week combinations
     message("Creating square data structure across entire dataset...")
     
     # Get all unique countries from the data
     all_countries <- unique(dedup$iso_code)
     
     # Create complete weekly sequence from min to max date across entire dataset
     min_date <- min(as.Date(dedup$date_start), na.rm = TRUE)
     max_date <- max(as.Date(dedup$date_start), na.rm = TRUE)
     
     # Generate all Monday dates (week starts) from min to max
     all_monday_dates <- seq(
          from = min_date,
          to = max_date,
          by = "week"
     )
     
     # Create complete time periods data frame
     complete_time_periods <- data.frame(
          date_start = all_monday_dates,
          date_stop = all_monday_dates + 6,  # Sunday = Monday + 6 days
          stringsAsFactors = FALSE
     )
     
     # Add year, week, and month information
     # DA-01: `%G` (ISO week-based year), NOT `%Y` (calendar year), is the only
     # valid partner for `%V`. A Monday in late December belongs to ISO week 1 of
     # the next ISO year; pairing it with the calendar year date-stamps the row
     # ~51 weeks early and collides it with the genuine (N, W01) row.
     complete_time_periods$year <- as.integer(format(complete_time_periods$date_start, "%G"))
     complete_time_periods$week <- as.integer(format(complete_time_periods$date_start, "%V"))
     complete_time_periods$month <- as.integer(format(complete_time_periods$date_start, "%m"))
     
     # Generate all possible country-time combinations (truly square)
     square_grid <- merge(
          data.frame(iso_code = all_countries, stringsAsFactors = FALSE),
          complete_time_periods,
          all = TRUE
     )
     
     # Add country names to the grid
     square_grid$country <- MOSAIC::convert_iso_to_country(square_grid$iso_code)
     
     # Join observations to the square grid on the unambiguous (iso_code,
     # date_start) Monday key ONLY; take calendar fields (country, year, week,
     # month, date_stop) from the grid so year-boundary week-1 rows align
     # correctly (a 7-column join on (year, week, ...) can mismatch when a source
     # labels a boundary Monday by ISO year while the grid uses calendar year).
     # Carry the value and provenance/trust columns from the deduplicated data.
     dedup$date_start <- as.Date(dedup$date_start)
     carry_cols <- intersect(c("cases", "deaths", "source", "source_deaths", "note",
                               "confidence_weight", "disaggregation_method"),
                             names(dedup))
     wk <- merge(
          square_grid,
          dedup[, c("iso_code", "date_start", carry_cols)],
          by = c("iso_code", "date_start"),
          all.x = TRUE
     )

     # For missing combinations, set cases/deaths to NA and source to NA
     wk$cases[is.na(wk$cases)] <- NA
     wk$deaths[is.na(wk$deaths)] <- NA
     wk$source[is.na(wk$source)] <- NA
     
     # Sort by country and date for clean output
     wk <- wk[order(wk$iso_code, wk$year, wk$week), ]
     
     message(sprintf("Created truly square data structure: %d total observations (%d countries \u00D7 %d weeks)", 
                     nrow(wk), 
                     length(all_countries), 
                     length(all_monday_dates)))
     message(sprintf("  - %d reported observations (%.1f%%)", 
                     sum(!is.na(wk$cases)), 
                     100 * sum(!is.na(wk$cases)) / nrow(wk)))
     message(sprintf("  - %d missing observations (%.1f%%)", 
                     sum(is.na(wk$cases)), 
                     100 * sum(is.na(wk$cases)) / nrow(wk)))

     # save combined weekly
     weekly_out <- file.path(PATHS$DATA_CHOLERA_WEEKLY,
                             "cholera_surveillance_weekly_combined.csv")
     utils::write.csv(wk, weekly_out, row.names = FALSE)
     message("Combined weekly data saved to: ", weekly_out)

     # downscale to daily
     daily_list <- lapply(split(wk, wk$iso_code), function(df_iso) {
          df_iso$date_start <- as.Date(df_iso$date_start)
          # Trust-tier gate (replaces the previous blanket source=="AI" blank).
          # Calibration consumers read this daily file (est_epidemic_peaks ->
          # calc_model_likelihood peak terms, est_initial_E_I, plotting, and -- once
          # re-pointed -- the reported_cases/reported_deaths fit matrices). We gate
          # on disaggregation_method, NOT source: keep `observed`, `documented_zero`
          # (informative true-absence), direct WHO/JHU/SUPP rows (method = NA), AND
          # `fourier_*` (synthetic reconstructions of REAL annual/quarterly totals).
          # The fourier rows are NOT hard-filtered: they enter the fit target carrying
          # their lower per-week confidence_weight (~0.4-0.5 vs ~0.9 for observed),
          # which calc_model_likelihood() consumes as a per-observation weight so the
          # reconstructed weeks inform the fit at reduced weight rather than being
          # dropped (fills otherwise-empty eras, e.g. the 2019-2022 JHU->WHO handoff
          # gap, with down-weighted real-magnitude data). Only `assumed_zero`
          # (surveillance silence != true absence -- a pure assumption, not a
          # reconstruction of real data) is NA-blanked. The weight is NA'd in lockstep
          # so every surviving cell carries a finite confidence_weight.
          meth <- df_iso$disaggregation_method
          # Trust-tier gate (v0.47.1): NA-blank ONLY `assumed_zero`. fourier_* synthetic
          # reconstructions are KEPT and down-weighted by their confidence_weight rather
          # than dropped -- low-confidence-but-real-magnitude weeks inform the fit at
          # reduced weight, per the per-observation weighting in calc_model_likelihood().
          drop_week <- !is.na(meth) & (meth == "assumed_zero")
          df_iso$cases[drop_week]  <- NA
          df_iso$deaths[drop_week] <- NA
          df_iso$source_deaths[drop_week] <- NA
          if ("confidence_weight" %in% names(df_iso))
               df_iso$confidence_weight[drop_week] <- NA

          dc <- MOSAIC::downscale_weekly_values(df_iso$date_start, df_iso$cases, integer = TRUE)
          names(dc)[2] <- "cases"
          dd <- MOSAIC::downscale_weekly_values(df_iso$date_start, df_iso$deaths, integer = TRUE)
          names(dd)[2] <- "deaths"
          df_day <- merge(dc, dd, by = "date", all = TRUE)

          # Carry per-week provenance/trust to daily resolution: each daily date
          # inherits the source, disaggregation_method, and confidence_weight of the
          # ISO week (Monday) it falls in. confidence_weight is replicated constant
          # across the 7 days -- it is a trust weight, NOT a summable count, so it is
          # never routed through downscale_weekly_values(). (Fixes the previous
          # scalar source[1] carry, which mislabeled every day with the first week.)
          day_monday <- df_day$date - (as.integer(format(df_day$date, "%u")) - 1L)
          mi <- match(day_monday, df_iso$date_start)
          cw_col <- if ("confidence_weight" %in% names(df_iso)) df_iso$confidence_weight[mi] else NA_real_

          # Keep all days to maintain square structure (do not remove NA days)
          data.frame(
               country               = MOSAIC::convert_iso_to_country(df_iso$iso_code[1]),
               iso_code              = df_iso$iso_code[1],
               month                 = lubridate::month(df_day$date),
               week                  = lubridate::wday(df_day$date),
               date                  = df_day$date,
               cases                 = as.integer(df_day$cases),
               deaths                = as.integer(df_day$deaths),
               source                = df_iso$source[mi],
               source_deaths         = df_iso$source_deaths[mi],
               disaggregation_method = df_iso$disaggregation_method[mi],
               confidence_weight     = cw_col,
               stringsAsFactors = FALSE
          )
     })
     daily_all <- do.call(rbind, daily_list)
     
     # Log daily data structure
     daily_date_range <- range(as.Date(daily_all$date), na.rm = TRUE)
     daily_days <- as.numeric(diff(daily_date_range)) + 1
     daily_countries <- length(unique(daily_all$iso_code))
     daily_reported <- sum(!is.na(daily_all$cases))
     daily_missing <- sum(is.na(daily_all$cases))
     
     message(sprintf("Created truly square daily data structure: %d total observations (%d countries \u00D7 %d days)", 
                     nrow(daily_all), daily_countries, daily_days))
     message(sprintf("  - %d reported daily observations (%.1f%%)", 
                     daily_reported, 100 * daily_reported / nrow(daily_all)))
     message(sprintf("  - %d missing daily observations (%.1f%%)", 
                     daily_missing, 100 * daily_missing / nrow(daily_all)))

     # save combined daily
     daily_out <- file.path(PATHS$DATA_CHOLERA_DAILY,
                            "cholera_surveillance_daily_combined.csv")
     utils::write.csv(daily_all, daily_out, row.names = FALSE)
     message("Combined daily data saved to: ", daily_out)

     invisible(NULL)
}


# Source priority among rows of the same trust tier.
.SURVEILLANCE_PRIORITY <- c(WHO = 1L, JHU = 2L, AI = 3L, SUPP = 4L)

# Thresholds of the AI-aggregate rules (see process_cholera_surveillance_data()).
.AI_AGGREGATE_MIN_CASES <- 20
.AI_AGGREGATE_RATIO     <- 5
.AI_AGGREGATE_HALF_DAYS <- 28L
.AI_AGGREGATE_MIN_REF   <- 2L
.AI_CUMULATIVE_TOLERANCE <- 0.15

# Imputed rows rescaled below half a case are emptied: the integer daily
# downscale would turn them into zero-case weeks that keep their weight.
.IMPUTED_RESIDUE_MIN_CASES <- 0.5
# An imputed excess over the WHO account below this many cases is rounding: the
# AI file stores 4 decimals, so a year of weeks spreading exactly the WHO total
# can sum a few thousandths above it.
.IMPUTED_ACCOUNT_TOLERANCE <- 0.01

# In the current year, an AFRO annual total within this many cases of the WHO weekly sum is that
# sum, and accounts only for the weeks WHO reported (.cap_imputed_to_annual_account).
.ANNUAL_EQUALS_WEEKLY_TOLERANCE <- 1


#' Trust tier of surveillance rows from their disaggregation method
#'
#' @param method Vector of \code{disaggregation_method} values (an all-missing
#'   column read from CSV arrives as logical).
#' @return Integer vector: 1 observed (NA, \code{observed}, \code{documented_zero});
#'   2 reconstructed (\code{who_catchup_*}); 3 imputed (any other method).
#' @noRd
.surveillance_tier <- function(method) {
     method <- as.character(method)
     tier <- rep(3L, length(method))
     tier[is.na(method) | method %in% c("observed", "documented_zero")] <- 1L
     tier[!is.na(method) & startsWith(method, "who_catchup")] <- 2L
     tier
}


# An AI inferred_zero week within this many days of another row's positive count
# of the same country is dropped (.drop_inferred_zeros_near_positive).
.INFERRED_ZERO_GUARD_DAYS <- 28L


#' Drop AI inferred zeros next to a positive count
#'
#' An AI \code{inferred_zero} week asserts absence by inference only: an
#' aggregate whose total finer evidence already reaches, or a country's absence
#' from a multi-country summary. When any other row of the country (any source)
#' reports cases within \code{.INFERRED_ZERO_GUARD_DAYS} of it, the inference is
#' contradicted nearby -- the AI builder claims the residual weeks of an aggregate
#' as zero even when finer evidence overshoots it (Kenya 2024: a WHO catch-up week
#' of 199 cases against a JHU total of 147 for the same seven weeks) -- so the week
#' is left to the other sources rather than scored as an absence. Documented zeros
#' (an outbreak's declared end, a source stating no cases) are not affected.
#'
#' @param df All candidate rows before selection.
#' @return list(data = df without the dropped rows, log = list of adjustment-log
#'   frames, rule \code{inferred_zero_near_positive}).
#' @noRd
.drop_inferred_zeros_near_positive <- function(df) {
     iz <- which(df$disaggregation_method %in% "inferred_zero")
     if (length(iz) == 0L) return(list(data = df, log = list()))
     pos <- !is.na(df$cases) & df$cases > 0
     ds  <- as.integer(as.Date(df$date_start))
     drop <- logical(length(iz))
     for (iso in unique(df$iso_code[iz])) {
          k <- which(df$iso_code[iz] == iso)
          p <- ds[pos & df$iso_code == iso]
          if (length(p) == 0L) next
          drop[k] <- vapply(ds[iz[k]], function(x) any(abs(p - x) <= .INFERRED_ZERO_GUARD_DAYS), logical(1))
     }
     if (!any(drop)) return(list(data = df, log = list()))
     rows <- iz[drop]
     log <- list(.surveillance_adjustment_log(
          df[rows, ], "inferred_zero_near_positive",
          detail = sprintf("another row of the country reports cases within %d days", .INFERRED_ZERO_GUARD_DAYS)))
     list(data = df[-rows, , drop = FALSE], log = log)
}


#' Adjustment-log rows for surveillance weeks changed by a reconciliation rule
#'
#' @param rows Data frame of the affected rows as the source reported them.
#' @param rule Rule label(s), recycled.
#' @param cases_after,deaths_after Values after the rule (NA = week emptied).
#' @param detail Free-text evidence, recycled.
#' @return Data frame with columns iso_code, date_start, source,
#'   disaggregation_method, rule, cases_before, deaths_before, cases_after,
#'   deaths_after, detail.
#' @noRd
.surveillance_adjustment_log <- function(rows, rule, cases_after = NA_real_,
                                         deaths_after = NA_real_, detail = NA_character_) {
     n <- nrow(rows)
     data.frame(iso_code              = rows$iso_code,
                date_start            = as.Date(rows$date_start),
                source                = rows$source,
                disaggregation_method = rows$disaggregation_method,
                rule                  = rep_len(rule, n),
                cases_before          = rows$cases,
                deaths_before         = rows$deaths,
                cases_after           = rep_len(cases_after, n),
                deaths_after          = rep_len(deaths_after, n),
                detail                = rep_len(detail, n),
                stringsAsFactors      = FALSE)
}


#' Flag AI weekly counts that are aggregates mislabelled as one week
#'
#' An AI \code{observed} row of at least 20 cases, in a week no direct source
#' (WHO, JHU, SUPP) reports a case count for (so the AI row would supply the
#' week), is flagged when the direct sources report at least two other weeks within
#' four weeks of it and the AI count is at least five times the largest of them.
#' Nigeria 2023 week 21 is the case in point: the AI row carries 1,851 cases and 52
#' deaths, the year-to-date total of the WHO weeks before it (1,917 cases), beside
#' WHO weeks of 0-21.
#'
#' @param df Combined source rows (\code{source}, \code{disaggregation_method},
#'   \code{iso_code}, \code{date_start}, \code{cases}).
#' @return Logical vector, TRUE for rows to drop.
#' @noRd
.flag_inconsistent_ai_rows <- function(df) {
     flag <- rep(FALSE, nrow(df))
     direct <- df$source %in% c("WHO", "JHU", "SUPP") & !is.na(df$cases)
     direct_week <- paste(df$iso_code, as.character(df$date_start))[direct]
     cand <- which(df$source == "AI" & df$disaggregation_method %in% "observed" &
                   !is.na(df$cases) & df$cases >= .AI_AGGREGATE_MIN_CASES &
                   !(paste(df$iso_code, as.character(df$date_start)) %in% direct_week))
     if (length(cand) == 0L) return(flag)
     for (iso in unique(df$iso_code[cand])) {
          di <- which(direct & df$iso_code == iso)
          if (length(di) < .AI_AGGREGATE_MIN_REF) next
          o  <- order(df$date_start[di])
          dd <- as.numeric(df$date_start[di][o])
          dv <- df$cases[di][o]
          for (i in cand[df$iso_code[cand] == iso]) {
               t0 <- as.numeric(df$date_start[i])
               k  <- which(abs(dd - t0) <= .AI_AGGREGATE_HALF_DAYS & dd != t0)
               if (length(k) >= .AI_AGGREGATE_MIN_REF &&
                   df$cases[i] >= .AI_AGGREGATE_RATIO * max(max(dv[k]), 1)) flag[i] <- TRUE
          }
     }
     flag
}


#' Flag AI weekly counts that repeat a WHO cumulative total
#'
#' An AI \code{observed} row of at least 20 cases, in a week no direct source
#' reports a case count for, is flagged when it is within 15% of a WHO weekly
#' cumulative of the same year: the year-to-date total of the WHO weeks before it
#' (what a cumulative report filed that week would show) or the total of all the
#' year's WHO weeks (an outbreak total), and a WHO week of that year reports
#' cases within four weeks of it. Congo 2023 week 29 is the case in point: the AI
#' row's 63 cases (a JHU-database record of 17-22 July) is the running total of
#' the Dolisie outbreak whose 69 cases the WHO dashboard reports in weeks 30 and
#' 34. Both totals use the as-published WHO counts.
#'
#' @param df Combined source rows (\code{source}, \code{disaggregation_method},
#'   \code{iso_code}, \code{date_start}, \code{cases}, \code{cases_reported}).
#' @return list(flag = logical vector, detail = character vector).
#' @noRd
.flag_ai_cumulative_rows <- function(df) {
     flag   <- rep(FALSE, nrow(df))
     detail <- rep(NA_character_, nrow(df))
     direct <- df$source %in% c("WHO", "JHU", "SUPP") & !is.na(df$cases)
     week_key <- paste(df$iso_code, as.character(df$date_start))
     cand <- which(df$source == "AI" & df$disaggregation_method %in% "observed" &
                   !is.na(df$cases) & df$cases >= .AI_AGGREGATE_MIN_CASES &
                   !(week_key %in% week_key[direct]))
     if (length(cand) == 0L) return(list(flag = flag, detail = detail))
     who <- which(df$source == "WHO" & !is.na(df$cases))
     pub <- if ("cases_reported" %in% names(df)) df$cases_reported else df$cases
     pub <- ifelse(is.na(pub), 0, pub)
     yr  <- as.integer(format(as.Date(df$date_start) + 3L, "%Y"))
     for (i in cand) {
          w  <- who[df$iso_code[who] == df$iso_code[i] & yr[who] == yr[i]]
          if (length(w) == 0L) next
          t0 <- as.numeric(df$date_start[i])
          near <- abs(as.numeric(df$date_start[w]) - t0) <= .AI_AGGREGATE_HALF_DAYS & df$cases[w] > 0
          if (!any(near)) next
          ref <- c(year_to_date = sum(pub[w][as.numeric(df$date_start[w]) < t0]),
                   year_total   = sum(pub[w]))
          hit <- ref >= .AI_AGGREGATE_MIN_CASES &
               abs(df$cases[i] - ref) <= .AI_CUMULATIVE_TOLERANCE * ref
          if (!any(hit)) next
          k <- which(hit)[1L]
          flag[i] <- TRUE
          detail[i] <- sprintf("AI weekly count %s is within %d%% of the WHO %s of %s",
                               format(df$cases[i]), round(100 * .AI_CUMULATIVE_TOLERANCE),
                               c(year_to_date = "year-to-date total before it",
                                 year_total = "weekly total for the year")[names(ref)[k]],
                               format(ref[k]))
     }
     list(flag = flag, detail = detail)
}


#' Reconcile other sources with WHO multi-week report windows
#'
#' Inside a window built by \code{process_WHO_weekly_data()} the WHO report accounts
#' for every week, so the window is kept whole: (1) a non-WHO observed row
#' repeating the dashboard's positive as-published value for its week, or the
#' report total, is a copy of the dashboard (the AI repo ingests it) and is
#' dropped; (2) when another source reports a positive count for every week of the
#' window, the WHO total is redistributed in proportion to those weekly counts, in
#' whole counts (\code{who_catchup_shaped}, confidence 0.9) -- the report's total
#' with an observed shape (a zero week cannot tell no cases from no report, so it
#' never shapes a window), unless the window already follows a curated epidemic
#' curve (\code{who_catchup_curated_shaped}); (3) every non-WHO row in the window
#' is then dropped, so a window never mixes the WHO total with another source's
#' partial weeks.
#'
#' @param df Combined source rows with \code{.tier} and the WHO window columns.
#' @return list(data = df after the rules, log = list of adjustment-log frames).
#' @noRd
.reconcile_who_catchup_windows <- function(df) {
     log <- list()
     is_win <- df$source == "WHO" & df$.tier == 2L & !is.na(df$catchup_start)
     if (!any(is_win)) return(list(data = df, log = log))
     half_up <- function(x) floor(x + 0.5)
     win_id  <- ifelse(is_win, paste(df$iso_code, as.character(df$catchup_start)), NA_character_)
     drop    <- rep(FALSE, nrow(df))
     shaped_by <- character(0)

     for (w in unique(win_id[is_win])) {
          wr      <- which(win_id %in% w)
          wr      <- wr[order(df$date_start[wr])]
          weeks   <- df$date_start[wr]
          total_c <- df$catchup_cases[wr[1L]]
          total_d <- df$catchup_deaths[wr[1L]]
          other   <- which(df$iso_code == df$iso_code[wr[1L]] & df$source != "WHO" &
                           df$date_start %in% weeks)
          if (length(other) == 0L) next

          published <- df$cases_reported[wr][match(df$date_start[other], weeks)]
          echo <- df$.tier[other] == 1L & !is.na(df$cases[other]) & df$cases[other] > 0 &
               ((!is.na(published) & half_up(df$cases[other]) == half_up(published)) |
                (total_c > 0 & half_up(df$cases[other]) == half_up(total_c)))
          if (any(echo))
               log[[length(log) + 1L]] <- .surveillance_adjustment_log(
                    df[other[echo], ], "who_copy_dropped",
                    detail = "repeats the WHO dashboard value of a multi-week report window")

          # Shape donors: positive weekly counts only. A zero cannot tell a week with
          # no cases from a week with no report -- the ambiguity the window resolves --
          # so a window with another source's zero week keeps the even spread. A
          # window already shaped by a curated epidemic curve keeps that curve.
          obs <- other[df$.tier[other] == 1L & !echo & !is.na(df$cases[other]) & df$cases[other] > 0]
          obs <- obs[order(match(df$date_start[obs], weeks), .SURVEILLANCE_PRIORITY[df$source[obs]])]
          obs <- obs[!duplicated(df$date_start[obs])]
          curve <- any(df$disaggregation_method[wr] %in% "who_catchup_curated_shaped")
          if (!curve && length(obs) == length(weeks) && sum(df$cases[obs]) > 0) {
               shape <- df$cases[obs][match(weeks, df$date_start[obs])]
               df$cases[wr]  <- .spread_count(total_c, shape)
               df$deaths[wr] <- .spread_count(total_d, shape)
               df$disaggregation_method[wr] <- "who_catchup_shaped"
               df$confidence_weight[wr]     <- .SHAPED_WINDOW_CONFIDENCE
               shaped_by[w] <- paste(sort(unique(df$source[obs])), collapse = "+")
          }
          inside <- sprintf("week inside a WHO report of %s cases spread over %d weeks from %s",
                            format(total_c), length(weeks), as.character(min(weeks)))
          absorbed <- other[df$.tier[other] == 1L & !echo]
          if (length(absorbed) > 0L)
               log[[length(log) + 1L]] <- .surveillance_adjustment_log(
                    df[absorbed, ], "absorbed_by_who_window", detail = inside)
          # Imputed rows would lose their week to the WHO row anyway; they are
          # listed because the window, not the source priority, removes them.
          superseded <- other[df$.tier[other] != 1L & !(is.na(df$cases[other]) & is.na(df$deaths[other]))]
          if (length(superseded) > 0L)
               log[[length(log) + 1L]] <- .surveillance_adjustment_log(
                    df[superseded, ], "imputed_dropped_in_who_window", detail = inside)
          drop[other] <- TRUE
     }

     wr <- which(is_win)
     src <- shaped_by[win_id[wr]]
     cur_id <- if ("catchup_curation_id" %in% names(df)) as.character(df$catchup_curation_id[wr]) else NA_character_
     how <- ifelse(!is.na(src), paste0(" in proportion to ", src, " weekly counts"),
                   ifelse(df$disaggregation_method[wr] %in% "who_catchup_curated_shaped",
                          " in proportion to the curated epidemic curve", " (uniform)"))
     log[[length(log) + 1L]] <- data.frame(
          iso_code = df$iso_code[wr], date_start = df$date_start[wr], source = "WHO",
          disaggregation_method = df$disaggregation_method[wr],
          rule = df$disaggregation_method[wr],
          cases_before = df$cases_reported[wr], deaths_before = df$deaths_reported[wr],
          cases_after = df$cases[wr], deaths_after = df$deaths[wr],
          detail = sprintf("WHO report of %s cases / %s deaths spread over %d weeks from %s%s%s",
                           format(df$catchup_cases[wr]), format(df$catchup_deaths[wr]),
                           df$catchup_weeks[wr], as.character(df$catchup_start[wr]), how,
                           ifelse(is.na(cur_id), "", paste0("; curated window ", cur_id))),
          stringsAsFactors = FALSE)
     list(data = df[!drop, ], log = log)
}


#' WHO annual case totals used as the account for imputed rows
#'
#' @param PATHS Path list; uses \code{DATA_WHO_ANNUAL}.
#' @return Data frame (iso_code, year, cases_total) without the AFRO aggregate, or
#'   NULL (with a message when the path is not set, a warning when the file is
#'   missing).
#' @noRd
.read_who_annual_account <- function(PATHS) {
     if (is.null(PATHS$DATA_WHO_ANNUAL)) {
          message("PATHS$DATA_WHO_ANNUAL not set: imputed rows are not reconciled against WHO annual totals")
          return(NULL)
     }
     f <- file.path(PATHS$DATA_WHO_ANNUAL, "who_afro_annual.csv")
     if (!file.exists(f)) {
          warning(sprintf("WHO annual file not found (%s): imputed rows are not reconciled against WHO annual totals", f),
                  call. = FALSE)
          return(NULL)
     }
     a <- utils::read.csv(f, stringsAsFactors = FALSE)
     a <- a[a$iso_code != "AFRO" & !is.na(a$cases_total), c("iso_code", "year", "cases_total")]
     if (anyDuplicated(a[, c("iso_code", "year")]))
          stop("who_afro_annual.csv has duplicated (iso_code, year) rows")
     a
}


#' WHO weekly year-to-date totals, the account where no AFRO annual row exists
#'
#' @param who WHO weekly rows (\code{iso_code}, \code{date_start}, \code{cases}).
#' @return Data frame (iso_code, year, cases_total, last_week) per country and ISO
#'   year with a positive total, or NULL. \code{last_week} is the Monday of the
#'   last WHO week of the year with a case count: the total accounts for the weeks
#'   up to it and says nothing about the weeks after it.
#' @noRd
.who_weekly_year_totals <- function(who) {
     if (is.null(who) || nrow(who) == 0L) return(NULL)
     ds <- as.Date(who$date_start)
     yr <- as.integer(format(ds + 3L, "%Y"))
     tot <- stats::aggregate(list(cases_total = who$cases), list(iso_code = who$iso_code, year = yr),
                             FUN = function(x) sum(x, na.rm = TRUE))
     rep_wk <- !is.na(who$cases)
     last <- stats::aggregate(list(last_week = ds[rep_wk]), list(iso_code = who$iso_code[rep_wk], year = yr[rep_wk]),
                              FUN = max)
     tot <- merge(tot, last, by = c("iso_code", "year"))
     tot <- tot[tot$cases_total > 0, ]
     if (nrow(tot) == 0L) NULL else tot
}


#' Imputed AI runs that touch a WHO-reported week
#'
#' An AI \code{fourier_*} total is spread over a run of consecutive weeks. A run
#' that covers a week WHO reports re-spreads a period the WHO account already
#' covers (Somalia 2026 in the 2026-09-18 AI snapshot: the WHO epidemiological
#' update's 233 cases spread over January-April, overlapping the WHO weeks); a run
#' that starts after WHO's last report is a different source (the Africa CDC weekly
#' reports of February-September 2026) that a WHO year-to-date total cannot account
#' for.
#'
#' @param df All candidate rows before selection, with \code{.tier}.
#' @return Logical vector along \code{df}: TRUE for imputed (tier 3) AI rows whose
#'   run of consecutive imputed AI weeks contains a week with a WHO case count.
#' @noRd
.imputed_runs_touching_who <- function(df) {
     out <- rep(FALSE, nrow(df))
     ai_imp <- which(df$source == "AI" & df$.tier == 3L & !is.na(df$cases))
     if (length(ai_imp) == 0L) return(out)
     who_keys <- df$key[df$source == "WHO" & !is.na(df$cases)]
     for (iso in unique(df$iso_code[ai_imp])) {
          i <- ai_imp[df$iso_code[ai_imp] == iso]
          i <- i[order(df$date_start[i])]
          run <- cumsum(c(1L, diff(as.integer(df$date_start[i])) != 7L))
          touch <- tapply(df$key[i] %in% who_keys, run, any)
          out[i] <- touch[as.character(run)]
     }
     out
}


#' Limit imputed allocations to the gap left by the WHO account
#'
#' An AI \code{fourier_*} row spreads an annual (or other multi-week) total over
#' the weeks of its span, and higher-priority sources then overwrite some of those
#' weeks without the overwritten share being netted out, so the imputed rows that
#' survive can duplicate cases the observed weeks already report, or spread a
#' total from another source that the WHO account contradicts. Per country and
#' year (the ISO year of each week, i.e. the calendar year of its Thursday) with a
#' WHO account \eqn{A}, observed plus reconstructed cases \eqn{O} and imputed cases
#' \eqn{I}, the imputed rows may only fill the gap to the account: they keep
#' \eqn{\min(I, \max(0, A - O))} cases, scaled by the same factor, cases and
#' deaths alike. When less than one case remains they are emptied (NA); so is any
#' rescaled row left with less than half a case, which the integer daily downscale
#' would otherwise turn into a zero-case week that keeps its weight. The account
#' is the WHO annual total; for a country-year with no AFRO annual row (Somalia,
#' an EMRO country, or a country the AFRO annual layer does not list) it is the
#' year-to-date total of the WHO weekly rows when that is positive -- the AFRO
#' annual rows for 2023 onward are sums of the same dashboard -- and otherwise the
#' imputed rows are not reconciled (zero WHO weeks cannot tell no cases from no
#' report). A year-to-date total, or in the current (latest, incomplete) year an
#' annual total equal to the year's WHO weekly sum (the AFRO annual rows from 2023
#' are sums of the dashboard), accounts only for the weeks up to WHO's last report
#' of the year, so it reconciles the imputed rows of those weeks, and of
#' any imputed AI run that touches a WHO-reported week (a re-spread of the same
#' period), against the observed cases of those weeks; an imputed run that starts
#' after WHO's last report is left as it is. AI \code{inferred_zero} weeks report
#' no cases, so the account never scales or empties them.
#'
#' @param dedup Selected rows (one per country-week) with \code{.tier} and, when
#'   the combiner supplies it, \code{.run_overlaps_who}
#'   (\code{.imputed_runs_touching_who()}).
#' @param annual Output of \code{.read_who_annual_account()} (or NULL).
#' @param who_weekly Output of \code{.who_weekly_year_totals()} (or NULL).
#' @return list(data = dedup after the rule, log = list of adjustment-log frames).
#' @noRd
.cap_imputed_to_annual_account <- function(dedup, annual, who_weekly = NULL) {
     log <- list()
     yr  <- as.integer(format(as.Date(dedup$date_start) + 3L, "%Y"))
     key <- paste(dedup$iso_code, yr)
     # inferred zeros report no cases, so the account neither scales nor empties them
     # (a fourier row of zero is still an allocation and is reconciled as before)
     imp <- dedup$.tier == 3L & !is.na(dedup$cases) & !(dedup$disaggregation_method %in% "inferred_zero")
     touch <- if (is.null(dedup$.run_overlaps_who)) rep(FALSE, nrow(dedup)) else dedup$.run_overlaps_who %in% TRUE
     acc <- data.frame(key = character(0), total = numeric(0), basis = character(0),
                       last = as.Date(character(0)))
     if (!is.null(annual)) {
          an <- data.frame(key = paste(annual$iso_code, annual$year),
                           total = annual$cases_total, basis = "WHO annual total", last = as.Date(NA))
          # In the current (latest, incomplete) year an annual total equal to the
          # WHO weekly sum is that provisional sum (the AFRO annual rows from 2023 are
          # sums of the dashboard: Zimbabwe 2026's 36 is its three weekly rows), so it
          # accounts only for WHO's reported weeks. A completed year's annual total is
          # the official count for the whole year.
          if (!is.null(who_weekly)) {
               # the latest year either source covers (an annual row for a later year
               # means the year is closed)
               cur_year <- max(as.integer(format(max(as.Date(who_weekly$last_week)) + 3L, "%Y")),
                               max(annual$year))
               wm <- match(an$key, paste(who_weekly$iso_code, who_weekly$year))
               dash <- !is.na(wm) & annual$year >= cur_year &
                    abs(an$total - who_weekly$cases_total[wm]) <= .ANNUAL_EQUALS_WEEKLY_TOLERANCE
               an$last[dash]  <- as.Date(who_weekly$last_week[wm[dash]])
               an$basis[dash] <- "WHO annual total (= the WHO weekly rows)"
          }
          acc <- rbind(acc, an)
     }
     if (!is.null(who_weekly)) {
          fb <- data.frame(key = paste(who_weekly$iso_code, who_weekly$year),
                           total = who_weekly$cases_total,
                           basis = "WHO weekly year-to-date total (no AFRO annual row)",
                           last = as.Date(who_weekly$last_week))
          acc <- rbind(acc, fb[!fb$key %in% acc$key, ])
     }
     empty_cols <- intersect(c("cases", "deaths", "source", "source_deaths", "note",
                               "confidence_weight", "disaggregation_method"), names(dedup))
     for (k in intersect(unique(key[imp]), acc$key)) {
          rows <- which(key == k)
          last <- acc$last[match(k, acc$key)]
          if (!is.na(last))  # a sum of WHO's weeks: their span, and runs that touch it
               rows <- rows[as.Date(dedup$date_start[rows]) <= last | (imp[rows] & touch[rows])]
          ir   <- rows[imp[rows]]
          if (length(ir) == 0L) next
          a    <- acc$total[match(k, acc$key)]
          obs  <- sum(dedup$cases[rows][dedup$.tier[rows] <= 2L], na.rm = TRUE)
          tot_imp <- sum(dedup$cases[ir])
          keep <- min(tot_imp, max(0, a - obs))
          if (tot_imp - keep < .IMPUTED_ACCOUNT_TOLERANCE) next
          detail <- sprintf("%s %s; observed weeks %s; imputed %s -> %s",
                            acc$basis[match(k, acc$key)], format(a), format(round(obs, 1)),
                            format(round(tot_imp, 1)), format(round(keep, 1)))
          if (keep < 1) {
               log[[length(log) + 1L]] <- .surveillance_adjustment_log(
                    dedup[ir, ], "imputed_dropped_annual_accounted", detail = detail)
               for (cc in empty_cols) dedup[[cc]][ir] <- NA
               next
          }
          f <- keep / tot_imp
          new_cases  <- dedup$cases[ir] * f
          new_deaths <- dedup$deaths[ir] * f
          residue <- new_cases < .IMPUTED_RESIDUE_MIN_CASES
          if (any(!residue))
               log[[length(log) + 1L]] <- .surveillance_adjustment_log(
                    dedup[ir[!residue], ], "imputed_scaled_annual_residual",
                    cases_after = new_cases[!residue], deaths_after = new_deaths[!residue],
                    detail = detail)
          if (any(residue))
               log[[length(log) + 1L]] <- .surveillance_adjustment_log(
                    dedup[ir[residue], ], "imputed_residue_dropped",
                    detail = sprintf("%s; rescaled to %s case(s), under half a case: emptied",
                                     detail, formatC(new_cases[residue], format = "g", digits = 3)))
          dedup$cases[ir]  <- new_cases
          dedup$deaths[ir] <- new_deaths
          for (cc in empty_cols) dedup[[cc]][ir[residue]] <- NA
     }
     list(data = dedup, log = log)
}
