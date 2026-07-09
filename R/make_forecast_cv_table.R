#' Publication table for rolling-origin forecast cross-validation
#'
#' Turns the scored cells from \code{\link{evaluate_rolling_cv}} into the
#' publication table companion to \code{\link{plot_forecast_cv_grid}}: a per
#' country \eqn{\times} cutoff \eqn{\times} horizon detail table plus per-country
#' and pooled summary rows at the primary horizon.
#'
#' The headline skill is WIS-skill vs the seasonal-climatology baseline
#' (\eqn{1 - WIS_{model}/WIS_{clim}}); bias-ratio is a secondary cumulative-bias
#' check. NGA is flagged exploratory (surveillance data-quality confound) and
#' excluded from the pooled summary; deaths are flagged conditional near-casts
#' (the 2-week embargo is shorter than the infection-to-reported-death dwell).
#'
#' @param cells Scored cells: \code{evaluate_rolling_cv(...)$cells}, or a path to
#'   a \code{scores_cells.parquet}/\code{.csv}. Must carry \code{iso_code,
#'   cutoff_date, metric, model, window} and skill/bias columns.
#' @param model Model to tabulate (default \code{"ensemble_opt"}).
#' @param metrics Metrics to include (default \code{c("cases","deaths")}).
#' @param horizons_months Horizons to include as detail rows (default
#'   \code{c(1,2,3)}); matched against \code{window == "OOS<=Nmo"}.
#' @param primary_horizon Horizon used for the summary rows (default 3).
#' @param exploratory_isos Isos flagged exploratory + dropped from the pooled
#'   summary (default \code{"NGA"}).
#' @param near_cast_metrics Metrics flagged conditional near-casts (default
#'   \code{"deaths"}).
#' @param train_start Anchor for the reported training-window length (default
#'   \code{2018-01-01}, the config window start).
#' @param dir_output If non-NULL, write \code{<file_prefix>_detail} and
#'   \code{_summary} as parquet (if \pkg{arrow}) else CSV.
#' @param file_prefix Output stem (default \code{"forecast_cv_table"}).
#'
#' @return A list with \code{$detail} (per country x cutoff x horizon x metric)
#'   and \code{$summary} (per country + pooled, at \code{primary_horizon}).
#' @seealso \code{\link{plot_forecast_cv_grid}}, \code{\link{evaluate_rolling_cv}}
#' @importFrom stats median
#' @export
make_forecast_cv_table <- function(cells,
                                   model = "ensemble_opt",
                                   metrics = c("cases", "deaths"),
                                   horizons_months = c(1, 2, 3),
                                   primary_horizon = 3,
                                   exploratory_isos = "NGA",
                                   near_cast_metrics = "deaths",
                                   train_start = as.Date("2018-01-01"),
                                   dir_output = NULL,
                                   file_prefix = "forecast_cv_table") {

     if (is.character(cells) && length(cells) == 1L) {
          if (grepl("\\.parquet$", cells)) {
               if (!requireNamespace("arrow", quietly = TRUE)) stop("Reading parquet needs 'arrow'.")
               cells <- as.data.frame(arrow::read_parquet(cells))
          } else cells <- utils::read.csv(cells, stringsAsFactors = FALSE)
     }
     cells <- as.data.frame(cells)
     req <- c("iso_code", "cutoff_date", "metric", "model", "window")
     miss <- setdiff(req, names(cells))
     if (length(miss)) stop("cells missing column(s): ", paste(miss, collapse = ", "))

     # numeric columns to carry (only those present)
     want <- c(wis_skill = "wis_skill_seasonal", mae_skill = "mae_skill_seasonal",
               R2_corr = "R2_corr", R2_sse = "R2_sse", bias_ratio = "bias_ratio",
               cov50 = "cov50", cov95 = "cov95", ess_ok = "ess_ok", n = "n")
     want <- want[want %in% names(cells)]

     windows <- paste0("OOS<=", horizons_months, "mo")
     d <- cells[cells$model == model & cells$metric %in% metrics & cells$window %in% windows, , drop = FALSE]
     if (!nrow(d)) stop("No scored cells for model='", model, "' at the requested horizons.")
     d$cutoff_date <- as.Date(d$cutoff_date)
     d$horizon_mo  <- as.numeric(sub("OOS<=(\\d+)mo", "\\1", d$window))

     detail <- data.frame(
          country      = d$iso_code,
          cutoff       = d$cutoff_date,
          horizon_mo   = d$horizon_mo,
          metric       = d$metric,
          train_years  = round(as.numeric(d$cutoff_date - as.Date(train_start)) / 365.25, 2),
          exploratory  = d$iso_code %in% exploratory_isos,
          near_cast    = d$metric %in% near_cast_metrics,
          stringsAsFactors = FALSE)
     for (nm in names(want)) detail[[nm]] <- d[[want[[nm]]]]
     detail <- detail[order(detail$metric, detail$country, detail$cutoff, detail$horizon_mo), ]

     # ---- summary rows at the primary horizon ----
     sub <- detail[detail$horizon_mo == primary_horizon, , drop = FALSE]
     agg_rows <- function(df, scope, country) {
          out <- data.frame(scope = scope, country = country, metric = df$metric[1],
                            horizon_mo = primary_horizon, n_origins = nrow(df),
                            stringsAsFactors = FALSE)
          if ("wis_skill" %in% names(df)) {
               out$wis_skill_median <- stats::median(df$wis_skill, na.rm = TRUE)
               out$wins_vs_clim <- sum(df$wis_skill > 0, na.rm = TRUE)
          }
          if ("mae_skill" %in% names(df)) out$mae_skill_median <- stats::median(df$mae_skill, na.rm = TRUE)
          if ("R2_corr"  %in% names(df)) out$R2_corr_median  <- stats::median(df$R2_corr,  na.rm = TRUE)
          if ("bias_ratio" %in% names(df)) out$bias_ratio_median <- stats::median(df$bias_ratio, na.rm = TRUE)
          if ("cov95" %in% names(df)) out$cov95_mean <- mean(df$cov95, na.rm = TRUE)
          if ("ess_ok" %in% names(df)) out$ess_ok_frac <- mean(as.logical(df$ess_ok), na.rm = TRUE)
          out
     }
     summ <- list()
     for (m in metrics) {
          sm <- sub[sub$metric == m, , drop = FALSE]
          for (cty in sort(unique(sm$country)))
               summ[[paste(m, cty)]] <- agg_rows(sm[sm$country == cty, ], "country", cty)
          pooled <- sm[!sm$exploratory, , drop = FALSE]        # NGA excluded from pooled
          if (nrow(pooled)) summ[[paste(m, "POOLED")]] <- agg_rows(pooled, "pooled", "POOLED(excl. exploratory)")
     }
     summary_tbl <- do.call(rbind, c(summ, list(make.row.names = FALSE)))

     if (!is.null(dir_output)) {
          dir.create(dir_output, recursive = TRUE, showWarnings = FALSE)
          wr <- function(df, tag) {
               if (requireNamespace("arrow", quietly = TRUE))
                    arrow::write_parquet(df, file.path(dir_output, sprintf("%s_%s.parquet", file_prefix, tag)))
               else utils::write.csv(df, file.path(dir_output, sprintf("%s_%s.csv", file_prefix, tag)), row.names = FALSE)
          }
          wr(detail, "detail"); wr(summary_tbl, "summary")
     }
     list(detail = detail, summary = summary_tbl)
}
