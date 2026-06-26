#' Plot comprehensive model trajectories for one location
#'
#' Renders the multi-panel "Model trajectories" figure for a single location from
#' a persisted \code{mosaic_trajectories} artifact (written by
#' \code{run_MOSAIC()} when \code{control$predictions$capture_trajectories =
#' TRUE}). Each panel shows the comprehensive internal-state channels over time —
#' surveillance fit (reported cases/deaths), burden, incidence drivers, force of
#' infection, the SVEIR + W compartments, and sanity checks (mass balance,
#' population, epidemic fraction) — with a uniform-thinned set of \strong{actual}
#' posterior member trajectories (the spaghetti, conveying spread) and the
#' weighted \strong{median} overlaid in bold. Observed surveillance points are
#' overlaid only on the \code{reported_cases}/\code{reported_deaths} panels.
#'
#' \strong{Pure read-render:} this function never imports LASER, never
#' re-simulates, and never re-weights — it consumes only the compact artifact
#' (per-channel weighted median + thinned actual lines, both already reduced over
#' the best subset). It is the trajectory analogue of
#' \code{\link{plot_model_ensemble}}.
#'
#' \strong{Central line:} for \code{reported_cases} / \code{reported_deaths} the
#' bold line is the ensemble \emph{central} series following
#' \code{control$predictions$central_method} (weighted median or weighted mean,
#' per channel) and is \strong{bit-identical} to the cases/deaths \emph{prediction}
#' plots -- it is reduced from the same captured draws over the same final
#' displayed member set and weights (no re-simulation). All other channels
#' (compartments, FOI, incidence, derived) have no prediction-plot counterpart and
#' use the conventional weighted \emph{median}.
#'
#' \strong{Weighting note:} the central series and lines are reduced over the
#' \emph{final displayed} member set and weights: the \emph{candidate} best subset
#' (\code{is_best_subset} / \code{weight_best}) when subset optimization is off,
#' and the \emph{optimized} subset (\code{is_best_subset_opt} /
#' \code{weight_best_opt}) when \code{control$predictions$optimize_subset = TRUE}.
#' Channels are captured at sim time (stream-to-disk) and reduced over the
#' optimized members with NO re-simulation. The CFR(t) panel carries dashed
#' endemic/epidemic regime reference lines when \code{trajectories$cfr_refs} is
#' present.
#'
#' @param trajectories A \code{mosaic_trajectories} object (from
#'   \code{readRDS("2_calibration/trajectories_ensemble.rds")}).
#' @param location Character. ISO/location code to plot; must appear in
#'   \code{trajectories$location_names}.
#' @param output_dir Directory to write the figures into (created if needed).
#' @param prefix File prefix. Default \code{"trajectories"} — outputs
#'   \code{<prefix>_<location>.pdf} and \code{<prefix>_<location>_p1.png}.
#' @param verbose Logical. Print progress. Default \code{TRUE}.
#'
#' @return Invisibly, a named list of the file paths written.
#' @seealso \code{\link{render_MOSAIC_figures}}, \code{\link{plot_model_ensemble}}.
#' @export
plot_model_trajectories <- function(trajectories,
                                    location,
                                    output_dir,
                                    prefix  = "trajectories",
                                    verbose = TRUE) {

  if (!inherits(trajectories, "mosaic_trajectories"))
    stop("trajectories must be a 'mosaic_trajectories' object")
  if (missing(location) || length(location) != 1L || !nzchar(location))
    stop("location must be a single non-empty string")
  loc_names <- trajectories$location_names
  if (!location %in% loc_names)
    stop("location '", location, "' not found; available: ",
         paste(loc_names, collapse = ", "))
  if (missing(output_dir) || is.null(output_dir) || !nzchar(output_dir))
    stop("output_dir is required")
  if (!dir.exists(output_dir))
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  loc_idx <- match(location, loc_names)
  n_time  <- trajectories$n_time_points
  d0      <- tryCatch(as.Date(trajectories$date_start), error = function(e) NA)
  dates   <- if (!is.na(d0)) d0 + (seq_len(n_time) - 1L) else
    as.Date(NA) + seq_len(n_time)

  # --- Panel spec (display order + labels + group). Mirrors the production
  # figure; `incidence` is the S->E new-infection flow (DM F1), Lambda/Psi are
  # per-capita per-day hazards (DM F2). Built with base data.frame -- no tibble.
  spec <- data.frame(
    channel = c("reported_cases", "reported_deaths", "disease_deaths", "expected_cases",
                "new_symptomatic", "incidence", "incidence_human", "incidence_env",
                "Lambda", "Psi", "beta_jt_human", "beta_jt_env", "CFR",
                "I_total", "E", "W", "V2", "V1", "R", "S",
                "mass_balance", "N", "epidemic_frac"),
    label = c("Reported cases (model + observed)",
              "Reported deaths (model + observed)",
              "Disease deaths (model, true burden)",
              "Expected cases (burden back-calculation)",
              "New symptomatic infections (E->I flow)",
              "New infections (S->E flow)",
              "Human-driven new infections",
              "Environmentally-driven new infections",
              "Human force of infection Lambda(t) [per-capita/day]",
              "Environmental force of infection Psi(t) [per-capita/day]",
              "Per-time human transmission rate beta_human(t)",
              "Per-time environmental transmission rate beta_env(t)",
              "Reported CFR(t) = reported deaths / reported cases (28d roll)",
              "Infectious (Isym + Iasym)", "Exposed (E)",
              "Environmental reservoir W(t)", "Two-dose vaccinated (V2)",
              "One-dose vaccinated (V1)", "Recovered (R)", "Susceptible (S)",
              "Mass balance: (S+E+I+R+V1+V2) / N", "Population (N)",
              "Epidemic fraction (ensemble share over threshold)"),
    grp = c(rep("surveillance", 4L), rep("incidence", 4L),
            rep("foi", 5L), rep("state", 7L), rep("check", 3L)),
    stringsAsFactors = FALSE)

  # Keep only panels actually present in the artifact.
  have_median <- names(trajectories$summary)
  spec <- spec[spec$channel %in% have_median, , drop = FALSE]
  if (nrow(spec) == 0L)
    stop("trajectories artifact has no plottable channels for ", location)
  spec$label <- factor(spec$label, levels = spec$label)

  grp_col <- c(surveillance = "#1f4e79", incidence = "#c0504d",
               foi = "#2e8b57", state = "#7030a0", check = "#555555")

  # --- Weighted-median lines (one row per channel x t) ------------------------
  med_parts <- lapply(seq_len(nrow(spec)), function(k) {
    ch <- spec$channel[k]
    m  <- trajectories$summary[[ch]]$median
    if (is.null(m)) return(NULL)
    v  <- as.numeric(m[loc_idx, ])
    data.frame(label = spec$label[k], date = dates, value = v,
               stringsAsFactors = FALSE)
  })
  med <- do.call(rbind, Filter(Negate(is.null), med_parts))
  med <- med[is.finite(med$value), , drop = FALSE]

  # --- Spaghetti (actual member trajectories) for this location --------------
  ln <- trajectories$lines
  ln <- ln[ln$location == location & ln$channel %in% spec$channel, , drop = FALSE]
  if (nrow(ln) > 0L) {
    ln$label <- factor(spec$label[match(ln$channel, spec$channel)],
                       levels = levels(spec$label))
    ln$date  <- dates[ln$t]
    ln$col   <- grp_col[spec$grp[match(ln$channel, spec$channel)]]
    ln <- ln[is.finite(ln$value), , drop = FALSE]
  }

  # --- Observed overlay (reported channels only; DM field-semantics guardrail).
  obs_specs <- list(
    list(ch = "reported_cases",  mat = trajectories$obs_cases),
    list(ch = "reported_deaths", mat = trajectories$obs_deaths))
  obs_parts <- lapply(obs_specs, function(o) {
    if (is.null(o$mat) || !(o$ch %in% spec$channel)) return(NULL)
    m <- if (is.matrix(o$mat)) o$mat else matrix(o$mat, nrow = length(loc_names))
    if (loc_idx > nrow(m)) return(NULL)
    data.frame(label = spec$label[spec$channel == o$ch][1],
               date = dates, value = as.numeric(m[loc_idx, ]),
               stringsAsFactors = FALSE)
  })
  obs <- do.call(rbind, Filter(Negate(is.null), obs_parts))
  if (!is.null(obs)) {
    obs <- obs[is.finite(obs$value) & obs$value >= 0, , drop = FALSE]
    if (nrow(obs)) obs$label <- factor(obs$label, levels = levels(spec$label))
  }

  # --- Dashed endemic/epidemic CFR regime reference lines on the CFR panel
  # (DM F4). Weighted over the same best subset; drawn only when present.
  ref_df  <- NULL
  cfr_lab <- spec$label[spec$channel == "CFR"]
  refs    <- trajectories$cfr_refs
  if (length(cfr_lab) == 1L && !is.null(refs) && is.data.frame(refs)) {
    row <- refs[refs$location == location, , drop = FALSE]
    if (nrow(row) == 1L) {
      vals <- c(endemic = row$cfr_baseline[1], epidemic = row$cfr_epidemic[1])
      vals <- vals[is.finite(vals)]
      if (length(vals))
        ref_df <- data.frame(
          label  = factor(cfr_lab, levels = levels(spec$label)),
          yint   = as.numeric(vals),
          regime = names(vals),
          col    = ifelse(names(vals) == "endemic", "#2e8b57", "#c0504d"),
          stringsAsFactors = FALSE)
    }
  }

  # --- Assemble ---------------------------------------------------------------
  p <- ggplot2::ggplot()
  if (nrow(ln) > 0L) {
    p <- p + ggplot2::geom_line(
      data = ln,
      ggplot2::aes(x = .data$date, y = .data$value,
                   group = .data$member_id, color = .data$col,
                   alpha = .data$weight),
      linewidth = 0.18) +
      ggplot2::scale_alpha(range = c(0.05, 0.5), guide = "none")
  }
  if (!is.null(ref_df)) {
    p <- p +
      ggplot2::geom_hline(data = ref_df,
                          ggplot2::aes(yintercept = .data$yint, color = .data$col),
                          linetype = "dashed", linewidth = 0.4) +
      ggplot2::geom_text(data = ref_df,
                         ggplot2::aes(x = min(med$date), y = .data$yint,
                                      label = .data$regime, color = .data$col),
                         hjust = 0, vjust = -0.3, size = 2, show.legend = FALSE)
  }
  # Identity colour scale needed whenever a layer maps the hex `col` aesthetic
  # (spaghetti and/or CFR refs).
  if (nrow(ln) > 0L || !is.null(ref_df))
    p <- p + ggplot2::scale_color_identity()
  p <- p +
    ggplot2::geom_line(data = med,
                       ggplot2::aes(x = .data$date, y = .data$value),
                       color = "black", linewidth = 0.7)
  if (!is.null(obs) && nrow(obs) > 0L) {
    p <- p + ggplot2::geom_point(data = obs,
                                 ggplot2::aes(x = .data$date, y = .data$value),
                                 color = "black", size = 0.45)
  }
  p <- p +
    ggplot2::facet_wrap(~label, ncol = 1L, scales = "free_y",
                        strip.position = "top") +
    ggplot2::labs(
      x = NULL, y = NULL,
      title = sprintf("Model trajectories - %s", location),
      subtitle = sprintf(paste0("%d best-subset members x %d stochastic reruns ",
                                 "(bold = weighted central line: reported_* follow ",
                                 "central_method & match the prediction plots, other ",
                                 "channels weighted median; thinned actual trajectories; ",
                                 "observed dots on reported panels)."),
                         trajectories$n_param_sets %||% NA,
                         trajectories$n_simulations_per_config %||% NA),
      caption = sprintf(paste0("CFR(t) is a %d-day trailing rolling ratio (window = ",
                               "min(28, series length)); its leading window is undefined, ",
                               "so on short series that panel is sparse by construction."),
                        min(28L, trajectories$n_time_points %||% 28L))) +
    MOSAIC::theme_mosaic(base_size = 9) +
    ggplot2::theme(
      strip.text   = ggplot2::element_text(size = 7, hjust = 0, face = "bold"),
      axis.text    = ggplot2::element_text(size = 6),
      panel.spacing = ggplot2::unit(1.5, "pt"),
      plot.title   = ggplot2::element_text(face = "bold", size = 13),
      plot.subtitle = ggplot2::element_text(size = 8, color = "#555555"),
      plot.caption  = ggplot2::element_text(size = 6, color = "#888888"))

  page_h <- max(6, 1.15 * nlevels(spec$label) + 1.5)
  pdf_path <- file.path(output_dir, sprintf("%s_%s.pdf", prefix, location))
  png_path <- file.path(output_dir, sprintf("%s_%s_p1.png", prefix, location))
  ggplot2::ggsave(pdf_path, p, width = 8.5, height = page_h, limitsize = FALSE)
  ggplot2::ggsave(png_path, p, width = 8.5, height = page_h, dpi = 110,
                  limitsize = FALSE)
  if (verbose) {
    message("Wrote ", pdf_path)
    message("Wrote ", png_path)
  }
  invisible(list(pdf = pdf_path, png = png_path))
}
