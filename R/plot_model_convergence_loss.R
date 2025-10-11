plot_model_convergence_loss <- function(results_dir,
                                        plots_dir = NULL,
                                        verbose = TRUE) {

     # --- I/O checks -------------------------------------------------------------
     if (!dir.exists(results_dir)) stop("results_dir does not exist: ", results_dir)
     if (is.null(plots_dir)) plots_dir <- file.path(dirname(results_dir), "plots")
     if (!dir.exists(plots_dir)) {
          dir.create(plots_dir, recursive = TRUE)
          if (verbose) message("Created plots directory: ", plots_dir)
     }

     results_sims_file     <- file.path(results_dir, "simulations.parquet")
     results_sims_best_file     <- file.path(results_dir, "best.parquet")
     results_file     <- file.path(results_dir, "convergence_results_loss.parquet")
     diagnostics_file <- file.path(results_dir, "convergence_diagnostics_loss.json")
     if (!file.exists(results_file))     stop("Required file not found: ", results_file)
     if (!file.exists(diagnostics_file)) stop("Required file not found: ", diagnostics_file)
     if (verbose) message("Reading convergence files from: ", results_dir)

     # --- Read data --------------------------------------------------------------

     results_sims <- arrow::read_parquet(results_sims_file)
     results_sims_best <- arrow::read_parquet(results_sims_best_file)
     results_data <- arrow::read_parquet(results_file)
     diagnostics  <- jsonlite::read_json(diagnostics_file)

     loss   <- results_data$loss
     delta  <- results_data$delta_loss
     seeds  <- results_data$seed
     n_draws <- nrow(results_data)
     n_successful <- sum(is.finite(loss) & !is.na(loss) & !is.nan(loss))

     # Fallback: compute in_top_pct if missing (older files)
     if (!("in_top_pct" %in% names(results_data))) {
          top_pct_val <- tryCatch(as.numeric(diagnostics$settings$top_pct), error = function(e) NA_real_)
          if (!is.finite(top_pct_val)) top_pct_val <- 0.05
          sel <- select_top_pct_by_loss(loss[is.finite(loss)], top_pct = top_pct_val)
          # Build logical vector across all rows (NA loss -> FALSE)
          in_top_pct <- rep(FALSE, n_draws)
          fin_idx <- which(is.finite(loss))
          in_top_pct[fin_idx[sel$idx]] <- TRUE
     } else {
          in_top_pct <- results_data$in_top_pct
     }

     # --- Safe extractors --------------------------------------------------------
     safe_numeric <- function(x, default = NA_real_) {
          out <- suppressWarnings(as.numeric(x)); ifelse(is.na(out), default, out)
     }
     safe_character <- function(x, default = "unknown") {
          out <- tryCatch(as.character(x), error = function(e) default)
          ifelse(is.na(out), default, out)
     }

     # Metrics (new names)
     metrics <- c(
          ESS_all        = safe_numeric(diagnostics$metrics$ess_all$value),
          A_B            = safe_numeric(diagnostics$metrics$A_B$value),
          CVw_B          = safe_numeric(diagnostics$metrics$cvw_B$value),
          B_size         = safe_numeric(diagnostics$metrics$B_size$value),
          loss_threshold = safe_numeric(diagnostics$metrics$loss_threshold$value)
     )

     # Targets (new names)
     targets <- c(
          ESS_min = safe_numeric(diagnostics$targets$ess_min$value),
          A_min   = safe_numeric(diagnostics$targets$A_min$value),
          CVw_max = safe_numeric(diagnostics$targets$cvw_max$value),
          B_min   = safe_numeric(diagnostics$targets$B_min$value)
     )

     # Status (new names)
     status <- c(
          ESS_all = safe_character(diagnostics$metrics$ess_all$status),
          A_B     = safe_character(diagnostics$metrics$A_B$status),
          CVw_B   = safe_character(diagnostics$metrics$cvw_B$status),
          B_size  = safe_character(diagnostics$metrics$B_size$status)
     )

     # Settings
     top_pct_lab <- tryCatch(as.numeric(diagnostics$settings$top_pct), error = function(e) NA_real_)
     if (!is.finite(top_pct_lab)) top_pct_lab <- 0.05

     # --- Prepare plotting DF ----------------------------------------------------
     sort_idx <- order(loss, decreasing = FALSE, na.last = TRUE)
     plot_data <- data.frame(
          rank       = seq_len(n_draws),
          loss       = loss[sort_idx],
          delta      = delta[sort_idx],
          in_top_pct = in_top_pct[sort_idx],
          seed       = seeds[sort_idx]
     )

     # Best = minimal loss (rank 1 after ascending sort)
     best_idx  <- which(plot_data$rank == 1L)
     best_loss <- plot_data$loss[best_idx]
     best_seed <- plot_data$seed[best_idx]

     # --- Diagnostics text -------------------------------------------------------
     safe_sprintf <- function(fmt, ...) {
          args <- lapply(list(...), function(x) if (is.na(x)) "NA" else x)
          tryCatch(do.call(sprintf, c(list(fmt), args)), error = function(e) paste("Error formatting:", fmt))
     }

     diagnostic_lines <- c(
          sprintf("Finite-loss: %d/%d", n_successful, n_draws),
          safe_sprintf("ESS (all): %.0f [target ≥ %.0f]", metrics["ESS_all"], targets["ESS_min"]),
          "",
          sprintf("Best subset (B): top %.1f%%", 100*top_pct_lab),
          sprintf("B = %s [target ≥ 50]", format(metrics["B_size"])),
          safe_sprintf("  A = %.3f [target ≥ %.3f]", metrics["A_B"],   targets["A_min"]),
          safe_sprintf("  CVw = %.3f [target ≤ %.3f]", metrics["CVw_B"], targets["CVw_max"]),
          if (is.finite(metrics["loss_threshold"])) sprintf("  Loss threshold: ≤ %.4f", metrics["loss_threshold"]) else NULL
     )

     # --- Footnote ---------------------------------------------------------------
     footnote_lines <- c(
          "ESS (all): Effective sample size over all finite-loss sims.",
          sprintf("B: Best subset = lowest %.1f%% by loss; A and CVw computed within B.", 100*top_pct_lab),
          "A: Agreement index (entropy-based, 0–1) within B.",
          "CVw: Coefficient of variation of weights within B."
     )
     footnote_text <- paste(footnote_lines, collapse = "\n")

     # --- Plot -------------------------------------------------------------------
     title_text <- sprintf("Loss Across %s/%s Simulations | Best: Simulation %s",
                           n_draws, n_successful, best_seed)

     p <-
          ggplot2::ggplot(plot_data, ggplot2::aes(x = rank, y = loss)) +
          # Best-loss line
          ggplot2::geom_hline(yintercept = best_loss,
                              linetype = "dashed", color = "#ba181b", alpha = 0.5, linewidth = 0.5) +
          # Line & points
          ggplot2::geom_line(color = "gray70", linewidth = 0.8, alpha = 0.7) +
          # Non-top subset (gray) vs top subset (black)
          ggplot2::geom_point(data = subset(plot_data, !in_top_pct),
                              color = "gray60", size = 1.2, alpha = 0.6) +
          ggplot2::geom_point(data = subset(plot_data, in_top_pct),
                              color = "black", size = 2) +
          # Best-loss point & label
          ggplot2::geom_point(data = plot_data[best_idx, ],
                              color = "black", fill = "#ba181b", size = 3.5, shape = 21) +
          ggplot2::annotate("text",
                            x = best_idx,
                            y = best_loss - (max(plot_data$loss, na.rm = TRUE) - min(plot_data$loss, na.rm = TRUE)) * 0.03,
                            label = sprintf("%.2f", best_loss),
                            hjust = 0.5, vjust = 1, size = 3.5, color = "#ba181b", fontface = "bold") +
          # Diagnostics in top-right (robust to reversed x)
          ggplot2::annotate("text",
                            x = n_draws*0.05, y = max(plot_data$loss)*0.95,
                            label = paste(diagnostic_lines, collapse = "\n"),
                            hjust = 1, vjust = 1, size = 3.8, color = "black",
                            lineheight = 0.9, family = "sans") +
          # Reverse x and pad a bit
          ggplot2::scale_x_reverse(expand = ggplot2::expansion(mult = c(0.02, 0.02))) +
          ggplot2::coord_cartesian(clip = "off") +
          ggplot2::theme_classic(base_size = 12) +
          ggplot2::theme(
               panel.grid.minor = ggplot2::element_blank(),
               panel.grid.major = ggplot2::element_blank(),
               plot.title   = ggplot2::element_text(face = "bold", size = 12, hjust = 0.5),
               plot.caption = ggplot2::element_text(size = 8, hjust = 0.5, color = "gray40",
                                                    margin = ggplot2::margin(t = 10)),
               axis.title.x = ggplot2::element_text(size = 10, margin = ggplot2::margin(t = 10)),
               axis.title.y = ggplot2::element_text(size = 10, margin = ggplot2::margin(r = 10)),
               plot.margin  = ggplot2::margin(15, 15, 20, 15)
          ) +
          ggplot2::labs(
               x = "Run index (sorted by loss value)",
               y = "Model Loss Function",
               title = title_text,
               caption = footnote_text
          )




     # === Extra safety: check columns for the scatter plots =======================
     needed_cases <- c("cor_cases", "msle_cases")
     needed_deaths <- c("cor_deaths", "msle_deaths")
     if (!all(needed_cases %in% names(results_sims))) {
          stop("results_sims is missing columns for cases scatter: ",
               paste(setdiff(needed_cases, names(results_sims)), collapse = ", "))
     }
     if (!all(needed_deaths %in% names(results_sims))) {
          stop("results_sims is missing columns for deaths scatter: ",
               paste(setdiff(needed_deaths, names(results_sims)), collapse = ", "))
     }
     if (!all(needed_cases %in% names(results_sims_best))) {
          stop("results_sims_best is missing columns for cases scatter: ",
               paste(setdiff(needed_cases, names(results_sims_best)), collapse = ", "))
     }
     if (!all(needed_deaths %in% names(results_sims_best))) {
          stop("results_sims_best is missing columns for deaths scatter: ",
               paste(setdiff(needed_deaths, names(results_sims_best)), collapse = ", "))
     }

     # === Helper: consistent clean theme =========================================
     .mosaic_panel_theme <- ggplot2::theme_classic(base_size = 12) +
          ggplot2::theme(
               panel.grid.minor = ggplot2::element_blank(),
               panel.grid.major = ggplot2::element_blank(),
               plot.title   = ggplot2::element_text(face = "bold", size = 11, hjust = 0.5),
               axis.title.x = ggplot2::element_text(size = 10, margin = ggplot2::margin(t = 8)),
               axis.title.y = ggplot2::element_text(size = 10, margin = ggplot2::margin(r = 8))
          )

     # === Professional ggplots for cor vs MSLE (cases & deaths) ==================
     # === Professional ggplots for cor vs MSLE (cases & deaths) ==================
     # Notes:
     # - MSLE axis reversed so lower (better) is toward the top.
     # - Contours (stat_density_2d) drawn over all simulations.
     # - Best model highlighted with a filled green point with thin black stroke.

     p_cases <- ggplot2::ggplot(results_sims, ggplot2::aes(x = cor_cases, y = msle_cases)) +
          ggplot2::geom_point(color = "gray40", alpha = 0.3, size = 1.5, na.rm = TRUE) +
          ggplot2::stat_density_2d(
               geom = "density_2d", bins = 8, color = "black", linewidth = 0.4, na.rm = TRUE
          ) +
          ggplot2::geom_point(
               data  = results_sims_best,
               ggplot2::aes(x = cor_cases, y = msle_cases),
               shape = 21, fill = "#ba181b", color = "black", size = 3.5, stroke = 0.3, na.rm = TRUE
          ) +
          ggplot2::scale_x_continuous(limits = c(0, max(c(results_sims$cor_cases, results_sims$cor_deaths))),
                                      expand = ggplot2::expansion(mult = c(0.02, 0.02))) +
          ggplot2::scale_y_reverse(expand = ggplot2::expansion(mult = c(0.02, 0.05))) +
          ggplot2::labs(
               title = "Cases: Correlation vs MSLE",
               x = "Correlation (higher is better)",
               y = "MSLE (lower is better; axis reversed)"
          ) +
          .mosaic_panel_theme

     p_deaths <- ggplot2::ggplot(results_sims, ggplot2::aes(x = cor_deaths, y = msle_deaths)) +
          ggplot2::geom_point(color = "gray40", alpha = 0.3, size = 1.5, na.rm = TRUE) +
          ggplot2::stat_density_2d(
               geom = "density_2d", bins = 8, color = "black", linewidth = 0.4, na.rm = TRUE
          ) +
          ggplot2::geom_point(
               data  = results_sims_best,
               ggplot2::aes(x = cor_deaths, y = msle_deaths),
               shape = 21, fill = "#ba181b", color = "black", size = 3.5, stroke = 0.3, na.rm = TRUE
          ) +
          ggplot2::scale_x_continuous(limits = c(0, max(c(results_sims$cor_cases, results_sims$cor_deaths))),
                                      expand = ggplot2::expansion(mult = c(0.02, 0.02))) +
          ggplot2::scale_y_reverse(expand = ggplot2::expansion(mult = c(0.02, 0.05))) +
          ggplot2::labs(
               title = "Deaths: Correlation vs MSLE",
               x = "Correlation (higher is better)",
               y = "MSLE (lower is better; axis reversed)"
          ) +
          .mosaic_panel_theme

     # === Combine all three: big loss plot on top, two small below ===============
     combined_plot <- NULL
     save_path <- file.path(plots_dir, "convergence_diagnostic.pdf")

     if (requireNamespace("patchwork", quietly = TRUE)) {
          combined_plot <- p / (p_cases | p_deaths)
          combined_plot <- combined_plot + patchwork::plot_layout(heights = c(2, 1))
          if (verbose) print(combined_plot)
          ggplot2::ggsave(save_path, plot = combined_plot, width = 11, height = 12, dpi = 600)
     } else if (requireNamespace("gridExtra", quietly = TRUE)) {
          lower_row <- gridExtra::arrangeGrob(p_cases, p_deaths, ncol = 2)
          g <- gridExtra::arrangeGrob(p, lower_row, heights = c(2, 1))
          if (verbose) grid::grid.newpage(); grid::grid.draw(g)
          ggplot2::ggsave(save_path, plot = g, width = 11, height = 12, dpi = 600)
     } else {
          warning("Neither 'patchwork' nor 'gridExtra' is available. Saving only the main loss plot.")
          # Fallback: save only the top plot
          ggplot2::ggsave(file.path(plots_dir, "convergence_diagnostic.pdf"),
                          plot = p, width = 11, height = 12, dpi = 600)
     }

     if (verbose) {
          if (!is.null(combined_plot)) {
               message("Combined convergence diagnostic plot saved to: ", save_path)
          }
     }


     invisible(combined_plot)





}
