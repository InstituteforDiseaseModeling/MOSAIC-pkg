# Display fields of calibration_diagnostics.json (MOSAIC 1.5.1)
#
# The report (plot_model_calibration_report()) and the dashboard show the same
# strings, so they are composed once, here, from the file's numbers and the
# metric glossary (inst/extdata/metric_glossary.yml), and written into the file:
# per check its operators and value / threshold / target displays, the flags
# with their text, the info rows (ESS growth, budget, ensemble size), the fit
# table and the sampling details as the report shows them. A display string is
# rich text: {name} names a glossary token (metric_glossary.yml `tokens`).
# Contract: inst/extdata/calibration_diagnostics_schema.md (tracked copy of
# claude/plan_convergence/SCHEMA_v2.md sections 1-2).

# Revision of the 1.5.1 additions to schema 2; a file without it is completed
# (in memory by the report, on disk by render_MOSAIC_figures()).
.mosaic_cd_revision <- function() "1.5.1"

#' A report string of the glossary
#'
#' @param gl \code{.mosaic_glossary()} result.
#' @param ... Path into the glossary's \code{report} section.
#' @return The string (or list); an error naming the path when it is missing,
#'   so the report never falls back to a hard-coded word.
#' @noRd
.mosaic_gl_text <- function(gl, ...) {
     path <- c(...)
     x <- attr(gl, "report")
     for (p in path) {
          x <- x[[p]]
          if (is.null(x)) stop("metric glossary has no report string ", paste(path, collapse = "$"), call. = FALSE)
     }
     x
}

#' Fill {name} placeholders, leaving every other {...} (tokens) in place
#' @param template A string.
#' @param ... Named values.
#' @return The string.
#' @noRd
.mosaic_fill <- function(template, ...) {
     v <- list(...)
     out <- template
     for (nm in names(v)) out <- gsub(paste0("{", nm, "}"), as.character(v[[nm]])[1], out, fixed = TRUE)
     out
}

#' Rich text to plain text
#' @param x Character vector with glossary tokens.
#' @param gl Glossary.
#' @param to \code{"text"} (Unicode, the dashboard), \code{"ascii"} (the PDF
#'   outside formulas) or \code{"katex"}.
#' @return Character vector.
#' @noRd
.mosaic_rich_plain <- function(x, gl, to = c("text", "ascii", "katex")) {
     to <- match.arg(to)
     if (to == "ascii") x <- gsub("\u2212", "-", x, fixed = TRUE)
     tk <- attr(gl, "tokens")
     for (nm in names(tk)) x <- gsub(paste0("{", nm, "}"), tk[[nm]][[to]], x, fixed = TRUE)
     x
}

#' Operators and glossary keys of the four status checks
#'
#' The comparison each check's colour rule applies (\code{calc_convergence_status()}):
#' green when the value is \code{operator} the green threshold, yellow when it
#' is \code{operator_yellow} the yellow one (informativeness: green below the
#' flat share, yellow at or above it, no red). Pinned to the rule by a test.
#' @return Data frame: \code{check, key, operator, operator_yellow}.
#' @noRd
.mosaic_convergence_check_spec <- function() {
     data.frame(check = c("ess", "mce", "khat", "informativeness"),
                key = c("ess_best", "mce", "khat_tempered", "flat_likelihood"),
                operator = c("ge", "le", "lt", "lt"),
                operator_yellow = c("ge", "le", "lt", "ge"),
                stringsAsFactors = FALSE)
}

# A real minus sign in a displayed number (U+2212); the PDF prints it as the
# WinAnsi minus.
.mosaic_cd_minus <- function(x) sub("^-", "\u2212", x)

# A JSON array of records (list of lists or a data frame) as a data frame
# holding every key; JSON nulls become NA.
.mosaic_cd_frame <- function(x) {
     if (is.null(x) || !length(x)) return(data.frame())
     if (is.data.frame(x)) return(x)
     keys <- unique(unlist(lapply(x, names)))
     cols <- lapply(keys, function(k) {
          v <- lapply(x, function(r) { z <- r[[k]]; if (is.null(z) || length(z) != 1L) NA else z })
          unlist(v)
     })
     stats::setNames(as.data.frame(cols, stringsAsFactors = FALSE, col.names = keys), keys)
}

.mosaic_cd_num <- function(x) {
     x <- suppressWarnings(as.numeric(unlist(x)))
     if (length(x) != 1L || !is.finite(x)) NA_real_ else x
}
.mosaic_cd_chr <- function(x) {
     x <- unlist(x)
     if (length(x) != 1L || is.na(x)) NA_character_ else as.character(x)
}

#' Complete a calibration_diagnostics list with its 1.5.1 display fields
#'
#' Pure (no I/O beyond the glossary passed in): the same numbers and glossary
#' always give the same strings, so the writer, the report and the upgrade of
#' an older file agree byte for byte. Fields already present are recomputed,
#' except \code{status.flags_detail} (kept when present: the writer has each
#' flag's own text) and \code{status.thresholds} (completed with the package
#' defaults when absent, noted in \code{status.thresholds_source}).
#' @param j The diagnostics as written by \code{.mosaic_calibration_diagnostics()}
#'   or parsed from JSON (\code{simplifyVector = FALSE}).
#' @param gl \code{.mosaic_glossary()} result.
#' @return \code{j} with \code{schema_revision}, \code{run.glossary_version},
#'   the check, flag, headline, palette, info, fit-table and sampling-detail
#'   display fields.
#' @noRd
.mosaic_calrep_complete <- function(j, gl) {
     num <- .mosaic_cd_num; chr <- .mosaic_cd_chr
     fmt <- .mosaic_calrep_fmt
     txt <- function(...) .mosaic_gl_text(gl, ...)
     r <- j$run %||% list(); s <- j$sampling %||% list(); st <- j$status %||% list()
     defaults <- .mosaic_convergence_defaults()

     # thresholds ---------------------------------------------------------------
     th <- st$thresholds
     if (is.null(th) || !length(th)) {
          th <- defaults$thresholds
          st$thresholds_source <- "package default"
     } else {
          th <- lapply(th, function(v) suppressWarnings(as.numeric(unlist(v))))
          th <- utils::modifyList(defaults$thresholds, th[vapply(th, function(v) length(v) == 1L && is.finite(v), logical(1))])
          st$thresholds_source <- st$thresholds_source %||% "run control"
     }
     st$thresholds <- th

     # checks -------------------------------------------------------------------
     ck <- .mosaic_cd_frame(st$checks)
     words <- txt("status_words")
     word_of <- function(colour) words[[colour %|NA|% "grey"]] %||% words[["grey"]]
     tok <- c(ge = "{ge}", le = "{le}", lt = "{lt}")
     if (nrow(ck)) {
          spec <- .mosaic_convergence_check_spec()
          m <- match(ck$check, spec$check)
          ck$key <- spec$key[m]
          ck$operator <- spec$operator[m]
          ck$operator_yellow <- spec$operator_yellow[m]
          n <- nrow(ck)
          vd <- gd <- yd <- tdisp <- tnote <- character(n)
          for (i in seq_len(n)) {
               v <- num(ck$value[i]); g <- num(ck$threshold_green[i]); y <- num(ck$threshold_yellow[i])
               thr <- c(g, y)
               id <- ck$check[i] %|NA|% ""
               vd[i] <- switch(id,
                    ess = .mosaic_fmt_thr(v, thr, 0),
                    mce = .mosaic_fmt_thr(v, thr, 1, scale = 100, suffix = "%"),
                    khat = if (is.finite(v)) .mosaic_fmt_thr(v, thr, 2, big.mark = "") else txt("values", "undefined"),
                    informativeness = if (!is.finite(v)) txt("values", "na")
                         else if (is.finite(g) && abs(v - g) / g < 0.05) .mosaic_fmt_thr(v, g, 2, scale = 100, suffix = "%")
                         else .mosaic_calrep_sig_pct(v),
                    .mosaic_calrep_sig(v, 3))
               fm <- switch(id, ess = function(x) fmt(x), mce = .mosaic_calrep_pct_thr,
                            informativeness = .mosaic_calrep_pct_thr,
                            function(x) if (is.finite(x)) format(x) else txt("values", "na"))
               gd[i] <- fm(g)
               yd[i] <- if (identical(id, "informativeness")) fm(g) else fm(y)
               k <- ck$key[i]
               e <- if (!is.na(k) && k %in% gl$key) gl[gl$key == k, , drop = FALSE] else NULL
               tt <- if (!is.null(e)) e$target_template else NA_character_
               tdisp[i] <- if (is.na(tt)) "" else .mosaic_fill(tt, op = tok[[ck$operator[i] %|NA|% "ge"]],
                                                                op_yellow = tok[[ck$operator_yellow[i] %|NA|% "ge"]],
                                                                green = gd[i], yellow = yd[i])
               tnote[i] <- if (!is.null(e) && !is.na(e$target_note)) e$target_note else NA_character_
          }
          ck$value_display <- .mosaic_cd_minus(vd)
          # One label: the glossary's short label (as text), the name the PDF prints.
          ck$label <- vapply(seq_len(n), function(i) {
               k <- ck$key[i]
               if (!is.na(k) && k %in% gl$key) .mosaic_rich_plain(gl$short_label[gl$key == k], gl, "text")
               else ck$label[i]
          }, character(1))
          ck$threshold_green_display <- gd
          ck$threshold_yellow_display <- yd
          ck$target_display <- tdisp
          ck$target_note <- tnote
          ck$status_display <- vapply(ck$colour, word_of, character(1), USE.NAMES = FALSE)
     }
     rownames(ck) <- NULL
     st$checks <- ck

     # flags --------------------------------------------------------------------
     ids <- as.character(unlist(st$flags) %||% character(0))
     fd <- .mosaic_cd_frame(st$flags_detail)
     if (!nrow(fd) && length(ids)) {
          binding <- chr(s$ensemble_binding)
          gen <- .mosaic_convergence_flags(
               budget_exhausted = "budget_cap_reached" %in% ids, stalled = "ess_growth_stalled" %in% ids,
               size_bound = if ("ensemble_size_bound" %in% ids) (if (binding %in% c("min_members", "max_members")) binding else "bound")
                            else NA_character_,
               fixed_mode = "fixed_budget" %in% ids, n_attempted = num(r$n_simulations), n_draws = num(s$sims_used),
               slope = num(s$ess_growth_slope), slope_stall = th$slope_stall)
          texts <- unname(unlist(gen[ids]))
          joined <- paste(unlist(st$flags_text), collapse = "; ")
          if (nzchar(joined) && !identical(paste(texts, collapse = "; "), joined)) {
               # The file was written with other numbers (e.g. another stall
               # threshold): split its own text before each flag's opening.
               opening <- c(budget_cap_reached = "budget cap reached", ess_growth_stalled = "stalled ESS growth",
                            ensemble_size_bound = "ensemble at ", fixed_budget = "fixed-budget run")
               parts <- strsplit(joined, paste0("; (?=(", paste(opening[ids], collapse = "|"), "))"), perl = TRUE)[[1]]
               if (length(parts) == length(ids)) texts <- parts
          }
          fd <- data.frame(id = ids, text = if (length(texts) == length(ids)) texts else gsub("_", " ", ids),
                           stringsAsFactors = FALSE)
     }
     if (nrow(fd)) {
          # Fixed versus adaptive is already in the run line, so the
          # fixed-budget flag is not displayed (owner rule, 1.5.1).
          fd$display <- fd$id != "fixed_budget"
          fd <- fd[, c("id", "text", "display")]
          rownames(fd) <- NULL
     } else fd <- data.frame(id = character(0), text = character(0), display = logical(0))
     st$flags_detail <- fd

     status <- chr(st$convergence_status)
     st$headline_display <- .mosaic_fill(txt("overall"), status = word_of(status),
                                         label = chr(st$convergence_label) %|NA|% txt("status_unavailable"))
     pal <- .mosaic_calrep_rag()
     st$palette <- as.list(pal)
     j$status <- st

     # info rows ----------------------------------------------------------------
     words_i <- txt("info")
     mode <- chr(r$mode)
     slope <- num(s$ess_growth_slope)
     stall <- th$slope_stall
     g_slope <- gl[gl$key == "ess_growth_slope", , drop = FALSE]
     growth_state <- if (!is.finite(slope)) "na"
                     else if ("ess_growth_stalled" %in% ids || slope < stall) "stalled" else "ok"
     ess_growth <- list(key = "ess_growth_slope", value = slope,
                        display = if (is.finite(slope)) .mosaic_cd_minus(sprintf("%.2f", slope)) else words_i$na,
                        target = stall, operator = "ge", target_value_display = format(stall),
                        target_display = if (nrow(g_slope) && !is.na(g_slope$target_template))
                             .mosaic_fill(g_slope$target_template, op = tok[["ge"]], target = format(stall)) else "",
                        status = growth_state, status_display = words_i[[growth_state]])
     N <- num(r$n_simulations) %|NA|% num(s$sims_used)
     cap <- if (identical(mode, "fixed")) num(r$n_simulations_requested) %|NA|% N else num(r$max_simulations_total)
     sampling_green <- nrow(ck) > 0 && all(ck$colour[ck$check %in% c("ess", "mce", "khat")] %in% "green")
     proj <- num(s$sims_projected_for_green)
     reliable <- suppressWarnings(as.logical(unlist(s$projection_reliable)))
     tgt <- if (identical(status, "green")) list(d = words_i$already_green, n = NA_character_)
            else if (sampling_green) list(d = words_i$sampling_green, n = words_i$sampling_green_note)
            else if (is.finite(proj)) list(d = .mosaic_fill(words_i$projected, n = .mosaic_fmt_draws(proj)),
                                           n = if (length(reliable) == 1L && isFALSE(reliable)) words_i$indicative else NA_character_)
            else list(d = words_i$na, n = NA_character_)
     at_cap <- isTRUE(suppressWarnings(as.logical(unlist(s$budget_exhausted)))) || "budget_cap_reached" %in% ids
     # A run with fewer than two valid draws has failed (calc_convergence_status()):
     # neither its budget nor its ensemble size is "ok".
     failed <- is.finite(num(s$sims_used)) && num(s$sims_used) < 2
     b_state <- if (failed) "failed" else if (at_cap) "at_cap" else "ok"
     budget <- list(key = "budget", value = N, cap = cap, display = fmt(N),
                    sub_display = if (is.finite(cap)) .mosaic_fill(words_i$of_cap, cap = fmt(cap)) else words_i$cap_na,
                    target = proj, target_display = tgt$d, target_note = tgt$n,
                    status = b_state, status_display = words_i[[b_state]])
     bind <- chr(s$ensemble_binding)
     mn <- num(r$min_ensemble_members); mx <- num(r$max_ensemble_members)
     g_size <- gl[gl$key == "ensemble_members", , drop = FALSE]
     at_bound <- "ensemble_size_bound" %in% ids || bind %in% c("min_members", "max_members")
     size <- list(key = "ensemble_members", value = num(s$ensemble_members), min = mn, max = mx, binding = bind,
                  display = fmt(num(s$ensemble_members)),
                  sub_display = if (is.na(bind)) words_i$na else (words_i[[paste0("binding_", bind)]] %||% bind),
                  target_display = if (is.finite(mn) && is.finite(mx) && nrow(g_size) && !is.na(g_size$target_template))
                       .mosaic_fill(g_size$target_template, min = fmt(mn), max = fmt(mx)) else words_i$na,
                  status = if (failed) "failed" else if (at_bound) "at_bound" else "ok",
                  status_display = if (failed) words_i$failed else if (at_bound) words_i$at_bound else words_i$ok)
     j$info <- list(ess_growth = ess_growth, budget = budget, ensemble_size = size)

     # fit table as shown ---------------------------------------------------------
     f <- j$fit %||% list()
     tb <- .mosaic_cd_frame(f$table)
     if (nrow(tb)) {
          if (is.null(tb$subset)) tb$subset <- "scored"
          tb$subset[is.na(tb$subset)] <- "scored"
          tb <- tb[tb$metric %in% c("cases", "deaths"), , drop = FALSE]
          tb <- tb[order(match(tb$metric, c("cases", "deaths")), match(tb$subset, c("scored", "tier12"))), , drop = FALSE]
          n <- function(cl) suppressWarnings(as.numeric(tb[[cl]] %||% rep(NA, nrow(tb))))
          chan <- vapply(tb$metric, function(m) txt("fit_table", paste0("channel_", m)), character(1), USE.NAMES = FALSE)
          f$table_display <- data.frame(
               metric = tb$metric, subset = tb$subset,
               label = ifelse(tb$subset == "tier12", vapply(chan, function(cn) .mosaic_fill(txt("fit_table", "tier12"), channel = cn),
                                                          character(1), USE.NAMES = FALSE), chan),
               r2 = fmt(n("r2"), 2), bias = fmt(n("bias"), 2), cov50 = .mosaic_calrep_pct(n("cov50"), 0),
               cov95 = .mosaic_calrep_pct(n("cov95"), 0), wis = .mosaic_calrep_sig(n("wis"), 3),
               n_scored = fmt(n("n_scored")), stringsAsFactors = FALSE)
     } else {
          f$table_display <- data.frame(metric = character(0), subset = character(0), label = character(0),
                                        r2 = character(0), bias = character(0), cov50 = character(0),
                                        cov95 = character(0), wis = character(0), n_scored = character(0))
     }
     j$fit <- f

     # sampling details as shown ---------------------------------------------------
     sd_txt <- txt("sampling_details")
     sims_used <- num(s$sims_used)
     alpha <- num(r$coarsening_alpha); eta <- num(r$eta)
     j$sampling$details_display <- data.frame(
          key = c("valid_draws", "posterior", "ess_all", "mass", "runs", "scored"),
          value = c(.mosaic_fill(sd_txt$valid_draws_value, valid = fmt(sims_used), attempted = fmt(N %|NA|% sims_used)),
                    paste0(.mosaic_fill(sd_txt$posterior_value, a = if (is.finite(alpha)) format(alpha) else words_i$na,
                                        e = if (is.finite(eta)) format(signif(eta, 2), trim = TRUE, scientific = FALSE) else words_i$na),
                           # eta pools the locations: alpha per location (red team round 1, D2)
                           if (isTRUE(num(r$n_locations) > 1) && is.finite(num(s$alpha_per_location)))
                                .mosaic_fill(sd_txt$posterior_value_per_location,
                                             apl = format(signif(num(s$alpha_per_location), 2), trim = TRUE))
                           else ""),
                    .mosaic_fill(sd_txt$ess_all_value, ess = fmt(num(s$ess_all)), target = fmt(num(s$ess_all_target))),
                    .mosaic_calrep_pct(num(s$best_subset_mass), 1),
                    fmt(num(r$n_iter_ensemble)),
                    .mosaic_fill(sd_txt$scored_value, n = fmt(num(r$n_obs_scored)), days = fmt(num(r$burn_in_days)))),
          stringsAsFactors = FALSE)

     j$run$glossary_version <- attr(gl, "version")
     j$schema_revision <- .mosaic_cd_revision()
     first <- c("schema_version", "schema_revision", "run", "status", "sampling", "info")
     j[c(intersect(first, names(j)), setdiff(names(j), first))]
}
