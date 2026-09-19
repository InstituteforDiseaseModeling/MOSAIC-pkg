# The metric panel. R2 and bias are reported for ALL data, the TRAINING window,
# and each OOS horizon bucket separately -- horizons are what a forecast is for,
# and today's review showed a headline full-window R2 of 0.79 concealing 0.30
# with 1.87x bias in the final year.
#
# R2 uses the package's own calc_model_R2() so the lab cannot drift from the
# pipeline's definition. Bias = sum(pred)/sum(obs) over the window.

score_run <- function(dir_out, config, t_cut = NULL, method = "corr",
                      central = "predicted_median") {
     pdir <- file.path(dir_out, "3_results", "predictions")
     cand <- list.files(pdir, pattern = "^predictions_ensemble_.*\\.csv$", full.names = TRUE)
     if (!length(cand)) return(NULL)
     p <- do.call(rbind, lapply(cand, utils::read.csv, stringsAsFactors = FALSE))
     p$date <- as.Date(p$date)
     if (!central %in% names(p)) central <- "predicted_central"
     p$pred <- suppressWarnings(as.numeric(p[[central]]))
     p$obs  <- suppressWarnings(as.numeric(p$observed))

     # `metric` carries cases vs deaths; normalise whatever labels are used.
     p$chan <- ifelse(grepl("case",  p$metric, ignore.case = TRUE), "cases",
               ifelse(grepl("death", p$metric, ignore.case = TRUE), "deaths", NA))
     p <- p[!is.na(p$chan), ]

     wins <- list(all = rep(TRUE, nrow(p)))
     if (!is.null(t_cut)) {
          wins$train <- p$date <= as.Date(t_cut)
          hb <- horizon_bucket(p$date, t_cut)
          for (h in c("h1", "h2", "h3", "h4-6"))
               wins[[paste0("oos_", h)]] <- !is.na(hb) & hb == h
          wins$oos_all <- !is.na(hb) & hb != "h>6"
     }

     f <- function(i) {
          ok <- i & is.finite(p$pred) & is.finite(p$obs)
          if (sum(ok) < 3) return(c(NA_real_, NA_real_, sum(ok)))
          r2 <- tryCatch(as.numeric(calc_model_R2(observed = p$obs[ok],
                                                  estimated = p$pred[ok],
                                                  method = method))[1],
                         error = function(e) NA_real_)
          c(r2, sum(p$pred[ok]) / max(sum(p$obs[ok]), .Machine$double.eps), sum(ok))
     }
     do.call(rbind, lapply(names(wins), function(w) {
          cc <- f(wins[[w]] & p$chan == "cases")
          dd <- f(wins[[w]] & p$chan == "deaths")
          data.frame(window = w, n_cases = cc[3], r2_cases = cc[1], bias_cases = cc[2],
                     n_deaths = dd[3], r2_deaths = dd[1], bias_deaths = dd[2],
                     stringsAsFactors = FALSE)
     }))
}

# The honest inference diagnostics, straight from the run's own summary.
inference_diagnostics <- function(dir_out) {
     sf <- file.path(dir_out, "3_results", "summary.json")
     if (!file.exists(sf)) return(NULL)
     s <- jsonlite::read_json(sf, simplifyVector = TRUE)
     g <- function(k) { v <- s[[k]]; if (is.null(v)) return(NA_real_)
          suppressWarnings(as.numeric(v)[1]) }
     data.frame(ess_is_all = g("ess_is_all"), ess_is_best = g("ess_is_best"),
                khat_all = g("khat_all"), n_best_subset = g("n_best_subset"),
                ess_best_optimized = g("ess_best_optimized"),
                khat_status = if (!is.null(s$khat_all_status)) s$khat_all_status else NA_character_,
                stringsAsFactors = FALSE)
}
