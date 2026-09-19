# =============================================================================
# shape_metrics.R -- metrics that detect a FLAT, persistence-like prediction.
#
# WHY THIS EXISTS. MAE can be improved by shrinking predictions toward a flat
# line: if the truth is noisy around a slowly-moving level, the constant that
# minimises MAE is the running median, and a model that predicts it scores well
# while carrying ZERO information about trend. This programme's blend makes that
# risk concrete -- lambda = 0 in 40% of cells, where the "forecast" IS
# persistence -- so a pooled MAE gain must be checked against metrics that only
# a prediction which MOVES WITH the data can win.
#
# The four added here, and what a flat predictor scores on each:
#   sd_ratio  sd(pred)/sd(obs) per country-block, weighted mean.
#             A constant predictor scores 0. Persistence scores 0 EXACTLY.
#             ~1 means the prediction has lifelike amplitude; >1 = overshooting.
#   dcor      corr(diff(pred), diff(obs)) per country-block, weighted mean.
#             THE trend-tracking metric. A constant predictor has diff == 0, so
#             the correlation is undefined -- reported as degenerate, not as 0,
#             because pretending it is 0 flatters it.
#   dir_acc   fraction of week-to-week steps where sign(diff(pred)) equals
#             sign(diff(obs)), over steps where the truth actually moved.
#             A constant predictor scores 0. Coin flip = 0.5.
#   bias      weighted mean(pred) / weighted mean(obs). 1 = unbiased;
#             <1 = systematic under-prediction, >1 = over-prediction.
#
# `degen` reports the share of country-blocks where the prediction never moved,
# i.e. where dcor could not be computed at all. It is the flatness headline.
#
# SOURCED BY BOTH accuracy_table.R AND arm_C_transforms.R so the two can never
# drift apart (the package has a standing lesson about N parallel scorers that
# must be updated in lockstep).
# =============================================================================

# d: data.frame with iso_code, date, observed, and a prediction column `col`.
#    Must already be restricted to the scored cells.
# wts: named vector of country weights (names = iso_code).
# blockcol: column identifying the forecast origin, so series are not spliced
#    across cutoffs -- a diff() across a block boundary is not a real change.
.shape_metrics <- function(d, col, wts, blockcol = "fold") {
     stopifnot(all(c("iso_code","date","observed") %in% names(d)), col %in% names(d))
     if (!blockcol %in% names(d)) d[[blockcol]] <- "all"
     d <- d[is.finite(d$observed) & is.finite(d[[col]]), , drop = FALSE]
     if (!nrow(d)) return(c(bias=NA, sd_ratio=NA, dcor=NA, dir_acc=NA, degen=NA))

     key <- paste(d$iso_code, d[[blockcol]], sep = "\r")
     grp <- split(seq_len(nrow(d)), key)

     per <- lapply(grp, function(ix) {
          z <- d[ix, , drop = FALSE]
          z <- z[order(z$date), , drop = FALSE]
          p <- z[[col]]; o <- z$observed
          if (length(p) < 4L) return(NULL)
          dp <- diff(p); do <- diff(o)
          flat <- isTRUE(all.equal(stats::sd(p), 0)) || stats::sd(p) < 1e-12
          dc <- if (flat || stats::sd(do) < 1e-12) NA_real_ else
                suppressWarnings(stats::cor(dp, do))
          moved <- abs(do) > 1e-12
          da <- if (!any(moved)) NA_real_ else mean(sign(dp[moved]) == sign(do[moved]))
          c(iso = z$iso_code[1], sdr = stats::sd(p) / stats::sd(o),
            dcor = dc, dir = da, flat = as.numeric(flat))
     })
     per <- per[!vapply(per, is.null, logical(1))]
     if (!length(per)) return(c(bias=NA, sd_ratio=NA, dcor=NA, dir_acc=NA, degen=NA))

     iso <- vapply(per, function(x) unname(x["iso"]), character(1))
     num <- function(k) vapply(per, function(x) as.numeric(unname(x[k])), numeric(1))
     w   <- unname(wts[match(iso, names(wts))]); w[!is.finite(w)] <- 0

     wmean <- function(v) { k <- is.finite(v) & w > 0
                            if (!any(k)) NA_real_ else sum(w[k]*v[k])/sum(w[k]) }
     wobs <- unname(wts[match(d$iso_code, names(wts))])
     c(bias     = sum(wobs*d[[col]], na.rm=TRUE) / sum(wobs*d$observed, na.rm=TRUE),
       sd_ratio = wmean(num("sdr")),
       dcor     = wmean(num("dcor")),
       dir_acc  = wmean(num("dir")),
       degen    = wmean(num("flat")))
}
