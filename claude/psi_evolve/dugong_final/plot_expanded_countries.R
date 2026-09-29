# =============================================================================
# plot_expanded_countries.R -- predictions for EVERY country in the scored
# pool, not the 4-6 the earlier figures sampled.
#
#   fig10_expanded_countries.pdf   one page per cutoff, all 16 countries,
#                                  ordered WORST-PRODUCTION-FIRST so the
#                                  countries the model actually fails on are
#                                  read first rather than hunted for.
#
# Ordering comes from per_country_weakness.csv (burden-weighted excess of the
# production architecture over persistence). Countries where production is
# beaten by persistence have a RED title; that is most of them, and the figure
# should say so at a glance rather than in a caption.
#
# usage: Rscript plot_expanded_countries.R
# =============================================================================
suppressMessages(library(MOSAIC))
source("/home/jgiles/psi_evolve/psi_plot_common.R")

wk <- file.path(HERE, "per_country_weakness.csv")
if (!file.exists(wk))
     stop("run per_country_weakness.R first -- it defines the country ordering")
w <- utils::read.csv(wk, stringsAsFactors = FALSE)
w <- w[order(-w$w_excess), ]
isos <- w$iso
cat(sprintf("%d countries, ordered worst-production-first: %s\n",
            length(isos), paste(isos, collapse=" ")))

nr <- 4L; nc <- ceiling(length(isos)/nr)
pdf(file.path(OUT,"fig10_expanded_countries.pdf"), width=4.2*nc, height=2.9*nr)
for (k in seq_len(nrow(grid))) {
     par(mfrow=c(nr,nc), mar=c(2.0,2.6,1.7,0.5), mgp=c(1.5,0.45,0),
         oma=c(4.4,2.4,4.0,0.5), cex.axis=0.65, tcl=-0.22)
     for (iso in isos) {
          r <- w[w$iso==iso, ]
          # red = the production architecture is WORSE than simple persistence
          beaten <- isTRUE(r$mae_P000E > r$mae_pers)
          psi_panel(iso, k,
                    title_col   = if (beaten) "#B5123B" else "grey20",
                    title_extra = sprintf("   w=%.3f%s", r$w,
                                          if (beaten) "  (persistence wins)" else ""))
     }
     mtext(sprintf("Every scored country -- cutoff %s   (OOS window %s to %s)",
                   format(grid$cutoff[k]), format(grid$test_start[k]),
                   format(grid$test_end[k])),
           outer=TRUE, line=2.1, cex=1.05, font=2)
     mtext(paste0("ordered by burden-weighted excess of the production architecture over persistence, worst first; ",
                  "RED title = persistence beats production here. Grey panel = the 13-week window scored; dashed vertical = origin."),
           outer=TRUE, line=0.7, cex=0.66, col="grey30")
     mtext("transmission intensity", side=2, outer=TRUE, line=0.9, cex=0.8)
     psi_legend(ncol=min(6, length(ARMS)+3))
}
dev.off()
f <- file.path(OUT,"fig10_expanded_countries.pdf")
cat(sprintf("wrote %s  (%d pages, %.0f bytes)\n", basename(f), nrow(grid),
            file.info(f)$size))
cat("arms drawn:", paste(ARMS, collapse=", "), "\n")
