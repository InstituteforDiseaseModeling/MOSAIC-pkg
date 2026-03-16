# test_epidemic_threshold_priors.R
# Visual and numerical verification of updated epidemic_threshold priors.
# Run after make_priors.R has been executed and priors_default.rda regenerated.
# Output: ./claude/epidemic_threshold_prior_check.png

library(MOSAIC)
library(ggplot2)

MOSAIC::set_root_directory("~/MOSAIC")
j <- MOSAIC::iso_codes_mosaic

# Load the freshly regenerated priors directly from data/ rather than the installed package
# (the installed package may have the old version until devtools::install() is run)
load("data/priors_default.rda")   # loads object named 'priors_default'
priors <- priors_default

# ---- Build summary data frame ----
threshold_df <- do.call(rbind, lapply(j, function(iso) {
     p <- priors$parameters_location$epidemic_threshold$location[[iso]]$parameters
     data.frame(
          iso_code     = iso,
          prior_median = exp(p$meanlog),
          ci_lo_95     = exp(p$meanlog - 1.96 * p$sdlog),
          ci_hi_95     = exp(p$meanlog + 1.96 * p$sdlog),
          stringsAsFactors = FALSE
     )
}))

# Old Beta prior means for comparison (before this change)
old_means_per_100k <- c(
     AGO=20, BDI=15, BEN=20, BFA=15, BWA=40, CAF=10, CIV=25, CMR=25,
     COD=15, COG=20, ERI=15, ETH=20, GAB=30, GHA=30, GIN=20, GMB=20,
     GNB=15, GNQ=25, KEN=35, LBR=20, MLI=20, MOZ=20, MRT=15, MWI=25,
     NAM=35, NER=15, NGA=25, RWA=40, SEN=30, SLE=20, SOM=10, SSD=10,
     SWZ=30, TCD=15, TGO=20, TZA=25, UGA=30, ZAF=50, ZMB=30, ZWE=20
)
threshold_df$old_prior_mean <- old_means_per_100k[threshold_df$iso_code] / 1e5

threshold_df <- threshold_df[order(threshold_df$prior_median), ]
threshold_df$iso_code <- factor(threshold_df$iso_code, levels = threshold_df$iso_code)

# ---- Forest plot ----
p <- ggplot(threshold_df, aes(x = iso_code)) +
     geom_pointrange(
          aes(y = prior_median, ymin = ci_lo_95, ymax = ci_hi_95),
          color = "steelblue", linewidth = 0.5
     ) +
     geom_point(aes(y = old_prior_mean), shape = 4, color = "firebrick", size = 2) +
     scale_y_log10(
          labels = scales::label_scientific(),
          name   = "epidemic_threshold (Isym/N, log scale)"
     ) +
     coord_flip() +
     labs(
          title    = "Updated epidemic_threshold priors vs old priors",
          subtitle = "Blue: new Lognormal median + 95% CI  |  Red \u00d7: old Beta prior mean",
          x        = NULL
     ) +
     theme_bw(base_size = 9)

dir.create("./claude", showWarnings = FALSE)
ggsave("./claude/epidemic_threshold_prior_check.png", p, width = 7, height = 9, dpi = 150)
cat("Plot saved to ./claude/epidemic_threshold_prior_check.png\n")

# ---- Scale sanity checks ----
cat("\n--- Scale checks ---\n")
cat("New prior_median range: [",
    format(min(threshold_df$prior_median), scientific = TRUE), ",",
    format(max(threshold_df$prior_median), scientific = TRUE), "]\n")
cat("Old prior_mean range:   [",
    format(min(threshold_df$old_prior_mean), scientific = TRUE), ",",
    format(max(threshold_df$old_prior_mean), scientific = TRUE), "]\n")
cat("Median fold-change (old/new):", round(median(threshold_df$old_prior_mean / threshold_df$prior_median), 0), "x\n")

stopifnot(
     "All new prior medians must be < 1e-4 (old wrong scale floor)" = max(threshold_df$prior_median) < 1e-4,
     "All new prior medians must be > 1e-9 (biologically implausible below this)" = min(threshold_df$prior_median) > 1e-9
)
cat("[PASS] All new prior medians are in range (1e-9, 1e-4)\n")

# Check all 40 countries have lognormal priors
for (iso in j) {
     dist   <- priors$parameters_location$epidemic_threshold$location[[iso]]$distribution
     params <- priors$parameters_location$epidemic_threshold$location[[iso]]$parameters
     if (dist != "lognormal")       stop(iso, ": distribution must be lognormal, got ", dist)
     if (!is.finite(params$meanlog)) stop(iso, ": meanlog must be finite")
     if (params$sdlog <= 0)          stop(iso, ": sdlog must be > 0")
}
cat("[PASS] All 40 countries have valid Lognormal(meanlog, sdlog) priors\n")

# ---- Model smoke test ----
cat("\n--- Model smoke test ---\n")
cat("Sampling parameters (seed=42) with updated priors...\n")

config_sampled <- MOSAIC::sample_parameters(
     seed        = 42,
     config      = MOSAIC::config_default,
     priors      = priors_default,
     sample_args = list(sample_initial_conditions = FALSE)
)

et_vals <- config_sampled$epidemic_threshold
cat(sprintf("  epidemic_threshold range:  [%.2e, %.2e]\n", min(et_vals), max(et_vals)))
cat(sprintf("  epidemic_threshold median:  %.2e\n", median(et_vals)))

stopifnot(
     "max epidemic_threshold must be < 1e-4" = max(et_vals) < 1e-4,
     "min epidemic_threshold must be > 1e-9" = min(et_vals) > 1e-9
)
cat("[PASS] Sampled epidemic_threshold values are in correct range\n")

# Confirm sampled values satisfy make_LASER_config validation: epidemic_threshold in [0, 1]
et_valid <- all(et_vals >= 0) && all(et_vals <= 1)
if (et_valid) {
     cat("[PASS] All sampled epidemic_threshold values satisfy make_LASER_config constraint [0, 1]\n")
} else {
     stop("Sampled epidemic_threshold values outside [0, 1]: range [",
          min(et_vals), ",", max(et_vals), "]")
}

cat("\nAll checks passed.\n")
