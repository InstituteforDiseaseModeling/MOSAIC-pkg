# Generate Running LASER Vignette Figures
#
# This script regenerates the pre-computed figures displayed in the
# "Running LASER" vignette (vignettes/Running-LASER.Rmd). The vignette chunks
# are set to eval = FALSE, so the figures must be produced here and committed.
#
# The plotting code below is kept in lockstep with the vignette chunks so the
# committed PNGs match exactly what the vignette source shows. If you change a
# plot in the vignette, change it here too (and vice versa).
#
# Field names: extract model$results$reported_cases / reported_deaths. These are
# the surveillance-comparable fields (rho/chi-adjusted cases; rho_deaths-applied,
# lagged deaths) introduced in laser-cholera v0.13. Do NOT use expected_cases or
# disease_deaths here — those are raw (unobserved) quantities and do not match
# the vignette, which plots reported_* throughout.
#
# Usage (from the MOSAIC-pkg root):
#   source("vignettes/articles/generate_running_laser_figures.R")

library(MOSAIC)

# Output directory: the vignette references figures via the relative path
# "figures/..." (i.e. vignettes/figures/), so write there.
fig_dir <- "vignettes/figures"
if (!dir.exists(fig_dir)) dir.create(fig_dir, recursive = TRUE)

message("Generating Running LASER vignette figures...")
message("This will take a few minutes as LASER models run...")

# =============================================================================
# [1/3] EPIDEMIC SCENARIO  ->  figures/running_mosaic_epidemic.png
# =============================================================================

message("\n[1/3] Running epidemic scenario...")

config <- MOSAIC::config_simulation_epidemic
model_epidemic <- run_LASER(config = config, seed = 123, quiet = TRUE)

cases_epidemic  <- as.matrix(model_epidemic$results$reported_cases)
deaths_epidemic <- as.matrix(model_epidemic$results$reported_deaths)

locations   <- config_simulation_epidemic$location_name
n_locations <- length(locations)

cat(sprintf("Epidemic Scenario Results:\n"))
cat(sprintf("  Total cases: %s\n", format(sum(cases_epidemic, na.rm = TRUE), big.mark = ",")))
cat(sprintf("  Total deaths: %s\n", format(sum(deaths_epidemic, na.rm = TRUE), big.mark = ",")))

png(file.path(fig_dir, "running_mosaic_epidemic.png"), width = 800, height = 600, res = 120)

colors <- c("#009ADE", "#FF1F5B", "#00CD6C")  # Blue, Pink, Green
par(mfrow = c(2, 1), mar = c(3, 4, 3, 1), oma = c(2, 0, 0, 0))

matplot(t(cases_epidemic), type = "l", lty = 1, lwd = 2, col = colors,
        xlab = "", ylab = "Reported Cases",
        main = "Epidemic Transmission Simulation")

matplot(t(deaths_epidemic), type = "l", lty = 1, lwd = 2, col = colors,
        xlab = "Time step", ylab = "Reported Deaths", main = "")

par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
legend("bottom", legend = locations, col = colors, lty = 1, lwd = 2,
       horiz = TRUE, bty = "n", xpd = TRUE)

dev.off()
message("  Saved: ", file.path(fig_dir, "running_mosaic_epidemic.png"))

# =============================================================================
# [2/3] ENDEMIC SCENARIO  ->  figures/running_mosaic_endemic.png
# =============================================================================

message("\n[2/3] Running endemic scenario...")

config <- MOSAIC::config_simulation_endemic
model_endemic <- run_LASER(config = config, seed = 123, quiet = TRUE)

cases_endemic  <- as.matrix(model_endemic$results$reported_cases)
deaths_endemic <- as.matrix(model_endemic$results$reported_deaths)

cat(sprintf("Endemic Scenario Results:\n"))
cat(sprintf("  Total cases: %s\n", format(sum(cases_endemic, na.rm = TRUE), big.mark = ",")))
cat(sprintf("  Total deaths: %s\n", format(sum(deaths_endemic, na.rm = TRUE), big.mark = ",")))

png(file.path(fig_dir, "running_mosaic_endemic.png"), width = 800, height = 600, res = 120)

colors <- c("#00CD6C", "#AF58BA", "#009ADE")  # Green, Purple, Blue
par(mfrow = c(2, 1), mar = c(3, 4, 3, 1), oma = c(2, 0, 0, 0))

matplot(t(cases_endemic), type = "l", lty = 1, lwd = 2, col = colors,
        xlab = "", ylab = "Reported Cases",
        main = "Endemic Transmission Simulation")

matplot(t(deaths_endemic), type = "l", lty = 1, lwd = 2, col = colors,
        xlab = "Time step", ylab = "Reported Deaths", main = "")

par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
legend("bottom", legend = config_simulation_endemic$location_name, col = colors,
       lty = 1, lwd = 2, horiz = TRUE, bty = "n", xpd = TRUE)

dev.off()
message("  Saved: ", file.path(fig_dir, "running_mosaic_endemic.png"))

# =============================================================================
# [3/3] MULTI-LOCATION RUN  ->  figures/multi_location_grid.png
# =============================================================================

message("\n[3/3] Running multi-location scenario (all MOSAIC countries)...")

iso_codes <- iso_codes_mosaic[iso_codes_mosaic != 'SSD'] # Current bug with SSD vaccination data

config <- get_location_config(iso = iso_codes)
config$beta_j0_hum <- config$beta_j0_hum * 2
config$beta_j0_env <- config$beta_j0_env * 4

results <- run_LASER(config, seed = 123, quiet = TRUE)
cases   <- as.matrix(results$results$reported_cases)

png(file.path(fig_dir, "multi_location_grid.png"), width = 1400, height = 1600, res = 130)
par(mfrow = c(ceiling(nrow(cases) / 5), 5), mar = c(2, 2, 2, 1), oma = c(2, 2, 2, 0))

for (i in 1:nrow(cases)) {
     plot(cases[i, ], type = "l", lwd = 1.5, col = "#009ADE",
          main = config$location_name[i],
          xlab = "", ylab = "",
          cex.main = 0.9)
     grid(col = "gray90")
}

mtext("Time step", side = 1, outer = TRUE, line = 0.5)
mtext("Reported Cases", side = 2, outer = TRUE, line = 0.5)
mtext("Reported Cases by Location", side = 3, outer = TRUE, line = 0.5, font = 2)
dev.off()
message("  Saved: ", file.path(fig_dir, "multi_location_grid.png"))

# =============================================================================
# COMPLETE
# =============================================================================

message("\nAll figures generated successfully!")
message("  Location: ", normalizePath(fig_dir))
message("\nFigures:")
message("  - running_mosaic_epidemic.png")
message("  - running_mosaic_endemic.png")
message("  - multi_location_grid.png")
