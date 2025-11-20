# Generate Running MOSAIC Vignette Figures
#
# This script generates the pre-computed figures for the Running MOSAIC vignette.
# Run this script manually when you need to update the vignette figures.
#
# Usage:
#   source("vignettes/articles/generate_running_mosaic_figures.R")

library(MOSAIC)

# Output directory
fig_dir <- "vignettes/articles/figures"
if (!dir.exists(fig_dir)) dir.create(fig_dir, recursive = TRUE)

message("Generating Running MOSAIC vignette figures...")
message("This will take a few minutes as LASER models run...")

# =============================================================================
# EPIDEMIC SCENARIO
# =============================================================================

message("\n[1/2] Running epidemic scenario...")

model_epidemic <- run_LASER(
  config = config_simulation_epidemic,
  seed = 123,
  quiet = TRUE
)

# Extract results
cases_epidemic <- model_epidemic$results$expected_cases
deaths_epidemic <- model_epidemic$results$disease_deaths

# Calculate summary statistics
total_cases_epidemic <- sum(cases_epidemic, na.rm = TRUE)
total_deaths_epidemic <- sum(deaths_epidemic, na.rm = TRUE)
locations <- model_epidemic$params$location_name
n_locations <- length(locations)
n_timesteps <- ncol(cases_epidemic)

cat(sprintf("Epidemic Scenario Results:\n"))
cat(sprintf("  Total cases: %s\n", format(total_cases_epidemic, big.mark = ",")))
cat(sprintf("  Total deaths: %s\n", format(total_deaths_epidemic, big.mark = ",")))
cat(sprintf("  Locations: %d\n", n_locations))
cat(sprintf("  Time steps: %d\n", n_timesteps))

# Create plot
png(file.path(fig_dir, "running_mosaic_epidemic.png"),
    width = 800, height = 600, res = 100)

total_cases_by_time <- colSums(cases_epidemic, na.rm = TRUE)

plot(total_cases_by_time,
     type = "l",
     col = "#d00000",
     lwd = 2,
     xlab = "Time step",
     ylab = "Total cases",
     main = "Epidemic Scenario: Aggregate Case Trajectory")
grid()

dev.off()
message("  Saved: ", file.path(fig_dir, "running_mosaic_epidemic.png"))

# =============================================================================
# ENDEMIC SCENARIO
# =============================================================================

message("\n[2/2] Running endemic scenario...")

model_endemic <- run_LASER(
  config = config_simulation_endemic,
  seed = 123,
  quiet = TRUE
)

# Extract results
cases_endemic <- model_endemic$results$expected_cases
deaths_endemic <- model_endemic$results$disease_deaths

# Calculate summary statistics
total_cases_endemic <- sum(cases_endemic, na.rm = TRUE)
total_deaths_endemic <- sum(deaths_endemic, na.rm = TRUE)

cat(sprintf("Endemic Scenario Results:\n"))
cat(sprintf("  Total cases: %s\n", format(total_cases_endemic, big.mark = ",")))
cat(sprintf("  Total deaths: %s\n", format(total_deaths_endemic, big.mark = ",")))

# Create plot
png(file.path(fig_dir, "running_mosaic_endemic.png"),
    width = 800, height = 600, res = 100)

total_cases_by_time_endemic <- colSums(cases_endemic, na.rm = TRUE)

plot(total_cases_by_time_endemic,
     type = "l",
     col = "#1f77b4",
     lwd = 2,
     xlab = "Time step",
     ylab = "Total cases",
     main = "Endemic Scenario: Aggregate Case Trajectory")
grid()

dev.off()
message("  Saved: ", file.path(fig_dir, "running_mosaic_endemic.png"))

# =============================================================================
# COMPLETE
# =============================================================================

message("\n✓ All figures generated successfully!")
message("  Location: ", normalizePath(fig_dir))
message("\nFigures:")
message("  - running_mosaic_epidemic.png")
message("  - running_mosaic_endemic.png")
