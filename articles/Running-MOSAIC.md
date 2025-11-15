# Running MOSAIC

## Overview

MOSAIC (Metapopulation Outbreak Simulation with Agent-based
Implementation for Cholera) simulates cholera transmission dynamics
using the LASER (Light-agent Spatial model for ERadication) engine. This
guide shows how to run models using the core
[`run_LASER()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/run_LASER.md)
function.

**Before you begin**: Complete the Installation vignette to set up
MOSAIC and its dependencies.

------------------------------------------------------------------------

## Quick Start

Load MOSAIC and run a single simulation using a pre-configured model:

``` r
library(MOSAIC)

# Run one simulation with default parameters
model <- run_LASER(
  paramfile = config_default,
  seed = 123
)

# View model outputs
str(model, max.level = 1)

# Access results
cases <- model$results$expected_cases
deaths <- model$results$disease_deaths
```

------------------------------------------------------------------------

## Pre-configured Models

MOSAIC includes three pre-configured parameter sets:

``` r
# 1. Default configuration (40 African countries)
model_default <- run_LASER(
  paramfile = config_default,
  seed = 123
)

# 2. Endemic scenario (stable transmission)
model_endemic <- run_LASER(
  paramfile = config_simulation_endemic,
  seed = 456
)

# 3. Epidemic scenario (outbreak dynamics)
model_epidemic <- run_LASER(
  paramfile = config_simulation_epidemic,
  seed = 789
)
```

**View configuration details:**

``` r
# Examine a configuration
config_default$date_start      # "2023-02-01"
config_default$date_stop       # "2026-03-31"
config_default$location_name   # Vector of 40 ISO codes
length(config_default)         # 71 parameters
```

------------------------------------------------------------------------

## Running Multiple Simulations

[`run_LASER()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/run_LASER.md)
executes **one simulation per call**. For multiple simulations, use
loops:

``` r
# Run 10 simulations with different seeds
results_list <- lapply(1:10, function(i) {
  run_LASER(
    paramfile = config_default,
    seed = i
  )
})

# Extract total cases from each simulation
total_cases <- sapply(results_list, function(model) {
  sum(model$results$expected_cases, na.rm = TRUE)
})

# Summary statistics across simulations
mean(total_cases)
sd(total_cases)
range(total_cases)
```

### Parallel Execution

For faster execution of multiple simulations:

``` r
library(parallel)

# Detect available cores
n_cores <- detectCores() - 1

# Run simulations in parallel
cl <- makeCluster(n_cores)
clusterEvalQ(cl, library(MOSAIC))

results_parallel <- parLapply(cl, 1:100, function(i) {
  run_LASER(
    paramfile = config_default,
    seed = i
  )
})

stopCluster(cl)
```

------------------------------------------------------------------------

## Understanding Model Outputs

The
[`run_LASER()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/run_LASER.md)
function returns a Python object with results and metadata:

``` r
# Run model
model <- run_LASER(paramfile = config_default, seed = 123)

# Output structure
names(model)
# $results   - Simulation outputs
# $params    - Parameters used
# $metadata  - Run information

# Access time series data
expected_cases <- model$results$expected_cases
disease_deaths <- model$results$disease_deaths

# Dimensions: [n_locations x n_timesteps]
dim(expected_cases)    # e.g., [40 x 1155]
```

### Extracting Results

``` r
# Total cases by location
total_cases_by_location <- rowSums(expected_cases, na.rm = TRUE)

# Total cases across all locations
total_cases_all <- sum(expected_cases, na.rm = TRUE)

# Peak cases by location
peak_cases_by_location <- apply(expected_cases, 1, max, na.rm = TRUE)

# Peak timing by location
peak_time_by_location <- apply(expected_cases, 1, which.max)

# Location names
locations <- model$params$location_name
names(total_cases_by_location) <- locations
```

------------------------------------------------------------------------

## Creating Custom Configurations

Use
[`make_LASER_config()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/make_LASER_config.md)
to create custom parameter sets:

### Example 1: Single Location

``` r
# Create configuration for Democratic Republic of Congo
config_cod <- make_LASER_config(
  date_start = "2024-01-01",
  date_stop = "2024-12-31",
  location_name = "COD",
  N_j_initial = 100000000,
  S_j_initial = 50000000,
  I_j_initial = 100,
  R_j_initial = 49999900
)

# Run simulation
model_cod <- run_LASER(
  paramfile = config_cod,
  seed = 123
)
```

### Example 2: Multiple Locations

``` r
# Three-country configuration
config_multi <- make_LASER_config(
  date_start = "2023-01-01",
  date_stop = "2025-12-31",
  location_name = c("COD", "AGO", "TZA"),
  # Additional required parameters depend on model setup
  # See ?make_LASER_config for all options
)

model_multi <- run_LASER(
  paramfile = config_multi,
  seed = 456
)
```

**Note**:
[`make_LASER_config()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/make_LASER_config.md)
has 71+ parameters. Use
[`?make_LASER_config`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/make_LASER_config.md)
for complete documentation.

------------------------------------------------------------------------

## Modifying Existing Configurations

Start with a pre-configured model and modify specific parameters:

``` r
# Start with endemic configuration
config_modified <- config_simulation_endemic

# Modify specific parameters
config_modified$date_start <- "2025-01-01"
config_modified$date_stop <- "2026-12-31"
config_modified$seed <- 999

# Run with modifications
model_modified <- run_LASER(
  paramfile = config_modified,
  seed = 999
)
```

------------------------------------------------------------------------

## Controlling Randomness

Use the `seed` parameter for reproducibility:

``` r
# Same seed = identical results
run1 <- run_LASER(paramfile = config_default, seed = 123)
run2 <- run_LASER(paramfile = config_default, seed = 123)
identical(run1$results$expected_cases, run2$results$expected_cases)  # TRUE

# Different seeds = different stochastic outcomes
run3 <- run_LASER(paramfile = config_default, seed = 456)
identical(run1$results$expected_cases, run3$results$expected_cases)  # FALSE
```

------------------------------------------------------------------------

## Working with Specific Locations

To run models for specific countries, you can filter or create
configurations:

### Option 1: Use Existing Multi-Country Config

``` r
# config_default includes 40 countries
# To analyze one country, extract from results after running
model <- run_LASER(paramfile = config_default, seed = 123)

# Find index for DRC (COD)
cod_index <- which(model$params$location_name == "COD")

# Extract DRC results
cod_cases <- model$results$expected_cases[cod_index, ]
cod_deaths <- model$results$disease_deaths[cod_index, ]
```

### Option 2: Create Single-Country Config

``` r
# Use make_LASER_config() to create country-specific configuration
# (See "Creating Custom Configurations" section above)
```

------------------------------------------------------------------------

## Common Workflows

### Scenario Comparison

Compare baseline vs. intervention scenarios:

``` r
# Baseline scenario
baseline <- run_LASER(
  paramfile = config_simulation_endemic,
  seed = 100
)

# Intervention scenario (modify parameters)
config_intervention <- config_simulation_endemic
# Modify intervention-related parameters here
# (e.g., vaccination rates, treatment coverage)

intervention <- run_LASER(
  paramfile = config_intervention,
  seed = 100  # Same seed for direct comparison
)

# Compare outcomes
baseline_cases <- sum(baseline$results$expected_cases, na.rm = TRUE)
intervention_cases <- sum(intervention$results$expected_cases, na.rm = TRUE)

cases_averted <- baseline_cases - intervention_cases
percent_reduction <- (cases_averted / baseline_cases) * 100

cat("Cases averted:", cases_averted, "\n")
cat("Percent reduction:", percent_reduction, "%\n")
```

### Sensitivity Analysis

Test model sensitivity to parameter values:

``` r
# Test different transmission rates
beta_values <- seq(0.00001, 0.0001, length.out = 5)

sensitivity_results <- lapply(beta_values, function(beta) {
  config <- config_default
  config$beta_j0_hum <- rep(beta, length(config$location_name))

  run_LASER(paramfile = config, seed = 123)
})

# Extract total cases for each beta
total_cases_by_beta <- sapply(sensitivity_results, function(model) {
  sum(model$results$expected_cases, na.rm = TRUE)
})

# Plot relationship
plot(beta_values, total_cases_by_beta,
     type = "b",
     xlab = "Human transmission rate (beta)",
     ylab = "Total cases",
     main = "Sensitivity to transmission parameter")
```

------------------------------------------------------------------------

## Direct Python Interface (Advanced)

For advanced users who need direct access to the Python LASER module:

``` r
# Import laser-cholera Python module
lc <- reticulate::import("laser_cholera.metapop.model")

# Run model directly
model <- lc$run_model(
  paramfile = config_default,
  seed = 123,
  quiet = TRUE
)

# Access Python objects
model$results$expected_cases
model$results$disease_deaths
model$params
model$metadata
```

------------------------------------------------------------------------

## Output Options

Control where outputs are saved:

``` r
# Save outputs to specific directory
model <- run_LASER(
  paramfile = config_default,
  seed = 123,
  outdir = "/path/to/output"
)

# Generate visualizations
model_viz <- run_LASER(
  paramfile = config_default,
  seed = 123,
  visualize = TRUE,
  pdf = TRUE,
  outdir = "/path/to/figures"
)
```

------------------------------------------------------------------------

## Troubleshooting

### Check Dependencies

``` r
# Verify laser-cholera is installed
check_dependencies()

# View Python configuration
reticulate::py_config()

# Reinstall if needed
# install_dependencies()
```

### Common Issues

**Issue**: `Error: Python module laser_cholera not found` **Solution**:
Run
[`install_dependencies()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/install_dependencies.md)

**Issue**: Results not reproducible **Solution**: Set a fixed seed:
`run_LASER(paramfile = ..., seed = 123)`

**Issue**: Slow execution **Solution**: Use parallel execution (see
“Running Multiple Simulations” section)

------------------------------------------------------------------------

## Next Steps

- **Installation**: Set up MOSAIC on your system (Installation vignette)
- **Deployment**: Deploy on VMs or clusters (Deployment vignette)
- **Calibration**: For Bayesian calibration workflows, see calibration
  documentation
- **Parameter Estimation**: Explore `est_*` functions for data-driven
  parameters

------------------------------------------------------------------------

## Getting Help

- **Package documentation**:
  [`help(package = "MOSAIC")`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference)
- **Function help**:
  [`?run_LASER`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/run_LASER.md),
  [`?make_LASER_config`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/make_LASER_config.md)
- **Report issues**:
  <https://github.com/InstituteforDiseaseModeling/MOSAIC-pkg/issues>
- **Examples**: See `MOSAIC-pkg/model/` directory for complete workflows
