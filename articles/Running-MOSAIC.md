# Running MOSAIC

## Overview

This guide shows how to run MOSAIC simulations with **parameter
stochasticity** using the
[`run_MOSAIC()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/run_MOSAIC.md)
function. This workflow samples parameters from prior distributions and
runs multiple simulations to quantify uncertainty.

For running single deterministic simulations, see the **Running
simulations** vignette.

**Before you begin**: Complete the Installation vignette to set up
MOSAIC and its dependencies. Calibration is pure R, so the optional
Python environment is not needed and there is no need to call
[`attach_mosaic_env()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/attach_mosaic_env.md).

------------------------------------------------------------------------

## Run a fixed number of simulations for one location

``` r

# Load required packages
library(MOSAIC)


# Create output directory and set up logging
dir_output <- path.expand("~/MOSAIC/output/ETH_fixed")
if (!dir.exists(dir_output)) dir.create(dir_output, recursive = TRUE)

set_root_directory("~/MOSAIC")


priors_ETH <- get_location_priors(iso="ETH")
config_ETH <- get_location_config(iso="ETH")

control_ETH <- mosaic_control_defaults()

control_ETH$calibration$n_simulations <- 500
control_ETH$calibration$n_iterations <- 2

control_ETH$parallel$enable <- TRUE
control_ETH$parallel$n_cores <- parallel::detectCores()-1

# The posterior and the ensemble (the 1.4.0 defaults, shown for clarity)
control_ETH$targets$coarsening_alpha <- 10        # coarsened posterior: eta = alpha / n_obs
control_ETH$targets$posterior_weighting <- "bestfit"   # ensemble = best-fitting draws
control_ETH$targets$best_subset_ratio <- 0.2      # holding 0.2 x ESS_all of the posterior
control_ETH$targets$ESS_best <- 200               # ensemble ESS for a green status

control_ETH$sampling$sample_tau_i <- FALSE # Travel probability
control_ETH$sampling$sample_mobility_gamma <- FALSE   # Gravity model exponent
control_ETH$sampling$sample_mobility_omega <- FALSE   # Mobility rate

control_ETH$likelihood$weight_cases <- 1
control_ETH$likelihood$weight_deaths <- 0.05

control_ETH$predictions$n_iter_best <- 30
control_ETH$predictions$n_iter_ensemble <- 5


control_ETH$paths$clean_output <- TRUE
control_ETH$io <- mosaic_io_presets("fast")

result_ETH <- run_MOSAIC(
     dir_output = dir_output,
     config = config_ETH,
     priors = priors_ETH,
     control = control_ETH,
     resume = FALSE
)
```

## Run the adaptive simulation algorithm for one location

``` r

# Load required packages
library(MOSAIC)


# Create output directory and set up logging
dir_output <- path.expand("~/MOSAIC/output/ETH_auto")
if (!dir.exists(dir_output)) dir.create(dir_output, recursive = TRUE)

set_root_directory("~/MOSAIC")


priors_ETH <- get_location_priors(iso="ETH")
config_ETH <- get_location_config(iso="ETH")

control_ETH <- mosaic_control_defaults()

control_ETH$calibration$n_simulations <- NULL     # NULL = adaptive mode
control_ETH$calibration$n_iterations <- 2
control_ETH$calibration$batch_size_adaptive <- 500
control_ETH$calibration$min_batches_adaptive <- 5   # no stop before 5 batches ...
control_ETH$calibration$min_draws <- 5000           # ... and 5,000 draws
control_ETH$calibration$max_simulations_total <- 150000   # budget cap

control_ETH$parallel$enable <- TRUE
control_ETH$parallel$n_cores <- parallel::detectCores()-1

# The posterior and the ensemble (the 1.4.0 defaults, shown for clarity)
control_ETH$targets$coarsening_alpha <- 10        # coarsened posterior: eta = alpha / n_obs
control_ETH$targets$posterior_weighting <- "bestfit"   # ensemble = best-fitting draws
control_ETH$targets$best_subset_ratio <- 0.2      # holding 0.2 x ESS_all of the posterior
control_ETH$targets$ESS_best <- 200               # ensemble ESS for a green status

control_ETH$sampling$sample_tau_i <- FALSE
control_ETH$sampling$sample_mobility_gamma <- FALSE   # Gravity model exponent
control_ETH$sampling$sample_mobility_omega <- FALSE   # Mobility rate

control_ETH$likelihood$weight_cases <- 1
control_ETH$likelihood$weight_deaths <- 0.05

control_ETH$predictions$n_iter_best <- 30
control_ETH$predictions$n_iter_ensemble <- 5


control_ETH$paths$clean_output <- TRUE
control_ETH$io <- mosaic_io_presets("fast")

result_ETH <- run_MOSAIC(
     dir_output = dir_output,
     config = config_ETH,
     priors = priors_ETH,
     control = control_ETH,
     resume = FALSE
)
```

## Run the adaptive simulation algorithm for multiple locations

``` r

# Load required packages
library(MOSAIC)

# Create output directory and set up logging
dir_output <- path.expand("~/MOSAIC/output/ETH_KEN")
if (!dir.exists(dir_output)) dir.create(dir_output, recursive = TRUE)

set_root_directory("~/MOSAIC")

iso_codes <- c("ETH", "KEN")

priors <- get_location_priors(iso=iso_codes)
config <- get_location_config(iso=iso_codes)

control <- mosaic_control_defaults()

control$calibration$n_simulations <- NULL     # NULL = adaptive mode
control$calibration$n_iterations <- 3
control$calibration$batch_size_adaptive <- 500
control$calibration$min_batches_adaptive <- 5
control$calibration$min_draws <- 5000
control$calibration$max_simulations_total <- 150000

control$parallel$enable <- TRUE
control$parallel$n_cores <- parallel::detectCores()-1

control$targets$coarsening_alpha <- 10        # coarsened posterior: eta = alpha / n_obs
control$targets$max_ensemble_members <- 300   # bound the ensemble (memory grows with members)

control$sampling$sample_tau_i <- length(iso_codes) > 1 # Travel probability
control$sampling$sample_mobility_gamma <- length(iso_codes) > 1  # Gravity model exponent
control$sampling$sample_mobility_omega <- length(iso_codes) > 1  # Mobility rate

control$likelihood$weight_cases <- 1
control$likelihood$weight_deaths <- 0.05

control$predictions$n_iter_best <- 50
control$predictions$n_iter_ensemble <- 10

control$paths$clean_output <- TRUE
control$io <- mosaic_io_presets("fast")

control$logging$verbose <- TRUE

result <- run_MOSAIC(
     dir_output = dir_output,
     config = config,
     priors = priors,
     control = control,
     resume = FALSE
)
```

## Reading the result: the convergence status

Every run answers “are there enough simulations?” with one traffic-light
status in `3_results/summary.json`. The adaptive mode stops when its
sampling checks (ensemble ESS, Monte Carlo error, k-hat) are green at
two consecutive checks, or at `max_simulations_total`; a fixed-budget
run assesses it at the end. Whether the model fits (R², bias, coverage)
is reported separately and never changes the status.

``` r

s <- jsonlite::read_json(file.path(dir_output, "3_results", "summary.json"))
s[c("convergence_status", "convergence_label", "stopping_rule_met",
    "ess_best", "ess_best_target", "ess_all", "ess_all_target", "mce_p90",
    "khat_tempered", "ensemble_members", "sims_used", "sims_projected_for_target")]
unlist(s$convergence_reasons)
```

A yellow single-location run reads, for example:

    convergence_status        "yellow"
    convergence_label         "Usable with caution"
    stopping_rule_met         FALSE          (adaptive; null in fixed mode)
    ess_best / target         140 / 200      (ensemble ESS; green at >= 200, yellow >= 100)
    ess_all / target          700 / 1000     (posterior ESS over all draws = ess_best / 0.2)
    mce_p90                   0.081          (Monte Carlo error, 8.1% of the posterior SD; green <= 10%)
    khat_tempered             0.21           (validity guard; green < 0.5)
    convergence_reasons       "ensemble ESS 140 of 200: about 22k more simulations would reach the target"
    convergence_flags         "budget_cap_reached"

The four checks (ensemble ESS, Monte Carlo error, Pareto k-hat, data
informativeness), their bands (`control$targets$status_thresholds`), the
ESS and best-fit paths, the per-parameter Monte Carlo error and
prior-to-posterior KL, and the fit tables are in
`3_results/calibration_diagnostics.json`. With `plots = TRUE` they are
drawn as the calibration-diagnostics report,
`3_results/figures/diagnostics/calibration_report.pdf`
([`plot_model_calibration_report()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/plot_model_calibration_report.md)
re-draws it from the JSON).
