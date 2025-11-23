# MOSAIC 0.13.5

## Improvements

* **Use linear interpolation for NA values in observed data**
  - **Problem**: Previously converted all NAs to 0, artificially introducing "no cases" observations that could mislead the model
  - **Solution**: Linear interpolation within each location preserves temporal trends
  - **Method**:
    - Uses `approx(method = "linear", rule = 1)` to interpolate interior NAs
    - `rule = 1` prevents extrapolation beyond data range (preserves boundaries)
    - Only sets start/end NAs to 0 when interpolation is impossible (no surrounding data points)
    - Applies independently to each location for multi-location data
    - Handles both cases and deaths time series
  - **Example**: Time series `NA, NA, 10, 20, NA, 30, 40, NA, 50, NA` becomes `0, 0, 10, 20, 25, 30, 40, 45, 50, 0`
    - Interior NAs interpolated: position 5 → 25 (between 20 and 30), position 8 → 45 (between 40 and 50)
    - Boundary NAs set to 0: positions 1-2 (before first data), position 10 (after last data)
  - **Impact**: More accurate representation of missing data, better model training quality
  - **Output**:
    - Reports number of NAs interpolated vs. set to 0
    - "Interpolated: X" shows successful linear interpolation
    - "Set to 0 (start/end): Y" shows boundary NAs
  - **Files modified**: `R/npe_posterior.R`

# MOSAIC 0.13.4

## Bug Fixes

* **Add comprehensive data validation to train_npe() to catch NAs/Infs early**
  - **Problem**: PyTorch silently propagates NaN/Inf values through the network, causing cryptic training failures
  - **Root cause**: No validation of input data (X, y, weights) before tensor conversion
  - **Impact**: If parameters or observations contain NAs/Infs (from corrupted files, invalid samples, or bugs), they propagate silently and cause losses to become NaN
  - **Fix**: Add explicit validation checks for X (parameters), y (observations), and weights before tensor conversion (line 170-266 in npe.R)
  - **Validation checks**:
    - `anyNA(X)` and `any(!is.finite(X))` - parameters matrix
    - `anyNA(y)` and `any(!is.finite(y))` - observations matrix
    - `anyNA(weights)` and `any(!is.finite(weights))` - weight vector
  - **Error messages include**:
    - Which data structure failed (X, y, or weights)
    - Whether NAs or Infs were found
    - Affected rows and columns (first 5-10 shown)
    - Likely sources of corruption
    - Specific solutions to diagnose and fix
  - **Benefits**:
    - Catches data corruption BEFORE training starts (saves time)
    - Clear diagnostic messages pinpoint exact problem
    - Prevents silent NaN propagation through network
    - Helps identify upstream bugs in data preparation
  - **Files modified**: `R/npe.R`
  - **Note**: This addresses the user's concern that NA/NaN errors with 25k evenly weighted samples (binary_retained) should not be due to numerical instability, but rather data corruption

# MOSAIC 0.13.3

## Bug Fixes

* **Fix cryptic "missing value where TRUE/FALSE needed" error in NPE training**
  - **Root cause**: Validation loss became NA/NaN during training, causing `if (val_loss < best_val_loss)` comparison to fail with cryptic error
  - **Error**: `Error during wrapup: missing value where TRUE/FALSE needed` followed by recursive error and abort
  - **Trigger**: Low ESS (Kish: 1.2, Perplexity: 3.0) with `continuous_retained` weight strategy causing numerical instability
  - **Fix**: Add explicit NA/NaN checking for train_loss and val_loss before early stopping comparison (line 384-405 in npe.R)
  - **Impact**: Now provides clear, actionable error message with diagnostic information and solutions
  - **Error message includes**:
    - Which epoch failed and what the loss values were
    - Common causes (low ESS, weight concentration, architecture complexity)
    - Specific solutions (try 'continuous_best', use 'light' tier, reduce learning rate)
  - **Files modified**: `R/npe.R`
  - **Related**: This error was masked by recursive error handling - actual issue is numerical instability from degenerate weight distributions

# MOSAIC 0.13.2

## Bug Fixes

* **CRITICAL: Fix run_NPE() JSON loading causing persistent list column errors**
  - **Root cause**: Used `jsonlite::fromJSON(..., simplifyVector = FALSE)` when loading config/priors from disk, keeping JSON arrays as R lists instead of converting to vectors
  - **Error**: Even after v0.13.1 fix in get_npe_observed_data(), `config$reported_cases` was still a list, creating list columns in data frames
  - **Fix**: Replace `jsonlite::fromJSON()` with `read_json_to_list()` (MOSAIC standard loader) at 3 locations in run_NPE.R (lines 244, 264, 284)
  - **Benefits**:
    - Uses MOSAIC codebase standard for JSON loading (consistent with other functions)
    - Properly simplifies JSON arrays to R vectors (`simplifyVector = TRUE` default)
    - Supports gzipped JSON files
    - More maintainable and consistent
  - **Impact**: Resolves persistent CSV write errors in run_NPE() standalone mode
  - **Files modified**: `R/run_NPE.R`
  - **Verified**: JSON loading, get_npe_observed_data(), CSV write all succeed

# MOSAIC 0.13.1

## Bug Fixes

* **Fix list column error in get_npe_observed_data() causing CSV write failures**
  - **Root cause**: When `config$location_name` or `config$iso_code` is a list (common in LASER config format), single-bracket extraction `location_names[1]` returned a list of length 1 instead of scalar, creating list columns in data frames
  - **Error**: `Error in utils::write.table(...): unimplemented type 'list' in 'EncodeElement'` when writing observed_data.csv in run_NPE()
  - **Fix**: Convert location_names to character vector using `unlist()` at function start (line 1008)
  - **Fix**: Changed all `location_names[index]` to `location_names[[index]]` for scalar extraction (lines 1032, 1055, 1134)
  - **Impact**: NPE workflow now handles list-type location identifiers correctly
  - **Files modified**: `R/npe_posterior.R` (get_npe_observed_data function)
  - **Verified**: CSV write succeeds, data frame structure correct, existing tests still pass

# MOSAIC 0.13.0

## Major Features

* **New run_NPE() function for flexible Neural Posterior Estimation**
  - Complete NPE workflow extracted into standalone function in `R/run_NPE.R` (1000+ lines)
  - **Dual-mode architecture**:
    - **Embedded mode**: Runs inside `run_MOSAIC()` with in-memory objects (no disk I/O)
    - **Standalone mode**: Runs independently after calibration completes, loading from disk
  - **Key features**:
    - Automatic mode detection based on arguments provided
    - Custom `output_dir` support for experimenting with multiple NPE strategies
    - Root directory auto-detection from `getOption('root_directory')`
    - Full control object support for all NPE hyperparameters
    - Complete error handling and validation
  - **Benefits**:
    - Post-hoc NPE without re-running expensive BFRS calibration
    - Experiment with different weight strategies (continuous_best, continuous_retained, etc.)
    - Cleaner, more maintainable code architecture
    - Reusable in custom workflows
  - **run_MOSAIC.R refactored**: Replaced 300+ lines of inline NPE code with clean `run_NPE()` call (lines 1500-1523)
  - **Standalone examples added**: `vm/launch_mosaic.R` now includes post-hoc NPE usage examples (lines 201-233)
  - See function documentation: `?run_NPE`

# MOSAIC 0.11.5

## Changes

* **Simplify plot_model_ppc: Remove by_location argument**
  - Function now always creates both aggregate and per-location plots by default
  - **Removed argument**: `by_location` (previously: "aggregate", "both", "per_location")
  - **New behavior**: Always creates comprehensive diagnostics (aggregate + per-location)
  - Simplifies API - no configuration needed for plot output mode
  - Legacy model mode still only creates aggregate plots (as before)
  - Updated function signature: `plot_model_ppc(predictions_dir, predictions_files, locations, model, output_dir, verbose)`
  - Updated call in run_MOSAIC.R to remove by_location argument

# MOSAIC 0.11.4

## Bug Fixes

* **Add backward compatibility for plot_model_ppc function signature**
  - Wrapped plot_model_ppc call in run_MOSAIC with tryCatch to handle old package versions
  - **Issue**: Clusters with cached old package versions (pre-v0.11.0) have different function signature
  - Old signature: `plot_model_ppc(model, output_dir, verbose)`
  - New signature: `plot_model_ppc(predictions_dir, predictions_files, by_location, locations, model, output_dir, verbose)`
  - **Solution**: Try new signature first; if "unused arguments" error, log warning and skip PPC plots
  - Prevents workflow from crashing on clusters that need package reinstallation
  - Fixed at lines 1415-1441 in `run_MOSAIC.R`
  - **Note**: Users should reinstall package on cluster to get full PPC functionality

# MOSAIC 0.10.25

## Bug Fixes

* **CRITICAL: Fixed missing Prior/BFRS curves for seasonality parameters in distribution plots**
  - Fixed gsub order in parameter name variant generation for seasonality params
  - **Root cause**: Wrong order in `gsub()` calls generated incorrect variant "a1j" instead of "a1"
  - Original: `gsub("_j$", "", gsub("_", "", "a_1_j"))` → "a1j" (WRONG)
  - Fixed: `gsub("_", "", gsub("_j$", "", "a_1_j"))` → "a1" (CORRECT)
  - **Impact**: Prior/BFRS use `a1`, NPE uses `a_1_j` - variant "a1j" didn't match either
  - Plotting function now correctly finds seasonality params in all three JSONs
  - Fixed at lines 515-516 in `plot_model_distributions.R`
  - **Result**: All three curves (Prior, BFRS, NPE) now appear for seasonality parameters

# MOSAIC 0.10.23

## Bug Fixes

* **CRITICAL: Fixed missing NPE posteriors in distribution plots**
  - Added uniform distribution handling to `.fit_distribution()` in NPE posterior processing
  - **Root cause**: Function only handled beta, gamma, lognormal, normal - NOT uniform
  - When dist_type="uniform", function fell through to normal case, setting mean/sd instead of min/max
  - Result: NPE posteriors.json had `{distribution: "uniform", parameters: []}`
  - Plotting function couldn't plot uniform without min/max parameters → NPE curves missing
  - **Solution**: Calculate min/max from 1% and 99% quantiles (robust to outliers) + 1% buffer
  - Fixed at lines 1516-1528 in `npe_posterior.R`
  - **Impact**: NPE posteriors now appear in distribution plots (Prior vs BFRS vs NPE)

# MOSAIC 0.10.20

## Bug Fixes

* **CRITICAL: Fixed derived parameters not added to posteriors.json**
  - Modified `calc_model_posterior_distributions()` to dynamically add parameters missing from priors template
  - Derived parameters (beta_j0_hum, beta_j0_env) now correctly added to posteriors.json
  - **Root cause**: Function used priors.json as template, which only contains sampled parameters
  - **Solution**: Dynamically create parameter structure for derived parameters not in priors
  - Handles multiple locations correctly (adds each location as it's processed)
  - Fixed at lines 391-420 in `calc_model_posterior_distributions.R`
  - **Result**: beta_j0_hum and beta_j0_env now appear in distributions_ETH_Prior_Posterior.pdf

# MOSAIC 0.10.19

## Bug Fixes

* **Fixed missing derived parameters (beta_j0_hum, beta_j0_env) in distribution plots**
  - Changed distribution type from "derived" to "gamma" in estimated_parameters source data
  - Derived rate parameters (beta_j0_hum, beta_j0_env) now fitted with gamma distributions
  - **Background**: beta_j0_hum = p_beta × beta_j0_tot, beta_j0_env = (1 - p_beta) × beta_j0_tot
  - Previously marked as "failed" because "derived" distribution type was unhandled
  - Now appear in distributions_ETH_Prior_Posterior.pdf alongside beta_j0_tot and p_beta
  - Fixed at line 289 in `data-raw/make_estimated_parameters_inventory.R`

# MOSAIC 0.10.17

## Bug Fixes

* **Fixed plot_model_convergence diagnostic text formatting**
  - Corrected convergence metrics display in diagnostic plots
  - Fixed in `R/plot_model_convergence.R`

# MOSAIC 0.10.15

## Bug Fixes

* **CRITICAL: Fixed "subscript out of bounds" error in convergence diagnostic plots**
  - Fixed regression from v0.10.14 where `targets[["ESS_min"]]` would error if element missing
  - **Root cause**: `safe_numeric()` returned `numeric(0)` for NULL inputs, causing elements to be dropped from vectors
  - **Solution 1**: Enhanced `safe_numeric()` to check for NULL and empty vectors before conversion
  - **Solution 2**: Changed extraction from `[[` to `as.numeric(x["name"])` for safe handling of missing elements
  - `[[` throws error on missing elements; `as.numeric(x["name"])` returns NA safely
  - Fixed in both `plot_model_convergence.R` and `plot_model_convergence_loss.R`
  - Now properly handles cases where JSON diagnostics may have missing target values

# MOSAIC 0.10.14

## Bug Fixes

* **Fixed sprintf formatting errors in convergence diagnostic plots**
  - Fixed "Error formatting: ESS: %.0f [target >= %.0f]" messages in convergence_diagnostic.pdf
  - **Root cause**: Using single brackets `metrics["ESS"]` returns named vector element, not just value
  - **Solution**: Changed to double brackets `metrics[["ESS"]]` to extract raw values
  - Fixed in both `plot_model_convergence.R` (lines 214-223, 330-336) and `plot_model_convergence_loss.R` (lines 108-114)
  - All convergence metrics (ESS, A, CVw, B) now format correctly in diagnostic text

# MOSAIC 0.10.13

## Bug Fixes

* **Fixed plotting scale for extremely small initial condition posteriors (E_initial, I_initial)**
  - Implemented automatic detection of tiny values (< 0.001) in posterior distributions
  - Applied scientific notation formatting for x-axis labels when values < 0.001
  - Increased padding from 10% to 30% for better visibility of tiny value distributions
  - Fixed all 8 panels: Prior, Retained, Retained Weighted, Best Unweighted, Best Weighted, Caterpillar, Distributions (Empirical), Distributions (Theoretical)
  - **Root cause**: E_initial and I_initial have epidemiologically correct tiny values (~10⁻⁷ proportion, representing ~50-60 people in population of 126M), which appeared as flat lines when plotted on wide [0,1] axis
  - **Solution**: Tight x-axis limits with scientific notation (e.g., "2.0e-07", "4.0e-07", "6.0e-07")
  - Fixed throughout `plot_model_posteriors_detail.R` (lines 380-769)
  - See `claude/initial_EI_plotting_issue.md` for complete analysis

# MOSAIC 0.10.12

## Bug Fixes

* **Fixed missing dplyr namespace prefix in `plot_model_posteriors_detail()`**
  - Added explicit `dplyr::` prefix to `slice()` function call
  - Fixes "could not find function 'slice'" error
  - Fixed at line 908 in `plot_model_posteriors_detail.R`

# MOSAIC 0.10.11

## Bug Fixes

* **Fixed S7 class conflict with patchwork operators in `plot_model_posteriors_detail()`**
  - Replaced `/` operator with explicit `patchwork::wrap_plots(ncol=1)` calls
  - Fixes "Can't find method for generic `/(e1, e2)`" error from S7 class system
  - S7 was intercepting the patchwork `/` operator between ggplot objects
  - Using explicit `wrap_plots()` avoids operator dispatch conflicts
  - Fixed at lines 727-754 in `plot_model_posteriors_detail.R`

# MOSAIC 0.10.10

## Bug Fixes

* **CRITICAL: Fixed incorrect weighting in NPE ensemble predictions**
  - Removed density-based weighting that caused double-weighting artifact
  - NPE samples are drawn directly from posterior p(θ|x), so uniform weights are correct
  - Using density as weights was creating effective distribution [p(θ|x)]²
  - This over-emphasized high-density regions and under-represented uncertainty
  - Now uses uniform weights (NULL) for proper posterior predictive sampling
  - Fixed at lines 1679-1686 in `run_MOSAIC.R`
  - **Impact:** NPE ensemble predictions should now have appropriate uncertainty bands
  - **Theory:** For posterior predictive p(y|x) ≈ (1/N) Σ p(y|θᵢ) where θᵢ ~ p(θ|x)
  - Density weights only correct for importance sampling from q(θ) ≠ p(θ|x)
  - See `claude/npe_weighting_analysis.md` for complete theoretical analysis

# MOSAIC 0.10.9

## Bug Fixes

* **Fixed missing namespace prefixes in `plot_model_posteriors_detail()`**
  - Added explicit `ggplot2::` prefixes to all ggplot2 functions (68+ occurrences)
  - Added `grid::` prefix for `unit()` calls
  - Added `arrow::` prefix for `read_parquet()`
  - Added `patchwork::` prefixes for `wrap_plots()` and `plot_layout()`
  - Added `cowplot::` prefixes for `plot_grid()` and `get_legend()`
  - Fixes "could not find function 'geom_histogram'" error
  - Fixed throughout `plot_model_posteriors_detail.R`

# MOSAIC 0.10.8

## Bug Fixes

* **Fixed inappropriate hard-coded bounds for truncated normal distribution**
  - Replaced arbitrary -45/45 defaults with -Inf/Inf to match fitting function behavior
  - Added intelligent plotting range selection for infinite bounds
  - Use mean ± 4sd for plotting range when bounds are infinite (covers 99.99%)
  - Properly handle one-sided truncation (e.g., a=-Inf, b=10)
  - Format display strings to show "Inf" for infinite bounds
  - Fixed at lines 445-481 in `plot_model_distributions.R`

# MOSAIC 0.10.7

## Bug Fixes

* **CRITICAL: Fixed remaining NA handling error in `calc_distribution_density()`**
  - Fixed "missing value where TRUE/FALSE needed" error for truncated normal distribution
  - Completed NA handling fix missed in v0.10.6
  - Fixed truncnorm distribution at lines 446-453 in `plot_model_distributions.R`
  - Now all distribution types properly handle NULL parameters

# MOSAIC 0.10.6

## Bug Fixes

* **CRITICAL: Fixed NA handling error in `calc_distribution_density()`**
  - Fixed "missing value where TRUE/FALSE needed" error in `plot_model_distributions()`
  - Replaced unsafe `as.numeric(NULL)` pattern with explicit `NA_real_` conversion
  - Fixed for uniform, normal, and gompertz distributions (lines 402-427)
  - Prevents crashes when parameter bounds are NULL

* **Fixed all ggplot2 3.4.0+ deprecation warnings**
  - Replaced deprecated `size=` with `linewidth=` in `geom_line()` (14 instances)
  - Replaced deprecated `size=` with `linewidth=` in `geom_smooth()` (2 instances)
  - Replaced deprecated `size=` with `linewidth=` in `element_line()` (10 instances)
  - Replaced deprecated `size=` with `linewidth=` in `geom_vline()` (6 instances)
  - Fixed across 10 files: plot_generation_time.R, plot_vibrio_decay_rate.R, est_symptomatic_prop.R, plot_suspected_cases.R, plot_CFR_by_country.R, plot_vaccine_effectiveness.R, est_WASH_coverage.R, npe_plots.R, plot_africa_map.R, plot_model_distributions.R

# MOSAIC 0.10.5

## Bug Fixes

* Fixed broken documentation links to deprecated `run_mosaic_iso()`
  - Removed references to `run_mosaic_iso()` in `run_MOSAIC()` documentation
  - Fixes roxygen2 warnings about unresolvable links
  - Updated @description and removed @seealso reference

# MOSAIC 0.10.4

## Bug Fixes

* Fixed missing namespace prefixes in `plot_npe_training_loss()`
  - Added explicit `ggplot2::` prefixes to all ggplot2 functions throughout the function
  - Fixes "could not find function 'facet_wrap'" error during NPE training visualization
  - Fixed throughout lines 1585-1768 including: `facet_wrap`, `ggplot`, `aes`, `geom_smooth`, `geom_line`, `geom_vline`, `geom_point`, `geom_text`, `labs`, `theme_minimal`, `theme`, and all theme element functions

# MOSAIC 0.10.3

## Bug Fixes

* Fixed missing `ggsave()` namespace prefix in `plot_model_distributions()`
  - Added explicit `ggplot2::ggsave()` prefix at two save locations
  - Fixes "could not find function 'ggsave'" error when saving plots
  - Fixed for both global parameters plot (line 834) and location-specific plots (line 980)

# MOSAIC 0.10.2

## Bug Fixes

* Fixed missing namespace prefixes in `plot_model_distributions()`
  - Added explicit `ggplot2::` prefixes to `scale_x_continuous()` and all other ggplot2 functions
  - Fixes "could not find function 'scale_x_continuous'" error in `create_multi_method_plot()`
  - Fixed in three locations: main plot creation and two legend plot sections
  - Also added `grid::unit()` prefix for grid package function

# MOSAIC 0.10.1

## Bug Fixes

* Fixed missing namespace prefixes in `plot_model_posterior_quantiles()`
  - Added explicit `ggplot2::` prefixes to all ggplot2 functions
  - Fixes "could not find function 'geom_errorbar'" error
  - Functions not imported in NAMESPACE now called with explicit prefix
  - Affects both global and location-specific plotting sections

# MOSAIC 0.10.0

## Breaking Changes

* **Major refactor: Lean run_MOSAIC() - aggressive simplification**
  - Moved `run_mosaic_iso()` to `deprecated/` directory (use `run_MOSAIC()` directly)
  - Stripped ESS calculation: removed conditional skipping, verbose summaries
  - Stripped convergence diagnostics: removed section banners, verbose logging
  - Stripped posterior sections: removed verbose progress, set verbose=FALSE everywhere
  - Stripped PPC sections: removed conditional checks, verbose status messages
  - Stripped parameter uncertainty: removed ensemble logging
  - Stripped NPE section: removed 66 log_msg calls, verbose diagnostics, section banners
  - Stripped POST-HOC optimization: removed tier-by-tier logging, convergence messages
  - Stripped WEIGHTS section: removed detailed ESS/temperature logging
  - Removed all major section banners (80× '=' decorative headers)
  - Pattern applied: calculate → write → log filepath (no defensive checks)
  - Total: removed 180+ verbose log_msg calls and 90+ section banners
  - **Result: 284 lines removed (2441 → 2157 lines, 12% reduction)**

## Deprecations

* `run_mosaic_iso()` - Use `run_MOSAIC()` with `get_location_config()` and `get_location_priors()` instead

# MOSAIC 0.9.1

## Bug Fixes

* Fixed premature cleanup of `ess_results` variable in `run_MOSAIC()`
  - Removed cleanup at line 1204 that occurred before `calc_convergence_diagnostics()` call
  - Variable is now retained until after convergence diagnostics are calculated
  - Fixes "object 'ess_results' not found" error at runtime

# MOSAIC 0.9.0

## Breaking Changes

* **Major refactor: Removed ALL excessive flow control from `run_MOSAIC()`**
  - Removed validation wrapper around `calc_convergence_diagnostics()` (90 lines → 30 lines)
  - Removed ALL tryCatch blocks around plotting functions (10+ instances)
  - Removed tryCatch around `calc_model_posterior_quantiles()`
  - Removed tryCatch around `calc_model_posterior_distributions()`
  - Removed tryCatch around `sample_parameters()` for best model
  - Removed tryCatch around `lc$run_model()` for best model
  - Removed tryCatch around `plot_model_fit_stochastic()` and related plotting
  - Removed defensive if-else wrapper around NPE posterior samples processing
  - Functions now fail fast with clear error messages at the source
  - **Impact:** Errors will stop execution immediately rather than continuing with NA/NULL values
  - **Benefit:** Much easier to debug - errors show exactly where the problem is
  - **Note:** Parallel worker tryCatch blocks retained (essential for batch processing)

## Why This Change?

Excessive defensive programming was masking real errors and making debugging extremely difficult.
The new fail-fast approach:
- ✅ Errors happen at the source with clear tracebacks
- ✅ Simpler code flow that's easier to understand and maintain
- ✅ Forces fixing root causes instead of papering over problems
- ✅ Better for research/development workflows
- ✅ Reduces code complexity (~150+ lines of error handling removed)

**If you encounter errors after upgrading:** The errors were always there, just hidden.
Fix the underlying issue rather than relying on fallback behavior.

# MOSAIC 0.8.9

## Bug Fixes

* Fixed uninitialized variable in `run_MOSAIC()`
  - Added defensive initialization of `ess_results` to NULL before parameter-specific ESS calculation
  - Prevents "object not found" error when debugging or if code execution is interrupted
  - Variable is properly set later based on sample availability (lines 1161 or 1164)

# MOSAIC 0.8.8

## Bug Fixes

* Fixed test failures in `test-calc_convergence_diagnostics.R`
  - Corrected threshold expectations for "lower is better" metrics (CVw)
  - Changed test values to properly demonstrate warn status (within 120-200% of target)
  - Updated helper function tests to use `MOSAIC:::` for internal function access
  - All 70 tests now pass

# MOSAIC (development version)

## New Features

### Initial Conditions Sampling
* Added biologically plausible Beta priors for initial condition compartments (S, V1, V2, E, I, R)
  - `prop_S_initial`: Beta(30, 7.5) - mean 80% susceptible
  - `prop_V1_initial`: Beta(0.5, 49.5) - mean 1% one-dose vaccination  
  - `prop_V2_initial`: Beta(0.5, 99.5) - mean 0.5% two-dose vaccination
  - `prop_E_initial`: Beta(0.01, 9999.99) - mean 0.0001% exposed
  - `prop_I_initial`: Beta(0.01, 9999.99) - mean 0.0001% infected
  - `prop_R_initial`: Beta(3.5, 14) - mean 20% recovered/immune

* Enhanced `sample_parameters()` function:
  - New `sample_initial_conditions` argument to control IC sampling
  - Automatic normalization of compartment proportions to sum to 1.0
  - Proper conversion from proportions to integer counts
  - Rounding error adjustment to ensure exact population totals

* Updated `create_sampling_args()` helper:
  - Added "initial_conditions_only" pattern for sampling just ICs
  - Support for new `sample_initial_conditions` flag

## Data Updates

* Renamed `priors` data object to `priors_default` to match naming convention with `config_default`
* Updated `priors_default` data object to include initial condition priors for all 40 African countries
* Priors now available in both R data format (`data/priors_default.rda`) and JSON (`inst/extdata/priors.json`)

## Documentation

* Added comprehensive documentation for the `priors_default` data object
* Updated `sample_parameters()` documentation to reflect IC sampling capability
* Added examples demonstrating initial conditions sampling workflow

## Internal Changes

* Added `sample_initial_conditions_impl()` internal function for IC sampling logic
* Updated NAMESPACE to import required stats functions (rbeta, rgamma, rlnorm, rnorm, runif)