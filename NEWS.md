# MOSAIC 0.10.16

## Bug Fixes

* **Fixed missing derived parameters (beta_j0_hum, beta_j0_env) in distribution plots**
  - Added handling for "derived" distribution type in `calc_model_posterior_distributions()`
  - Derived rate parameters (beta_j0_hum, beta_j0_env) now fitted with gamma distributions
  - **Background**: beta_j0_hum = p_beta × beta_j0_tot, beta_j0_env = (1 - p_beta) × beta_j0_tot
  - Previously marked as "failed" because "derived" type was unhandled
  - Now appear in distributions_ETH_Prior_Posterior.pdf alongside beta_j0_tot and p_beta
  - Fixed at lines 303-314 in `calc_model_posterior_distributions.R`

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