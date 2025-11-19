# Unused Internal Helper Functions - Deprecation List

**Date**: 2025-11-14
**Status**: DEPRECATED - Safe to remove
**Total Functions**: 41 internal helpers across 33 files

---

## Summary

This document lists all internal helper functions (not exported) that have **ZERO usage** anywhere in the codebase. These were identified through comprehensive usage analysis and are safe to remove.

## Removal Instructions

These functions can be safely deleted from their source files. They are internal helpers with no callers.

---

## Functions by File

### calc_spatial_correlation.R
- **vec_ok** - Vector validation helper

### check_likelihood_guardrails.R
- **check_cumulative_mismatches** - Cumulative data validation

### combine_vaccination_data.R
- **find_matches** - Data matching helper

### convert_matrix_to_config.R
- **should_update_parameter** - Parameter update logic helper

### create_results_schema.R
- **get_col_indices** - Column index extraction
- **insert_metadata** - Metadata insertion helper
- **insert_parameters** - Parameter insertion helper

### est_demographic_rates.R
- **convert_to_date** - Date conversion helper
- **smooth_series** - Time series smoothing helper

### est_epidemic_peaks.R
- **calculate_peak_interval** - Peak timing calculation

### est_immune_decay_vaccine.R
- **objective_function** - Optimization objective (unused)

### est_initial_E_I.R
- **draw_loc_or_default** - Location-specific drawing helper
- **mc_function** - Monte Carlo helper function

### est_initial_R.R
- **process_location** - Location processing helper

### est_initial_S.R
- **build_prior** - Prior construction helper

### est_initial_V1_V2_location.R
- **allocate_second_doses** - Dose allocation logic

### est_seasonal_dynamics.R
- **normalize_if_needed** - Normalization helper

### est_WASH_coverage.R
- **calculate_weighted_correlation** - Weighted correlation calculation

### fit_beta_from_ci.R
- **compare_distributions** - Distribution comparison
- **objective** - Optimization objective for beta fitting

### fit_gompertz_from_ci.R
- **obj_t** - Objective function for Gompertz fitting

### fit_truncnorm_from_ci.R
- **find_truncnorm_mode** - Mode finding for truncated normal

### get_DMI_forecast.R
- **process_dmi_text** - DMI forecast text parsing

### get_effective_aic_range_alternatives.R
- **compare_approaches** - Approach comparison helper

### get_ENSO_forecast.R
- **combine_with_recency_priority** - Forecast combination logic
- **process_enso_text** - ENSO forecast text parsing

### get_ENSO_historical.R
- **process_enso_historical** - Historical ENSO data processing

### get_WHO_vaccine_data.R
- **get_column_names** - Column name extraction
- **get_lines** - Line reading helper
- **process_raw_text** - Raw text processing

### manage_forecast_data.R
- **get_freshness_status** - Data freshness checking

### plot_model_distributions.R
- **unnest_json** - JSON unnesting helper

### plot_model_fit_stochastic_param.R
- **calculate_overall_stats** - Overall statistics calculation

### plot_model_fit_stochastic.R
- **calculate_stats** - Statistics calculation helper

### plot_model_posteriors_detail.R
- **unwrap_value** - Value unwrapping helper

### plot_vaccination_data.R
- **get_last_cumsum** - Cumulative sum extraction

### plot_vaccine_effectiveness.R
- **get_param** - Parameter extraction helper

### process_cholera_surveillance_data.R
- **clean_in** - Input data cleaning

### process_WHO_annual_data.R
- **capitalize_words** - Text capitalization helper

### read_hdf5_to_list.R
- **read_group** - HDF5 group reading helper

### run_WHO_annual_data_app.R
- **server** - Shiny server function (unused, likely old version)

---

## Removal Strategy

### Option 1: Systematic Removal (Recommended)
Remove functions one file at a time, testing after each change:

```bash
# For each file:
1. Open the file
2. Locate and delete the unused helper function(s)
3. Save the file
4. Run: Rscript -e "devtools::document(); devtools::test()"
5. If tests pass, commit the change
6. Move to next file
```

### Option 2: Batch Removal (Faster, Higher Risk)
Remove all unused helpers at once:

```bash
# After removing all functions:
Rscript -e "devtools::document()"
Rscript -e "devtools::test()"
R CMD check .
```

---

## Analysis Methodology

Functions were identified as unused through:
1. Regex search for function definitions across all R/ files
2. Usage analysis across R/, model/, and tests/ directories
3. Verification that functions are not exported (`@export` tag)
4. Confirmation of zero call sites in codebase

## Notes

- All listed functions are **internal** (not exported to package namespace)
- Zero usage detected means no calls found in: R functions, model scripts, or tests
- Functions may still be called from:
  - External scripts not analyzed
  - Dynamic calls via `do.call()` or `get()`
  - Commented-out experimental code

---

## Post-Removal Verification

After removing functions, verify with:

```r
# Check package builds
devtools::document()
devtools::check()

# Run tests
devtools::test()

# Build and install
R CMD build .
R CMD INSTALL MOSAIC_*.tar.gz
```

---

**Estimated Impact**: Removing these 41 functions will reduce codebase by ~400-600 lines while maintaining 100% functionality.

**Risk Level**: LOW - All functions have zero usage in analyzed codebase.

**Next Step**: Review this list and begin systematic removal, OR keep for reference and remove during next major refactoring.
