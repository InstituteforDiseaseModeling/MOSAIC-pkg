# Test and Diagnostic Functions - Deprecated

**Date**: 2025-11-14
**Status**: DEPRECATED - Test functions with no usage
**Total Functions**: 4 functions (1 function incorrectly identified)

---

## Summary

These functions were created for testing or diagnostic purposes but are not used in the current codebase. They can be safely removed or moved to the test suite.

## Functions Actually Deprecated

### From fit_beta_from_ci.R (2 functions)

1. **test_beta_fitting**
   - Test function for beta distribution fitting
   - Zero usage detected
   - **Action**: Remove from R/fit_beta_from_ci.R

2. **compare_distributions**
   - Diagnostic function for comparing distributions
   - Zero usage detected
   - **Action**: Remove from R/fit_beta_from_ci.R

### From fit_gamma_from_ci.R (1 function)

3. **test_gamma_fitting**
   - Test function for gamma distribution fitting
   - Zero usage detected
   - **Action**: Remove from R/fit_gamma_from_ci.R

### From calc_model_ess_parameter.R (1 function)

4. **generate_test_data_for_ess**
   - Test data generator for ESS calculation
   - Zero usage detected
   - **Action**: Remove from R/calc_model_ess_parameter.R

### From get_effective_aic_range_alternatives.R (1 function)

5. **compare_approaches** ✓ ALREADY MOVED
   - Comparison function for AIC calculation methods
   - Zero usage detected
   - **Status**: File moved to deprecated/ (2025-11-14)

---

## Function Incorrectly Identified (KEEP - Actively Used)

### From est_suitability.R (1 function) - DO NOT REMOVE

6. **create_lstm_sequences**
   - **Status**: ACTIVELY USED (lines 462, 475)
   - Internal helper function for `est_suitability()`
   - **Analysis Error**: Original analysis missed internal usage within the same file

---

## Removal Instructions

### For fit_beta_from_ci.R
Remove these 2 test functions:
- `test_beta_fitting`
- `compare_distributions`

### For fit_gamma_from_ci.R
Remove this test function:
- `test_gamma_fitting`

### For calc_model_ess_parameter.R
Remove this test function:
- `generate_test_data_for_ess`

---

**Recommendation**: Remove test functions immediately. They add no value to the package and can be recreated in tests/ if needed.
