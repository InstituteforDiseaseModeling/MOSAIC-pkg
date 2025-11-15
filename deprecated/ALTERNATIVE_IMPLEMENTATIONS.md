# Alternative Implementation Functions - Deprecated

**Date**: 2025-11-14
**Status**: PARTIALLY DEPRECATED - Analysis corrected
**Total Functions**: 2 functions (5 functions incorrectly identified)

---

## Summary

This document originally listed 7 functions as "alternative implementations". After further analysis, **only 2 functions** from `get_effective_aic_range_alternatives.R` are truly unused alternatives. The 5 functions from `calc_model_likelihood.R` are **ACTIVELY USED** internal helpers and should NOT be deprecated.

## Functions Actually Deprecated

### From get_effective_aic_range_alternatives.R (2 functions) ✓ MOVED

1. **get_effective_aic_range_empirical**
   - Alternative AIC range calculation (empirical method)
   - Exported function
   - Zero usage detected
   - **Status**: File moved to deprecated/

2. **get_effective_aic_range_likelihood**
   - Alternative AIC range calculation (likelihood method)
   - Exported function
   - Zero usage detected
   - **Status**: File moved to deprecated/

---

## Functions Incorrectly Identified (KEEP - Actively Used)

### From calc_model_likelihood.R (5 functions) - DO NOT REMOVE

These functions were incorrectly labeled as "alternative implementations with zero usage". They are **actively used internal helpers** called by the main `calc_model_likelihood()` function:

1. **calc_multi_peak_magnitude_ll** - Used on lines 279, 285
2. **calc_multi_peak_timing_ll** - Used on lines 264, 270
3. **compute_wis_parametric_row** - Used on lines 314, 318
4. **ll_cumulative_progressive_nb** - Used on lines 298, 299
5. **max_ll_poisson** - Used on lines 306, 307

**Analysis Error**: The original usage analysis only counted calls from outside the defining file, missing internal usage within the same file.

---

## Action Taken

**File**: `R/get_effective_aic_range_alternatives.R`
- ✓ Entire file moved to `deprecated/` (2025-11-14)

**File**: `R/calc_model_likelihood.R`
- ✗ NO ACTION - Functions are active internal helpers, not alternatives

---

**Recommendation**: The deprecation of get_effective_aic_range_alternatives.R is complete. No further action needed for calc_model_likelihood.R functions.
