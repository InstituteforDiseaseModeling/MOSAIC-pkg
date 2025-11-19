# SMC Weight Collapse Analysis

## Issue Summary

**Symptoms:**
1. ESS = 1 (only 0.2% of 500 samples)
2. All posterior quantiles collapse to single point estimates
3. All 500 resampled posterior samples are identical (sample #432 selected 500 times)
4. Plots appear to show variation (likely showing NPE posterior, not SMC posterior)

**Status:** This is NOT a bug - it's the mathematically correct SMC behavior when the NPE approximation is very poor.

---

## Diagnostic Results

### Weight Distribution
```
Total samples: 500
Non-zero weights (>1e-10): 1
Sample with max weight: #432 (weight = 1.0)
All other samples: weight ≈ 0
ESS: 1.0 (0.2%)
```

### Log Weight Components (Sample #432 - the winner)
```
log_prior:      -54.24
log_likelihood: -5256.43  (BEST likelihood of all 500 samples!)
log_q_npe:      -136.56
log_weight:     -5174.11  (= -54.24 + -5256.43 - (-136.56))
```

### Gap Analysis
```
Max log weight:     -5174.11  (sample #432)
2nd best log weight: -5333.73  (unknown sample)
Gap:                 159.62

Linear weight ratio: exp(159.62) ≈ 1.2 × 10^69

This is why sample #432 gets 100% of the weight!
```

### Component Ranges
```
Log Prior Range:       [-70.84, -32.57]    (range: 38 log units)
Log Likelihood Range:  [-582,231, -5,256]  (range: 576,975 log units!!!)
Log q_NPE Range:       [-136.56, -81.06]   (range: 55 log units)
```

---

## Root Cause Analysis

### The Mathematics

The SMC weight formula is:
```
w̃ᵢ ∝ p(θᵢ) · p(x|θᵢ) / q_φ(θᵢ|x)
```

In log space:
```
log w̃ᵢ = log p(θᵢ) + log p(x|θᵢ) - log q_φ(θᵢ|x)
```

### What's Happening

1. **Likelihood dominates**: The likelihood range (576,975 log units) is 10,000× larger than the prior range (38 log units) or NPE range (55 log units)

2. **NPE missed the high-likelihood region**: The NPE learned a distribution that doesn't cover the region of parameter space with good likelihoods

3. **Most samples have terrible likelihoods**:
   - Sample #432: log_likelihood = -5,256 ✓ (relatively good)
   - Most samples: log_likelihood < -10,000 ✗ (extremely bad)
   - Worst samples: log_likelihood < -500,000 ✗✗✗ (catastrophically bad)

4. **Weight collapse is mathematically correct**: When one sample has a likelihood that's exp(160) ≈ 10^69 times better than the next best, it should get essentially all the weight

### Why NPE Failed

The NPE diagnostics likely showed poor performance:
- Low coverage (not covering high-likelihood regions)
- Poor calibration
- Training didn't converge properly
- Insufficient training data
- Model architecture too simple for the complexity

---

## Explanation of Symptoms

### Why ESS = 1?
```
ESS = 1 / sum(wᵢ²)

When w₁ = 1.0 and all others ≈ 0:
ESS = 1 / (1² + 0² + ... + 0²) = 1 / 1 = 1
```

### Why All Quantiles Are Identical?

The resampling process:
```r
sample(1:500, size=500, replace=TRUE, prob=weights)
```

When `weights = c(1.0, 0, 0, ..., 0)`, `sample()` will select index 432 all 500 times.

Result: All 500 rows of `smc_posterior` are identical copies of `npe_samples[432,]`.

When computing quantiles of 500 identical values:
```
q0.025 = q0.25 = q0.5 = q0.75 = q0.975 = <that single value>
```

### Why Plots Show Variation?

The user likely saw plots of:
1. **NPE posterior** - shows variation (500 different samples from NPE)
2. **NPE vs SMC comparison** - would show NPE with variation, SMC as single point

The SMC posterior has zero variation by construction.

---

## Is This a Bug?

**No.** The SMC implementation is mathematically correct. The issue is:

1. **Poor NPE approximation**: The NPE failed to learn the true posterior
2. **Correct importance sampling**: SMC correctly identified that 499/500 NPE samples have negligible probability under the true posterior

This is actually SMC working as designed - it's exposing that the NPE approximation is unreliable.

---

## Solutions and Mitigations

### Option 1: Improve NPE Training (RECOMMENDED)
**Goal**: Train NPE to cover high-likelihood regions better

**Actions**:
1. Review NPE diagnostics (coverage, calibration)
2. Increase training samples (if too few)
3. Increase training epochs (if not converged)
4. Try different architecture (more layers, attention, etc.)
5. Check prior bounds (may be too wide or too narrow)
6. Use better summary statistics (if they exist)

**Pros**: Fixes root cause, improves NPE reliability
**Cons**: Requires retraining (potentially expensive)

### Option 2: Increase Sample Size
**Goal**: Find more high-likelihood samples by brute force

**Actions**:
```r
smc_result <- compute_smc_posterior(
    sample_subset = 5000,  # Was 500
    ...
)
```

**Pros**: Easy to implement, might find more good samples
**Cons**: 10× more simulations = 10× more compute time, doesn't fix NPE

**Expected outcome**: ESS might improve from 1 to 5-10, but likely still poor

### Option 3: Use Defensive Resampling (NEW FEATURE)
**Goal**: Resample with mixture of SMC weights and uniform to prevent complete collapse

**Implementation**:
```r
# Mix SMC weights with uniform distribution
alpha <- 0.1  # 10% uniform, 90% SMC
mixed_weights <- alpha * (1/n) + (1-alpha) * smc_weights
```

**Pros**: Preserves some NPE variation while correcting toward truth
**Cons**: Not pure exact posterior anymore, requires new parameter tuning

### Option 4: Use BFRS Posterior Instead
**Goal**: Avoid SMC correction entirely if NPE is this poor

**Rationale**: If NPE is so bad that ESS=1, the BFRS posterior (which is a smooth fit to simulations) might be more reliable than the NPE-based SMC correction

**Pros**: BFRS is robust to NPE failures
**Cons**: Not exact posterior, less theoretically justified

### Option 5: Adaptive SMC (ADVANCED)
**Goal**: Use SMC to identify good regions, then resample NPE there

**Implementation**: Multi-stage process
1. Run SMC with current NPE samples
2. Identify high-weight regions
3. Retrain NPE focused on those regions
4. Run SMC again with new NPE

**Pros**: Iteratively improves approximation
**Cons**: Complex, requires multiple NPE training rounds

---

## Recommended Action Plan

### Immediate (for current run)
1. **Accept the results as-is**: The SMC posterior is mathematically correct, just highly concentrated
2. **Report ESS in diagnostics**: Make it clear to users that SMC found only 1 effective sample
3. **Compare with BFRS**: Check if BFRS posterior is similar to sample #432
4. **Document limitation**: Note that SMC correction failed due to poor NPE approximation

### Short-term (next calibration run)
1. **Increase sample_subset to 2000-5000**: More samples might find more good regions
2. **Review NPE diagnostics**: Check coverage and calibration metrics
3. **Try defensive resampling**: Implement Option 3 as experimental feature

### Long-term (improve methodology)
1. **Improve NPE training**:
   - Better diagnostics during training
   - Early stopping when coverage/calibration fail
   - Architecture search (try different MAF configurations)
   - More training data if needed
2. **Consider adaptive SMC**: Implement iterative refinement
3. **Develop ESS-based workflow**: Automatically switch to BFRS if SMC ESS < threshold

---

## Technical Note: When Is SMC Unreliable?

SMC importance sampling works well when:
- NPE is a good approximation (ESS > 10-20% of samples)
- NPE covers the high-posterior-mass regions
- Likelihood doesn't vary too wildly (range < 100 log units)

SMC fails when:
- NPE is very poor (ESS < 1% of samples) ← **Current situation**
- NPE completely misses important regions
- Likelihood has huge dynamic range (> 1000 log units) ← **Current situation**

**Rule of thumb**: If ESS < 0.5% (5 effective samples per 1000), SMC correction is unreliable and results should be interpreted with extreme caution or discarded in favor of BFRS/NPE.

---

## Code Changes Needed

### 1. Add ESS-based Warning (HIGH PRIORITY)

Location: `R/smc.R` in `compute_smc_posterior()`

```r
# After computing weights
if (weights_result$ess < 0.005 * n_samples_used) {
    warning("CRITICAL: ESS < 0.5% (", round(weights_result$ess), " / ", n_samples_used, ")",
           "\nSMC correction is unreliable. The NPE approximation is very poor.",
           "\nRecommendations:",
           "\n  1. Review NPE training diagnostics",
           "\n  2. Consider using BFRS posterior instead",
           "\n  3. Increase sample_subset to 2000-5000",
           "\n  4. Retrain NPE with better configuration",
           call. = FALSE)
}
```

### 2. Save Unique Sample Count (MEDIUM PRIORITY)

Location: `R/smc_utils.R` in `.save_smc_results()`

```r
# Add to diagnostics
diagnostics <- list(
    ...
    n_unique_resampled = if(!is.null(smc_result$smc_posterior)) {
        nrow(unique(smc_result$smc_posterior))
    } else {
        NA
    },
    ess_percent = 100 * smc_result$weights$ess / smc_result$n_subset,
    ess_warning = smc_result$weights$ess < 0.005 * smc_result$n_subset,
    ...
)
```

### 3. Implement Defensive Resampling (OPTIONAL)

Location: New parameter in `resample_smc_posterior()`

```r
resample_smc_posterior <- function(
    npe_samples,
    smc_weights,
    n_resample = NULL,
    defensive_alpha = 0.0,  # NEW: 0=pure SMC, 0.1=10% uniform
    verbose = FALSE
) {
    # Mix with uniform if requested
    if (defensive_alpha > 0) {
        uniform_weights <- rep(1/length(smc_weights), length(smc_weights))
        mixed_weights <- defensive_alpha * uniform_weights + (1-defensive_alpha) * smc_weights
        mixed_weights <- mixed_weights / sum(mixed_weights)

        if (verbose) {
            message("Using defensive resampling (alpha=", defensive_alpha, ")")
        }
        smc_weights <- mixed_weights
    }

    # Rest of function unchanged...
}
```

---

## Conclusion

This is **not a bug** - it's SMC correctly identifying that the NPE approximation is extremely poor. The solution is to improve the NPE training, not to modify the SMC algorithm.

However, we should add warnings to alert users when ESS is critically low, as this indicates the SMC results are unreliable.
