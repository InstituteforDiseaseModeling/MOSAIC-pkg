# SMC Weights Usage Guide

## Overview

SMC importance weights serve to correct the NPE approximate posterior to the exact Bayesian posterior. There are **two valid approaches** to using these weights, each with different trade-offs.

---

## What Are The Weights?

The importance weights represent the **relative probability** of each sample under the true posterior compared to the NPE proposal:

```
w̃ᵢ ∝ p(θᵢ) · p(x|θᵢ) / q_φ(θᵢ|x)

where:
- p(θᵢ) = prior density
- p(x|θᵢ) = likelihood (from simulation)
- q_φ(θᵢ|x) = NPE posterior density
```

After normalization: `Σ wᵢ = 1`

The weights tell you: *"This sample is wᵢ times as probable under the exact posterior as a uniform average sample would be."*

---

## Two Approaches to Using Weights

### Approach 1: Resampling (Currently Implemented)

**What it does:**
- Resample NPE samples **with replacement** according to weights
- Creates unweighted samples from the exact posterior
- Standard statistics can be applied (mean, quantiles, etc.)

**Implementation:**
```r
# In compute_smc_posterior()
smc_posterior <- resample_smc_posterior(
    npe_samples = npe_samples,
    smc_weights = weights,
    n_resample = 500
)

# Then use standard statistics
quantiles <- apply(smc_posterior, 2, quantile, probs = c(0.025, 0.5, 0.975))
means <- colMeans(smc_posterior)
```

**Pros:**
- Simple to work with (no weights needed after resampling)
- Compatible with all standard R functions
- Easy to visualize (just plot the resampled values)
- Matches user intuition ("these are the posterior samples")

**Cons:**
- **Loses precision when ESS is low**: If ESS=1, you get 500 copies of the same sample
- **Sample impoverishment**: Resampling can lose diversity, especially with low ESS
- **Fixed sample size**: Must choose n_resample upfront

**When to use:**
- When ESS > 10-20% (sufficient effective samples)
- For end-user reporting (easier to explain)
- When you need many samples for downstream analysis

---

### Approach 2: Weighted Inference (Not Currently Implemented for SMC)

**What it does:**
- Keep NPE samples **with their weights**
- Use weighted statistics directly
- No resampling step

**Implementation:**
```r
# Keep weighted samples
npe_samples_weighted <- list(
    samples = npe_samples,
    weights = smc_weights
)

# Use weighted statistics (MOSAIC has these functions!)
library(MOSAIC)

quantiles <- apply(npe_samples, 2, function(x) {
    weighted_quantiles(x, w = smc_weights, probs = c(0.025, 0.5, 0.975))
})

means <- apply(npe_samples, 2, function(x) {
    weighted.mean(x, w = smc_weights)
})

variances <- apply(npe_samples, 2, function(x) {
    weighted_var(x, w = smc_weights)
})
```

**Pros:**
- **Preserves all information**: Even with ESS=1, you keep all 500 distinct NPE samples
- **More accurate expectations**: Weighted mean is mathematically exact
- **Better for low ESS**: Doesn't lose sample diversity through resampling
- **Diagnostic friendly**: Can inspect which samples have high/low weights

**Cons:**
- More complex code (must pass weights everywhere)
- Not all R functions support weights
- Harder to visualize (density plots need weighted KDE)
- Less intuitive for end users

**When to use:**
- When ESS is low (< 10%)
- For exploratory analysis (understanding what NPE got wrong)
- When you need accurate point estimates (weighted mean)
- For diagnostic purposes

---

## MOSAIC's Current Implementation

### What Happens Now

By default, `compute_smc_posterior()` uses **Approach 1 (Resampling)**:

```r
smc_result <- compute_smc_posterior(
    ...,
    resample = TRUE  # Default
)

# Returns:
smc_result$smc_posterior        # Resampled unweighted samples (500 rows)
smc_result$npe_samples          # Original NPE samples (also 500 rows)
smc_result$weights$weights      # The importance weights
```

The **problem** with your current run:
- ESS = 1 means only 1 effective sample
- Resampling with one dominant weight → 500 copies of same sample
- All quantiles collapse to single value

### What SHOULD Happen with Low ESS

When ESS is critically low (< 0.5%), **weighted inference would preserve more information**:

```r
# Current behavior (ESS=1):
# Resampling → 500 copies of sample #432
# Quantiles: all identical to sample #432's values

# Weighted behavior (if implemented):
# Keep all 500 NPE samples with weights
# Sample #432 gets weight=1.0, others get weight≈0
# Quantiles: Still dominated by #432, but with tiny contribution from others
```

The weighted quantiles would be **almost identical** to resampled quantiles when ESS=1, but would preserve some numerical diversity.

---

## Why ESS Matters

**ESS (Effective Sample Size)** tells you how many "independent" samples you effectively have:

```
ESS = 1 / Σ wᵢ²
```

**Interpretation:**

| ESS % | Interpretation | Resampling Quality | Weighted Inference |
|-------|----------------|-------------------|-------------------|
| > 30% | Good coverage | ✓ Works well | ✓ Works well |
| 10-30% | Moderate | ✓ Acceptable loss | ✓ Recommended |
| 1-10% | Poor | ⚠ High loss | ✓ Better choice |
| < 1% | Critical | ✗ Extreme collapse | ⚠ Dominated by 1-2 samples |

**Your case:** ESS = 1 (0.2%) → Both approaches give nearly identical results, but weighted preserves numerical precision better

---

## Recommendations

### For MOSAIC SMC Implementation

**Short-term fix:**
1. Keep resampling as default (it's simpler for users)
2. Add clear warnings when ESS < 0.5% (✓ Already implemented)
3. Document that results are unreliable with low ESS

**Medium-term enhancement:**
Add a `use_weighted_inference` parameter:

```r
compute_smc_posterior <- function(
    ...,
    resample = TRUE,
    use_weighted_inference = NULL  # NEW: NULL = auto-select based on ESS
) {
    # Auto-select: if ESS < 5%, recommend weighted inference
    if (is.null(use_weighted_inference)) {
        use_weighted_inference <- (ess_percent < 5.0)
        if (use_weighted_inference) {
            message("Low ESS detected, using weighted inference instead of resampling")
        }
    }

    if (!use_weighted_inference) {
        # Current behavior: resample
        smc_posterior <- resample_smc_posterior(...)
    } else {
        # New behavior: keep weighted samples
        # Use weighted_quantiles() for posterior_quantiles.csv
        # Use weighted_var() for posterior variance
    }
}
```

**Long-term solution:**
Fix the root cause → improve NPE training so ESS > 10%

---

## Comparison: Resampling vs Weighted for Your Data

### Current Results (Resampling, ESS=1)
```
Sample #432 weight: 1.0
All other weights: ~0

Resampling result:
- All 500 samples = copy of sample #432
- q0.025 = q0.5 = q0.975 = sample #432 values
- Zero posterior uncertainty (collapsed)
```

### If Weighted Inference Were Used
```
Sample #432 weight: 1.0
Sample #17 weight: 1e-70
Sample #89 weight: 1e-69
... (all others negligible)

Weighted quantiles:
- Effectively same as sample #432 values
- Tiny numerical contributions from other samples
- Still nearly zero posterior uncertainty

Weighted mean:
μ = 1.0 * θ₄₃₂ + 1e-70 * θ₁₇ + ...
  ≈ θ₄₃₂

Difference: Minimal (both approaches give ~same answer)
```

**Key insight:** When ESS=1, both approaches are similarly bad. The fundamental problem is that NPE only found 1 good sample. Weighted inference marginally preserves more numerical precision, but doesn't solve the core issue.

---

## Practical Guidance for Users

### If You See Low ESS

**Don't:**
- ❌ Trust the SMC posterior as "exact"
- ❌ Assume uncertainty quantification is reliable
- ❌ Use SMC results for critical decisions without validation

**Do:**
- ✓ Check NPE training diagnostics
- ✓ Compare with BFRS posterior (may be more reliable)
- ✓ Increase sample_subset to 2000-5000 and rerun
- ✓ Consider retraining NPE
- ✓ Report ESS alongside all SMC results

### Interpreting Results

When ESS=1 (your case):
- SMC has identified that **only 1 out of 500 NPE samples** is consistent with the observed data
- This is a **catastrophic failure of NPE** to learn the posterior
- The single "good" sample (##432) might be correct, but you have no uncertainty quantification
- **Recommendation:** Use BFRS posterior instead, which is more robust to NPE failures

---

## Technical Note: The Math of Weighted Inference

### Why Weighted Statistics Are "Exact"

The expectation of any function f(θ) under the true posterior is:

```
E[f(θ)] = ∫ f(θ) p(θ|x) dθ
```

Using importance sampling:

```
E[f(θ)] ≈ Σᵢ wᵢ f(θᵢ)

where θᵢ ~ q(θ|x) and wᵢ are importance weights
```

This is **unbiased** and **asymptotically exact** (as n→∞).

For example:
- Posterior mean: `Σᵢ wᵢ θᵢ`
- Posterior variance: `Σᵢ wᵢ (θᵢ - μ)²`
- Posterior quantiles: Use weighted empirical CDF

### Why Resampling Also Works

Resampling creates unweighted samples `θ*₁, ..., θ*ₙ` where:

```
P(θ*ⱼ = θᵢ) = wᵢ
```

These resampled values are distributed according to the exact posterior:

```
θ*ⱼ ~ p(θ|x)
```

So standard unweighted statistics on resampled values give the same expectations:

```
E[f(θ)] ≈ (1/n) Σⱼ f(θ*ⱼ)
```

### The Difference

- **Weighted:** Uses n distinct samples (preserves diversity)
- **Resampling:** Creates n samples with repetition (loses diversity if ESS low)

Both are mathematically valid, but resampling has higher variance when ESS is low.

---

## Summary

### What Should You Do with SMC Weights?

**Current implementation (resampling):**
- ✓ Simple and intuitive
- ✓ Works great when ESS > 10%
- ✗ Collapses when ESS < 1%

**Alternative (weighted inference):**
- ✓ Preserves sample diversity
- ✓ Better for low ESS scenarios
- ✗ More complex, not currently implemented in MOSAIC SMC

**Your specific case (ESS=1):**
- Both approaches give nearly identical results
- The real problem is NPE approximation quality, not the choice of inference method
- **Recommended action:** Improve NPE training rather than switching inference methods

### Key Takeaway

The weights are **importance weights** that correct NPE samples to the exact posterior. They can be used via:
1. **Resampling** (current) - simple but loses diversity with low ESS
2. **Weighted inference** (not implemented) - preserves diversity but more complex

**When ESS is critically low (< 0.5%)**, both approaches fail to provide reliable uncertainty quantification. The solution is to improve the NPE approximation, not to change how weights are used.
