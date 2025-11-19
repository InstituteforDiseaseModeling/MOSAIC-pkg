# How SMC Weights Are Intended to Be Used - Quick Summary

## The Two Approaches

### 1. Resampling (Current MOSAIC Implementation) ✓

**What happens:**
```r
# MOSAIC does this by default
smc_posterior <- resample_smc_posterior(
    npe_samples = npe_samples,
    smc_weights = weights,
    n_resample = 500
)

# Creates 500 unweighted samples from exact posterior
# Sample with weight=0.1 appears ~50 times
# Sample with weight=0.001 appears ~0.5 times (rounded to 0 or 1)
```

**Then use standard statistics:**
```r
mean(smc_posterior)
quantile(smc_posterior, c(0.025, 0.975))
```

**Analogy:** Imagine a bag of 500 colored marbles where each color represents an NPE sample. Resampling means:
1. Pick a marble (with probability = weight)
2. Record its color
3. Put it back
4. Repeat 500 times

You end up with a new bag of 500 marbles (possibly with many duplicates).

---

### 2. Weighted Inference (Not Implemented in MOSAIC SMC)

**What it would do:**
```r
# Keep original samples with weights
weighted.mean(npe_samples[,1], w = weights)
weighted_quantiles(npe_samples[,1], w = weights, probs = c(0.025, 0.975))
```

**Analogy:** Keep all 500 marbles in the original bag, but write their weights on them. When computing statistics, marbles with higher weights count more.

**MOSAIC has these functions** (`weighted_quantiles`, `weighted_var`) but they're not currently used in the SMC stage.

---

## Which Is Better?

### When ESS is High (> 10%)

**Both work equally well**, but resampling is simpler:

```
500 samples, ESS = 100 (20%)

Resampling:
- Sample #1 (weight=0.05) appears ~25 times
- Sample #2 (weight=0.02) appears ~10 times
- All samples well-represented
- Output: 500 diverse samples

Weighted:
- Keep all 500 original samples
- Use weighted stats
- Output: Same expectations, slightly less variance
```

**Winner:** Resampling (simpler, same accuracy)

---

### When ESS is Low (< 10%)

**Weighted inference preserves more information:**

```
500 samples, ESS = 10 (2%)

Resampling:
- Top 10 samples account for most weight
- Many duplicates of these 10
- Other 490 samples rarely/never appear
- Output: ~10 unique samples, repeated ~50 times each

Weighted:
- All 500 samples retained
- Top 10 dominate statistics
- But other 490 still contribute tiny amounts
- Output: 500 unique samples with varying importance
```

**Winner:** Weighted inference (preserves diversity)

---

### When ESS ≈ 1 (Your Case)

**Both approaches fail similarly:**

```
500 samples, ESS = 1 (0.2%)

Resampling:
- Sample #432 (weight=1.0) appears 500 times
- All others (weight≈0) appear 0 times
- Output: 500 identical copies of sample #432
- Quantiles: ALL identical

Weighted:
- Sample #432 dominates with weight=1.0
- All others contribute ~0
- Output: 500 samples, but effectively only #432 matters
- Quantiles: Essentially identical to #432

Difference: Negligible
```

**Winner:** Neither - both expose that NPE failed

---

## Why Your Quantiles Collapsed

With ESS=1 and resampling:

1. **Resampling step:**
   ```r
   sample(1:500, size=500, replace=TRUE, prob=weights)
   # weights = c(0, 0, ..., 1.0, ..., 0)  [only position 432 is 1.0]
   # Result: c(432, 432, 432, ..., 432)   [500 times]
   ```

2. **All posterior samples identical:**
   ```r
   smc_posterior[1,] == smc_posterior[2,] == ... == smc_posterior[500,]
   # All TRUE
   ```

3. **Quantiles of constant values:**
   ```r
   quantile(c(0.774, 0.774, 0.774, ..., 0.774), c(0.025, 0.5, 0.975))
   # Result: 0.774, 0.774, 0.774  (all the same!)
   ```

This is **not a bug** - it's the correct mathematical behavior when one sample dominates all weight.

---

## What Weighted Inference Would Give (If Implemented)

```r
# Weighted quantile for parameter phi_1:
weighted_quantiles(
    x = npe_samples[, "phi_1"],  # 500 different values
    w = c(0, 0, ..., 1.0, ..., 0),  # weight concentrated on #432
    probs = c(0.025, 0.5, 0.975)
)

# Result: Still ~0.774 for all quantiles
# Why? Because weight=1.0 on sample #432 dominates everything
# The 499 other samples contribute sum(weights) ≈ 1e-200 ≈ 0
```

**Key insight:** With ESS=1, weighted inference gives **essentially the same answer** as resampling. The problem isn't the method - it's that only 1 NPE sample is good.

---

## Recommendation for MOSAIC

### Current Status (Good Design)

The current implementation (resampling by default) is the **right choice** for most use cases:
- Simpler code
- More intuitive results
- Works great when ESS is healthy (> 10%)

### Enhancement Opportunity

Add weighted inference as an **automatic fallback** when ESS is low:

```r
compute_smc_posterior <- function(..., auto_weighted = TRUE) {

    # Calculate weights
    weights <- compute_smc_weights(...)

    # Auto-select method based on ESS
    if (auto_weighted && ess_percent < 5.0) {
        message("Low ESS detected: using weighted inference")
        # Use weighted_quantiles() for quantile calculation
        # Keep npe_samples + weights (no resampling)
    } else {
        # Current behavior: resample
        smc_posterior <- resample_smc_posterior(...)
    }
}
```

This would:
1. Keep resampling for normal cases (ESS > 5%)
2. Switch to weighted inference when ESS is low
3. Preserve more numerical precision in edge cases
4. Not require user to choose

---

## Bottom Line

### How are SMC weights intended to be used?

**Answer:** Via **resampling** to create unweighted exact posterior samples.

**Why?** Simpler, more intuitive, works well in normal cases.

**Alternative:** Weighted inference (keeping weighted samples) is valid but more complex. It's slightly better when ESS is low but doesn't fundamentally solve the problem.

**Your specific issue:** ESS=1 means NPE found only 1 good sample. Both resampling and weighted inference would give collapsed posteriors. The solution is to **improve NPE training**, not to change the inference method.

### What you should do:

1. **Accept that current results are correct but unreliable** (ESS=1 is a failure mode)
2. **Review NPE training diagnostics** to understand why it failed
3. **Consider using BFRS posterior** which may be more robust
4. **Increase sample_subset** (try 2000-5000) to find more good samples
5. **Retrain NPE** with better diagnostics/architecture if ESS remains low
