# Why SMC Plots Look Reasonable Despite ESS=1

## The Mystery

**Observed:**
- ESS = 1 (extreme weight collapse)
- All 500 posterior samples are identical
- All quantiles collapse to single value
- But SMC distribution plots show **reasonable-looking bell curves**

**Why?** Two sources of artificial spread: KDE smoothing and distribution fitting.

---

## Source 1: Kernel Density Estimation (KDE) Smoothing

### What's Happening in the Code

In `plot_npe_vs_smc_comparison()` (R/smc_plots.R:150-151):

```r
smc_vals <- smc_samples[, param]  # 500 identical values!
smc_dens <- density(smc_vals, na.rm = TRUE)  # Smooths into a curve
```

### The Problem

The `density()` function uses **kernel density estimation** with automatic bandwidth selection. Even when all values are identical, it creates artificial spread:

```r
# Test with collapsed samples
smc_collapsed <- rep(0.774372, 500)

> density(smc_collapsed)

Call:
    density.default(x = smc_collapsed)

Data: smc_collapsed (500 obs.);  Bandwidth 'bw' = 0.2011

         x                 y
 Min.   :0.1711    Min.   :2.067e-11
 Max.   :1.3777    Max.   :1.983e+00
```

**Result:**
- True range: [0.774, 0.774] (zero width)
- Plotted range: [0.17, 1.38] (width = 1.2!)
- The curve spreads **±3 × bandwidth** around the single point

### Visualization

Created test in `claude/test_density_collapse.R`:

```
Histogram (Truth):          density() Output (Misleading):

    500│                        │     ╱───╲
       │                        │    ╱     ╲
    400│                        │   ╱       ╲
       │                        │  ╱         ╲
    300│                        │ ╱           ╲
       │      ││                │╱             ╲
    200│      ││                ├───────────────
       │      ││                0.2    0.77   1.3
    100│      ││
       │      ││                All values at 0.774,
     0 ├──────┼┼───────        but KDE spreads them
       0.5  0.77  1.0           into a smooth curve!
```

**This makes collapse invisible** - the smooth curve looks like a proper distribution.

---

## Source 2: Fitted Distribution Parameters

### What's in posteriors.json

From `local/calibration/calibration_test_17/3_smc/posteriors.json`:

```json
"phi_1": {
  "distribution": "beta",
  "parameters": {
    "shape1": 774371.6836,
    "shape2": 225628.3164,
    "fitted_mean": 0.7744
  }
}
```

### What This Means

When `fit_posterior_distributions()` fits a Beta distribution to 500 identical values:

**Target to match:**
- Mean = 0.7744 ✓
- Variance = 0 (all samples identical)

**How Beta fits this:**
- Uses enormous shape parameters (shape1=774K, shape2=225K)
- Creates a **super-concentrated** Beta distribution
- Mean = shape1/(shape1+shape2) = 0.7744 ✓
- Variance = shape1·shape2/[(shape1+shape2)²·(shape1+shape2+1)] ≈ 1.75×10⁻¹³

**Result:** The fitted Beta has TINY but non-zero variance, creating a very narrow peak instead of a true spike.

If you plotted this fitted Beta:

```r
x <- seq(0.77, 0.78, length.out=1000)
y <- dbeta(x, shape1=774371, shape2=225628)
plot(x, y, type='l')
```

You'd see a very sharp but still visible peak, not a true Dirac delta.

---

## What You SHOULD See: Empirical Plots

### Histogram (Truth)

If we plotted with `hist()` instead of `density()`:

```r
hist(smc_samples[, "phi_1"], breaks=50)
```

**Result:**
- Single bar at x = 0.7744
- All 500 counts in one bin
- Clear visual indication of collapse

### Rug Plot (Truth)

If we used rug plots:

```r
plot(density(npe_vals), col='blue')
rug(smc_vals, col='red', lwd=2)  # All 500 ticks at same location → one thick line
```

**Result:**
- NPE shows spread (blue curve)
- SMC shows single vertical line (red rug)
- Collapse immediately obvious

### Scatter Plot (Truth)

If we plotted sample index vs value:

```r
plot(1:500, smc_samples[, "phi_1"])
```

**Result:**
- Horizontal line at y = 0.7744
- All 500 points at same height
- Zero vertical variation

---

## Comparison with BFRS Empirical Plots

### BFRS: plot_model_distributions_detailed()

The BFRS stage uses `plot_model_distributions_detailed()` which shows **empirical samples** more honestly:

```r
# From BFRS plotting code (simplified):
plot(results$beta_ij, results$aic_rank, pch=16, cex=0.5)
```

This plots:
- Raw parameter values (x-axis)
- Some metric like AIC rank (y-axis)
- Shows the actual sample spread

If BFRS had ESS=1, you'd see:
- All points at same x-coordinate (vertical line)
- Collapse would be immediately obvious

### Why SMC Doesn't Do This

The SMC comparison plots use `density()` for smoothness and aesthetic appeal:
- Looks professional
- Easy to compare visually
- But **hides collapse when ESS is low**

---

## Solution: Add Empirical Distribution Plots

I've created two new plotting functions in `R/plot_smc_empirical_distributions.R`:

### 1. plot_smc_empirical_distributions()

Shows SMC posterior using **histograms** instead of KDE:

```r
plot_smc_empirical_distributions(
    smc_samples = smc_result$smc_posterior,
    output_dir = file.path(dir_smc, "plots"),
    verbose = TRUE
)
```

**Features:**
- Histogram bins show actual sample distribution
- Annotates with `n_unique` (number of unique values)
- Labels collapsed parameters with "COLLAPSED" warning
- Shows reality of weight collapse

**Output:**
```
phi_1 (n_unique = 1)

500│  ██
   │  ██
400│  ██
   │  ██        COLLAPSED
300│  ██      (all samples
   │  ██       identical)
200│  ██
   │  ██
100│  ██
   │  ██
  0├──┴┴────────
    0.77 0.78
```

### 2. plot_npe_vs_smc_empirical()

Compares NPE vs SMC using **overlaid histograms**:

```r
plot_npe_vs_smc_empirical(
    npe_samples = posterior_samples,
    smc_samples = smc_result$smc_posterior,
    output_dir = file.path(dir_smc, "plots"),
    verbose = TRUE
)
```

**Features:**
- Semi-transparent overlaid histograms
- Shows empirical distributions without KDE smoothing
- Annotates SMC with `n_unique` count
- Makes collapse obvious when it occurs

**Output (when ESS=1):**
```
NPE: ╱‾‾╲     (spread)
SMC:    ▌     (spike)
```

---

## Recommendations

### Immediate: Document the Issue

Add warning to SMC documentation:

> **Note:** When ESS is low (< 1%), the `plot_npe_vs_smc_comparison()` plots use
> kernel density estimation which can create misleading smooth curves even when
> posterior samples are collapsed to a single point. Always check the `n_unique_resampled`
> field in `diagnostics.json` to verify sample diversity.

### Short-term: Use Empirical Plots

For your current analysis:

```r
# Add to calibration script
if (smc_result$weights$ess < 10) {
    # Use empirical plots when ESS is low
    plot_smc_empirical_distributions(
        smc_samples = smc_result$smc_posterior,
        output_dir = file.path(dir_smc_plots, "empirical"),
        verbose = TRUE
    )
}
```

### Long-term: Improve Default Plots

**Option 1:** Switch to empirical plots by default
- More honest representation
- Shows collapse clearly
- Con: Less aesthetic appeal

**Option 2:** Add annotations to KDE plots
- Keep KDE smoothing
- Add text: "n_unique = 1" or "COLLAPSED"
- Show both beauty and truth

**Option 3:** Conditional plotting
- KDE when ESS > 10% (looks nice, works well)
- Empirical when ESS < 10% (shows reality)

---

## Summary: Why You See "Reasonable Shapes"

### The Truth (ESS=1)

```
smc_samples[, "phi_1"]
→ c(0.774, 0.774, 0.774, ..., 0.774)  [500 identical values]

quantile(smc_samples[, "phi_1"], c(0.025, 0.5, 0.975))
→ 0.774, 0.774, 0.774  [all same]
```

### What Gets Plotted (Misleading)

**From density():**
```r
dens <- density(smc_samples[, "phi_1"])
# Automatic bandwidth ≈ 0.20
# Creates curve from 0.17 to 1.38
# Smooth bell shape around single point
```

**From fitted distribution:**
```r
# Beta(774371, 225628)
# Extremely concentrated, but not zero-width
# Variance ≈ 1.75e-13 (tiny but non-zero)
```

### What You Should See (Truth)

```r
hist(smc_samples[, "phi_1"], breaks=50)
# → Single bar at 0.774
# → All 500 samples in one bin
# → Collapse obvious
```

---

## Conclusion

Your intuition was **exactly correct**:

> "If the weights are all concentrated to one model, I would expect the posterior
> distributions to be a single spike."

They ARE a single spike - but:
1. `density()` KDE smoothing creates artificial spread
2. Fitted distributions have tiny but non-zero variance
3. The plots you're looking at are misleading

If you plotted the **empirical samples** (histograms, scatter plots, rug plots), you would see the spike you expected.

**Action:** Use the new `plot_smc_empirical_distributions()` function to see the truth.
