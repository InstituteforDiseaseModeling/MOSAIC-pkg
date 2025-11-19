# Is SMC Exact Posterior Necessary?

## Short Answer

**It depends on NPE quality.** If NPE diagnostics are good, you can use NPE posteriors directly. If NPE diagnostics are poor, SMC may help—but can also fail catastrophically (as you've seen).

---

## Three Calibration Approaches in MOSAIC

### 1. BFRS (Bayesian Fusion Rejection Sampling)
**What it is:** Fit smooth distributions to best-performing simulations weighted by AIC

**Pros:**
- ✓ Robust to poor approximations
- ✓ Always produces smooth posteriors
- ✓ Fast (uses existing simulations)
- ✓ Interpretable (fits standard distributions)

**Cons:**
- ✗ Approximate (not exact posterior)
- ✗ Limited by simulation budget
- ✗ May not capture complex multimodal posteriors

**When to use:** As a baseline, or when NPE/SMC fail

---

### 2. NPE (Neural Posterior Estimation)
**What it is:** Neural network learns to approximate p(θ|x) directly

**Pros:**
- ✓ Fast sampling (once trained)
- ✓ Can capture complex posteriors
- ✓ Scalable to high dimensions
- ✓ Unlimited posterior samples

**Cons:**
- ✗ Approximate (not exact)
- ✗ Training can fail (coverage, calibration issues)
- ✗ Black box (hard to diagnose failures)
- ✗ Requires careful architecture tuning

**When to use:** When NPE diagnostics are good (coverage > 95%, calibration OK)

---

### 3. SMC (Sequential Monte Carlo Exact Posterior)
**What it is:** Uses NPE samples as proposals, corrects to exact posterior via importance sampling

**Pros:**
- ✓ Mathematically exact (under forward model)
- ✓ Corrects NPE approximation errors
- ✓ Provides validation (ESS tells you if NPE is good)

**Cons:**
- ✗ Very expensive (requires forward simulations per sample)
- ✗ Fails catastrophically if NPE is very poor (ESS << 1%)
- ✗ May give worse results than NPE if NPE is poor
- ✗ Still approximate (exact under simulation, not reality)

**When to use:** When NPE is decent but you want exact correction, or to validate NPE

---

## Decision Tree: Which Approach to Use?

```
Start with BFRS (always run this as baseline)
    ↓
NPE diagnostics good? (coverage > 95%, calibration OK)
    ├─ YES → Use NPE posterior ✓
    │         (SMC optional for validation)
    │
    └─ NO → NPE diagnostics poor?
            ├─ Coverage 90-95% → Try SMC
            │                      ├─ ESS > 10%? → Use SMC ✓
            │                      └─ ESS < 10%? → Use BFRS ✓
            │
            └─ Coverage < 90% → Use BFRS ✓
                                 (NPE failed, SMC will likely fail too)
```

---

## Your Specific Case (ESS = 1)

### What Happened
- NPE training completed
- SMC correction attempted
- **ESS = 1 (0.2%)** → extreme weight collapse
- All 500 SMC samples identical

### What This Means
1. **NPE approximation is very poor** - doesn't cover high-likelihood regions
2. **SMC found only 1 "good" sample** out of 500 NPE samples
3. **Neither NPE nor SMC are reliable** for this calibration

### Recommended Action
**Use the BFRS posterior** - it's more robust when NPE/SMC fail

**Why?**
- BFRS fits smooth distributions to best simulations
- Doesn't rely on NPE approximation quality
- Always produces interpretable results
- Less prone to catastrophic failure

---

## When NPE Alone Is Sufficient

### Criteria for Using NPE Without SMC

✓ **NPE diagnostics pass:**
- Coverage > 95% (SBC coverage within expected bands)
- Calibration looks good (rank statistics uniform)
- Training converged (loss plateaued)

✓ **Computational constraints:**
- Can't afford SMC's computational cost (n_samples × n_iter simulations)

✓ **NPE posterior looks reasonable:**
- Posterior within prior bounds
- Not collapsed to edges
- Uncertainty reasonable

✓ **Validation against known test cases:**
- NPE recovers true parameters on synthetic data

### Example Decision

```r
# Check NPE diagnostics
npe_diagnostics <- calc_npe_diagnostics(...)

if (npe_diagnostics$coverage > 0.95 &&
    npe_diagnostics$calibration_ok) {

    # NPE is good - use it directly!
    final_posterior <- npe_posterior

} else {

    # NPE questionable - try SMC or fall back to BFRS
    smc_result <- compute_smc_posterior(...)

    if (smc_result$weights$ess_percent > 10) {
        final_posterior <- smc_result$smc_posterior  # SMC worked
    } else {
        final_posterior <- bfrs_posterior  # SMC failed, use BFRS
    }
}
```

---

## When SMC Is Worth the Cost

### Scenarios Where SMC Adds Value

1. **NPE diagnostics are borderline** (coverage 90-95%)
   - NPE is decent but not great
   - SMC can correct approximation errors
   - ESS will likely be 10-30%

2. **High-stakes decisions**
   - Need exact posterior (not approximation)
   - Can afford computational cost
   - Want mathematical guarantee of correctness

3. **Validating NPE**
   - Even if not using SMC posterior
   - ESS tells you if NPE is trustworthy
   - Low ESS = warning that NPE is poor

4. **Publication/regulatory requirements**
   - Need to show exact Bayesian inference
   - Reviewers may question NPE approximation
   - SMC provides mathematical rigor

### When SMC Is NOT Worth It

1. **NPE diagnostics are excellent** (coverage > 97%)
   - NPE already very accurate
   - SMC correction will be minimal (ESS ≈ 50-80%)
   - Not worth the computational cost

2. **NPE diagnostics are terrible** (coverage < 85%)
   - NPE completely failed
   - SMC will also fail (ESS < 1%)
   - Waste of computation - use BFRS instead

3. **Computational budget is limited**
   - SMC requires n_samples × n_iter simulations (e.g., 500 × 3 = 1,500)
   - This is expensive on top of initial calibration + NPE training
   - Better to spend budget on more initial simulations for BFRS

---

## Computational Cost Comparison

### BFRS
```
Cost: N simulations (e.g., 5,000-15,000)
Time: ~1-3 days (depending on n_cores)
Result: Smooth posterior distributions
```

### NPE
```
Cost: N simulations + NPE training
Time: ~1-3 days simulations + ~30 min training
Result: Neural approximation of posterior
Additional samples: Free (just sample from network)
```

### SMC
```
Cost: N simulations + NPE training + M×K simulations
      (e.g., 10,000 + 500×3 = 11,500 total)
Time: +50% more simulations vs NPE alone
Result: Exact posterior (if ESS is sufficient)
```

**Bottom line:** SMC adds ~50% more computational cost. Only worth it if NPE is borderline and you need exact correction.

---

## The Three-Stage Workflow

### Current MOSAIC Design
```
Stage 1: BFRS (robust baseline)
Stage 2: NPE (fast approximation)
Stage 3: SMC (exact correction)  ← Optional!
```

### When to Skip SMC
- NPE diagnostics excellent → Use NPE posterior ✓
- NPE diagnostics terrible → Use BFRS posterior ✓
- Computational budget limited → Use NPE or BFRS ✓

### When to Run SMC
- NPE diagnostics borderline (90-95% coverage)
- Need validation of NPE quality
- High-stakes application requiring exact posterior
- Have computational budget available

---

## Practical Recommendations

### Workflow 1: Conservative (Always Run All Three)
```r
# 1. Run BFRS (baseline)
bfrs_result <- run_bfrs_calibration(...)

# 2. Run NPE
npe_result <- train_npe(...)
npe_diagnostics <- calc_npe_diagnostics(...)

# 3. Run SMC (validation/correction)
smc_result <- compute_smc_posterior(...)

# 4. Choose best result
if (smc_result$ess_percent > 10) {
    final_posterior <- smc_result$smc_posterior  # SMC worked
} else if (npe_diagnostics$coverage > 0.95) {
    final_posterior <- npe_result$posterior  # NPE is good
} else {
    final_posterior <- bfrs_result$posterior  # Fall back to BFRS
}
```

**Pros:** Maximum information, can choose best result
**Cons:** Highest computational cost

---

### Workflow 2: Efficient (Conditional SMC)
```r
# 1. Run BFRS
bfrs_result <- run_bfrs_calibration(...)

# 2. Run NPE
npe_result <- train_npe(...)
npe_diagnostics <- calc_npe_diagnostics(...)

# 3. Conditional SMC
if (npe_diagnostics$coverage > 0.97) {
    # NPE excellent - skip SMC
    final_posterior <- npe_result$posterior
    message("NPE diagnostics excellent, using NPE posterior")

} else if (npe_diagnostics$coverage > 0.90) {
    # NPE borderline - run SMC
    smc_result <- compute_smc_posterior(...)

    if (smc_result$ess_percent > 10) {
        final_posterior <- smc_result$smc_posterior
        message("SMC correction successful")
    } else {
        final_posterior <- bfrs_result$posterior
        message("SMC failed (low ESS), using BFRS")
    }

} else {
    # NPE poor - skip SMC, use BFRS
    final_posterior <- bfrs_result$posterior
    message("NPE diagnostics poor, using BFRS posterior")
}
```

**Pros:** Saves computation when NPE excellent or terrible
**Cons:** May miss cases where SMC would help

---

### Workflow 3: NPE-Only (Trust NPE)
```r
# 1. Run calibration simulations
simulations <- run_calibration(...)

# 2. Run NPE with thorough diagnostics
npe_result <- train_npe(...)
npe_diagnostics <- calc_npe_diagnostics(...)

# 3. Validate thoroughly
if (npe_diagnostics$coverage < 0.95) {
    stop("NPE diagnostics failed - retrain with different architecture")
}

# 4. Use NPE posterior
final_posterior <- npe_result$posterior
```

**Pros:** Fastest approach (no BFRS or SMC)
**Cons:** Risky if NPE fails; no robust fallback

---

## Your Specific Situation

### Current Status
- BFRS: ✓ Completed (have robust baseline)
- NPE: ✓ Completed (but likely poor diagnostics)
- SMC: ✓ Completed (ESS = 1 - failed!)

### What You Should Do
1. **Check NPE diagnostics** - coverage and calibration metrics
2. **Compare posteriors:**
   - BFRS posterior
   - NPE posterior
   - SMC single sample (parameter set #432)
3. **Use BFRS as final answer** if:
   - NPE coverage < 90%
   - SMC ESS < 1%
   - BFRS looks reasonable

### Going Forward
For your next calibration run:

```r
# Add conditional SMC logic
run_smc <- FALSE  # Set to FALSE to skip SMC

if (run_smc && npe_diagnostics$coverage > 0.90) {
    smc_result <- compute_smc_posterior(...)
} else {
    message("Skipping SMC stage")
    # Use NPE or BFRS based on diagnostics
}
```

This saves ~50% computational cost if you're confident in NPE or willing to use BFRS.

---

## Summary

### Is SMC Necessary?

**No** - NPE posterior alone is sufficient when:
- ✓ NPE diagnostics are excellent (coverage > 95%)
- ✓ Computational budget is limited
- ✓ Application doesn't require exact posterior

**Yes** - SMC adds value when:
- ✓ NPE diagnostics borderline (coverage 90-95%)
- ✓ Need exact posterior (not approximation)
- ✓ Want to validate NPE quality
- ✓ Have computational budget

**Neither NPE nor SMC** - Use BFRS when:
- ✓ NPE diagnostics are poor (coverage < 90%)
- ✓ SMC fails (ESS < 1%)
- ✓ Need robust baseline

### Your Case
With ESS = 1, **I recommend using the BFRS posterior as your final answer**. Both NPE and SMC have indicated they are unreliable for this calibration.

### Future Runs
Consider making SMC **optional** and only running it when:
1. NPE coverage is 90-95% (borderline)
2. You have computational budget
3. You want validation even if not using SMC result

This could save significant computation time while still getting good posteriors from NPE or BFRS.
