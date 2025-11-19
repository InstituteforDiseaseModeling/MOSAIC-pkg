# SMC (Sequential Monte Carlo) - DEPRECATED

## Status: Deprecated as of 2025-11-04

The SMC exact posterior correction stage has been deprecated and removed from the main calibration workflow.

## Reason for Deprecation

SMC is computationally expensive and often unnecessary:

1. **When NPE is good** (coverage > 95%): SMC adds ~50% computational cost with minimal benefit
2. **When NPE is poor** (coverage < 90%): SMC fails catastrophically (ESS << 1%), making results unreliable
3. **For most use cases**: Either NPE alone or BFRS fallback provides sufficient posterior estimates

## Recommended Workflow

### Two-Stage Calibration (Current)
```
Stage 1: BFRS  - Robust baseline
Stage 2: NPE   - Fast neural approximation
```

**Decision logic:**
- If NPE diagnostics good (coverage > 95%) → Use NPE posterior
- If NPE diagnostics poor (coverage < 90%) → Use BFRS posterior

## Files in This Directory

### R Code
- `smc.R` - Main SMC computation functions
- `smc_utils.R` - Utility functions (parsing, saving, quantiles)
- `smc_plots.R` - Plotting functions (KDE and empirical)

### Documentation
- `when_is_smc_necessary.md` - Comprehensive guide on when SMC adds value
- `smc_implementation_summary.md` - Technical implementation details
- `smc_weight_collapse_analysis.md` - Analysis of ESS=1 failure mode
- `smc_weights_usage_guide.md` - How importance weights work
- `smc_weights_summary.md` - Quick reference
- `smc_plotting_misleading.md` - Why KDE plots hide collapse
- `smc_bug_fixes_2025-11-04.md` - Bug fixes implemented
- `smc_readiness_assessment.md` - Original readiness assessment

## If You Need SMC

### Scenarios Where SMC Might Be Useful
1. High-stakes applications requiring exact posterior
2. NPE diagnostics borderline (coverage 90-95%)
3. Need to validate NPE quality (ESS diagnostic)
4. Publication/regulatory requirements

### How to Re-enable SMC

1. **Copy SMC files back to R/**
   ```bash
   cp deprecated/smc/smc*.R R/
   ```

2. **Add SMC stage to calibration script**
   ```r
   # After NPE stage, add:
   source("R/smc.R")
   source("R/smc_utils.R")
   source("R/smc_plots.R")

   # Create SMC directory
   dir_smc <- file.path(dir_output, "3_smc")
   dir.create(dir_smc, recursive = TRUE, showWarnings = FALSE)

   # Run SMC
   smc_result <- compute_smc_posterior(
       npe_samples = posterior_samples,
       npe_log_probs = posterior_log_probs,
       setup_dir = dir_setup,
       output_dir = dir_smc,
       config_base = config_base,
       n_cores = n_cores,
       sample_subset = 500,
       n_iter = 3,
       verbose = TRUE
   )
   ```

3. **Check ESS before using results**
   ```r
   ess_percent <- 100 * smc_result$weights$ess / smc_result$n_subset

   if (ess_percent < 1) {
       warning("SMC failed (ESS < 1%) - use BFRS instead")
       final_posterior <- bfrs_posterior
   } else if (ess_percent < 10) {
       warning("SMC unreliable (ESS < 10%) - use BFRS instead")
       final_posterior <- bfrs_posterior
   } else {
       final_posterior <- smc_result$smc_posterior
   }
   ```

## Key Lessons Learned

### What We Discovered
1. **ESS is critical diagnostic** - ESS < 1% indicates complete failure
2. **KDE plots are misleading** - Smooth curves hide weight collapse
3. **NPE quality determines SMC success** - Poor NPE → poor SMC
4. **Computational cost is high** - +50% more simulations vs NPE alone

### Why ESS=1 Happens
- NPE missed high-likelihood regions
- Only 1 out of 500 NPE samples has reasonable likelihood
- All weight concentrates on single sample
- Posterior collapses to point mass

### Better Alternatives
- **Good NPE** → Use NPE posterior directly
- **Poor NPE** → Use BFRS posterior (more robust)
- **Uncertain** → Check NPE diagnostics first

## Computational Cost Comparison

```
BFRS:  N simulations (e.g., 10,000)
NPE:   N simulations + 30 min training
SMC:   N + 500×3 more simulations = N + 1,500 (+15%)
```

For 30-second simulations:
- BFRS: ~83 hours (10K simulations)
- NPE:   ~83 hours + 30 min
- SMC:   ~96 hours (11,500 simulations)

**SMC adds 13 hours** - only worth it if NPE is borderline and you need exact correction.

## Technical Notes

### Importance Sampling Formula
```
w̃ᵢ ∝ p(θᵢ) · p(x|θᵢ) / q_φ(θᵢ|x)

where:
- p(θᵢ):     Original prior density
- p(x|θᵢ):   Likelihood from forward simulation
- q_φ(θᵢ|x): NPE proposal density
```

### ESS Interpretation
```
ESS = 1 / Σ wᵢ²

ESS > 50%: Excellent - NPE very accurate
ESS 25-50%: Good - NPE reasonable
ESS 10-25%: Moderate - Some error
ESS 1-10%: Poor - High error
ESS < 1%: Failed - Use BFRS instead
```

### Resampling
Creates unweighted samples from weighted proposals:
```r
indices <- sample(1:n, size=n, replace=TRUE, prob=weights)
smc_posterior <- npe_samples[indices, ]
```

When ESS=1, this selects the same sample n times (collapse).

## References

- **Main guide**: `when_is_smc_necessary.md`
- **Technical details**: `smc_implementation_summary.md`
- **Failure analysis**: `smc_weight_collapse_analysis.md`

## Contact

For questions about SMC implementation or re-enabling SMC, see the documentation files in this directory or consult the MOSAIC development team.

---

**Last updated**: 2025-11-04
**Deprecated by**: User decision based on cost-benefit analysis
**Alternative**: Two-stage workflow (BFRS + NPE)
