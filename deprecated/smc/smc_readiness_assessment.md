# SMC Exact Posterior Readiness Assessment
## calibration_test_17.R Workflow Analysis

**Date**: 2025-11-03
**Status**: ✅ **READY TO IMPLEMENT STAGE 3**

---

## Executive Summary

After comprehensive examination of `calibration_test_17.R` and the NPE infrastructure, **we are ready to implement Stage 3 (SMC Exact Posterior)**. All critical data components are in place, and most helper functions exist. Only the main SMC workflow functions need to be created.

---

## ✅ COMPONENTS ALREADY IN PLACE

### 1. NPE Outputs (Stage 2 Complete)
**Location**: `local/calibration/calibration_test_17/2_npe/`

#### Posterior Samples ✅
- **File**: `posterior/posterior_samples.parquet`
- **Rows**: 10,000 samples
- **Columns**: 41 parameters (θᵢ ~ q_φ(θ|x))
- **Parameters**: `phi_1, phi_2, omega_1, omega_2, iota, gamma_1, gamma_2, epsilon, rho, sigma, ...`
- **Status**: ✅ Ready for use

#### NPE Log Probabilities ✅ **CRITICAL COMPONENT**
- **File**: `posterior/posterior_log_probs.parquet`
- **Rows**: 10,000 (matching samples)
- **Column**: `log_q_theta` (log q_φ(θᵢ|x))
- **Range**: [-165.45, -122.76]
- **Implementation**: Lines 80-87, 719-727 in `npe_posterior.R`
- **Status**: ✅ **FULLY IMPLEMENTED AND SAVED**

#### Observed Data ✅
- **File**: `observed_data.csv`
- **Rows**: 913 time-location pairs
- **Columns**: `j` (location), `t` (time), `cases`, `deaths`
- **Status**: ✅ Ready for likelihood calculations

### 2. Original Priors (Stage 0) ✅
**Location**: `local/calibration/calibration_test_17/0_setup/priors.json`

**Structure**:
```json
{
  "metadata": {...},
  "parameters_global": {
    "alpha_1": {
      "description": "...",
      "distribution": "beta",
      "parameters": {
        "shape1": 4.1883,
        "shape2": 10.5648
      }
    },
    ...
  },
  "parameters_location": {...}
}
```

- **Global parameters**: 23
- **Distributions**: beta, gamma, lognormal, normal, uniform
- **Status**: ✅ Available (needs parser for nested structure)

### 3. BFRS Infrastructure ✅
**Location**: `local/calibration/calibration_test_17/1_bfrs/`

- `simulations.parquet`: Complete BFRS results with weights
- `config_best.json`: Best model configuration
- `subset_selection_summary.csv`: Convergence metrics
- **Status**: ✅ Available for reference

### 4. Helper Functions Already Implemented ✅

#### `evaluate_prior_densities()` ✅
**File**: `R/npe_posterior.R` (lines 830-880)

```r
evaluate_prior_densities <- function(samples, priors, verbose = FALSE)
```

**Purpose**: Computes log p(θᵢ) for each sample
**Distributions supported**: beta, gamma, lognormal, normal, uniform
**Status**: ✅ Implemented (needs minor adaptation for nested priors format)

#### `estimate_npe_posterior()` ✅
**File**: `R/npe_posterior.R` (lines 30-157)

**Purpose**: Generates NPE samples with log probabilities
**Critical feature**: Lines 80-87 compute and adjust log q_φ(θᵢ|x)
**Status**: ✅ Fully functional

#### `calc_model_likelihood()` ✅
**File**: `R/calc_model_likelihood.R`

**Purpose**: Computes log p(x|θᵢ) via Negative Binomial likelihood
**Status**: ✅ Used throughout BFRS stage

#### Parallel Simulation Infrastructure ✅
**Location**: Lines 443-493 in `calibration_test_17.R`

- Cluster setup with `makeCluster()`
- Worker initialization with laser-cholera
- Parallel execution via `pblapply()`
- **Status**: ✅ Ready for reuse

---

## ⚠️ COMPONENTS NEEDING ADAPTATION

### 1. Prior Density Evaluation (Minor Fix Needed)

**Issue**: `evaluate_prior_densities()` expects flat prior structure:
```r
prior$alpha  # Current expectation
```

But `priors.json` has nested structure:
```json
"parameters": {
  "shape1": 4.1883,  # Actual structure
  "shape2": 10.5648
}
```

**Solution**: Add parser function to flatten priors before calling `evaluate_prior_densities()`

```r
.parse_prior_spec <- function(prior_spec) {
    # Extract distribution type
    dist <- prior_spec$distribution
    params <- prior_spec$parameters

    # Flatten to match evaluate_prior_densities expectations
    result <- list(distribution = dist)

    if (dist == "beta") {
        result$alpha <- params$shape1
        result$beta <- params$shape2
    } else if (dist == "gamma") {
        result$shape <- params$shape
        result$rate <- params$rate
    }
    # ... etc for other distributions

    return(result)
}
```

**Complexity**: Low (1-2 hours)

---

## ❌ COMPONENTS TO CREATE

### 1. SMC Likelihood Evaluation Function

**Purpose**: Parallel forward simulation to compute log p(x|θᵢ)

**Pseudocode**:
```r
compute_smc_likelihoods <- function(
    npe_samples,          # 10,000 × 41 matrix from NPE
    config_base,          # Base MOSAIC configuration
    observed_data,        # Observed outbreak data
    n_cores = 9,          # Parallel cores
    sample_subset = NULL, # Optional: use only N samples (e.g., 500)
    verbose = TRUE
) {
    # Setup cluster (reuse BFRS infrastructure)
    cl <- makeCluster(n_cores)
    clusterExport(cl, c("config_base", "observed_data"))

    # Select subset if requested
    if (!is.null(sample_subset)) {
        indices <- sample(1:nrow(npe_samples), sample_subset)
        npe_samples <- npe_samples[indices, ]
    }

    # Parallel likelihood evaluation
    log_likelihoods <- pblapply(1:nrow(npe_samples), function(i) {
        # Create config with NPE parameters
        config_i <- config_base
        for (param in colnames(npe_samples)) {
            config_i[[param]] <- npe_samples[i, param]
        }

        # Run MOSAIC simulation
        model <- lc$run_model(paramfile = config_i, quiet = TRUE)

        # Calculate likelihood
        if (!is.null(model)) {
            log_lik <- calc_model_likelihood(
                config = config_base,
                obs_cases = config_base$reported_cases,
                est_cases = model$results$expected_cases,
                obs_deaths = config_base$reported_deaths,
                est_deaths = model$results$disease_deaths,
                weight_cases = 1.0,
                weight_deaths = 0,
                enable_guardrails = FALSE
            )
        } else {
            log_lik <- -Inf
        }

        return(log_lik)
    }, cl = cl)

    stopCluster(cl)

    return(unlist(log_likelihoods))
}
```

**Complexity**: Medium (4-6 hours)
**Testing required**: Yes (verify with known good parameters)

---

### 2. SMC Weight Calculation Function

**Purpose**: Compute importance sampling weights

**Pseudocode**:
```r
compute_smc_weights <- function(
    npe_log_probs,    # log q_φ(θᵢ|x) from NPE
    log_priors,       # log p(θᵢ) from priors
    log_likelihoods,  # log p(x|θᵢ) from simulations
    verbose = TRUE
) {
    # Importance weight formula:
    # log w̃ᵢ = log p(θᵢ) + log p(x|θᵢ) - log q_φ(θᵢ|x)

    log_weights_unnorm <- log_priors + log_likelihoods - npe_log_probs

    # Normalize using log-sum-exp
    log_weights_norm <- log_weights_unnorm - matrixStats::logSumExp(log_weights_unnorm)

    # Convert to linear space
    weights <- exp(log_weights_norm)

    # Calculate ESS
    ess <- 1 / sum(weights^2)

    return(list(
        weights = weights,
        log_weights = log_weights_norm,
        log_weights_unnorm = log_weights_unnorm,
        ess = ess
    ))
}
```

**Complexity**: Low (2-3 hours)
**Testing**: Straightforward

---

### 3. SMC Resampling Function

**Purpose**: Create unweighted exact posterior samples

**Pseudocode**:
```r
resample_smc_posterior <- function(
    npe_samples,
    smc_weights,
    n_resample = NULL  # NULL = same as input
) {
    if (is.null(n_resample)) {
        n_resample <- nrow(npe_samples)
    }

    # Resample with replacement based on IS weights
    resampled_indices <- sample(
        x = 1:nrow(npe_samples),
        size = n_resample,
        replace = TRUE,
        prob = smc_weights
    )

    smc_posterior <- npe_samples[resampled_indices, ]

    return(smc_posterior)
}
```

**Complexity**: Low (1 hour)

---

### 4. Main SMC Workflow Function

**Purpose**: Orchestrate complete SMC pipeline

**Pseudocode**:
```r
compute_smc_posterior <- function(
    npe_dir,           # Path to NPE outputs
    bfrs_dir,          # Path to BFRS outputs
    setup_dir,         # Path to setup (priors)
    output_dir,        # Where to save SMC results
    n_cores = 9,
    sample_subset = 500,  # Use subset for efficiency
    resample = TRUE,
    verbose = TRUE
) {
    # 1. Load NPE outputs
    npe_samples <- arrow::read_parquet(file.path(npe_dir, "posterior/posterior_samples.parquet"))
    npe_log_probs_df <- arrow::read_parquet(file.path(npe_dir, "posterior/posterior_log_probs.parquet"))
    npe_log_probs <- npe_log_probs_df$log_q_theta

    # 2. Load priors and observed data
    priors_raw <- jsonlite::read_json(file.path(setup_dir, "priors.json"))
    priors_flat <- .flatten_priors(priors_raw)  # Parse nested structure
    observed_data <- read.csv(file.path(npe_dir, "observed_data.csv"))
    config_base <- jsonlite::read_json(file.path(setup_dir, "config_base.json"))

    # 3. Subset if requested
    if (!is.null(sample_subset) && sample_subset < nrow(npe_samples)) {
        set.seed(42)
        indices <- sample(1:nrow(npe_samples), sample_subset)
        npe_samples <- npe_samples[indices, ]
        npe_log_probs <- npe_log_probs[indices]
    }

    # 4. Evaluate prior densities
    log_priors <- evaluate_prior_densities(
        samples = npe_samples,
        priors = priors_flat,
        verbose = verbose
    )

    # 5. Compute likelihoods (expensive step)
    log_likelihoods <- compute_smc_likelihoods(
        npe_samples = npe_samples,
        config_base = config_base,
        observed_data = observed_data,
        n_cores = n_cores,
        verbose = verbose
    )

    # 6. Calculate IS weights
    smc_weights_result <- compute_smc_weights(
        npe_log_probs = npe_log_probs,
        log_priors = log_priors,
        log_likelihoods = log_likelihoods,
        verbose = verbose
    )

    # 7. Optional resampling
    if (resample) {
        smc_posterior <- resample_smc_posterior(
            npe_samples = npe_samples,
            smc_weights = smc_weights_result$weights
        )
    } else {
        smc_posterior <- npe_samples
    }

    # 8. Save results
    .save_smc_results(
        npe_samples = npe_samples,
        smc_weights = smc_weights_result,
        smc_posterior = if (resample) smc_posterior else NULL,
        log_components = list(
            log_priors = log_priors,
            log_likelihoods = log_likelihoods,
            log_q_npe = npe_log_probs
        ),
        output_dir = output_dir,
        verbose = verbose
    )

    return(list(
        smc_posterior = smc_posterior,
        weights = smc_weights_result,
        ess = smc_weights_result$ess,
        n_samples = nrow(npe_samples),
        resampled = resample
    ))
}
```

**Complexity**: Medium (6-8 hours including testing)

---

### 5. SMC Diagnostics Functions

**Purpose**: Validate SMC correction

```r
# Weight distribution diagnostics
plot_smc_weight_diagnostics()

# NPE vs SMC posterior comparison
plot_npe_vs_smc_posteriors()

# Parameter shift analysis
analyze_smc_parameter_shifts()

# ESS tracking
compute_smc_ess()
```

**Complexity**: Medium (4-6 hours)

---

## 📊 COMPUTATIONAL ESTIMATES

### Full SMC Run (10,000 samples):
- **Time**: ~80 hours (3.3 days) on 9 cores
- **Recommendation**: NOT practical for routine use

### Efficient SMC Run (500 samples):
- **Time**: ~4 hours on 9 cores
- **ESS expected**: ~300-400 (if NPE is well-calibrated)
- **Recommendation**: ✅ **USE THIS APPROACH**

### Rationale for Subset:
If NPE is well-calibrated (diagnostics show good coverage), then:
- Weights will be relatively uniform
- 500 samples provides sufficient ESS
- 20× speedup over full run
- Can run overnight rather than multi-day

---

## 🎯 IMPLEMENTATION PLAN

### Phase 1: Core Functions (1-2 days)
1. ✅ Prior parser for nested JSON structure (2 hours)
2. ✅ `compute_smc_likelihoods()` with parallel infrastructure (6 hours)
3. ✅ `compute_smc_weights()` with proper log-space arithmetic (3 hours)
4. ✅ `resample_smc_posterior()` (1 hour)
5. ✅ File I/O helpers for saving SMC results (2 hours)

### Phase 2: Main Workflow (1 day)
1. ✅ `compute_smc_posterior()` main function (6 hours)
2. ✅ Integration with calibration_test_17.R (2 hours)
3. ✅ End-to-end testing with small subset (N=50) (4 hours)

### Phase 3: Diagnostics & Validation (1 day)
1. ✅ Weight distribution analysis (3 hours)
2. ✅ NPE vs SMC comparison plots (3 hours)
3. ✅ ESS and convergence diagnostics (3 hours)
4. ✅ Full production run (N=500) (4 hours runtime + analysis)

**Total estimated time**: 3-4 development days + compute time

---

## ✅ VALIDATION CHECKLIST

Before declaring SMC stage complete:

- [ ] Prior densities correctly evaluated for all 41 parameters
- [ ] Likelihood calculations match BFRS values for known-good parameters
- [ ] IS weights sum to 1.0 (within numerical tolerance)
- [ ] ESS is reasonable (> 100 for N=500 samples)
- [ ] No weight collapse (max weight < 0.1)
- [ ] SMC posterior is bounded within prior ranges
- [ ] NPE vs SMC comparison shows reasonable agreement
- [ ] Can reproduce NPE posteriors when using NPE as both proposal and target (sanity check)

---

## 🚀 READY TO PROCEED

**Status**: ✅ **GREEN LIGHT**

All critical data components are in place. The NPE stage has successfully saved:
- ✅ Posterior samples (θᵢ)
- ✅ **Log probabilities (log q_φ(θᵢ|x))** ← CRITICAL & CONFIRMED
- ✅ Original priors
- ✅ Observed data

**Next Steps**:
1. Create `R/calc_smc_posterior.R` with core SMC functions
2. Add Stage 3 block to `calibration_test_17.R` (lines 2265-2267)
3. Test with small subset (N=50) first
4. Run production SMC with N=500 samples
5. Generate comparison plots and diagnostics

**Estimated timeline**: Ready to implement immediately, 3-4 days to completion.
