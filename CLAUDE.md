# CLAUDE.md - MOSAIC R Package

This file provides comprehensive guidance for Claude Code when working with the MOSAIC R package.

## 📚 Table of Contents

**Quick Start:**
- [⚡ Quick Reference Card](#-quick-reference-card)
- [🚀 First Time on MOSAIC](#-first-time-on-mosaic)
- [📋 Common Tasks](#-common-tasks-step-by-step)

**Understanding MOSAIC:**
- [Package Overview](#package-overview)
- [Architecture](#architecture)
- [The run_MOSAIC Workflow](#the-run_mosaic-workflow-centerpiece-of-the-package) ⭐ CENTERPIECE
- [Function Organization](#function-organization-223-files)
- [Parameter System](#parameter-system-architecture)

**Development:**
- [Development Standards](#working-with-claude-code-development-standards) 📋 CRITICAL
- [Version Management](#2-package-version-management)
- [Testing](#3-testing-requirements)
- [Git Workflow](#6-git-workflow)
- [Documentation](#7-documentation-standards)

**Deep Dives:**
- [Python Integration](#python-integration)
- [Likelihood Calculation](#likelihood-calculation)
- [NPE Performance](#critical-npe-performance-optimization) ⚡ PERFORMANCE CRITICAL

**Troubleshooting & Reference:**
- [When Things Go Wrong](#-when-things-go-wrong)
- [Key Files Reference](#key-files-reference)
- [Key Design Principles](#key-design-principles)
- [System Requirements](#system-requirements)

---

## ⚡ Quick Reference Card

**Check current state (do this FIRST):**
```bash
git status                      # Any uncommitted changes?
git log --oneline -5           # Recent commits
grep "^Version:" DESCRIPTION   # Current version (you'll bump this)
Rscript -e "devtools::test()" # Baseline: all tests should pass
```

**Development cycle:**
```bash
# 1. Write production-ready code (no placeholders!)
# 2. Add/update tests
Rscript -e "devtools::test()"              # Must pass

# 3. Update docs if function signatures changed
Rscript -e "devtools::document()"

# 4. Check package builds cleanly
R CMD check .
Rscript -e "pkgdown::build_site()"        # If docs changed
```

**Commit workflow (ALWAYS):**
```bash
# 1. Bump version in DESCRIPTION (patch: 0.13.25→0.13.26, minor: 0.13.25→0.14.0, major: 0.13.25→1.0.0)
# 2. Stage relevant files: git add DESCRIPTION R/my_file.R tests/...
# 3. Commit with version: git commit -m "Fix bug (v0.8.8)"
# 4. Push: git push origin main
```

**Essential commands:**
```bash
# Testing
Rscript -e "devtools::test()"                    # Run all tests
Rscript -e "testthat::test_file('tests/testthat/test-foo.R')"  # Single test

# Documentation
Rscript -e "devtools::document()"                # Update docs
Rscript -e "?function_name"                      # Check doc renders
Rscript -e "pkgdown::build_site()"              # Rebuild website

# Package checks
R CMD check .                                     # Full package check
Rscript -e "MOSAIC::check_dependencies()"        # Verify Python env

# Git
git log --oneline -10                            # Recent commits
git diff                                          # See changes
git log --grep="NPE"                             # Search commits
```

**Critical paths (DON'T BREAK THESE):**
- `run_MOSAIC()` → R/run_MOSAIC.R:575 (main calibration workflow)
- `calc_model_likelihood()` → R/calc_model_likelihood.R:66 (called 1000s of times)
- `sample_parameters()` → R/sample_parameters.R (301 parameters)
- NPE optimization → outputs/ subdirectory pattern (100-1200× speedup)

**File rules:**
- ✅ Use `./claude/` for ALL temporary/exploratory files
- ✅ Use `get_paths()` for ALL file operations (never hardcode paths)
- ❌ Never modify: laser-cholera/, ees-cholera-mapping/, jhu_cholera_data/ (read-only)
- ❌ Never modify: MOSAIC-data/raw/ (read-only)
- ❌ Never create files in package root without necessity

**When stuck:**
- Check [Common Tasks](#-common-tasks-step-by-step) for step-by-step guides
- Check [Troubleshooting](#-when-things-go-wrong) for common issues
- Check [Design Decisions](#-why-we-do-this-design-decisions) for context
- ASK the user if unclear!

---

## 🚀 First Time on MOSAIC

**You're a new Claude instance starting work on MOSAIC. Here's what to do FIRST.**

### Step 1: Understand Current State (5 minutes)

```bash
# What version are we at?
grep "^Version:" DESCRIPTION
# Output example: Version: 0.13.25

# What happened recently?
git log --oneline -10
# Shows recent commits and their versions

# Read recent changes
head -50 NEWS.md
# Shows what's been added/fixed recently

# Any uncommitted work?
git status
# Should be clean. If not, ask user before proceeding.

# Do tests pass? (CRITICAL - establish baseline)
Rscript -e "devtools::test()"
# All tests should pass. If not, something's wrong - ask user.
```

### Step 2: Understand the Task

**Ask yourself:**
- [ ] Does this functionality already exist? (223 functions - check first!)
- [ ] Is this a bug fix (patch 0.13.25→0.13.26) or new feature (minor 0.13.25→0.14.0)?
- [ ] Which files will I modify?
- [ ] How does this fit into the run_MOSAIC workflow?
- [ ] Are there tests I can look at for similar functionality?

**Find existing functionality:**
```bash
# Search for similar functions
ls R/ | grep -i "keyword"

# Search code for patterns
git grep "pattern" R/

# Check function reference
Rscript -e "library(MOSAIC); help(package='MOSAIC')"
```

### Step 3: Read Relevant Context

**For different task types, read these sections:**

| Task Type | Read These Sections |
|-----------|-------------------|
| Bug in run_MOSAIC | [run_MOSAIC Workflow](#the-run_mosaic-workflow-centerpiece-of-the-package), [Key Files](#key-files-reference) |
| Bug in likelihood | [Likelihood Calculation](#likelihood-calculation), [Performance](#5-performance-standards) |
| Add parameter | [Parameter System](#parameter-system-architecture), [Common Tasks](#common-tasks-step-by-step) |
| Performance issue | [NPE Optimization](#critical-npe-performance-optimization), [Performance Standards](#5-performance-standards) |
| Documentation | [Documentation Standards](#7-documentation-standards) |
| Any change | [Development Standards](#working-with-claude-code-development-standards) (always!) |

### Step 4: Plan Before Coding

**Before writing ANY code:**
1. Understand what exists (read relevant files)
2. Understand the design (check [Design Decisions](#-why-we-do-this-design-decisions))
3. Plan the approach (think about scale: 40K simulations, 16-32 workers)
4. Write tests first (TDD - test-driven development)
5. Then implement

**Red flags that mean "ask user first":**
- Need to add new R package dependency
- Need to add new Python package
- Need to change function signature of exported function
- Need to modify run_MOSAIC core loop
- Unsure about approach (multiple valid solutions)

### Step 5: Follow Development Standards

See [Development Standards](#working-with-claude-code-development-standards) for complete checklists.

**Core principles:**
- ✅ Production-ready code only (no placeholders)
- ✅ Test before and after changes
- ✅ Bump version on every commit
- ✅ Push to remote when done
- ❌ Never break existing workflows
- ❌ Never add debug code
- ❌ Never hardcode paths

**Now you're ready!** Proceed with your task following the [Development Standards](#working-with-claude-code-development-standards).

---

## 📋 Common Tasks: Step-by-Step

Detailed walkthroughs for common development tasks on MOSAIC.

### Task 1: Fix a Bug

**Test-Driven Development workflow:**

```bash
# 1. Create test that reproduces the bug
cat > tests/testthat/test-my_bugfix.R << 'EOF'
test_that("function handles edge case correctly", {
  edge_case_input <- c(1, 2, NA, 3)
  result <- my_function(edge_case_input, na.rm = TRUE)
  expect_equal(result, expected_value)
  expect_true(is.finite(result))
})
EOF

# 2. Verify test FAILS (proves it catches the bug)
Rscript -e "testthat::test_file('tests/testthat/test-my_bugfix.R')"

# 3. Fix the bug in R/my_function.R
# - Handle NAs, validate inputs, add comments explaining WHY

# 4. Verify test PASSES
Rscript -e "testthat::test_file('tests/testthat/test-my_bugfix.R')"

# 5. Verify ALL tests pass
Rscript -e "devtools::test()"

# 6. Update docs if needed
Rscript -e "devtools::document()"

# 7. Bump version (e.g. 0.13.25 → 0.13.26), commit, push
git add DESCRIPTION R/my_function.R tests/testthat/test-my_bugfix.R
git commit -m "Fix NA handling in my_function (v0.13.26)

- Handle NA values explicitly instead of crashing
- Add validation and clear error messages
- Add regression test for NA edge case"
git push origin main
```

### Task 2: Modify calc_model_likelihood() (HOT PATH!)

**⚠️ CRITICAL: Called THOUSANDS of times - profile before/after!**

```bash
# 1. Understand function: NB likelihood, shape terms, guardrails
# 2. Create performance baseline
cat > /tmp/test_likelihood_performance.R << 'EOF'
library(MOSAIC)
obs_cases <- matrix(rpois(10*100, 10), nrow=10)
est_cases <- matrix(rpois(10*100, 10), nrow=10)
obs_deaths <- matrix(rpois(10*100, 1), nrow=10)
est_deaths <- matrix(rpois(10*100, 1), nrow=10)
system.time({
  for (i in 1:100) {
    ll <- calc_model_likelihood(obs_cases, est_cases, obs_deaths, est_deaths)
  }
})
EOF
Rscript /tmp/test_likelihood_performance.R  # Record baseline

# 3. Make minimal, focused changes - don't break guardrails
# 4. Test performance - must not regress!
Rscript /tmp/test_likelihood_performance.R

# 5. Standard workflow: test, document, version bump, commit, push
```

---

## Package Overview

**MOSAIC** (Metapopulation Outbreak Simulation And Interventions for Cholera) v0.13.25 is a production-quality R package for simulating cholera transmission dynamics across Sub-Saharan Africa. It integrates with the Python laser-cholera simulation engine (https://laser-cholera.readthedocs.io) and provides 223 functions for data processing, parameter estimation, Bayesian calibration, and visualization.

**Key Capabilities:**
- Cholera transmission simulation using metapopulation SEIR models
- Neural Posterior Estimation (NPE) for Bayesian calibration
- Environmental suitability modeling with climate forcing
- Vaccination and WASH intervention analysis
- Spatial transmission dynamics with human mobility

## Critical File Management Rules

### NEVER Create Files in Package Root Without Necessity

**MOSAIC-pkg is a standard R package sensitive to unwanted files.** Always follow these rules:

1. **Use `./claude/` for ALL temporary, exploratory, or analysis files**
2. **Only create files in these locations when essential:**
   - `R/` - New R function files (follow naming conventions)
   - `tests/testthat/` - New test files (prefix with `test-`)
   - `man/` - Auto-generated documentation (via roxygen2)
   - `data/` - Package data objects (.rda files)
   - Files explicitly requested by user

3. **Before creating ANY file, ask:**
   - Is this essential for R package functionality?
   - Has the user explicitly requested this file?
   - Can this go in `./claude/` instead?

### Claude Directory Conventions

When creating files in `./claude/`:
- **Development plans**: `plan_*.md`
- **Analysis reports**: Descriptive names with context
- **Bug summaries**: Include issue type (e.g., `na_bugs_summary.md`)
- **Feature docs**: Clear descriptive names

## Architecture

### Multi-Repository Structure

```
MOSAIC/                          # Root (set via set_root_directory())
├── MOSAIC-pkg/                  # THIS PACKAGE (EDITABLE)
│   ├── R/                       # 223 function files
│   ├── tests/testthat/          # 40 test files
│   ├── inst/
│   │   ├── extdata/             # Default parameters (JSON, 3.9 MB)
│   │   └── py/                  # Python environment.yml
│   ├── data/                    # R data objects (.rda)
│   ├── model/
│   │   ├── input/               # LASER model inputs
│   │   ├── output/              # LASER model outputs
│   │   ├── scenarios/           # Scenario-specific configs and outputs
│   │   └── LAUNCH.R             # Main workflow pipeline
│   ├── vm/                      # VM/cluster launch scripts
│   │   ├── launch_mosaic.R      # Multi-country workflow launcher
│   │   ├── launch_mosaic_individual.R  # Per-country launcher
│   │   └── setup_mosaic*.sh     # Environment setup scripts
│   ├── claude/                  # USE THIS for temporary files
│   ├── man/                     # Auto-generated documentation
│   └── DESCRIPTION              # Package metadata
├── MOSAIC-data/                 # Data repository (EDITABLE)
│   ├── raw/                     # READ-ONLY - never modify
│   └── processed/               # Output from processing functions
├── MOSAIC-docs/                 # Documentation website (EDITABLE)
│   ├── figures/                 # Output from plot_*() functions
│   └── tables/                  # Output tables
├── laser-cholera/               # Python simulation engine (READ-ONLY)
│                                # Docs: https://laser-cholera.readthedocs.io
├── ees-cholera-mapping/         # Web scraping tools (READ-ONLY)
└── jhu_cholera_data/            # JHU scraper (READ-ONLY)
```

### Files Ignored by R CMD check (.Rbuildignore)

These directories are excluded from package builds:
- `deprecated/`, `data-raw/`, `local/`, `azure/`, `model/`, `src/`
- `docs/`, `pkgdown/`, `vignettes/articles`, `.github`, `claude/`

## The run_MOSAIC Workflow: Centerpiece of the Package

### Overview

The `run_MOSAIC()` / `run_mosaic_iso()` workflow is **the centerpiece of the MOSAIC package**. It orchestrates the complete Bayesian calibration pipeline from prior sampling through posterior estimation.

**Files:**
- `R/run_MOSAIC.R` (2,028 lines) - Main workflow and public API (NPE stage refactored out to run_NPE.R in v0.13.0)
- `R/run_MOSAIC_helpers.R` (1,037 lines) - Internal helpers and validation
- `R/run_MOSAIC_infrastructure.R` (350 lines) - Directory setup and I/O
- `R/run_NPE.R` (1000+ lines) - Standalone NPE workflow (see NPE section)

### Two User Interfaces

#### 1. Simple Interface: `run_mosaic_iso()`

**Recommended for most users.** Automatically loads defaults for specified countries.

```r
# Single location
run_mosaic_iso(
  iso_code = "ETH",
  dir_output = "./output/eth_calibration"
)

# Multiple locations
run_mosaic_iso(
  iso_code = c("ETH", "KEN", "TZA"),
  dir_output = "./output/east_africa",
  control = mosaic_control_defaults(
    parallel = list(enable = TRUE, n_cores = 16)
  )
)
```

#### 2. Advanced Interface: `run_MOSAIC()`

**Full control over model specification.** Accepts custom config and priors.

```r
# Load and customize config
config <- get_location_config(iso = "ETH")
config$date_start <- "2020-01-01"
config$date_stop <- "2024-12-31"

# Load and customize priors
priors <- get_location_priors(iso = "ETH")

# Run with custom settings
run_MOSAIC(
  config = config,
  priors = priors,
  dir_output = "./output/custom_run",
  control = mosaic_control_defaults(
    calibration = list(
      n_simulations = 5000,
      n_iterations = 5
    )
  )
)
```

### Workflow Stages

The workflow executes two main stages:

#### **STAGE 1: Bayesian Filtering with Resampling (BFRS)**

Three-phase adaptive calibration:

**Phase 1: Adaptive Calibration**
- Runs batches until convergence (R² target achieved)
- **Auto mode** (default): `n_simulations = NULL`
  - Adaptive batch sizing based on convergence
  - Stops when `target_r2` reached (default 0.5)
  - Monitors ESS (Effective Sample Size) per parameter
- **Fixed mode**: `n_simulations = 5000`
  - Runs exact number specified
  - No early stopping

**Phase 2: Single Predictive Batch**
- Size calculated from calibration phase
- Generates samples for posterior inference
- Uses importance sampling with weights from Phase 1

**Phase 3: Adaptive Fine-tuning**
- 5-tier batch sizing system:
  - **Massive**: 20,000 simulations
  - **Large**: 10,000 simulations
  - **Standard**: 5,000 simulations
  - **Precision**: 2,500 simulations
  - **Final**: 1,000 simulations
- Tier selected based on convergence metrics
- Refines posterior around high-likelihood regions

**Outputs:**
Results are written to `dir_output/1_bfrs/` with parameter files, simulation outputs, and convergence diagnostics.

**Note:** Output directory structure is subject to change as the package evolves.

#### **STAGE 2: Neural Posterior Estimation (NPE)** *(Optional)*

- **When enabled:** `control$npe$enable = TRUE`
- Trains normalizing flow model on BFRS results
- Provides fast posterior sampling without additional simulations
- Uses PyTorch/Zuko for neural density estimation
- **Standalone mode:** `run_NPE()` can be called post-hoc on any BFRS output directory without re-running calibration — useful for experimenting with different weight strategies (`continuous_best`, `continuous_retained`, etc.)
- **NA handling (v0.13.5):** Interior NAs in observed data are linearly interpolated; boundary NAs set to 0 (prevents artificial "no cases" observations from misleading the model)
- **Input validation (v0.13.4):** `train_npe()` validates X, y, weights before tensor conversion — catches NaN/Inf data corruption early

**Outputs:**
Results are written to `dir_output/2_npe/` with trained model, posterior samples, and diagnostics.

### Control Structure

Created with `mosaic_control_defaults()`:

```r
control <- mosaic_control_defaults(
  # === CALIBRATION SETTINGS ===
  calibration = list(
    n_simulations = NULL,          # NULL = auto mode, integer = fixed mode
    n_iterations = 3,              # LASER iterations per simulation
    max_simulations = 100000,      # Safety limit for auto mode
    batch_size = 1000,             # Simulations per batch
    min_batches = 3,               # Minimum batches before convergence check
    max_batches = 100,             # Maximum batches in auto mode
    target_r2 = 0.5                # R² convergence threshold
  ),

  # === PARAMETER SAMPLING ===
  sampling = list(
    sample_tau_i = TRUE,           # Sample diffusion parameter
    sample_beta_j0_tot = TRUE,     # Sample transmission rate
    sample_p_beta = TRUE,          # Sample H2H proportion
    sample_theta_j = TRUE,         # Sample WASH coverage
    sample_mu_j = TRUE,            # Sample case fatality ratio
    # ... (see sample_parameters.R for all 34 parameters)
  ),

  # === PARALLELIZATION ===
  parallel = list(
    enable = FALSE,                # Set TRUE for cluster execution
    n_cores = 1,                   # Number of parallel workers
    type = "PSOCK",                # "PSOCK" or "FORK"
    progress = TRUE                # Show progress bar
  ),

  # === CONVERGENCE TARGETS ===
  targets = list(
    ESS_param = 100,               # Target ESS per parameter
    ESS_param_prop = 0.1,          # Proportion threshold (10%)
    ESS_best = 50,                 # Target for top-weighted samples
    A_best = 0.01,                 # Proportion for "best" (top 1%)
    CVw_best = 2.0,                # CV of weights threshold
    percentile_max = 99            # Percentile for fine-tuning
  ),

  # === FINE-TUNING ===
  fine_tuning = list(
    batch_sizes = list(
      massive = 20000,
      large = 10000,
      standard = 5000,
      precision = 2500,
      final = 1000
    )
  ),

  # === NEURAL POSTERIOR ESTIMATION ===
  npe = list(
    enable = FALSE,                # Set TRUE to enable NPE stage
    weight_strategy = "quantile",  # or "threshold"
    quantile = 0.95                # Use top 5% of weighted samples
  ),

  # === LIKELIHOOD CALCULATION ===
  likelihood = list(
    add_max_terms = TRUE,
    add_peak_timing = TRUE,
    add_peak_magnitude = TRUE,
    add_cumulative_total = TRUE,
    add_wis = TRUE,
    weight_cases = 1.0,
    weight_deaths = 1.0,
    weight_max_terms = 0.5,
    weight_peak_timing = 0.5,
    weight_peak_magnitude = 0.5,
    weight_cumulative_total = 0.3,
    weight_wis = 0.8,
    enable_guardrails = TRUE,
    floor_likelihood = -999999999
  ),

  # === I/O SETTINGS ===
  io = list(
    format = "parquet",            # "parquet" or "csv"
    compression = "zstd",          # "none", "snappy", "gzip", "lz4", "zstd"
    compression_level = 10         # 1-22 for zstd
  )
)
```

### Simulation Worker Architecture

**Key Features:**
- **Reproducible seeding scheme**:
  - `seed_sim = sim_id` (parameters)
  - `seed_iter = (sim_id - 1) * n_iterations + j` (LASER model)
- **Iteration collapsing**: Multiple LASER runs averaged via log-mean-exp
- **Memory management**: Explicit garbage collection to prevent Python object buildup
- **Error handling**: Graceful failure without crashing entire batch

**Located:** `R/run_MOSAIC.R` lines 29-281 (`.mosaic_run_simulation_worker()`)

### Parallel Execution

**CRITICAL Thread Safety:**
```r
# Each worker limited to single-threaded BLAS
MOSAIC:::.mosaic_set_blas_threads(1L)

# Without this:
# 16 workers × 8 BLAS threads = 128 total (severe oversubscription!)
# With this:
# 16 workers × 1 BLAS thread = 16 threads (optimal!)
```

**Cluster Types:**
- **PSOCK** (default): Works on all platforms, requires explicit export
- **FORK** (Linux/Mac): Faster startup, shares memory

**Setup:** `R/run_MOSAIC.R` lines 757-826

### Adaptive Convergence Detection

**Metrics monitored:**
- **R² (correlation)**: Model fit to observed data
- **ESS (Effective Sample Size)**: Per parameter and overall
- **CVw (Coefficient of Variation of Weights)**: Weight distribution quality
- **A_best**: Proportion of high-quality samples

**Early stopping when:**
- R² ≥ `target_r2` (default 0.5)
- ESS meets target (default 100 per parameter)
- CVw_best < 2.0 (well-distributed weights)

**Located:** `R/run_MOSAIC_helpers.R` (`.mosaic_check_convergence()`)

### Output Structure

**Main directories:**
- `dir_output/1_bfrs/` - Stage 1 calibration results (parameters, simulations, diagnostics)
- `dir_output/2_npe/` - Stage 2 NPE results (if enabled)
- `dir_output/results/` - Final posterior summaries and visualizations

**Note:** Specific file structure is subject to change as the package evolves. Use functions like `read_calibration_results()` rather than hardcoding file paths.

### Performance Considerations

**Batch Size:**
- Too small: Overhead dominates, slow convergence
- Too large: Memory issues, long iteration times
- **Recommended**: 500-2000 simulations per batch

**Iterations per Simulation:**
- `n_iterations = 1`: Fastest, but noisier likelihood estimates
- `n_iterations = 3`: Good balance (default)
- `n_iterations = 5+`: More stable, but slower

**Parallel Scaling:**
- Linear speedup up to ~32 cores (depends on batch size)
- Beyond 32 cores: Diminishing returns due to overhead
- **Best practice**: `batch_size` ≥ `n_cores` for efficiency

**Memory Usage:**
- ~1-2 GB per worker (LASER simulations)
- Scale accordingly: 16 cores ≈ 32 GB RAM
- NPE training: Additional 4-8 GB for PyTorch

## Function Organization (223 Files)

### Naming Conventions

Functions follow strict prefixes indicating their purpose:

| Prefix | Purpose | Count | Examples |
|--------|---------|-------|----------|
| `process_*()` | Data cleaning & preprocessing | 25 | `process_WHO_weekly_data()`, `process_climate_data()` |
| `est_*()` | Parameter estimation | 18 | `est_initial_E_I()`, `est_vaccination_rate()`, `est_WASH_coverage()` |
| `plot_*()` | Visualization & figures | 30+ | `plot_model_ppc()`, `plot_vaccination_maps()`, `plot_model_convergence()` |
| `get_*()` | Data retrieval & utilities | 25+ | `get_paths()`, `get_climate_historical()`, `get_WASH_data()` |
| `calc_*()` | Mathematical calculations | 25+ | `calc_model_likelihood()`, `calc_spatial_correlation()` |
| `check_*()` | Validation & verification | 5 | `check_dependencies()`, `check_affine_normalization()` |
| `sample_*()` | Parameter sampling | 2 | `sample_parameters()`, `sample_from_prior()` |

### Function Categories by Purpose

**Core LASER Integration:**
- `run_LASER.R` - Wrapper for Python laser-cholera simulations (68 lines)
- `make_LASER_config.R` - Config validation & creation (897 lines, validates 60+ parameters)
- `get_default_LASER_config.R` - Retrieve default configurations

**Run MOSAIC Workflow (CENTERPIECE):**
- `run_MOSAIC.R` - Main calibration workflow (2,028 lines)
- `run_MOSAIC_helpers.R` - Internal helpers (1,037 lines)
- `run_MOSAIC_infrastructure.R` - Directory setup (350 lines)
- `run_NPE.R` - Standalone NPE workflow (1000+ lines); dual-mode: embedded (called by run_MOSAIC) OR standalone post-hoc on any BFRS output

**Neural Posterior Estimation (NPE):**
- `npe.R` - Main NPE workflow (train_npe, sample_posterior, etc.)
- `npe_utils.R` - Helper functions for data preparation
- `npe_diagnostics.R` - Convergence & quality checks
- `npe_plots.R` - Posterior visualization
- `npe_posterior.R` - Posterior sampling & analysis
- `calc_npe_diagnostics.R` - Quantitative diagnostics

**Parameter System:**
- `sample_parameters.R` - Sample from priors (301 parameters: 21 global + 280 location-specific)
- `priors_default.R` - Documentation for default priors
- Initial condition estimation: `est_initial_S.R`, `est_initial_E_I.R`, `est_initial_R.R`, `est_initial_V1_V2.R`

**Likelihood & Model Evaluation:**
- `calc_model_likelihood.R` - Multi-component likelihood (NB + shape terms + guardrails)
- `calc_model_convergence.R` - MCMC convergence diagnostics
- `calc_model_posterior_distributions.R` - Posterior summaries
- `calc_model_weights_gibbs.R` - Importance weight calculation
- `calc_model_ess.R` - Effective sample size

**Data Processing Pipeline:**
- Climate: `download_climate_data.R`, `process_climate_data.R`
- Surveillance: `process_WHO_weekly_data.R`, `process_JHU_data.R`, `process_SUPP_data.R`
- Demographics: `process_UN_demographics_data.R`, `est_demographic_rates.R`
- Vaccination: `process_WHO_vaccination_data.R`, `est_vaccination_rate.R`
- WASH: `get_WASH_data.R`, `est_WASH_coverage.R`

**Visualization:**
- Model diagnostics: `plot_model_ppc.R`, `plot_model_convergence.R`, `plot_model_fit_stochastic.R`
- Spatial: `plot_mosaic_country_map.R`, `plot_vaccination_maps.R`, `plot_mobility.R`
- Parameters: `plot_seasonal_transmission.R`, `plot_shedding_rate.R`, `plot_vibrio_decay_rate.R`

**Utilities:**
- `get_paths.R` - Directory structure management (CRITICAL for all file operations)
- `set_root_directory.R` - Set MOSAIC root directory
- ISO code conversions: 8 files for geographic coding

**Climate & ENSO Utilities:**
- `adjust_ENSO_baseline.R` - Harmonizes climate baselines for ENSO data (v0.13.22+); includes time series visualization

**Python Environment:**
- `install_dependencies.R` - Set up virtualenv from `inst/py/environment.yml`
- `check_dependencies.R` - Comprehensive diagnostics with capability checks
- `remove_MOSAIC_python_env.R` - Cleanup for troubleshooting
- `check_python_env.R`, `lock_python_env.R`, `get_python_paths.R`

## Key Data Objects

### Package Data (`data/*.rda` - 17 files)

**Configurations:**
- `config_default.rda` - Default LASER configuration (158 KB)
- `config_simulation_endemic.rda` - 5-year endemic simulation
- `config_simulation_epidemic.rda` - 1-year outbreak simulation

**Parameters:**
- `priors_default.rda` - Prior distributions for 301 parameters (9.7 KB)
- `estimated_parameters.rda` - Literature-derived estimates

**Geographic:**
- `iso_codes_africa.rda`, `iso_codes_ssa.rda`, `iso_codes_who_afro.rda`
- `iso_codes_africa_{north,west,east,central,south}.rda`
- `iso_codes_mosaic.rda` - 40 countries used in MOSAIC

**Outbreak Data:**
- `epidemic_peaks.rda` - Historical outbreak timing data

### External Data (`inst/extdata/`)

- `default_parameters.json` - Full parameter file (3.9 MB, 133 KB compressed)
- `priors_default.json` - JSON format priors (147 KB)
- `sim_endemic_parameters.json` - Endemic simulation config (297 KB)
- `enso_forecast_current.json`, `dmi_forecast_current.json` - Hardcoded climate fallbacks

## Python Integration

### Environment Setup

**Location:** `~/.virtualenvs/r-mosaic` (fixed path)

**Three capability tiers:**
1. **Core** (required): laser-cholera, laser-core, numpy, h5py, pyarrow
2. **Suitability** (optional): tensorflow, keras
3. **NPE** (optional): torch, sbi, lampe, zuko, scikit-learn

**Install/Check:**
```r
# First time setup
MOSAIC::install_dependencies()

# Check installation
MOSAIC::check_dependencies()

# Troubleshooting
MOSAIC::remove_MOSAIC_python_env()
MOSAIC::install_dependencies(force = TRUE)
```

### Critical: Thread Safety

**ALWAYS set BLAS threads to 1 before importing PyTorch** to prevent cluster deadlocks:

```r
# This is built into train_npe() but critical for custom PyTorch usage
if (exists(".mosaic_set_blas_threads", envir = asNamespace("MOSAIC"))) {
  MOSAIC:::.mosaic_set_blas_threads(1L)
}

Sys.setenv(
  OMP_NUM_THREADS = "1",
  MKL_NUM_THREADS = "1",
  OPENBLAS_NUM_THREADS = "1",
  NUMEXPR_NUM_THREADS = "1",
  TBB_NUM_THREADS = "1",       # Intel TBB (used by some PyTorch backends)
  NUMBA_NUM_THREADS = "1"      # Numba JIT (used by laser-cholera)
)

# Then import
torch <- reticulate::import("torch")
```

**Why:** PyTorch initializes BLAS/MKL on import. Numba (used by laser-cholera) and Intel TBB also spawn threads. Without all six env vars, parallel workers can trigger "Attempted to fork from a non-main thread" deadlocks on clusters.

### LASER Model Integration

```r
# Import laser-cholera
lc <- reticulate::import("laser_cholera")

# Run model directly
model <- lc$metapop$model$run_model(
  paramfile = "config.json",
  seed = 123L
)

# Or use wrapper (recommended)
model <- run_LASER(
  paramfile = "config.json",
  seed = 123L,
  visualize = FALSE,
  outdir = tempdir()
)
```

## Parameter System Architecture

### Total: 301 Parameters

**21 Global Parameters:**
- **Transmission:** alpha_1, alpha_2, beta dynamics
- **Disease progression:** iota (incubation), gamma_1/gamma_2 (recovery), epsilon (immunity waning)
- **Vaccination:** phi_1/phi_2 (effectiveness), omega_1/omega_2 (waning)
- **Environment:** decay_days_short/long, decay_shape_1/2, zeta_1/zeta_2, kappa
- **Observation:** rho (reporting), sigma (symptomatic proportion)
- **Mobility:** mobility_gamma, mobility_omega

**280 Location-Specific (40 countries × 7 params):**
- `beta_j0_tot` - Total baseline transmission rate
- `p_beta` - Proportion human-to-human transmission
- `tau_i` - Travel/diffusion probability
- `theta_j` - WASH coverage (proportion with adequate WASH)
- `mu_j` - Case fatality ratio
- `a1, a2, b1, b2` - Seasonality Fourier coefficients
- `psi_star_a, psi_star_b, psi_star_z, psi_star_k` - Suitability calibration

**6 Initial Conditions (per location):**
- `S_j_initial`, `E_j_initial`, `I_j_initial`, `R_j_initial`, `V1_j_initial`, `V2_j_initial`

### Prior Distributions

**Default priors in `priors_default.rda` / `inst/extdata/priors_default.json`**

**Initial Conditions (Beta distributions for epidemiological realism):**
- S (Susceptible): Beta(30, 7.5) → 80% mean
- V1 (One dose): Beta(0.5, 49.5) → 1% mean
- V2 (Two doses): Beta(0.5, 99.5) → 0.5% mean
- E (Exposed): Beta(0.01, 9999.99) → 0.0001% mean
- I (Infected): Beta(0.01, 9999.99) → 0.0001% mean
- R (Recovered): Beta(3.5, 14) → 20% mean

**Transmission Parameters:**
- alpha_1: Beta distribution (range 0-1)
- alpha_2: Beta distribution (range 0-1)
- beta_j0_hum/env: Gamma distributions (country-specific)

**Disease Progression:**
- iota: Lognormal distribution
- gamma_1: Uniform(1/7, 1/3) day⁻¹
- gamma_2: Uniform(1/14, 1/7) day⁻¹
- epsilon: Lognormal distribution

## Likelihood Calculation

### calc_model_likelihood() Architecture

**Located:** `R/calc_model_likelihood.R`

**Core Component:** Negative Binomial time-series likelihood
- Per location and outcome (cases, deaths)
- Weighted MoM dispersion estimation with k_min floor (default 3)
- Handles NA values gracefully

**Shape Terms (optional, with default weights):**
1. **Multi-peak timing** (weight 0.5): Normal likelihood on time differences between matched peaks
2. **Multi-peak magnitude** (weight 0.5): Log-Normal on peak ratio with adaptive sigma
3. **Cumulative progression** (weight 0.3): NB at fractions [0.25, 0.5, 0.75, 1.0]
4. **Max terms** (weight 0.5): Legacy Poisson maxima (default ON)
5. **WIS** (weight 0.8): Weighted Interval Score across quantiles (default ON)

**Guardrails (enabled by default):**
- Cumulative over-prediction: `sum(est) / sum(obs) > 10` → floor
- Cumulative under-prediction: `sum(est) / sum(obs) < 0.1` → floor
- Per-timestep max ratio: `est[i] / obs[i] > 100` → floor
- Per-timestep min ratio: `est[i] / obs[i] < 0.01` → floor
- Negative correlation: `cor(est, obs) < 0` → floor
- Returns `floor_likelihood = -999999999` on violations

**Usage:**
```r
ll <- calc_model_likelihood(
  obs_cases = obs_cases_matrix,      # n_locations × n_timesteps
  est_cases = est_cases_matrix,
  obs_deaths = obs_deaths_matrix,
  est_deaths = est_deaths_matrix,
  weight_cases = 1,
  weight_deaths = 1,
  weights_location = NULL,           # Uniform if NULL
  weights_time = NULL,               # Uniform if NULL
  config = config,                   # Optional (for metadata)
  nb_k_min = 3,                      # Dispersion floor
  add_peak_timing = TRUE,            # Enable shape term
  add_peak_magnitude = TRUE,
  add_cumulative_total = TRUE,
  add_max_terms = TRUE,
  add_wis = TRUE,
  enable_guardrails = TRUE,          # Enable safety checks
  guardrail_verbose = FALSE,         # Print violations
  verbose = FALSE                    # Print component summaries
)
```

## CRITICAL: NPE Performance Optimization

### Problem: 40K Simulation Files

During BFRS sampling, ~40K simulations create individual `out_NNNNNNN.parquet` files. Only 1-5% receive non-zero NPE weights.

### WRONG Approach (Removed in v0.8.7)
```
# OLD: Combined all files, then filtered
simulations.parquet (194 MB, all 40K) → filter to weighted → 2-5 MB
Time: 30-120 seconds
Memory: High
```

### CORRECT Approach (Current)
```
outputs/                          # Subdirectory for organization
├── timeseries_0000001.parquet   # Individual simulation files
├── timeseries_0000002.parquet
└── ... (39,465 files)

simulations.parquet               # Metadata only (18 MB)
convergence_results.parquet       # Diagnostics
priors.json                       # Parameter priors

# Load only weighted simulation files directly
Time: 0.1-0.5 seconds
Memory: Low
Speedup: 100-1200× faster (2-3 orders of magnitude)
```

### Implementation Details

**Directory structure created at:** `R/run_MOSAIC.R` (directory setup section)

**Output file creation:** `.mosaic_run_simulation_worker()` line 268 writes to `outputs/` subdirectory

**Loading logic:** `R/npe.R` in `.prepare_npe_data()` function
```r
# Load only weighted simulation files
outputs_dir <- file.path(iteration_dir, "outputs")
weighted_files <- sprintf("timeseries_%07d.parquet", sim_ids_weighted)
# Read each file directly
```

**Cleanup:** After NPE completes
```r
# Remove entire outputs/ directory
unlink(file.path(iteration_dir, "outputs"), recursive = TRUE)
# Frees 50-200 MB disk space
```

**IMPORTANT:**
- ❌ DO NOT combine individual files into `outputs.parquet`
- ✅ DO organize files in `outputs/` subdirectory
- ✅ DO keep individual files until NPE training completes
- ✅ Entire `outputs/` directory removed automatically after NPE

## Testing

### Running Tests

```bash
# All tests
Rscript -e "devtools::test()"

# Specific test file
Rscript -e "testthat::test_file('tests/testthat/test-calc_model_likelihood.R')"

# With coverage
Rscript -e "covr::package_coverage()"

# Single function test
Rscript -e "testthat::test_file('tests/testthat/test-calc_spatial_correlation.R')"
```

### Test Structure (40 files)

**Organized by component:**
- Likelihood: `test-calc_log_likelihood_negbin.R`, `test-calc_log_likelihood_poisson.R`
- Model evaluation: `test-calc_model_likelihood.R`, `test-calc_model_convergence.R`
- Spatial: `test-calc_spatial_correlation.R`, `test-calc_spatial_hazard.R`
- Initial conditions: `test-est_initial_E_I.R`, `test-est_initial_R.R`, `test-est_initial_V1_V2.R`
- NPE: `test-npe_v5_2.R`, `test-get_npe_observed_data.R`, `test-get_npe_simulated_data.R`
- Configuration: `test-convert_config_to_dataframe.R`, `test-get_location_config.R`
- Statistics: `test-weighted_statistics.R`, `test-calc_kl_divergence.R`

## Development Guidelines

### Adding New Functions

1. **Create function file in `R/` following naming convention**
   ```r
   # R/est_new_parameter.R or R/plot_new_diagnostic.R
   ```

2. **Add roxygen2 documentation**
   ```r
   #' Estimate New Parameter
   #'
   #' @param data Input data
   #' @param ... Additional arguments
   #' @return Estimated parameter value
   #' @export
   est_new_parameter <- function(data, ...) {
     # Implementation
   }
   ```

3. **Update documentation**
   ```bash
   Rscript -e "devtools::document()"
   ```

4. **Add unit tests in `tests/testthat/`**
   ```r
   # tests/testthat/test-est_new_parameter.R
   test_that("est_new_parameter returns valid output", {
     result <- est_new_parameter(test_data)
     expect_true(is.numeric(result))
     expect_true(result > 0)
   })
   ```

5. **Run tests**
   ```bash
   Rscript -e "devtools::test()"
   ```

6. **Check package**
   ```bash
   R CMD check .
   ```

### Path Management

**ALWAYS use `get_paths()` for file operations:**

```r
PATHS <- get_paths()

# Read from processed data
data <- arrow::read_parquet(file.path(PATHS$DATA_PROCESSED, "cholera/weekly/ETH.parquet"))

# Write to model input
write.csv(config, file.path(PATHS$MODEL_INPUT, "parameters.csv"))

# Save figures
ggplot2::ggsave(file.path(PATHS$DOCS_FIGURES, "transmission_map.png"))
```

**Available paths (see `R/get_paths.R`):**
- `PATHS$ROOT` - MOSAIC root directory
- `PATHS$DATA_RAW`, `PATHS$DATA_PROCESSED` - Data directories
- `PATHS$DATA_CLIMATE`, `PATHS$DATA_ENSO`, `PATHS$DATA_WHO_WEEKLY`, etc.
- `PATHS$MODEL_INPUT`, `PATHS$MODEL_OUTPUT` - Model directories
- `PATHS$DOCS_FIGURES`, `PATHS$DOCS_TABLES` - Documentation outputs

### Error Handling

**Parameter validation:**
```r
# Extensive validation in make_LASER_config()
if (!is.numeric(iota) || length(iota) != 1 || iota <= 0) {
  stop("iota must be a numeric scalar greater than zero.")
}

if (!is.matrix(b_jt) || nrow(b_jt) != length(location_name) || ncol(b_jt) != length(t)) {
  stop("b_jt must be a matrix with rows equal to length(location_name) and columns equal to the daily sequence from date_start to date_stop.")
}
```

**Graceful degradation:**
```r
# Handle missing data
data <- tryCatch(
  arrow::read_parquet(file_path),
  error = function(e) {
    warning("Could not read file: ", file_path, "\nUsing defaults.")
    return(default_data)
  }
)
```

### Documentation

**All exported functions MUST have roxygen2 documentation:**
```r
#' Function Title
#'
#' @description
#' Detailed description of what the function does.
#'
#' @param param1 Description of param1
#' @param param2 Description of param2
#' @return Description of return value
#'
#' @examples
#' \dontrun{
#' result <- my_function(param1 = "value")
#' }
#'
#' @export
my_function <- function(param1, param2) {
  # Implementation
}
```

**Build documentation:**
```bash
Rscript -e "devtools::document()"
Rscript -e "pkgdown::build_site()"
```

## Key Design Principles

- **Version every commit:** Enables exact bug reproduction. `git log --grep="v0.13.25"` finds the change instantly.
- **NPE uses `outputs/` subdirectory:** Load only weighted files (1-5%) directly — 100-1200× faster than combining all 40K files first.
- **Thread safety (6 env vars):** BLAS, OpenMP, Intel TBB, and Numba all need `= "1"` per worker or parallel execution deadlocks.
- **`get_paths()` everywhere:** Hardcoded paths break across machines. User calls `set_root_directory()` once; `get_paths()` handles the rest.
- **No placeholder code:** Every commit is production-ready. Can't implement fully? Don't commit — ask for help.
- **Concise roxygen2:** One-line `@param`. Detailed explanations go in vignettes/CLAUDE.md. Verbose docs cause R CMD check warnings.
- **Design for scale, test small:** Test with 10-100 sims (seconds), write code that handles 40K. Pre-allocate, vectorize, avoid growing objects.
- **Push after every completed unit:** Version bump = "this is done" = push immediately.

---

## 🔧 When Things Go Wrong

Systematic troubleshooting guide for common issues.

### Tests Failing After Your Changes

**Step 1: Identify which test is failing**
```bash
# Run all tests to see failures
Rscript -e "devtools::test()"

# Run just the failing test for details
Rscript -e "testthat::test_file('tests/testthat/test-my_function.R')"
```

**Step 2: Common causes and fixes**

| Symptom | Likely Cause | Fix |
|---------|-------------|-----|
| "Object not found" | Function not exported or typo | Check @export tag, check spelling |
| "Unexpected type" | Function signature changed | Update all calls to new signature |
| "NA/NaN produced" | Edge case not handled | Add NA handling, check for empty inputs |
| "Dimensions don't match" | Matrix/vector size changed | Check assumptions about data dimensions |
| "Test times out" | Infinite loop or very slow | Add browser() before problem line, debug |

**Step 3: Debug interactively**
```r
# Load package in dev mode
devtools::load_all()

# Run the problematic code step by step
x <- test_input
result <- my_function(x)  # Add browser() in function to step through
```

**Step 4: Check if you broke a dependency**
```bash
# Find functions that call your modified function
grep -r "my_function(" R/
```

---

### R CMD Check Errors or Warnings

**Common errors and quick fixes:**

| Error | Fix |
|-------|-----|
| "Undocumented parameters" | Add missing `@param` tags matching function signature |
| "Functions not exported" | Add `@export` tag, run `devtools::document()` |
| "Broken \\link{}" | Use `\code{\link{function}}` not `\link{function}` |
| "Undefined global variable" | Add to `R/globals.R`: `utils::globalVariables(c("var"))` |
| "Complex \\eqn{} error" | Simplify equation or move to vignette |

---

### pkgdown::build_site() Failing

**Error: "Can't find function"**

**Causes:** (1) Function not exported - add `@export` tag; (2) Function in `_pkgdown.yml` but doesn't exist - remove or fix typo; (3) Docs not regenerated - run `devtools::document()` first

**Workflow for function changes:**
```bash
devtools::document()              # Update docs
# Update _pkgdown.yml if needed
pkgdown::build_site()             # Test build
```

---

### Python Environment Issues

**Symptom:** "Failed to import laser_cholera" or reticulate crashes

**Solution steps:**
```bash
# Step 1: Check current configuration
Rscript -e "MOSAIC::check_python_env()"

# Step 2: Check dependencies comprehensively
Rscript -e "MOSAIC::check_dependencies()"

# Step 3: Check if environment exists
ls -la ~/.virtualenvs/r-mosaic/

# Step 4: If environment is broken, reinstall
Rscript -e "MOSAIC::remove_MOSAIC_python_env()"
Rscript -e "MOSAIC::install_dependencies(force = TRUE)"

# Step 5: Restart R and try again
Rscript -e "library(MOSAIC); MOSAIC::check_dependencies()"
```

**If problem persists:**
```bash
# Check virtualenv
ls -la ~/.virtualenvs/r-mosaic/bin/python

# Check Python version
~/.virtualenvs/r-mosaic/bin/python --version  # Should be 3.9+

# Manually check laser-cholera
~/.virtualenvs/r-mosaic/bin/python -c "import laser_cholera; print('OK')"
```

---

### PyTorch Hangs When Importing (Cluster Deadlock)

**Symptom:** Code hangs indefinitely when running `torch <- reticulate::import("torch")`

**Cause:** BLAS threading conflict

**Fix is built into run_MOSAIC and train_npe, but for custom code:**
```r
# BEFORE importing torch:
MOSAIC:::.mosaic_set_blas_threads(1L)

Sys.setenv(
  OMP_NUM_THREADS = "1", MKL_NUM_THREADS = "1", OPENBLAS_NUM_THREADS = "1",
  NUMEXPR_NUM_THREADS = "1", TBB_NUM_THREADS = "1", NUMBA_NUM_THREADS = "1"
)

# NOW safe to import:
torch <- reticulate::import("torch")
```

---

### Memory Issues During Calibration

**Symptom:** "Cannot allocate vector" or out of RAM

**Diagnosis:** `free -h` (Linux) or `vm_stat` (macOS)

**Common causes:**
1. **Too many workers** - Each uses ~2 GB. With 16 GB RAM: max 8 workers
2. **Growing objects in loops** - Use `results <- vector("list", n)` not `results <- c()`
3. **No cleanup** - Add periodic `gc()` every 100 iterations

---

### Git Push Rejected

**Cause:** Remote has commits you don't have

**Fix:** `git fetch origin && git pull origin main` → resolve conflicts if any → `git push origin main`

---

### Quick Diagnostic Checklist

When something breaks, run through this checklist:

- [ ] Did tests pass before my changes? (`git log` to see what changed)
- [ ] Do tests pass now? (`devtools::test()`)
- [ ] Does package check cleanly? (`R CMD check .`)
- [ ] Did I regenerate docs? (`devtools::document()`)
- [ ] Did I update _pkgdown.yml if needed?
- [ ] Do I have all dependencies? (`MOSAIC::check_dependencies()`)
- [ ] Am I using `get_paths()` for file operations?
- [ ] Did I add `@export` if function should be exported?
- [ ] Did I bump version?
- [ ] Can I reproduce the issue with a minimal example?

**Still stuck? ASK THE USER!**

---

## Key Files Reference

### Critical Core Files

1. **`R/run_MOSAIC.R`** (2,028 lines) **[CENTERPIECE]**
   - Complete Bayesian calibration workflow
   - Three-phase adaptive calibration
   - Parallel execution infrastructure
   - **Must read to understand MOSAIC workflow**

2. **`R/run_MOSAIC_helpers.R`** (1,037 lines)
   - Control validation and merging
   - Convergence detection algorithms
   - Internal helper functions
   - **Reference for extending run_MOSAIC**

3. **`R/run_MOSAIC_infrastructure.R`** (350 lines)
   - Directory setup and organization
   - I/O operations (parquet/CSV)
   - Logging infrastructure
   - **Reference for file structure**

4. **`R/make_LASER_config.R`** (897 lines)
   - Validates all 60+ model parameters
   - Extensive checks on dimensions, types, ranges
   - Handles compartments, demographics, vaccination, FOI
   - **Always read before modifying parameter structure**

5. **`R/run_LASER.R`** (68 lines)
   - Simple wrapper for Python laser-cholera
   - Suppresses NumPy warnings
   - **Reference for Python integration patterns**

6. **`R/sample_parameters.R`** (500+ lines)
   - Samples all 301 parameters from priors
   - Handles both global and location-specific
   - **Read before modifying parameter sampling**

7. **`R/calc_model_likelihood.R`** (1000+ lines)
   - Multi-component likelihood calculation
   - Guardrails for numerical stability
   - **Critical for understanding calibration**

8. **`R/npe.R`** (1500+ lines)
   - Core NPE functions (train_npe, sample_posterior, etc.)
   - PyTorch integration
   - **Reference for NPE internals**

8b. **`R/run_NPE.R`** (1000+ lines)
   - Standalone NPE workflow callable post-hoc
   - Dual-mode: embedded (called by run_MOSAIC) OR standalone on existing BFRS output
   - Experiment with weight strategies without re-running calibration
   - **Reference for advanced calibration**

9. **`R/get_paths.R`** (81 lines)
   - Defines all directory paths
   - **Must use for all file operations**

10. **`R/check_dependencies.R`** (274 lines)
    - Comprehensive environment diagnostics
    - Three capability tiers (core, suitability, NPE)
    - **Reference for troubleshooting**

### Configuration Files

- `inst/extdata/default_parameters.json` (3.9 MB) - Full default config
- `inst/py/environment.yml` - Python dependencies
- `.Rbuildignore` - Files excluded from package build

### Documentation Files

- `README.md` - Package overview
- `NEWS.md` - Version history and changes
- `man/` - Auto-generated function documentation

## System Requirements

**R:** >= 4.1.1

**System Libraries:**
- GDAL >= 2.0.0
- PROJ >= 4.8.0
- GEOS >= 3.4.0
- UDUNITS-2

**Python:** >= 3.9 (managed via virtualenv at `~/.virtualenvs/r-mosaic`)

**Critical R Packages:**
- reticulate (Python interface)
- arrow (fast parquet I/O)
- sf (spatial operations)
- dplyr, data.table (data manipulation)
- ggplot2 (visualization)

**Optional R Packages:**
- mobility (JAGS-based mobility estimation - rarely needed)
- tensorflow, keras3 (suitability modeling)
- testthat (testing)

**Python Packages (managed automatically):**
- **Core:** laser-cholera, laser-core, numpy, h5py, pyarrow
- **Suitability:** tensorflow, keras
- **NPE:** torch, sbi, lampe, zuko, scikit-learn

## Additional Resources

**Documentation:** https://institutefordiseasemodeling.github.io/MOSAIC-docs/

**Package Website:** https://institutefordiseasemodeling.github.io/MOSAIC-pkg/

**GitHub Issues:** https://github.com/InstituteforDiseaseModeling/MOSAIC-pkg/issues

## Working with Claude Code: Development Standards

### Core Principles

Claude Code working on MOSAIC must act as a **production systems software engineer** maintaining a clean, efficient, production-ready scientific computing codebase.

### Non-Negotiable Requirements

#### 1. Code Quality Standards

**NEVER:**
- Use placeholder code (e.g., `# TODO: implement this`, `pass`, `return None`)
- Leave commented-out code in commits
- Add debug print/cat statements
- Use hardcoded values (paths, magic numbers without explanation)
- Add superfluous arguments or features beyond what was requested

**ALWAYS:**
- Write production-ready code from the start
- Complete implementations fully before committing
- Remove all debugging artifacts
- Use constants/parameters for configurable values
- Be concise - do only what is asked, nothing more

**If you have a good idea for improvement:** ASK FIRST before implementing it.

#### 2. Package Version Management

**CRITICAL: Always bump version when committing.** Edit DESCRIPTION file, commit with version in message. See [Why bump version](#why-bump-version-on-every-commit) for detailed rationale.

**Scheme:** Patch (0.13.25→0.13.26): bugs/docs; Minor (0.13.25→0.14.0): features; Major (0.13.25→1.0.0): breaking changes

#### 3. Testing Requirements

**Before making ANY changes:**
```bash
# Run existing tests to establish baseline
Rscript -e "devtools::test()"
```

**After making changes:**
```bash
# Run tests again
Rscript -e "devtools::test()"

# For new functions, add tests
# tests/testthat/test-my_new_function.R

# Run R CMD check
R CMD check .
```

**For bug fixes:**
- Add regression test that fails with old code, passes with fix
- Test edge cases: NA, NULL, empty vectors, single values, large inputs

**For new features:**
- Unit tests for all new functions
- Integration test showing it works in workflow
- Test with realistic data, not just toy examples

#### 4. Never Break Existing Workflows

**Critical paths that MUST continue working:**
- `run_mosaic_iso()` / `run_MOSAIC()` - Complete calibration pipeline
- `calc_model_likelihood()` - Likelihood calculation
- `sample_parameters()` - Parameter sampling
- NPE training workflow
- Parallel execution on clusters

**Before committing, verify:**
```r
# Test basic workflow still works
library(MOSAIC)
set_root_directory("~/MOSAIC")

# Try a small test run
config <- get_location_config(iso = "ETH")
priors <- get_location_priors(iso = "ETH")

# Sample parameters
test_params <- sample_parameters(seed = 123, priors = priors, config = config)

# If you changed likelihood, test it
# If you changed run_MOSAIC, test a small calibration
# etc.
```

#### 5. Performance Standards

**NEVER regress performance:**
- Consider performance implications before coding
- Preserve critical optimizations (NPE outputs/ subdirectory, vectorization)
- Profile before/after if touching hot paths (use small test cases)

**Hot paths (be extra careful):**
- `run_MOSAIC()` - Main calibration loop
- `calc_model_likelihood()` - Called thousands of times
- `.mosaic_run_simulation_worker()` - Parallel worker
- `.prepare_npe_data()` - NPE data loading

**Scaling awareness:** See [Why design for scale, test small](#why-design-for-scale-test-small) for detailed guidance. Test with 10-100 examples, think about 40K. Pre-allocate, avoid growing objects, vectorize, consider memory (N workers × ~2 GB per worker), ensure thread safety (BLAS threads = 1).

#### 6. Git Workflow

**Standard workflow:** Bump version → test → document → stage files → commit (with version in message) → push

**Commit format:** Brief summary (50 chars) with version, blank line, bullets explaining what/why

**Atomic commits:** One logical change per commit, don't mix changes, don't commit broken code

#### 7. Documentation Standards

**When to update:** Adding functions, changing signatures, modifying behavior

**Keep it concise:** See [Why concise documentation](#why-concise-documentation) for rationale. One-line `@param`, avoid complex roxygen2 syntax (nested lists, complex LaTeX), detailed explanations go in vignettes/CLAUDE.md

**Maintain pkgdown:** Update `_pkgdown.yml` when adding/removing functions, test with `pkgdown::build_site()`

**Checklist:** `devtools::document()` → check for warnings → test `pkgdown::build_site()`

#### 8. Dependencies

**NEVER add new dependencies without explicit user approval.**

**If changing Python integration:**
```bash
# Always verify afterward
Rscript -e "MOSAIC::check_dependencies()"
```

**Maintain backward compatibility:**
- Don't change function signatures without deprecation
- Support old parameter names with warnings
- Document breaking changes clearly

#### 9. R Package Specific

**Code style:**
```r
# Use explicit namespace calls
dplyr::mutate(df, ...)  # Good
mutate(df, ...)          # Bad (unless in depends)

# Use get_paths() for ALL file operations
PATHS <- get_paths()
file.path(PATHS$MODEL_OUTPUT, "results.parquet")  # Good
"~/MOSAIC/output/results.parquet"                 # Bad (hardcoded)

# Handle NAs properly
mean(x, na.rm = TRUE)    # Consider what NAs mean
sum(is.na(x))            # Check for unexpected NAs

# Vectorize when possible
x + y                    # Good
sapply(seq_along(x), function(i) x[i] + y[i])  # Bad
```

**Function design:**
- Validate inputs early with clear error messages
- Return consistent types (don't return NULL sometimes, list other times)
- Use S3 methods when appropriate
- Keep functions focused (single responsibility)

#### 10. Scientific Computing Standards

**Preserve reproducibility:**
- Maintain seeding schemes (seed_sim, seed_iter in run_MOSAIC)
- Document stochastic components
- Test reproducibility with same seed

**Numerical stability:**
- Preserve likelihood guardrails
- Handle edge cases (0, Inf, -Inf)
- Use log-space for very small/large numbers
- Test with extreme values

**Parallel execution:**
- Ensure thread safety (BLAS threads = 1)
- Test parallel and sequential modes
- Avoid race conditions
- Clean up resources (close clusters)

### Workflow Checklist

**Before:** Read git log, check if exists, run tests, understand workflow fit
**During:** Production-ready code (no placeholders), test incrementally, comment complex logic, no hardcoding, stay focused
**Before commit:** Bump version, test, document, R CMD check, verify no debug code, check workflows still work
**Commit:** Stage relevant files, clear message with version, push
**After:** Verify push, check CI/CD, update issues

