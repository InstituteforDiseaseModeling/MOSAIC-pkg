# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working
with the MOSAIC R package.

## CRITICAL FILE CREATION RESTRICTIONS

**IMPORTANT**: MOSAIC-pkg is a standard R package that is sensitive to
unwanted file additions. To maintain package integrity:

### File Creation Rules

- **NEVER** create new files directly in the MOSAIC-pkg directory unless
  explicitly necessary for R package functionality
- **ALWAYS** use the `./claude/` directory for any temporary files, test
  outputs, or exploratory work
- **EXCEPTIONS**: Only create files in the following locations when
  necessary:
  - `R/` - New R function files following naming conventions
  - `tests/testthat/` - New test files
  - `model/input/` or `model/output/` - Model-specific data files
  - Files explicitly requested by the user

### Working Directory Structure

    MOSAIC-pkg/
    ├── claude/           # USE THIS for all temporary/exploratory files
    ├── R/                # R functions only (follow naming conventions)
    ├── tests/testthat/   # Test files only
    ├── model/            # Model inputs/outputs only
    └── ...               # Other standard R package directories

### Claude Directory File Naming Conventions

- **Development plans**: Use `plan_*.md` format (e.g.,
  `plan_posterior_parameter_distributions.md`)
- **Analysis reports**: Descriptive names with context (e.g.,
  `model_convergence_diagnostics_feedback.md`)
- **Bug summaries**: Include issue type (e.g., `na_bugs_summary.md`)
- **Feature documentation**: Clear descriptive names (e.g.,
  `vaccination_maps_bug_fixes.md`)

**Before creating any file**, ask yourself: 1. Is this file essential
for the R package functionality? 2. Has the user explicitly requested
this file? 3. Can this be placed in `./claude/` instead?

If the answer to 1 or 2 is NO, place the file in `./claude/`.

## R Package Structure

This is a standard R package for the MOSAIC cholera transmission
modeling framework. The package contains 100+ functions organized by
purpose and integrates with external Python dependencies.

## Key Development Commands

``` bash
# Package development
R CMD check .
R CMD build .
R CMD INSTALL MOSAIC_*.tar.gz

# Testing
Rscript -e "library(testthat); test_check('MOSAIC')"
Rscript -e "testthat::test_file('tests/testthat/test-calc_log_likelihood_negbin.R')"
Rscript -e "devtools::test()"
Rscript -e "devtools::check()"
Rscript -e "covr::package_coverage()"

# Model-specific testing
Rscript -e "testthat::test_file('tests/testthat/test-calc_model_likelihood.R')"

# Documentation
Rscript -e "devtools::document()"
Rscript -e "pkgdown::build_site()"

# Python dependencies
Rscript -e "MOSAIC::check_dependencies()"
Rscript -e "MOSAIC::install_dependencies()"
```

## Function Development Guidelines

### Function Naming Conventions

- **`process_*()`**: Data cleaning and preprocessing functions
- **`est_*()`**: Parameter estimation and modeling functions  
- **`plot_*()`**: Visualization and figure generation functions
- **`get_*()`**: Data retrieval and utility functions
- **`calc_*()`**: Mathematical calculations and computations
- **`check_*()`**: Validation and verification functions

### Adding New Functions

- Follow existing patterns in `R/` directory organization
- All functions must be documented with roxygen2 comments
- Add corresponding tests in `tests/testthat/test-[function-name].R`
- Update `NAMESPACE` will be auto-generated via roxygen2
- Consider if function outputs should go to `../MOSAIC-docs/figures/`
  for documentation

### Development Workflow for New Functions

1.  Create function file in `R/` following naming convention
2.  Add roxygen2 documentation with examples
3.  Run `devtools::document()` to update documentation
4.  Add unit tests in `tests/testthat/`
5.  Run `devtools::test()` to verify tests pass
6.  Update `model/LAUNCH.R` if function is part of main workflow

## Function Organization

### Key Directories

- **`R/`**: All function definitions (100+ files)
- **`data/`**: Package datasets (ISO codes, default configurations)
- **`tests/testthat/`**: Unit tests
- **`man/`**: Auto-generated documentation
- **`model/`**: Contains LAUNCH.R (main workflow) and input/output
  directories
- **`inst/extdata/`**: Default parameter files (JSON format)
- **`local/`**: Development and testing workspace
- **`azure/`**: Parallel execution scripts for scaling
- **`vignettes/articles/`**: Extended documentation and examples

## Critical Dependencies

### Python Integration

- Uses `reticulate` to interface with `laser-cholera` Python package
- Import pattern: `lc <- reticulate::import("laser_cholera")`
- Model execution: `lc$metapop$model$run_model(paramfile, seed)`

### System Dependencies

- **Geospatial libraries**: GDAL, PROJ, GEOS for spatial operations
- **R packages**: Extensive dependencies listed in DESCRIPTION
- **reticulate configuration**: For Python environment management
- Use
  [`check_dependencies()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/check_dependencies.md)
  and
  [`install_dependencies()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/install_dependencies.md)
  for setup

### Optional Dependencies

- **mobility package**: Optional GitHub package for regenerating
  mobility estimates
  - Only needed if regenerating mobility data (most users won’t need
    this)
  - Requires JAGS (\>= 4.3.0) system installation
  - Pre-computed mobility files included in `model/input/`
  - Install with:
    `remotes::install_github("COVID-19-Mobility-Data-Network/mobility")`
  - Used by:
    [`est_mobility()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/est_mobility.md)
    function

## LASER Model Configuration

### Configuration Files

- Default configurations stored in `data/` as `.rda` files
- JSON/YAML format for cross-language compatibility
- Key configs: `config_default`, `config_simulation_endemic`,
  `config_simulation_epidemic`
- Parameter files in `inst/extdata/` provide JSON templates
- Relationship between `inst/extdata/` parameter files and `data/` R
  objects

### Model Execution Flow

1.  Create/modify configuration using
    [`make_LASER_config()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/make_LASER_config.md)
2.  Run model via `run_LASER_model()` or direct Python interface
3.  Process results using
    [`calc_model_likelihood()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_model_likelihood.md)
    and related functions

## Data Processing Pipeline

### Input Data Sources

- Raw data from `../MOSAIC-data/raw/` (read-only)
- Processed data from `../MOSAIC-data/processed/`
- External APIs: OpenMeteo, WHO dashboards

### Processing Functions

- Climate data:
  [`download_climate_data()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/download_climate_data.md),
  [`process_climate_data()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/process_climate_data.md)
- Cholera surveillance:
  [`process_WHO_weekly_data()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/process_WHO_weekly_data.md),
  [`process_cholera_surveillance_data()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/process_cholera_surveillance_data.md)
- Demographics:
  [`process_UN_demographics_data()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/process_UN_demographics_data.md),
  [`est_demographic_rates()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/est_demographic_rates.md)
- Vaccination:
  [`process_WHO_vaccination_data()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/process_WHO_vaccination_data.md),
  [`est_vaccination_rate()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/est_vaccination_rate.md)

## Testing Guidelines

### Test Structure

- Tests organized by function type in `tests/testthat/`
- Focus on likelihood calculations, spatial operations, and parameter
  estimation
- Test files follow pattern: `test-[function-name].R`

### Running Tests

``` bash
# All tests
Rscript -e "devtools::test()"

# Specific test file
Rscript -e "testthat::test_file('tests/testthat/test-calc_spatial_correlation.R')"

# Test coverage
Rscript -e "covr::package_coverage()"
```

## Common Development Patterns

### Path Management

- Use
  [`set_root_directory()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/set_root_directory.md)
  to establish project root
- Access paths via `PATHS <- get_paths()`
- All file operations use absolute paths from PATHS

### Error Handling

- Extensive parameter validation in estimation functions
- Graceful handling of missing data in processing functions
- Clear error messages for configuration issues

### Documentation

- All functions documented with roxygen2
- Examples provided for key functions
- Package documentation built with pkgdown

### NPE Output Handling (Performance Critical)

**Background**: During BFRS sampling, ~40K simulations generate
individual `out_NNNNNNN.parquet` files containing time series data. Only
1-5% of these simulations receive non-zero NPE weights.

**Directory Structure**:

    1_bfrs/
    ├── simulations.parquet          # Metadata (18 MB, all simulations)
    ├── convergence_results.parquet  # Convergence diagnostics
    ├── priors.json                  # Parameter priors
    └── outputs/                     # Subdirectory for individual output files
        ├── out_0000001.parquet     # Simulation 1 time series
        ├── out_0000002.parquet     # Simulation 2 time series
        └── ... (39,465 files)

**Optimized Workflow**: 1. **During simulation**: Individual output
files created in `outputs/` subdirectory 2. **After simulation**: ISO
codes mapped in-place to files in `outputs/` 3. **During NPE training**:
`.prepare_npe_data()` loads only weighted simulation files from
`outputs/` 4. **After NPE completes**: Entire `outputs/` directory
removed to free disk space

**Key Points**: - ❌ **DO NOT** combine individual files into
`outputs.parquet` (this was removed - it created a 194 MB file just to
filter to 2-5 MB) - ✅ **DO** organize output files in `outputs/`
subdirectory (avoids 39K+ files cluttering the main directory) - ✅
**DO** keep individual files until NPE training completes - ✅ Entire
`outputs/` directory is automatically removed after NPE completes (frees
50-200 MB disk space) - 🔧 For debugging: Comment out cleanup section in
calibration script if you need to preserve files

**Performance benefit**: - Old approach: 30-120 seconds to load and
filter 45M rows from combined file - New approach: 0.1-0.5 seconds to
load only weighted simulation files - **Speedup: 100-1200× faster (2-3
orders of magnitude)**

**Code locations**: - Loading logic: `R/npe.R` in `.prepare_npe_data()`
function (lines 893-896 for outputs/ path) - Calibration workflow:
`local/calibration/calibration_test_17.R` - Directory structure: lines
111-117 - Output file creation: line 457 (writes to outputs/
subdirectory) - ISO mapping: lines 1077-1109 (reads from outputs/
subdirectory) - Cleanup: lines 2403-2423 (removes entire outputs/
directory)

## Integration Points

### External Repositories

- **laser-cholera**: Core simulation engine (Python)
- **MOSAIC-data**: Data input/output
- **MOSAIC-docs**: Figure output destination

### Key Integration Functions

- `run_LASER_model()`: Main interface to Python simulation
- [`calc_model_likelihood()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_model_likelihood.md):
  Process simulation results
- Plotting functions: Output to `../MOSAIC-docs/figures/`
- **Vignette workflow**: `vignettes/articles/Running-MOSAIC.Rmd` shows
  direct laser-cholera integration
- **Azure scaling**: `azure/azure_run_laser.R` for parallel execution
  with command-line args

## Common Pitfalls and Troubleshooting

- **Path setup is critical**:
  [`set_root_directory()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/set_root_directory.md)
  must be called before
  [`get_paths()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/get_paths.md)
- **Python environment issues**: Use `remove_MOSAIC_python_env()` for
  troubleshooting
- **File naming typos**: Note `make_defaut_LASER_config.R` (should be
  “default”) and `processs_suitability_data.R` (extra “s”)
- **Model workflow**: `model/LAUNCH.R` contains the complete data
  processing and modeling pipeline
