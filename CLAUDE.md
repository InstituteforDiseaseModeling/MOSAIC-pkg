# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with the MOSAIC R package.

## CRITICAL FILE CREATION RESTRICTIONS

**IMPORTANT**: MOSAIC-pkg is a standard R package that is sensitive to unwanted file additions. To maintain package integrity:

### File Creation Rules
- **NEVER** create new files directly in the MOSAIC-pkg directory unless explicitly necessary for R package functionality
- **ALWAYS** use the `./claude/` directory for any temporary files, test outputs, or exploratory work
- **EXCEPTIONS**: Only create files in the following locations when necessary:
  - `R/` - New R function files following naming conventions
  - `tests/testthat/` - New test files
  - `model/input/` or `model/output/` - Model-specific data files
  - Files explicitly requested by the user

### Working Directory Structure
```
MOSAIC-pkg/
├── claude/           # USE THIS for all temporary/exploratory files
├── R/                # R functions only (follow naming conventions)
├── tests/testthat/   # Test files only
├── model/            # Model inputs/outputs only
└── ...               # Other standard R package directories
```

### Claude Directory File Naming Conventions
- **Development plans**: Use `plan_*.md` format (e.g., `plan_posterior_parameter_distributions.md`)
- **Analysis reports**: Descriptive names with context (e.g., `model_convergence_diagnostics_feedback.md`)
- **Bug summaries**: Include issue type (e.g., `na_bugs_summary.md`)
- **Feature documentation**: Clear descriptive names (e.g., `vaccination_maps_bug_fixes.md`)

**Before creating any file**, ask yourself:
1. Is this file essential for the R package functionality?
2. Has the user explicitly requested this file?
3. Can this be placed in `./claude/` instead?

If the answer to 1 or 2 is NO, place the file in `./claude/`.

## R Package Structure

This is a standard R package for the MOSAIC cholera transmission modeling framework. The package contains 100+ functions organized by purpose and integrates with external Python dependencies.

## Key Development Commands

```bash
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
- Consider if function outputs should go to `../MOSAIC-docs/figures/` for documentation

### Development Workflow for New Functions
1. Create function file in `R/` following naming convention
2. Add roxygen2 documentation with examples
3. Run `devtools::document()` to update documentation
4. Add unit tests in `tests/testthat/`
5. Run `devtools::test()` to verify tests pass
6. Update `model/LAUNCH.R` if function is part of main workflow

## Function Organization

### Key Directories
- **`R/`**: All function definitions (100+ files)
- **`data/`**: Package datasets (ISO codes, default configurations)
- **`tests/testthat/`**: Unit tests
- **`man/`**: Auto-generated documentation
- **`model/`**: Contains LAUNCH.R (main workflow) and input/output directories
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
- **JAGS**: Required for Bayesian analysis (rjags package)
- **Geospatial libraries**: GDAL, PROJ, GEOS for spatial operations
- **R packages**: Extensive dependencies listed in DESCRIPTION
- **reticulate configuration**: For Python environment management
- Use `check_dependencies()` and `install_dependencies()` for setup

## LASER Model Configuration

### Configuration Files
- Default configurations stored in `data/` as `.rda` files
- JSON/YAML format for cross-language compatibility
- Key configs: `config_default`, `config_simulation_endemic`, `config_simulation_epidemic`
- Parameter files in `inst/extdata/` provide JSON templates
- Relationship between `inst/extdata/` parameter files and `data/` R objects

### Model Execution Flow
1. Create/modify configuration using `make_LASER_config()`
2. Run model via `run_LASER_model()` or direct Python interface
3. Process results using `calc_model_likelihood()` and related functions

## Data Processing Pipeline

### Input Data Sources
- Raw data from `../MOSAIC-data/raw/` (read-only)
- Processed data from `../MOSAIC-data/processed/`
- External APIs: OpenMeteo, WHO dashboards

### Processing Functions
- Climate data: `download_climate_data()`, `process_climate_data()`
- Cholera surveillance: `process_WHO_weekly_data()`, `process_cholera_surveillance_data()`
- Demographics: `process_UN_demographics_data()`, `est_demographic_rates()`
- Vaccination: `process_WHO_vaccination_data()`, `est_vaccination_rate()`

## Testing Guidelines

### Test Structure
- Tests organized by function type in `tests/testthat/`
- Focus on likelihood calculations, spatial operations, and parameter estimation
- Test files follow pattern: `test-[function-name].R`

### Running Tests
```bash
# All tests
Rscript -e "devtools::test()"

# Specific test file
Rscript -e "testthat::test_file('tests/testthat/test-calc_spatial_correlation.R')"

# Test coverage
Rscript -e "covr::package_coverage()"
```

## Common Development Patterns

### Path Management
- Use `set_root_directory()` to establish project root
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

## Integration Points

### External Repositories
- **laser-cholera**: Core simulation engine (Python)
- **MOSAIC-data**: Data input/output
- **MOSAIC-docs**: Figure output destination

### Key Integration Functions
- `run_LASER_model()`: Main interface to Python simulation
- `calc_model_likelihood()`: Process simulation results
- Plotting functions: Output to `../MOSAIC-docs/figures/`
- **Vignette workflow**: `vignettes/articles/Running-MOSAIC.Rmd` shows direct laser-cholera integration
- **Azure scaling**: `azure/azure_run_laser.R` for parallel execution with command-line args

## Common Pitfalls and Troubleshooting

- **Path setup is critical**: `set_root_directory()` must be called before `get_paths()`
- **Python environment issues**: Use `remove_MOSAIC_python_env()` for troubleshooting
- **JAGS installation**: Required for Bayesian functions, may need manual installation
- **File naming typos**: Note `make_defaut_LASER_config.R` (should be "default") and `processs_suitability_data.R` (extra "s")
- **Model workflow**: `model/LAUNCH.R` contains the complete data processing and modeling pipeline