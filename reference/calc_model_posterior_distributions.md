# Calculate Model Posterior Distributions from Quantiles

Fits theoretical distributions to posterior quantiles and creates a
posteriors.json file that mirrors the structure of the priors.json file
with updated parameter values.

## Usage

``` r
calc_model_posterior_distributions(
  quantiles_file = "./results/posterior_quantiles.csv",
  priors_file,
  output_dir = "./results",
  verbose = TRUE
)
```

## Arguments

- quantiles_file:

  Path to the posterior_quantiles.csv file (default:
  "./results/posterior_quantiles.csv")

- priors_file:

  Path to the priors.json file to use as template

- output_dir:

  Directory to save posteriors.json (default: "./results")

- verbose:

  Logical; print progress messages (default: TRUE)

## Value

List containing:

- posteriors:

  The complete posteriors object

- n_parameters_updated:

  Number of parameters successfully updated

- n_parameters_failed:

  Number of parameters that failed to fit

- output_file:

  Path to the created posteriors.json file

## Details

The function:

1.  Reads the posterior_quantiles.csv file produced by
    calc_model_posterior_quantiles or est_npe_posterior

2.  Processes all quantile rows in the file (user controls what to
    include)

3.  Loads the priors.json as a template structure

4.  For each posterior parameter in the quantiles table:

    - Extracts the 2.5%, 50%, and 97.5% quantiles

    - Calls the appropriate fit\_\*\_from_ci function based on
      distribution type

    - Updates the corresponding entry in the posteriors structure

5.  Preserves all non-estimated parameters from the priors unchanged

6.  Properly handles location-specific parameters for single or multiple
    countries

7.  Writes posteriors.json to the output directory

## Examples

``` r
if (FALSE) { # \dontrun{
# Standard usage after running calc_model_posterior_quantiles
posterior_dists <- calc_model_posterior_distributions(
  quantiles_file = "./results/posterior_quantiles.csv",
  priors_file = "./config/priors.json",
  output_dir = "./results"
)
} # }
```
