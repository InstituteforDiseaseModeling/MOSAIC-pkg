# Plot Model Likelihood Curve

Creates a sorted likelihood curve plot showing the performance of all
calibration simulations. The plot displays simulations sorted by their
log-likelihood values, with the best simulation highlighted.

## Usage

``` r
plot_model_likelihood(results, output_dir, verbose = TRUE)
```

## Arguments

- results:

  A data frame containing calibration results with at minimum a
  'likelihood' column, and optionally 'sim' and 'iter' columns for
  identifying individual simulations. This is typically the output from
  a model calibration run.

- output_dir:

  Character string specifying the directory where the plot should be
  saved. Directory will be created if it doesn't exist.

- verbose:

  Logical indicating whether to print messages. Default is TRUE.

## Value

Invisibly returns the ggplot object for the likelihood curve.

## Details

This function creates a publication-quality plot showing:

- All simulations sorted by log-likelihood (x-axis: simulation index,
  y-axis: log-likelihood)

- A smooth line connecting all points to show the curve

- Individual points for each simulation

- The best simulation highlighted with a red point

- A horizontal dotted line at the maximum likelihood value

The plot helps visualize:

- The overall calibration performance

- The distribution of likelihood values

- How many simulations achieved good fits

- Whether there's a clear optimum or multiple good solutions

## Examples

``` r
if (FALSE) { # \dontrun{
# Run calibration and collect results
results <- run_calibration()

# Create likelihood curve plot
p_likelihood <- plot_model_likelihood(
    results = results,
    output_dir = "calibration_output"
)

# The plot is automatically saved, but you can also access it
print(p_likelihood)
} # }
```
