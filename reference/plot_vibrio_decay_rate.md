# Plot Vibrio Decay Rate as a Function of Environmental Suitability

This function generates a plot that visualizes the suitability-dependent
decay rate of *V. cholerae* survival based on climate-driven
environmental suitability (\\\psi\_{jt}\\). The figure compares five
transformation types for survival time using the cumulative Beta
distribution:

- **Linear**: \\f(\psi\_{jt}) = \text{pbeta}(\psi\_{jt} \mid s_1 = 1,
  s_2 = 1)\\

- **Concave**: \\f(\psi\_{jt}) = \text{pbeta}(\psi\_{jt} \mid s_1 = 1,
  s_2 = 5)\\

- **Convex**: \\f(\psi\_{jt}) = \text{pbeta}(\psi\_{jt} \mid s_1 = 5,
  s_2 = 1)\\

- **Sigmoidal**: \\f(\psi\_{jt}) = \text{pbeta}(\psi\_{jt} \mid s_1 = 5,
  s_2 = 5)\\

- **Arcsine**: \\f(\psi\_{jt}) = \text{pbeta}(\psi\_{jt} \mid s_1 = 0.5,
  s_2 = 0.5)\\

## Usage

``` r
plot_vibrio_decay_rate(PATHS, decay_days_short = 3, decay_days_long = 90)
```

## Arguments

- PATHS:

  A list containing path locations for saving output. Must include:

  - `DOCS_FIGURES`: File path to the directory for saving the plot

- decay_days_short:

  Numeric. Minimum survival time (in days). Default is 3.

- decay_days_long:

  Numeric. Maximum survival time (in days). Default is 90.

## Value

Saves a PNG file to `PATHS$DOCS_FIGURES`. Invisibly returns the ggplot
object.

## Details

The primary y-axis shows survival time in days. The secondary y-axis
shows the decay rate \\\delta\_{jt} = 1 / \text{days}(\psi\_{jt})\\.
Horizontal dashed lines indicate the minimum and maximum bounds on
survival time. The output plot is saved as a PNG file in the directory
specified by `PATHS$DOCS_FIGURES`.

## Examples

``` r
if (FALSE) { # \dontrun{
PATHS <- get_paths()
plot_vibrio_decay_rate(PATHS)
plot_vibrio_decay_rate(PATHS, decay_days_short = 2, decay_days_long = 100)
} # }
```
