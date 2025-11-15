# Launch an interactive viewer for site-specific \\\beta\_{\text{hum}}\\

Display a Shiny dashboard that lets you explore the **global**
human-to-human transmission coefficient (\\\beta\_{\text{hum}}\\)
together with location-specific modifiers, incidence, population size
and WASH covariates. The app is intended as a quick visual sanity-check
of the output produced by
[`est_transmission_spatial_structure()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/est_transmission_spatial_structure.md)
and is **not** meant for automated pipelines.

## Usage

``` r
run_spatial_transmission_shiny_app(mod)
```

## Arguments

- mod:

  A list produced by
  [`est_transmission_spatial_structure()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/est_transmission_spatial_structure.md),
  containing at least `relative_multiplier`, `data`, and the columns
  `iso_code`, `incidence_total`, `population_size`, `WASH` inside
  `mod$data`.

## Value

(Invisibly) the `shiny.appobj` returned by
\[[shinyApp](https://rdrr.io/pkg/shiny/man/shinyApp.html)\]—the function
is called for its side effect of launching a Shiny app in the default
browser or RStudio viewer.

## Details

The slider *Global Beta* sets a baseline value
\\\beta\_{\text{hum}}^{\ast}\\, while the slider *Variance Expansion
Factor* applies an exponent \\\kappa\\ to each relative multiplier
\\m_j\\, producing the site-specific coefficient
\$\$\beta\_{\text{hum},j} = \beta\_{\text{hum}}^{\ast} \\
m_j^{\kappa}.\$\$

The main panel is organised as a 4 × 2 layout created with
**patchwork**:

- Top row:

  A histogram of the current \\\beta\_{\text{hum},j}\\ values; the
  dashed red line marks \\\beta\_{\text{hum}}^{\ast}\\.

- Bottom row:

  Four aligned strip plots, all ordered by \\\beta\_{\text{hum},j}\\:

  1.  Relative multipliers \\m_j\\;

  2.  Site-specific \\\beta\_{\text{hum},j}\\;

  3.  Incidence per population (\\I_j / N_j\\);

  4.  WASH covariate value.

## Side Effects

Opens a Shiny window and blocks the R session while the app is running.

## Examples

``` r
if (FALSE) { # \dontrun{
  paths  <- list(MODEL_INPUT = tempdir())
  config <- MOSAIC::config_default[1:3]          # toy example
  fit    <- est_transmission_spatial_structure(paths, config)

  ## Launch the interactive viewer (stop the app to regain the prompt):
  run_spatial_transmission_shiny_app(fit)
} # }
```
