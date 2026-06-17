# Plot Best-Subset Optimization Diagnostic from a mosaic_subset_optimization Object

Renders a four-panel diagnostic from a `mosaic_subset_optimization`
object produced by
[`optimize_ensemble_subset`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/optimize_ensemble_subset.md).
Each panel plots a diagnostic against the candidate subset size `N`
(top-N members by likelihood): (1) the objective score, (2) \\R^2\\ for
cases and deaths, (3) the bias ratio for cases and deaths (with a dotted
reference line at bias = 1), and (4) the effective sample size (ESS).
The preliminary (tier/diagnostics) subset size is marked with a dashed
grey vertical line and the optimizer-selected size with a solid black
vertical line plus a point on the score panel.

## Usage

``` r
plot_model_subset_optimization(
  subset_opt,
  output_dir,
  file_prefix = "subset_optimization",
  title_label = NULL,
  verbose = TRUE
)
```

## Arguments

- subset_opt:

  A `mosaic_subset_optimization` object returned by
  [`optimize_ensemble_subset`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/optimize_ensemble_subset.md).
  Its `$evaluation_table` carries the per-N diagnostics; the scalar
  fields `$optimal_n`, `$diagnostics_n`, `$objective`, and
  `$central_method` drive the markers and labels. The object is consumed
  directly; the diagnostics are **not** re-derived from the on-disk CSV.

- output_dir:

  Character. Directory where plots are saved. Created if it does not
  exist.

- file_prefix:

  Character. Prefix used in output filenames:
  `<file_prefix>_diagnostic.pdf` and `<file_prefix>_diagnostic.png`.
  Default `"subset_optimization"`.

- title_label:

  Character or `NULL`. Leading label used in the plot title. When `NULL`
  (default) a title derived from the objective and central method is
  used.

- verbose:

  Logical. Print progress messages. Default `TRUE`.

## Value

Invisibly returns the `ggplot` object.

## Details

The PDF is rendered with
[`grDevices::cairo_pdf`](https://rdrr.io/r/grDevices/cairo.html) when
the cairo device is available (`capabilities("cairo")`), so the proper
glyphs \\R^2\\, \\\rightarrow\\, and \\\Delta\\ are encoded correctly.
When cairo is unavailable the function falls back to the default PDF
device with ASCII-safe labels (`R2`, `->`, `delta`). The PNG is always
rendered with the standard raster device.

Semantic colors for the cases and deaths series come from
[`mosaic_colors`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/mosaic_colors.md)
and the panel theming from
[`theme_mosaic`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/theme_mosaic.md),
both with graceful fallbacks if unavailable.

## See also

[`optimize_ensemble_subset`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/optimize_ensemble_subset.md)
to compute the optimization;
[`plot_model_ensemble`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/plot_model_ensemble.md)
for the ensemble prediction plots.

## Examples

``` r
if (FALSE) { # \dontrun{
subset_opt <- optimize_ensemble_subset(ensemble, likelihoods)
plot_model_subset_optimization(subset_opt, output_dir = "diagnostics")
} # }
```
