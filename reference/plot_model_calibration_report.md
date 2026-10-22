# Calibration diagnostics report (PDF)

Pages:

1.  **Enough simulations?** The overall convergence status (green /
    yellow / red) over a table of its four checks (metric and formula,
    what it tells you, value, green / yellow target, status), then,
    below a "not part of the status" divider, the ESS growth, the budget
    and the ensemble size; the fit to data (cases and deaths, every
    scored day and, when they fit, tier 1-2 days); the reasons a check
    is not green, the flags (the fixed-budget flag is not shown: the run
    line says so) and the sampling details. On the right, the posterior
    ESS against the draws (log-log) over the best log-likelihood found
    so far.

2.  **Parameters.** Monte Carlo error of each posterior mean (left) and
    prior-to-posterior KL divergence (right). Above 60 parameters the
    location-specific parameters are grouped by base name.

3.  **Fit.** The fit table (and the worst-fitting locations of a
    multi-location run) over the weekly series: every complete week, the
    ensemble central line and its 50 and 95 percent predictive intervals
    from weekly-summed member trajectories, observed weekly totals on
    the scored weeks; unscored weeks shaded light grey, the burn-in
    darker.

4.  **Predicted vs observed.** Weekly log-log scatter over the
    square-root-scale distributions of weekly counts, cases and deaths.

5.  **Appendix.** What each metric shown means, generated from the
    glossary, and the metrics retired in 1.4.0.

A file written before MOSAIC 1.5.1 lacks the display fields; they are
completed in memory from its numbers and the glossary
([`render_MOSAIC_figures()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/render_MOSAIC_figures.md)
also writes the completed file, with the full-week series rebuilt from
the run's ensemble). A panel whose inputs are missing is replaced by a
note rather than failing the report.

## Usage

``` r
plot_model_calibration_report(
  diagnostics,
  dir_output = NULL,
  file = "calibration_report.pdf",
  verbose = TRUE
)
```

## Arguments

- diagnostics:

  Path to `calibration_diagnostics.json`, or the list it parses to. Must
  carry `schema_version` `"2"`.

- dir_output:

  Directory the PDF is written to (created if needed). Default `NULL`:
  the directory holding `diagnostics` when it is a path.

- file:

  File name of the PDF. Default `"calibration_report.pdf"`.

- verbose:

  Logical; report the written path. Default `TRUE`.

## Value

The path of the PDF, invisibly.

## Details

Renders the MOSAIC calibration-diagnostics report from
`3_results/calibration_diagnostics.json` (summary.json schema 2
companion). The report answers the three calibration questions, each on
its own page, and reads nothing but that file and the metric glossary
(`inst/extdata/metric_glossary.yml`): every number and display string
comes from the file, every word, formula and target template from the
glossary. No samples, predictions or configs are opened, so it can be
re-rendered from a finished run.

The PDF is vector (base [`pdf()`](https://rdrr.io/r/grDevices/pdf.html)
device, 11 x 8.5 in, WinAnsi encoding for the dashes; formulas and Greek
letters in plotmath) and is written to a temporary file then renamed, so
an interrupted render never leaves a truncated report. The notation
writes the likelihood of draw \\i\\ as \\L_i\\ (no script l: the PDF
fonts have no glyph for it). A run made before MOSAIC 1.4.0 has no
`calibration_diagnostics.json`; its figures carry the notice from
`.mosaic_calibration_report_legacy_notice()` in place of a status.

## See also

[`calc_convergence_status`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_convergence_status.md)

## Examples

``` r
if (FALSE) { # \dontrun{
plot_model_calibration_report(
  file.path(dir_output, "3_results", "calibration_diagnostics.json"),
  dir_output = file.path(dir_output, "3_results", "figures"))
} # }
```
