# Calibration diagnostics report (PDF)

Pages:

1.  **Simulations and ensemble ESS.** The simulation-convergence status
    (green / yellow / red, from `status`) with its four checks, reasons
    and flags; tables for all draws (the posterior) and for the best-fit
    ensemble; the posterior ESS against the draws (log-log) stacked on
    the best log-likelihood found so far, on a shared x axis.

2.  **Parameters.** Monte Carlo error of each posterior mean (left) and
    prior-to-posterior KL divergence (right). Above 60 parameters the
    location-specific parameters are grouped by base name (point = worst
    or median location, bar = range over locations).

3.  **Fit.** The fit table (and the worst-fitting locations of a
    multi-location run) over weekly time series of observed and
    predicted counts on the scored weeks.

4.  **Predicted vs observed.** Weekly log-log scatter over the
    square-root-scale distributions of weekly counts, cases and deaths.

5.  **Appendix.** What each metric means, generated from the glossary.

Every threshold drawn comes from `status$checks[]$threshold_green` /
`threshold_yellow`; the status, labels, reasons and flags are shown
exactly as written by
[`calc_convergence_status`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_convergence_status.md).
A panel whose inputs are missing (deaths not scored, a path with no
points, an empty fit block) is replaced by a note rather than failing
the report.

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
(`inst/extdata/metric_glossary.yml`): no samples, predictions or configs
are opened, so it can be re-rendered from a finished run.

The PDF is vector (base [`pdf()`](https://rdrr.io/r/grDevices/pdf.html)
device, 11 x 8.5 in) and is written to a temporary file then renamed, so
an interrupted render never leaves a truncated report. A run made before
MOSAIC 1.4.0 has no `calibration_diagnostics.json`; its figures carry
the notice from `.mosaic_calibration_report_legacy_notice()` in place of
a status.

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
