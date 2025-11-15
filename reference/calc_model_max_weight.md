# Maximum normalized weight over the retained set

Computes \\\max\_{i\in \mathcal B} \tilde w_i\\ where \\\mathcal B = \\i
: w_i \> 0\\\\ and \\\tilde w\\ are weights normalized within \\\mathcal
B\\.

## Usage

``` r
calc_model_max_weight(w)
```

## Arguments

- w:

  Numeric vector of weights (raw or normalized). Smaller is better; 1
  indicates a single retained draw.

## Value

Scalar maximum normalized weight \\\max_i \tilde w_i\\; `NA` if none
retained.

## See also

[`calc_model_convergence()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_model_convergence.md)

Other calibration-metrics:
[`calc_convergence_diagnostics()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_convergence_diagnostics.md),
[`calc_model_agreement_index()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_model_agreement_index.md),
[`calc_model_aic_delta()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_model_aic_delta.md),
[`calc_model_akaike_weights()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_model_akaike_weights.md),
[`calc_model_convergence()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_model_convergence.md),
[`calc_model_cvw()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_model_cvw.md),
[`calc_model_ess()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_model_ess.md),
[`plot_model_convergence()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/plot_model_convergence.md)

## Examples

``` r
set.seed(1)
ll <- 1000 + rnorm(200, sd = 3)
d  <- calc_model_aic_delta(ll)
aw <- calc_model_akaike_weights(d)
calc_model_max_weight(aw$w)
#> [1] 0.2057606
```
