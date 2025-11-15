# Coefficient of variation of (retained) weights

Computes \\\mathrm{CV}\_{\tilde{\mathbf w}} = \mathrm{sd}(\tilde{\mathbf
w}) / \mathrm{mean}(\tilde{\mathbf w})\\ over the retained set
\\\mathcal B = \\i : w_i \> 0\\\\, after normalizing within \\\mathcal
B\\.

## Usage

``` r
calc_model_cvw(w)
```

## Arguments

- w:

  Numeric vector of weights (raw or normalized). Smaller CV indicates
  less skew.

## Value

Scalar CV of weights over the retained set; `NA` if \\\|\mathcal B\| \<
2\\.

## See also

[`calc_model_convergence()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_model_convergence.md)

Other calibration-metrics:
[`calc_convergence_diagnostics()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_convergence_diagnostics.md),
[`calc_model_agreement_index()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_model_agreement_index.md),
[`calc_model_aic_delta()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_model_aic_delta.md),
[`calc_model_akaike_weights()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_model_akaike_weights.md),
[`calc_model_convergence()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_model_convergence.md),
[`calc_model_ess()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_model_ess.md),
[`calc_model_max_weight()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_model_max_weight.md),
[`plot_model_convergence()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/plot_model_convergence.md)

## Examples

``` r
set.seed(1)
ll <- 1000 + rnorm(200, sd = 3)
d  <- calc_model_aic_delta(ll)
aw <- calc_model_akaike_weights(d)
calc_model_cvw(aw$w)
#> [1] 0.9977111
```
