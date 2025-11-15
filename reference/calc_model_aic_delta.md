# Compute ΔAIC from log-likelihoods

Computes AIC differences for each draw using \$\$\Delta_i =
\text{AIC}\_i - \text{AIC}\_{\min} = -2\\\log
\mathcal{L}(\theta^{(i)}) - \log \mathcal{L}\_{\max}\\.\$\$

## Usage

``` r
calc_model_aic_delta(loglik)
```

## Arguments

- loglik:

  Numeric vector of log-likelihoods (one per draw). Higher is better.

## Value

Numeric vector of \\\Delta\\ values (same length as `loglik`).

## See also

[`calc_model_akaike_weights()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_model_akaike_weights.md),
[`calc_model_convergence()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_model_convergence.md)

Other calibration-metrics:
[`calc_convergence_diagnostics()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_convergence_diagnostics.md),
[`calc_model_agreement_index()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_model_agreement_index.md),
[`calc_model_akaike_weights()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_model_akaike_weights.md),
[`calc_model_convergence()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_model_convergence.md),
[`calc_model_cvw()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_model_cvw.md),
[`calc_model_ess()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_model_ess.md),
[`calc_model_max_weight()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_model_max_weight.md),
[`plot_model_convergence()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/plot_model_convergence.md)

## Examples

``` r
set.seed(1)
ll <- 1000 + rnorm(10, sd = 2)
calc_model_aic_delta(ll)
#>  [1] 8.886938 5.646550 9.723638 0.000000 5.063092 9.662997 4.431407 3.427824
#>  [9] 4.077998 7.602677
```
