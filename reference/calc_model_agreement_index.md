# Agreement index A based on entropy of weights

For the retained set \\\mathcal B = \\i : w_i \> 0\\\\, computes entropy
\\H(\tilde{\mathbf w}) = -\sum\_{i\in \mathcal B}\tilde w_i \log \tilde
w_i\\ and \\A = H(\tilde{\mathbf w}) / \log\|\mathcal B\|\\, with the
convention \\A=0\\ when \\\|\mathcal B\| \le 1\\.

## Usage

``` r
calc_model_agreement_index(w)
```

## Arguments

- w:

  Numeric vector of weights (raw or normalized). Only strictly positive
  entries form \\\mathcal B\\. Internally normalized within \\\mathcal
  B\\; larger \\A\\ is better.

## Value

A list with `A` (agreement index in \\0,1\\), `H` (entropy), and
`B_size` (retained set size).

## See also

[`calc_model_convergence()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_model_convergence.md)

Other calibration-metrics:
[`calc_convergence_diagnostics()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_convergence_diagnostics.md),
[`calc_model_aic_delta()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_model_aic_delta.md),
[`calc_model_akaike_weights()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_model_akaike_weights.md),
[`calc_model_convergence()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_model_convergence.md),
[`calc_model_cvw()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_model_cvw.md),
[`calc_model_ess()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_model_ess.md),
[`calc_model_max_weight()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_model_max_weight.md),
[`plot_model_convergence()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/plot_model_convergence.md)

## Examples

``` r
set.seed(1)
ll <- 1000 + rnorm(200, sd = 3)
d  <- calc_model_aic_delta(ll)
aw <- calc_model_akaike_weights(d)
calc_model_agreement_index(aw$w)
#> $A
#> [1] 0.8504612
#> 
#> $H
#> [1] 2.458149
#> 
#> $B_size
#> [1] 18
#> 
```
