# Truncated Akaike weights with optional temperature scaling

Converts \\\Delta\\ values into Akaike weights \$\$w_i(\tau) \propto
\exp\\\left(-\tfrac{1}{2}\Delta_i / \tau\right)\$\$ and normalized
weights \\\tilde w_i = w_i/\sum_j w_j\\, with optional truncation at
`delta_max`. The temperature \\\tau\\ flattens (\\\tau \> 1\\) or
sharpens (\\\tau \< 1\\) the weight distribution.

## Usage

``` r
calc_model_akaike_weights(delta, delta_max = 6, temperature = 1)
```

## Arguments

- delta:

  Numeric vector of AIC differences \\\Delta_i\\. Derived from
  [`calc_model_aic_delta()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_model_aic_delta.md).

- delta_max:

  Numeric Δ cutoff for truncation (default `6`). Weights with \\\Delta_i
  \> \texttt{delta_max}\\ are set to zero; set to `Inf` to disable.

- temperature:

  Positive scalar temperature \\\tau\\; `1` reproduces standard Akaike
  weights (default). \\\tau \> 1\\ flattens weights (raises ESS), \\\tau
  \< 1\\ sharpens (more concentration on the top draw).

## Value

A list with:

- `w` — raw (possibly truncated) Akaike weights (after temperature).

- `w_tilde` — normalized weights \\\tilde w\\.

- `retained` — logical vector; TRUE if \\\Delta_i \le
  \texttt{delta_max}\\.

- `B_idx` — integer indices of retained draws.

- `delta_max`— the cutoff used.

- `temperature` — the temperature used.

## Choosing `temperature`

- **Default:** `temperature = 1`.

- **Mild flattening:** `1.5–2`. Use when one or two draws dominate and
  you want smoother posterior plots and higher ESS.

- **Strong flattening:** `3–5`. Use sparingly; weights become nearly
  uniform and discrimination drops. A practical target is to pick
  `temperature` so that `max(normalized weight)` \\\le\\ `0.5`.

## See also

[`calc_model_aic_delta()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_model_aic_delta.md),
[`calc_model_convergence()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_model_convergence.md)

Other calibration-metrics:
[`calc_convergence_diagnostics()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_convergence_diagnostics.md),
[`calc_model_agreement_index()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_model_agreement_index.md),
[`calc_model_aic_delta()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_model_aic_delta.md),
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
# Standard (tau = 1)
aw1 <- calc_model_akaike_weights(d, delta_max = 20)
# Mild flattening
aw2 <- calc_model_akaike_weights(d, delta_max = 20, temperature = 2)
c(max_w1 = max(aw1$w / sum(aw1$w)), max_w2 = max(aw2$w / sum(aw2$w)))
#>     max_w1     max_w2 
#> 0.18274515 0.06495264 
```
