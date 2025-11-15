# Calculate Effective Sample Size (ESS)

Computes the effective sample size using either the Kish formula or the
perplexity (entropy-based) method.

## Usage

``` r
calc_model_ess(w, method = c("kish", "perplexity"), na_rm = TRUE)
```

## Arguments

- w:

  Numeric vector of nonnegative weights (normalized or not).

- method:

  Character string specifying the calculation method: `"kish"` (default)
  or `"perplexity"`.

- na_rm:

  Logical; remove non-finite entries in `w`. Default `TRUE`.

## Value

Numeric scalar: the effective sample size.

## Details

Two methods are available:

**Kish method** (`method = "kish"`): Uses the standard Kish ESS formula:
\\\mathrm{ESS} = (\sum w)^2 / \sum w^2\\. This provides a measure of how
much information is retained when using weighted samples compared to an
unweighted sample of the same size.

**Perplexity method** (`method = "perplexity"`): Uses the exponential of
the entropy: \\\mathrm{ESS} = \exp(-\sum w_i \log w_i)\\, where weights
are first normalized to sum to 1. This measures the effective number of
models based on the entropy of the weight distribution. Also known as
the perplexity or exponential entropy.

Both methods yield values between 1 (all weight on one model) and \\n\\
(uniform weights), but they measure slightly different aspects of weight
concentration. The Kish formula is more commonly used in survey
sampling, while perplexity is common in information theory and model
averaging.

## References

Kish, L. (1965). *Survey Sampling*. Wiley.

## See also

Other calibration-metrics:
[`calc_convergence_diagnostics()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_convergence_diagnostics.md),
[`calc_model_agreement_index()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_model_agreement_index.md),
[`calc_model_aic_delta()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_model_aic_delta.md),
[`calc_model_akaike_weights()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_model_akaike_weights.md),
[`calc_model_convergence()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_model_convergence.md),
[`calc_model_cvw()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_model_cvw.md),
[`calc_model_max_weight()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_model_max_weight.md),
[`plot_model_convergence()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/plot_model_convergence.md)

## Examples

``` r
# Uniform weights
w_uniform <- rep(1/10, 10)
calc_model_ess(w_uniform, method = "kish")       # 10
#> [1] 10
calc_model_ess(w_uniform, method = "perplexity") # 10
#> [1] 10

# Concentrated weights
w_conc <- c(0.9, rep(0.01, 10))
calc_model_ess(w_conc, method = "kish")       # ~1.2
#> [1] 1.233046
calc_model_ess(w_conc, method = "perplexity") # ~1.5
#> [1] 1.742536
```
