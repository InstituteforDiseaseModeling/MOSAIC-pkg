# Helper Function to Fit Beta Distribution with Variance Inflation for est_initial_R

Fits a Beta distribution to Monte Carlo R/N samples by the method of
moments, keeping the sample mean and multiplying the sample SD by
`variance_inflation` (so the variance scales by its square). Values in
(0, 1) tighten, values \> 1 widen, and 0 or 1 leave the spread
unchanged. The concentration is floored at 2 (SD capped at
\\\sqrt{m(1-m)/3}\\) so a very large factor cannot produce an invalid
Beta, and at \\1/m\\ so shape1 stays at least 1: with prop_R means of
1e-3 to 1e-2, a factor of 13-100 otherwise gave shape1 of ~0.004-0.06, a
prior whose median sat tens of decades below its mean (e.g. ETH median
5.5e-87 against mean 1.8e-3). The mean is kept regardless; the factor is
effectively capped where it would break that floor.

Before v0.100.0 the half-widths of the sample 95% CI were scaled
linearly, the lower bound floored at 1e-10 and the result passed to
[`fit_beta_from_ci()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/fit_beta_from_ci.md).
With the mode-exact logit-scale fitter that unreachable floored bound
dominated the fit (prior mean ~2.5x the sample mean for typical prop_R
inputs), so the SD is now scaled directly, which is what the factor was
documented to do.

## Usage

``` r
fit_beta_with_variance_inflation_R(samples, variance_inflation = 0, label = "")
```

## Arguments

- samples:

  Numeric vector of proportions in (0,1)

- variance_inflation:

  Numeric SD multiplier (0 or 1 = unchanged)

- label:

  Character string for error messages

## Value

List with shape1 and shape2 parameters, or NULL with fewer than two
usable samples or zero sample variance
