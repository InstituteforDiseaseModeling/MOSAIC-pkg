# Build the priors `mu_jt` block from annual CFR estimates

The prior the integrated deaths likelihood reads (`priors$mu_jt`): per
location and year the centre (`logit_mean`, the value
[`make_mu_jt()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/make_mu_jt.md)
puts in the config) and the SE of the country-trend mean (`logit_se`),
plus the global widths. Used by `data-raw/make_priors_default.R` and by
[`run_rolling_cv()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/run_rolling_cv.md)
for its per-cutoff priors.

## Usage

``` r
.mosaic_mu_jt_prior(
  cfr_estimates,
  location_name,
  sd_year,
  tau,
  sd_product = 0.3,
  year_min = 2010L
)
```

## Arguments

- cfr_estimates:

  Data frame with `iso_code`, `year`, `logit_mean` and `cfr_se`
  (`est_CFR_hierarchical()$predictions`).

- location_name:

  Character vector of ISO codes to include.

- sd_year:

  Positive scalar: the GAM country-year SD (sigma).

- tau:

  Scalar: the GAM between-country SD (reference only).

- sd_product:

  Positive scalar: residual error of the GAM centre against the observed
  reported CFR in a calibration window, on the logit scale.

- year_min:

  Integer: first year kept.

## Value

A list with `description`, `sd_year`, `sd_product`, `tau` and `location`
(one list of `year`, `logit_mean`, `logit_se` per ISO code).
