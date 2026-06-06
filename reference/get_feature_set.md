# Resolve a named feature set to a covariate vector

Resolve a named feature set to a covariate vector

## Usage

``` r
get_feature_set(name = c("v7.3", "default"))
```

## Arguments

- name:

  Feature-set name: `"v7.3"` (38 screening-informed features) or
  `"default"` (full production candidate set; returns `NULL` so the
  caller uses all available covariates).

## Value

Character vector of covariate names for `"v7.3"`; `NULL` for
`"default"`. Errors on an unknown name.

## See also

[`est_suitability`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/est_suitability.md)
