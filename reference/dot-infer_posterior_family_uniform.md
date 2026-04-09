# Infer posterior fitting family from a uniform prior's bounds

Uses the parameter domain encoded in min/max to select the natural
distributional family for posterior fitting when the prior is uniform.

## Usage

``` r
.infer_posterior_family_uniform(prior_entry)
```

## Arguments

- prior_entry:

  A prior list with `$parameters$min` and `$parameters$max`.

## Value

Character string: `"beta"`, `"lognormal"`, or `"normal"`.
