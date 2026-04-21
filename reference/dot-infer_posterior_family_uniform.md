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

Character string: `"beta"` (for proportion-valued priors) or
`"truncnorm"` (for any other finite bounded support).

## Details

v0.28.1: The non-Beta branch returns `"truncnorm"` instead of
`"lognormal"` / `"normal"`. Truncnorm preserves the uniform's
`[min, max]` support through the posterior fit and across all subsequent
calibration stages (family-match guard in
update_priors_from_posteriors.R). Previously, uniform priors on
non-`[0,1]` supports were fit as unbounded Lognormal or Normal, silently
erasing the prior's bounds in stage-2+ posteriors.

Bound plumbing: `calc_model_posterior_distributions.R` falls back to
`min`/`max` when the truncnorm `a`/`b` fields are absent on the prior
entry, so no changes are needed to the uniform priors themselves.
