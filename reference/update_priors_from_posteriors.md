# Update Priors from Posteriors for Staged Estimation

Merges posterior distributions from a completed calibration stage into a
complete priors object, producing a new priors object suitable as input
to the next calibration stage.

## Usage

``` r
update_priors_from_posteriors(priors, posteriors, verbose = TRUE)
```

## Arguments

- priors:

  Complete priors object (list) or path to a priors JSON file.

- posteriors:

  Posteriors object (list) or path to a posteriors JSON file (as
  produced by `calc_model_posterior_distributions`).

- verbose:

  Logical. Print merge diagnostics (default TRUE).

## Value

A complete priors object (list) with identical top-level structure to
the input `priors`. Posterior fits replace their corresponding prior
entries; all other entries are preserved unchanged.

## Details

The posteriors object produced by `calc_model_posterior_distributions`
is *pruned*: it only contains parameters that were sampled and
successfully fitted. Parameters that were frozen (not sampled) are
removed entirely. A valid priors object for `sample_parameters` must be
*complete* — it must have entries for every parameter that could be
sampled. This function merges the two: posterior fits replace their
corresponding priors, everything else is preserved from the original
priors.

**Merge rules (per parameter):**

- Posterior has a fitted distribution (beta, gamma, lognormal, normal,
  truncnorm, uniform, gompertz): **replace** the prior.

- Posterior has `"fixed"` distribution: **replace** the prior. The
  parameter had zero posterior variance and will return a constant when
  sampled.

- Posterior has `"failed"` distribution: **keep the original prior**.
  The fitting failed, so the prior is more informative than a failure
  marker.

- Parameter absent from posteriors: **keep the original prior**. It was
  not sampled in this stage.

The function also strips any residual fitted-diagnostic metadata (e.g.
`fitted_mode`, `fitted_ci`) from posterior parameter entries, keeping
only the canonical distribution fields that `sample_from_prior` reads.

## Validation

Before returning, the function checks that:

- The output has `metadata`, `parameters_global`, and
  `parameters_location` top-level slots.

- Every parameter entry has `distribution` (character) and `parameters`
  (list) fields.

- No `"failed"` distribution markers remain (they are replaced with
  original priors).

- Global parameter count matches the original priors.

- Location-specific parameter groups and their location counts match.

## Examples

``` r
if (FALSE) { # \dontrun{
# After a completed calibration stage
priors <- jsonlite::read_json("1_inputs/priors.json")
posteriors <- jsonlite::read_json("2_calibration/posterior/posteriors.json")

# Merge posteriors into priors for the next stage
updated_priors <- update_priors_from_posteriors(priors, posteriors)

# Use in next calibration stage
run_MOSAIC(priors = updated_priors, ...)
} # }
```
