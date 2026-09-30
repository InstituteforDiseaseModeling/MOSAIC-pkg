# Grid Search for Best Subset with Early Stopping

Performs exhaustive grid search to find the smallest subset size that
meets convergence criteria (ESS, A, CVw) using Gibbs weighting. Stops at
first convergence.

## Usage

``` r
grid_search_best_subset(
  results,
  target_ESS,
  target_A,
  target_CVw,
  min_size,
  max_size,
  step_size = 1,
  ess_method = c("kish", "perplexity"),
  weighting = c("saturated", "tempered"),
  verbose = FALSE
)
```

## Arguments

- results:

  Data frame of calibration results with columns: sim, likelihood

- target_ESS:

  Numeric target for Effective Sample Size (ESS)

- target_A:

  Numeric target for Agreement Index (A)

- target_CVw:

  Numeric target for Coefficient of Variation of weights (CVw)

- min_size:

  Integer minimum subset size to search

- max_size:

  Integer maximum subset size to search

- step_size:

  Integer step size for search (default 1)

- ess_method:

  Character ESS calculation method: "kish" or "perplexity"

- weighting:

  Character best-subset weighting scheme, matching
  `control$targets$best_subset_weighting`: "saturated" (default) or
  "tempered"

- verbose:

  Logical print progress messages

## Value

List with elements:

- n: Optimal subset size (smallest n meeting criteria)

- subset: Data frame of selected simulations

- metrics: List with ESS, A, CVw values at optimal n

- converged: Logical indicating if criteria were met

- evaluations: Integer number of n values tested

## Details

The function searches from min_size to max_size by step_size, stopping
at the first size where all three criteria are met simultaneously:

- ESS \>= target_ESS

- A \>= target_A

- CVw \<= target_CVw

For each candidate size n the top-n draws by likelihood are weighted,
and ESS, A and CVw are calculated from those weights:

- `"saturated"` (default): \\\Delta_i = -2(\ell_i - \max \ell)\\,
  saturated at 4, and \\w_i \propto \exp(-0.5 \min(\Delta_i, 4))\\
  (MOSAIC-docs calibration chapter, equation aic-weights). Weights lie
  in \\\[e^{-2}, 1\]\\ before normalisation. Once most of the subset is
  past \\\Delta = 4\\ the weights are nearly flat, so in practice the
  ESS target alone sets n and the A and CVw targets rarely bind.

- `"tempered"`: the adaptive-eta Gibbs weights (\\\eta\\ chosen so the
  worst draw of the subset sits at a weight floor of 1e-15). Because
  \\\eta\\ is rescaled to each subset's own \\\Delta\\ range, the ESS
  grows roughly in proportion to n (about 0.06 n when \\\Delta\\ rises
  linearly with rank), so the targets are met, if at all, at a much
  larger n than under `"saturated"`.

Weights come from the same helper as `results$weight_best` and the final
ESS_B/A/CVw gate in
[`run_MOSAIC()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/run_MOSAIC.md),
which passes `control$targets$best_subset_weighting` here: the size the
search certifies against a tier is then the size at which the
posterior's own weights meet that tier. Searching under one scheme and
weighting the posterior under the other certified a subset the final
gate then failed.

If no size meets criteria, returns results at max_size with
converged=FALSE.

## Examples

``` r
if (FALSE) { # \dontrun{
result <- grid_search_best_subset(
  results = calibration_results,
  target_ESS = 500,
  target_A = 0.95,
  target_CVw = 0.7,
  min_size = 30,
  max_size = 1000
)
} # }
```
