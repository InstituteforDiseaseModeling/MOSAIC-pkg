# Compute Adaptive Likelihood Weights for Joint Calibration Stage

Computes data-driven `weight_cases` and `weight_deaths` values for a
joint calibration stage (S3) based on how well cases and deaths were fit
in the preceding single-outcome stages (S1 cases, S2 deaths). The
weights are computed once before S3 begins and set as scalars in
[`mosaic_control_defaults`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/mosaic_control_defaults.md).

The underlying principle is **complementary deficiency weighting**:
outcomes that are more poorly fit receive more weight in S3 so that S3
can improve them. Two deficiency signals are combined for each outcome:

1.  **R² deficiency**: `1 - R²`. Measures how poorly the model tracks
    the temporal pattern of each outcome.

2.  **Bias deficiency**: `1 - exp(-|log(bias_ratio)|)`. Measures how far
    the total predicted magnitude is from observed. Symmetric on log
    scale (2x over-prediction = 2x under-prediction). Maps bias_ratio =
    1 → 0, ratio = 2 → 0.5, ratio = 5 → 0.8.

The two signals are combined as a weighted mean (`r2_weight` on R²,
remainder on bias) because R² is more statistically stable at small
simulation counts. The resulting weights always sum to 1 and are clamped
to `[w_min, w_max]`.

## Usage

``` r
mosaic_adaptive_s3_weights(
  r2_cases,
  r2_deaths,
  bias_cases,
  bias_deaths,
  frac_floored_cases = NULL,
  frac_floored_deaths = NULL,
  r2_weight = 0.6,
  w_min = 0.2,
  w_max = 0.8,
  degenerate_floor = 0.9,
  fallback_w_cases = 0.5,
  verbose = TRUE
)
```

## Arguments

- r2_cases:

  Numeric. R² for cases from the S1 (cases) calibration stage. Read from
  `3_results/summary.json$r2_cases`.

- r2_deaths:

  Numeric. R² for deaths from the S2 (deaths) calibration stage. Read
  from `3_results/summary.json$r2_deaths`.

- bias_cases:

  Numeric. Bias ratio for cases from S1:
  `sum(predicted) / sum(observed)`. From
  `summary.json$bias_ratio_cases`.

- bias_deaths:

  Numeric. Bias ratio for deaths from S2. From
  `summary.json$bias_ratio_deaths`.

- frac_floored_cases:

  Optional numeric. Fraction of S1 simulations that hit the likelihood
  floor (guardrail). When \> `degenerate_floor`, the stage is considered
  degenerate and fallback weights are used.

- frac_floored_deaths:

  Optional numeric. Same for S2 deaths stage.

- r2_weight:

  Numeric in (0,1). Weight given to R²-deficiency vs bias-deficiency
  when computing combined deficiency. Default 0.6 (R² more reliable than
  bias at typical sim counts).

- w_min:

  Numeric. Hard lower bound for either weight. Default 0.2 (neither
  outcome can receive less than 20% of total weight).

- w_max:

  Numeric. Hard upper bound for either weight. Default 0.8.

- degenerate_floor:

  Numeric. Fraction-floored threshold above which a stage is considered
  degenerate. Default 0.90.

- fallback_w_cases:

  Numeric. weight_cases to use when a degenerate stage is detected.
  Default 0.5 (equal weights).

- verbose:

  Logical. Print computed weights and diagnostics. Default TRUE.

## Value

Named list with:

- weight_cases:

  Scalar for
  `mosaic_control_defaults(likelihood=list(weight_cases=...))`.

- weight_deaths:

  Scalar for `weight_deaths`.

- degenerate:

  Logical. TRUE if a degenerate stage was detected.

- diagnostics:

  List of intermediate values for inspection.

## See also

[`inflate_priors`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/inflate_priors.md),
[`mosaic_control_defaults`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/mosaic_control_defaults.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# After S1 and S2 complete, load their summaries and compute adaptive weights
summ_s1 <- jsonlite::read_json("stage_1/3_results/summary.json")
summ_s2 <- jsonlite::read_json("stage_2/3_results/summary.json")

weights <- mosaic_adaptive_s3_weights(
  r2_cases    = summ_s1$r2_cases,
  r2_deaths   = summ_s2$r2_deaths,
  bias_cases  = summ_s1$bias_ratio_cases,
  bias_deaths = summ_s2$bias_ratio_deaths
)

ctrl_s3 <- mosaic_control_defaults(
  likelihood = list(
    weight_cases  = weights$weight_cases,
    weight_deaths = weights$weight_deaths
  )
)
} # }
```
