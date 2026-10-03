# Per-country logit-scale bias-correction of suitability (psi) predictions

Fits a per-country affine recalibration on the **logit scale**,
\\logit(obs) \approx a \cdot logit(pred) + b\\, using **outbreak
(non-zero observed) weeks within the training window only**, and applies
it to all predictions via the inverse logit. Addresses the systematic
negative/midrange bias (right shape, wrong scale) of the suitability
LSTM without altering its training, and feeds the canonical `psi`
column.

## Usage

``` r
calibrate_psi_predictions(
  pred_df,
  obs_df,
  fit_date_stop,
  pred_col = "pred_smooth",
  obs_col = "transmission_intensity",
  out_col = "pred_bias_corrected",
  min_train = 8L,
  eps = 1e-06,
  slope_range = c(0.25, 4),
  offset_range = c(-4, 4),
  amp_range = c(0.5, 2),
  min_pred_sd = 0.05
)
```

## Arguments

- pred_df:

  Data frame of predictions with at least `iso_code`, `date`, and
  `pred_col`.

- obs_df:

  Data frame of observed targets with `iso_code`, `date`, and `obs_col`
  (in \\\[0,1\]\\).

- fit_date_stop:

  Date (or coercible); the fit uses `date <= fit_date_stop`.

- pred_col:

  Prediction column to correct (default `"pred_smooth"`).

- obs_col:

  Observed-target column (default `"transmission_intensity"`).

- out_col:

  Name of the corrected output column (default `"pred_bias_corrected"`;
  renamed from the v0.33 `"pred_calibrated"`).

- min_train:

  Minimum outbreak (non-zero observed) weeks per country to fit;
  otherwise identity (default 8).

- eps:

  Clamp applied to predictions and observations before `qlogis` to keep
  the logit transform finite (default 1e-6).

- slope_range:

  Length-2 numeric `c(lo, hi)`; the per-country logit slope is clamped
  to this range before being applied (default `c(0.25, 4)`).

- offset_range:

  Length-2 numeric `c(lo, hi)`; the per-country logit offset is clamped
  to this range (default `c(-4, 4)`).

- amp_range:

  Length-2 numeric `c(lo, hi)`, as multiples of the input prediction's
  logit-scale sd (default `c(0.5, 2)`); a fit below `lo` falls back to
  identity, one above `hi` is shrunk toward identity.

- min_pred_sd:

  Minimum logit-pred standard deviation (over outbreak weeks) for a
  country to be eligible for a fit; below it, identity (default 0.05).

## Value

`pred_df` with an added `out_col` in \\(0,1)\\. A per-country diagnostic
data frame is attached as `attr(., "calibration_diagnostics")` (see
[`check_psi_amplitude`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/check_psi_amplitude.md)).
Its `status` is `"fit"`, `"guarded"` (slope or offset clamped, or
amplitude shrunk from the ceiling), `"identity"` (too few outbreak
weeks, degenerate predictor or rank-deficient fit), or `"collapsed"`
(identity because the fit fell below the amplitude floor; `slope` and
`offset` then report the rejected, clamped fit).

## Details

The fit is on `logit(obs) ~ logit(pred)` (v0.34; the v0.33 method was a
raw-scale `lm(obs ~ pred)` with a hard \\\[0,1\]\\ clip). Two changes
matter:

- **Outbreak-weeks-only fit.** Zero observed weeks carry no *level*
  information for a scale/offset correction and would make
  `logit(obs = 0)` degenerate for the zero-week majority, so the fit
  uses only weeks with `date <= fit_date_stop` and `obs > 0`.

- **Logit-scale, monotone, no hard clip.** The fit is applied to all
  weeks and mapped back with `plogis`, which is monotone and bounded in
  \\(0,1)\\ by construction — so low predictions map near 0 without the
  hard-\\\[0,1\]\\-clip truncation that distorted the `psi_bar`-relative
  deviation feeding the engine.

A country with fewer than `min_train` outbreak weeks (this includes
zero-history countries, which have none), no logit-pred variance, or an
outbreak-week slope that would collapse its amplitude (see *Robustness*)
falls back to the **identity** transform (output = input prediction)
with a warning. This is the defined behavior whenever the correction
cannot be estimated: the country's psi is the uncorrected
(region-FiLM-modulated) model output.

## Robustness (B1)

An *unregularized*, *unbounded* per-country `lm` corrupts degenerate /
low-signal countries: a near-flat logit-pred predictor yields a wild
slope that either collapses the country's psi to a constant or blows its
amplitude up many-fold (the `prediction from rank-deficient fit`
warning). To prevent that, each per-country fit is screened and the
affine is guarded:

- **Degeneracy screen.** Fits that are rank-deficient, have a too-low
  logit-pred standard deviation (`min_pred_sd`), too few distinct
  logit-pred values, or a non-finite / explosive slope fall back to the
  identity transform.

- **Bounded affine.** The estimated slope is clamped to `slope_range`
  and the offset to `offset_range` (logit scale) before being applied,
  so no country's correction can be steeper / more offset than is
  plausible for a scale/level fix.

- **Amplitude floor: a collapsing fit is not applied.** The corrected
  logit series is affine in the input, so its logit-scale standard
  deviation is the clamped slope times the input's. A slope below
  `amp_range[1]` would flatten the country's seasonal contrast below
  that fraction of the model's. Such a slope is not an estimate of a
  scale correction: on the outbreak weeks the fitted slope is the
  correlation times the ratio of standard deviations, so it lands near
  zero (or below) when the model does not predict outbreak magnitude
  there. The country then falls back to the **identity** transform with
  status `"collapsed"` (v0.102.0). Up to v0.101.0 the map was instead
  blended toward identity until it reached the floor. That map was set
  by the guard constants rather than the data, because the slope was
  clamped without re-fitting the intercept. So CIV received slope 0.505
  and offset -2.64 in every 2026 production refit.

- **Amplitude ceiling.** A corrected logit-scale standard deviation
  above `amp_range[2]` times the input's is shrunk toward identity
  (blend on a 0.02 grid) until it is within the range, so a country's
  psi cannot be inflated beyond a sane range.

Well-behaved countries (ample outbreak weeks, real logit-pred variance,
a slope/offset/amplitude inside the guard ranges) are **unaffected** —
the guard is a no-op for them.

## See also

[`est_suitability`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/est_suitability.md),
[`check_psi_amplitude`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/check_psi_amplitude.md)
