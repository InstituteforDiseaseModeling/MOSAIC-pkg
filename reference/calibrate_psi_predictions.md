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
  eps = 1e-06
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

## Value

`pred_df` with an added `out_col` in \\(0,1)\\.

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
  deviation feeding LASER.

A country with fewer than `min_train` outbreak weeks (this includes
zero-history countries, which have none) or no logit-pred variance falls
back to the **identity** transform (output = input prediction) with a
warning. This is the defined behavior for low-incidence / zero-history
countries: their psi is the uncorrected (region-FiLM-modulated) model
output.

## See also

[`est_suitability`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/est_suitability.md)
