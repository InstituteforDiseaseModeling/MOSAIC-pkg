# Per-country affine bias-calibration of suitability (psi) predictions

Fits a per-country linear recalibration \\obs \approx a_i \cdot pred +
b_i\\ on the **training window only** and applies it to all predictions,
clipped to \\\[0, 1\]\\. Addresses the systematic negative bias (right
shape, wrong scale) of the suitability LSTM without altering its
training.

## Usage

``` r
calibrate_psi_predictions(
  pred_df,
  obs_df,
  fit_date_stop,
  pred_col = "pred_smooth",
  obs_col = "transmission_intensity",
  out_col = "pred_calibrated",
  min_train = 8L
)
```

## Arguments

- pred_df:

  Data frame of predictions with at least `iso_code`, `date`, and
  `pred_col`.

- obs_df:

  Data frame of observed targets with `iso_code`, `date`, and `obs_col`.

- fit_date_stop:

  Date (or coercible); calibration is fit on `date <= fit_date_stop`.

- pred_col:

  Prediction column to calibrate (default `"pred_smooth"`).

- obs_col:

  Observed-target column (default `"transmission_intensity"`).

- out_col:

  Name of the calibrated output column (default `"pred_calibrated"`).

- min_train:

  Minimum training points per country to fit; otherwise identity
  (default 8).

## Value

`pred_df` with an added `out_col` (calibrated, clipped to \\\[0,1\]\\).

## Details

For each `iso_code`, the affine coefficients are estimated by OLS on the
merged (prediction, observed) pairs with `date <= fit_date_stop` — i.e.
the training period only, so the calibration is leakage-clean with
respect to the out-of-sample window. Countries with fewer than
`min_train` usable training points (or zero predictor variance) fall
back to the identity transform with a warning.

## See also

[`est_suitability`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/est_suitability.md)
