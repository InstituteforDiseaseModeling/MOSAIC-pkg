# Amplitude guard for post-correction suitability (psi)

Lightweight, non-fatal sanity check on the per-country amplitude of the
produced suitability series `psi`. Implements the long-referenced (but
previously unwritten) "amplitude guard": it flags countries whose
*post-correction* psi amplitude has **collapsed** toward a constant or
has been **inflated** implausibly relative to the input model
prediction. Such distortions are introduced by a degenerate per-country
bias-correction fit (see
[`calibrate_psi_predictions`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calibrate_psi_predictions.md))
or by weighting that down-weights the outbreak (non-zero) class.

## Usage

``` r
check_psi_amplitude(
  psi_df,
  psi_col = "psi",
  pred_col = "pred_smooth",
  amp_range = c(0.5, 2),
  collapse_abs = 0.02,
  eps = 1e-06,
  warn = TRUE
)
```

## Arguments

- psi_df:

  Data frame with at least `iso_code` and `psi_col`.

- psi_col:

  Name of the produced suitability column (default `"psi"`).

- pred_col:

  Optional name of the pre-correction prediction column used as the
  reference amplitude (default `"pred_smooth"`); ignored if absent.

- amp_range:

  Length-2 numeric `c(lo, hi)`; a country is flagged `collapsed` when
  its logit-sd ratio to the reference is below `lo`, and `inflated` when
  above `hi` (default `c(0.5, 2)`).

- collapse_abs:

  Absolute logit-sd floor; a country below it is flagged `collapsed`
  regardless of the reference (default 0.02).

- eps:

  Clamp applied before `qlogis` (default 1e-6).

- warn:

  Logical; emit a [`warning()`](https://rdrr.io/r/base/warning.html)
  when any country is flagged (default `TRUE`).

## Value

Invisibly, a per-country data frame with columns `iso_code`, `n`,
`sd_psi_logit`, `sd_ref_logit`, `amp_ratio`, `collapsed`, `inflated`,
and `flag` (`"ok"`, `"collapsed"`, or `"inflated"`).

## Details

The check is intentionally cheap and never errors: it emits a single
[`warning()`](https://rdrr.io/r/base/warning.html) listing the flagged
countries (if any) and returns a per-country diagnostic data frame so
callers can log / inspect it.

Amplitude is measured as the standard deviation of the series on the
**logit scale** (the scale on which the bias correction operates), using
an `eps` clamp to keep `qlogis` finite. For each country two flags are
computed:

- **collapsed** — the corrected logit-sd is below `collapse_abs`
  (near-constant in absolute terms) *or* below `amp_range[1]` times the
  input prediction's logit-sd (lost most of its shape relative to the
  model).

- **inflated** — the corrected logit-sd exceeds `amp_range[2]` times the
  input prediction's logit-sd (amplitude blown up relative to the
  model).

When `pred_col` is supplied and present, the relative checks use the
input prediction as the reference amplitude; otherwise only the absolute
collapse check applies.

## See also

[`calibrate_psi_predictions`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calibrate_psi_predictions.md),
[`est_suitability`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/est_suitability.md)
