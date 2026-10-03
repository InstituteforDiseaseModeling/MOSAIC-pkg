# Provenance: the production psi in config_default (psi C3, MOSAIC v0.101.0; re-corrected in v0.102.0)

`config_default` v6.2 carries the environmental-suitability series `psi_jt` of the
production refit **C3**, with its bias correction re-applied under the v0.102.0 rule (see the
section of that name below): CIV, GMB, TGO and UGA fall back to the identity correction, and the
other 36 countries carry C3's psi unchanged. `data-raw/make_config_default.R` builds `psi_jt`
from `model/input/pred_psi_suitability_day.csv`.

The full provenance bundle is in MOSAIC-data `processed/psi_provenance/v0.101.0_C3/`,
added in `b5feb59` (its `build_nino4_C.R` path case was corrected in `2ee3725`). It holds the
scripts, logs, md5s, the variant ENSO input and the repository revisions. Its `README.md` gives
the step-by-step rebuild and its `PROVENANCE.md` the full record. This note summarises it.

| file in `model/input/` | md5 (v0.102.0) | C3 as run (v0.101.0) |
|---|---|---|
| `pred_psi_suitability_day.csv` (the source of `psi_jt`) | `eac62fb99bf04c1dd8ff547388a0c1b1` | `d0eb1e2da04c24a8b2cbf43d548c0413` |
| `pred_psi_suitability_week.csv` | `617c34f67f2b6e8b1e2ade6be445b276` | `6a0f85cd17b2d4cecf3bf026b1807b82` |
| `psi_suitability_config.json` (the run manifest without its per-fold predictions) | `a8ef948cd3fb69d16d6762ddb6050597` | `e098f115724b4c0cd183cd051e0c6083` |

The other psi files in `model/input/` (`pred_psi_suitability.csv`,
`pred_psi_suitability_day_extended.csv`, `psi_lstm_best.weights.h5`) are older artifacts, not
outputs of C3.

## The v0.102.0 re-correction

`calibrate_psi_predictions()` (v0.102.0, commit `1e4a1f149`) no longer applies a per-country fit
that would shrink psi's logit-scale amplitude below `amp_range[1]` (0.5) of the model's. Such a
country falls back to the identity correction, `psi = pred_bias_corrected = pred_smooth`. Up to
v0.101.0 such a fit was blended toward identity until it sat on the 0.5x floor, a map that came
from the guard constants rather than the data.

- **Which countries.** In C3, CIV, GMB, TGO and UGA sat on that floor.
- **How it was applied.** The rule was re-applied to C3's stored predictions without retraining.
  For those four, the `psi` and `pred_bias_corrected` fields were replaced by the row's own
  `pred_smooth`. Every other row of the day and week files is C3's, byte for byte.
- **Where it is recorded.** The manifest's `bias_correction_reapplied` record holds the rule, the
  four countries, their C3 maps, the source md5s and the code commit.
- **Mean psi over the config window:**
  - CIV 0.011 -> 0.029
  - GMB 0.007 -> 0.010
  - TGO 0.256 -> 0.068
  - UGA 0.091 -> 0.123
- **TGO is borderline.** It sat on the floor in C3 only, of the seven 2026 refits, so its level
  depends on which side of the floor a run lands.

## The fit

`est_suitability()`, run on dugong with MOSAIC 0.100.1 @ `14433d851`. That version's
`est_suitability()` code path was checked byte-identical to MOSAIC-pkg `fix/v0101-trust`
(bundle `PROVENANCE.md` section 4). The bundle's `run_psi.R` makes this call:

```r
est_suitability(PATHS,
                fit_date_start  = NULL,          # 2015-01-01
                fit_date_stop   = NULL,          # 2026-09-17: last week with cases and complete ENSO
                pred_date_start = "2018-01-01",
                pred_date_stop  = NULL,          # 2027-04-29: last ENSO-complete week
                feature_set     = "v7.3",
                response_var    = "target_D_rate_per_country_floored",
                bias_correct    = TRUE,
                architecture    = "lstm_v2_hierarchical_film",
                arch_control    = list(n_seeds = 10L, region_map = "snf_k5",
                                       parallel_seeds = 10L, seed_base = 11L),
                source_csv      = "<panel C, below>")
```

- **Seeds:** 10 seeds, 11, 22, ..., 110, combined by the cross-seed median on the logit scale.
  Confidence weights are on (`use_confidence_weight = TRUE`, the default).
- **Windows:** fit 2015-01-01 to 2026-09-17; predictions 2018-01-01 to 2027-04-29.
- **Rolling CV:** 12 steps (4-week gap, 1-month step, 5-month test; `rw_subsample` 6).
- **Replicate:** C3B used the disjoint seeds 121..220. Over 2023+ it agrees with C3 at a median
  per-country r of 0.972 and a median per-country mean |difference| of 0.024.
- **Reproducibility:** the fit reproduces statistically, not bitwise, because keras recurrent
  dropout is non-deterministic. The panel rebuilds bitwise.

## The panel (panel C)

- **Compiled by** `compile_suitability_data(PATHS, cutoff = NULL, use_epidemic_peaks = TRUE,
  date_start = "2000-01-01", date_stop = NULL, forecast_mode = TRUE, forecast_horizon = 9,
  include_lags = TRUE)`. These are the arguments `update_mosaic_data()` uses, and
  `backfill_case_gaps` stays at its default, `FALSE`.
- **Code and inputs:** MOSAIC-pkg `b65cee162` on MOSAIC-data `04a6d0f`, with
  `PATHS$DATA_ENSO` pointed at the Nino4 variant below.
- **Output:** `cholera_country_weekly_suitability_data.csv`, md5
  `2c575f8c3ea05273d33b0280b5c9c7d5`. It is gitignored and not committed.
- **Surveillance:** the AI-enhanced combined weekly file, as `update_mosaic_data()` builds it
  with `process_cholera_surveillance_data(PATHS, include_ai = TRUE)`.
- **Release revision:** `config_default` v6.1 and v6.2 were built on MOSAIC-data `922ef89` data.
  The panel compiled there differs only in two ZAF 2023 deaths cells, which psi never reads.

## The Nino4 NMME gap-fill variant of the ENSO input

The panel's ENSO input is the canonical `processed/ENSO/enso_weekly.csv` (enso-data `3729c87`)
with 10 weekly ENSO4 values replaced, 2026-W31 to W40.

- **The problem.** NOAA PSL Nino4 ends in August 2026, so the canonical compile anchored
  1 September on BOM's relative Nino4 index, which sits about one degree below NOAA. That anchor
  (0.49, between NOAA's 1.29 for August and NMME's 2.13 for October) put a spurious dip into
  August-September 2026.
- **The variant.** It takes that anchor from the compile's next source, the NMME ensemble mean
  baselined to NOAA (1.8762).
- **Against observations.** The variant is closer to NOAA CPC's weekly observations (weekly
  ENSO4 mean absolute error 0.28-0.34, against 0.41-0.47), but it over-corrects: its September
  is NMME's lead-0 forecast.
- **Effect.** Its effect on psi is essentially confined to the forecast window, after the last
  observed week (2026-09-17).
- **Rebuild.** The bundle's `nmme_gapfill_anchor.py` and `build_nino4_C.R` rebuild it as
  `enso_C/enso_weekly.csv`.

## Rebuilding

Follow the bundle's `README.md`, section Reproduce: the variant ENSO input, the panel, the fit.
A fit run with MOSAIC >= 0.102.0 applies the collapse rule itself. Then copy the day file into
`model/input/` and run `data-raw/make_config_default.R`. Do not inject `psi_jt` into a config by
hand.

`model/LAUNCH_sanitized.R` step 4B still carries the older "G" recipe (fit from 2010,
`rw_subsample = 5`). It does not reproduce C3.

## Training-data fields

- **`confidence_weight`** is a trust weight in (0, 1].
  - Direct WHO/JHU/SUPP weeks carry 1 and AI-mined weeks less: observed and documented-zero
    weeks 0.9, Fourier reconstructions mostly 0.5 (0.33-0.87).
  - A WHO multi-week report spread over its weeks carries 0.7-0.9, set by its window's length,
    and a curated window 0.8-0.9.
  - The LSTM multiplies each sample's loss weight by it (`use_confidence_weight = TRUE`).
  - The calibration reads it as `reported_cases_weight` / `reported_deaths_weight`.
- **`disaggregation_method`** is the trust-tier label.
  - Direct rows (no method), `observed` and `documented_zero` (a confirmed absence) are observed.
  - `who_catchup_*` rows are reconstructed: a WHO multi-week report spread over its weeks.
  - Every other method, `fourier_*` included, is imputed. `fourier_*` rows are synthetic
    seasonal reconstructions of annual or quarterly totals, with no climate input.
  - `process_AI_cholera_data()` drops `assumed_zero` AI rows.
- **The calibration target** has kept `fourier_*` weeks since v0.47.1, down-weighted through
  `confidence_weight`; `config_default` v6.1 and v6.2 have 638 imputed location-days in their window.
  - Since v0.101.0, `config_default$reported_tier` (1 observed, 2 reconstructed, 3 imputed)
    keeps reconstructed and imputed weeks out of the dispersion estimates.
  - The deaths dispersion falls back to every scored week where the observed weeks are too few.
  - Every week is still scored.
