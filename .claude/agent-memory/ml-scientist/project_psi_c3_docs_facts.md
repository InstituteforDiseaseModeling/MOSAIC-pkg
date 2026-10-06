---
name: psi-c3-docs-facts
description: Verified numbers behind the v1.0 psi docs (MOSAIC-docs 13b4f02) - 48% zero target weeks (package docstrings' 72% is stale), forecast-window ENSO beyond training range, 22/5/13 correction split, CV origin grid, provenance of the old docs figures
metadata:
  type: project
---

These numbers were verified on 2026-10-03 for the v1.0 rewrite of 04-model-description's psi section (MOSAIC-docs 13b4f02, MOSAIC-pkg e6beba91d). Reuse them, but re-check them if psi is refit.

**Stale package docstrings.**
- The `R/loss_suitability.R` and `est_suitability()` roxygen say "~72% of training weeks are zero". In C3's training rows the share is **48%**: 15,324 country-weeks with a target, 2015-01-01 to 2026-09-17, from release panel 5e1e2498.
- The 72% predates leaving NA weeks untrained.
- Fixing the docstrings is a MOSAIC-pkg follow-up.

**The forecast window is ENSO extrapolation.**
- NMME takes Nino3.4 above its 2015-2026 training maximum (3.01) for 2026-09-24 to 2027-03-04, which is 23 of the 31 weekly rows.
- Nino4 is above its maximum (2.01) for 2026-09-24 to 2026-12-24, 14 rows.
- Both maxima fall on the last fit week. The panel drops ISO week 53, so the 32-week window has 31 weekly rows.
- Local climate after 2026-09-23 is the MRI-AGCM3-2-S projection; ERA5 ends that day for all 40 countries.

**Bias-correction split in the shipped day file.**
- The maps are exactly affine and recoverable.
- 22 countries are fitted, with slopes 0.93-1.72; 21 of 22 slopes are above 1, so the correction steepens psi.
- 5 are blended at the 2x ceiling: BFA, CAF, LBR, SWZ, ZAF.
- 13 are identity. CIV, GMB, TGO and UGA are collapsed under v0.102.0. BWA, ERI, GAB, GIN, GNB, GNQ, MRT, SEN and SLE have too few outbreak weeks or a degenerate fit.
- Out of sample, the median all-weeks calibration slope is 0.337 over 26 countries; in sample it is 1.148 over 29 (amplitude_by_country.csv).

**CV grid.** There are 12 origins every 6 months, 2020-11-08 to 2026-05-08 (67 monthly origins thinned by `rw_subsample` 6). Each validates on 5 months after a 4-week gap. Final-fit epochs per seed are 6-16.

**Leakage caveat on the OOS numbers.** The internal CV windows share full-record target anchors, the IS-wide scaler, full-panel climatology anomalies and the full-data flood GAM, so the 0.34 is not a clean forecast test.

**The old docs figures are not lstm_v2.**
- `suitability_cases_MOZ.png` and `suitability_by_country.png` come from the 2026-04-20 legacy-LSTM fit, MOSAIC-pkg 16cf7d82a, with fit_date_stop 2026-03-19.
- The "2026-03-12" line is MOZ's last case date, not the cutoff.
- In the by-country figure, orange starts at the plotting date.
- `cases_binary.png` and `suitability_LSTM_fit.png` are now unreferenced but still in `figures/`.

**Flagged, not fixed:** `03-data.Rmd` still says "24 covariates". The parameter table's `k_psi*` row omits MOZ's prior, Truncnorm(-5, 20, -90, 90).

See [[psi-refit-v0101-c3]], [[psi-collapse-fix-v0102]] and [[mosaic-docs-style-guide]].
