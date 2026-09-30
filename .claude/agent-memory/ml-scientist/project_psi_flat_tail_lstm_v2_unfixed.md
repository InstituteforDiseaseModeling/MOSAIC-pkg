---
name: psi-flat-tail-lstm-v2-unfixed
description: drop-tail fix IS now wired into lstm_v2, but the guard fails OPEN and the shipped psi artefact still carries a 98-day carry-forward tail; always check pred_raw for a dead-flat run
metadata:
  type: project
---

**Status update (verified 2026-09-16 at MOSAIC v0.85.0 — supersedes the "legacy-path only" claim).**

**The fix IS wired into lstm_v2 now.** `R/run_rolling_cv_suitability.R:357` calls
`.drop_filled_prediction_tail(out_daily, ens$genuine_last_pred)`, and `genuine_last_pred` is built
at `R/ensemble_suitability.R:206-215` from `max(data_bundle$dates_pred)` per iso. That input is
correct: `.psi_build_data()` does `d <- d[complete.cases(d[, feats]), ]` *before* building the
prediction sequences, so `max(dates_pred)` really is the last covariate-supported week.

**But the guard FAILS OPEN, and the shipped artefact proves it did not run.**
1. `.drop_filled_prediction_tail` (`R/est_suitability.R:14-21`) validates `df` but never
   `genuine_last`. `NULL`, zero-row, or ISO-case-mismatched `genuine_last` → `cutoff` all-`NA` →
   `keep <- is.na(cutoff) | ...` keeps **every** row, no warning. Measured on a 3-row fixture:
   NULL → 3/3 kept, empty df → 3/3 kept, lower-case ISO keys → 3/3 kept (1/3 with correct keys).
   The caller then logs "Dropped 0 rows" as a normal outcome.
2. The shipped `model/input/pred_psi_suitability_day.csv` (committed v0.77.0, `43fd94647`) runs to
   2027-02-04 while the weekly file — the daily file inner-joined to the panel's weekly keys, psi
   agreeing to max|diff| **0** — ends 2026-10-29. Over the last **98 days × 40 countries = 3,920
   rows**, `pred_raw` is exactly constant per country (1 unique value) while `pred_smooth`/`psi`
   vary (98 unique values), because LOESS is fitted *after* the na.locf fill and rolls the constant
   into a plausible curve. Feeding that exact CSV + `last_genuine_date=2026-10-29` to HEAD's helper
   drops exactly 3,920 rows — so HEAD's helper is right and the artefact came from a path/version
   where the call did not happen.
3. `make_config_default.R` derives `date_stop` from that file's max date, so `config_default`'s
   window ends 2027-02-04: the last 98 of 3,322 ticks of *every* default simulation run on
   fabricated psi. The script's comment (`:73`) claims the common-coverage rule prevents a flat
   tail, but it only truncates to the CSV's coverage — it cannot detect a fill est_suitability
   failed to drop.

**Diagnostic recipe (reuse this):** never look at `psi` or `pred_smooth` for a fill tail — they
wiggle. Compare `max(date)` in `pred_psi_suitability_day.csv` against `max(date)` in
`pred_psi_suitability_week.csv` (the weekly file is bounded by the observed panel), and check
`rle(round(pred_raw, 10))`'s terminal run length per iso. A terminal `pred_raw` run > ~7 days = fill.
Separately, terminal runs at exactly **0.01** are the `ensemble_logit_eps` clamp, not a fill — see
[[psi-artefact-provenance-v077]].

See also [[forecast-cv-leakage-redteam]] (flagged this as UNGUARDED), and
[[psi-artefact-provenance-v077]] for the wider artefact-consistency problem in the same commit.
