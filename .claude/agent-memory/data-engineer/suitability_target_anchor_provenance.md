---
name: suitability-target-anchor-provenance
description: target_D_rate_per_country_floored semantics — full-window cp99r anchor (leaks), train-only anchor exists but ONLY for transmission_intensity, LBR saturation, 63% AI/Fourier cells, and the unpinned canonical-panel scoring path
metadata:
  type: reference
---

The suitability panel's response column `target_D_rate_per_country_floored` is the
scoring AND training target for the psi programme (`est_suitability()` default at
`R/est_suitability.R:259`). Verified facts about its semantics, measured 2026-09-21
against the 2026-09-21 panel rebuild:

**Definition reproduces exactly.** `pmin(1, log1p(rate)/log1p(cp99r))` where
`cp99r = max(quantile(rate[!is_ai], 0.99), 5/pop*1e5)` and
`rate = cases/total_population*1e5`. Re-deriving it from `rate` + `source` +
`total_population` matches the stored column to 4e-15 over all 39,304 observed
cells — so the anchor can be recomputed/audited without re-running the compile.

**The anchor is FULL-WINDOW, and the train-only facility does not cover it.**
`R/build_suitability_sequences.R:249-284` has a block labelled
"TRAIN-ONLY anchor" — but it only applies when `response_var == "intensity"`
(`transmission_intensity`), where `cases_99th` is recomputed over
`d$date <= cutoff_date`. When `response_var` names a pre-computed `target_*`
column the code does `raw <- as.numeric(d[[response_var]])` and sets
`cases_99th <- NA_real_`: the full-window anchor baked into the panel is used
verbatim. So the leak-free path exists 20 lines away and the production response
bypasses it. Measured leakage at cutoff 2026-04-15: cp99r full-window vs
`<=cutoff` moves +17.7% for NGA (target values shift up to 0.088) and <1.7% for
every other pool country.

**is_ai mask is correct; the panel is 63% synthetic.** `is_ai` keys on
`source == "AI"` and excludes those rows from the anchor. Verified ZERO rows are
`fourier_*`-disaggregated while carrying a non-AI `source`, so no synthetic value
leaks into the anchor. But 24,620 of 39,304 observed cells (63%) ARE `source=="AI"`,
and 20,870 of those are `fourier_*` synthetic disaggregations of annual totals.
Anchors are trusted-only; the values they score are mostly synthetic.

**Saturation is country-specific and can be total.** 7.9% of all observed cells
(4.6% of pool cells) sit at exactly 1.0. But LBR is 46% saturated, and LBR's
2005-2009 era is **100% saturated — 260 consecutive weeks at exactly 1.0** —
because its anchor comes from a low JHU 2014-2023 era while its AI 1999-2013 era
is ~2x higher. LBR also carries the largest target MASS of any pool country
(774 vs COD 510), so unweighted training sees it heavily. AGO 6.6%, ZWE 4.4%,
ETH 2.8% saturated. 7 non-pool countries (BFA GNB MLI MRT SEN SWZ ZAF) have
cp99r pinned to the 5-cases/wk floor, i.e. their whole target scale is a constant.

**The scoring path is unpinned.** `claude/psi_evolve/{score_arm_driver,confirm_read,
confirm_read2,promote_gate}.R` all hardcode the dugong path
`/home/jgiles/MOSAIC/MOSAIC-data/processed/cholera/weekly/cholera_country_weekly_suitability_data.csv`
and read `obs` from it. `score_psi_arm()` sha256-pins `weights_frozen.csv` and
column-validates `EVAL_GRID.csv`, but nothing hashes or records the TARGET panel.
`psi_manifest.json` (written by `prefit_rolling_cv_psi`) records
`mosaic_version`/`est_suitability_spec`/pred window but NOT the source panel path
or sha. So a psi cache or a registry score cannot be traced to a panel build.

See [[iso_week_labelling_convention]] for the DA-01 fix that regenerated the
panel's inputs, and [[ai_source_integration_provenance]] for the AI/Fourier
precedence rules.
