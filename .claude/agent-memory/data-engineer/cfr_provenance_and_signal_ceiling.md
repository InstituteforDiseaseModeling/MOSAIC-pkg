---
name: cfr-provenance-and-signal-ceiling
description: CFR_target provenance chain (WHO-annual-only 1970-2026 GAM), three defects in est_CFR_hierarchical, and the hard observational ceiling on per-country mortality params (only 11/40 ISOs support even one)
metadata:
  type: project
---

Audited 2026-09-23 (MOSAIC 0.91.14, config_default v4.7, priors_default v15.18). Full report:
`/Users/johngiles/MOSAIC/MOSAIC-pkg/claude/cfr_review/06_data_audit.md` (local laptop).

## Provenance chain (CFR_target prior centres)
OWiD/WHO-annual + WHO ArcGIS dashboard -> `process_WHO_annual_data()` ->
`MOSAIC-data/processed/WHO/annual/who_afro_annual.csv` ->
`est_CFR_hierarchical(min_cases=3, k_year=15)` [LAUNCH.R:279, update_mosaic_data.R:400 — NOT the
function defaults of 20/12] -> `model/input/param_mu_disease_mortality.csv` ->
`make_priors_default.R:1815-1884` takes the **mean of GAM predictions over t in 2021:2025**, clamps
to `[0.002, 0.40]`, stores lognormal(meanlog=log(that), sdlog=0.787).

**CFR_target is WHO-ANNUAL-ONLY.** WHO-weekly / JHU / SUPP / AI feed the *fit target*
(`config_default$reported_deaths`) but never the prior. `process_CFR_data()` output
(`case_fatality_ratio_2014_<max>.csv`) is DOCS-ONLY (04-model-description.Rmd:1030), not the prior path.

**GOTCHA — the committed `model/input/cfr_*` artifacts lag `priors_default`.** At the time of the
R6 fix the git-tracked copies were a 2026-06-02 vintage (1058 obs, AIC 27016.77) while
`priors_default` v15.18 had been built from an UNCOMMITTED working-tree regeneration
(2026-09-18 09:54, 1062 obs, AIC 28213.33). Re-running the processor reproduces the WORKING-TREE
file bit-exactly, not the committed one. So never take the committed artifact as the prior's
provenance — diff it against a fresh run first, or you will attribute a data-refresh delta
(e.g. CAF 2.46% -> 8.62%) to a code change.

## Three defects in est_CFR_hierarchical.R (FIXED in R6, branch feature/cfr-restructure)
Fixes: factor keyed on `iso_code`; `exclude=` derived from the fitted smooths (both country-indexed
terms, also applied to `cfr_temporal_trend.csv`); RE coefs selected by
`smooth$first.para:last.para` with a cardinality assert. Net effect on `CFR_target`: only CIV moves
materially (-22.1%, 5.835% -> 4.546%, because the two CIV name-levels were each contributing a
duplicated prediction row that `make_priors_default` averaged); 4 data-poor ISOs move -5 to -7%
(GAB GNQ ERI GMB BWA) from the 41->40-level refit; 31 ISOs move < 1%. Clamp-floor set unchanged.
Defect #2 (out-of-model countries inheriting level-1's trend) was LATENT at `min_cases=3` on the
2026-09-18 data — all 40 ISOs clear the filter — but measures **3.0-3.7x** when it fires.
1. **`cfr_country_effects.csv` is misaligned: 205 rows for 41 countries.** L358
   `grep("country_factor", ...)` catches the 41 `s(country_factor)` RE coefs AND the 164
   `s(year,country_factor)` fs basis coefs; the 41-name vector recycles 5x. Verified
   `sort(shipped) == sort(c(RE, fs))` exactly. TRUE random intercepts span logit [-0.030,+0.011]
   (sd 0.0067); the file claims sd 0.644 with "Congo +4.40". CLAUDE.md Lesson #15 shape.
2. **CIV has two country-factor levels** ("Cote d'Ivoire" + "Côte D'ivoire") because the GAM keys on
   the country STRING not `iso_code` -> 41 levels for 40 ISOs, CIV split in half.
3. **The RE term is dead; the fs term does everything.** EDF: s(year) 13.7/14, s(country_factor)
   **0.26/40**, s(year,country_factor) **134.8/153**. So there is NO hierarchical shrinkage —
   data-poor countries get an extrapolated per-country cubic, which is why LBR's GAM CFR is
   **2.5e-9** and BFA's 2.8e-4. Five ISOs (BFA, GIN, LBR, RWA, SEN) ship at the **0.002 clamp
   floor** — their prior is a numerical guardrail, not data.

Fit is on **1970-2026** (485/1062 rows pre-2000; 68% of cases pre-2014). Pearson dispersion **28.8**
=> all CIs ~5.4x too narrow; the function's own `validation_coverage = 0.102` records this.

## Observational ceiling (2023-01-01 fit window, from config_default matrices)
844,278 cases / 14,551 deaths / crude CFR 1.723%.
- **11/40 ISOs have ZERO reported cases**; **14/40 have ZERO deaths** (BWA ERI GAB GIN GMB GNB GNQ
  LBR MLI MRT RWA SEN SLE SWZ).
- Only **11 ISOs have >=200 deaths** (COD SSD NGA MWI AGO ZWE ETH ZMB MOZ CMR TZA) = the only tier
  that supports a country-specific mortality level.
- CFR_target falls inside the Poisson 95% CI of in-window observed CFR for only **10 of 26**
  countries with deaths. BFA is off 22x (0.20% vs 4.37%).
- Deaths provenance 2023+: WHO 96.2%, fourier 3.5%, AI 0.36%, JHU 0. But fourier is concentrated
  where it hurts: BFA/BEN 100%, LBR 86%, ZAF 55%, CIV 51%, UGA 39%, SSD 15%.
- Single-week domination: NAM/ZAF 100% of deaths in one week, CIV 60%, UGA 53%, CAF 54%.

## No temporal structure to fit
- **mu_j_epidemic_factor**: using the model's OWN threshold definition (week >= country median
  weekly incidence/100k over outbreak weeks — `make_priors_default.R:1998-2060`), the paired
  within-country CFR ratio is **1.103 [0.777, 1.567], p=0.56**; RE-pooled 1.022 [0.728, 1.436];
  8/17 > 1; 2 sig up (COD 2.01, SOM 5.77), 2 sig down (ZWE 0.41, KEN 0.60); tau=0.64. Contrast
  WEAKENS at q=0.75/0.90. Only COD/ETH/NGA can resolve 1.5x and they give 2.01/0.98/0.58.
  Prior Gamma(3,6) mean ratio 1.50 sits at the data's upper 95% bound.
- **mu_j_slope**: engine means fractional IFR change over the WHOLE window
  (`sim_components.R:190-192`, `t_factor = tick/nticks`). Annual WHO trend 2014-2026: 21/40
  testable, 3 significant (2 neg CMR/SSD, 1 pos ZWE). Median slope -0.026 logit/yr = -10% over the
  4.1-yr window = exactly the prior's 95% bound. Between-country sd of implied window change
  **0.364 vs prior sd 0.05 (7.3x too narrow)**. Within-window weekly trends flip sign vs annual
  (COD +0.46/yr weekly vs +0.014/yr annual) = outbreak composition, not lethality.
- **delta_reporting_deaths has NO observational anchor**: deaths and cases share the same WHO AFRO
  bulletin row, so weekly deaths-vs-cases cross-correlation peaks at **lag 0 in 11/15 countries**.

## Denominator artifact direction (for any epidemic-CFR claim)
Two DOWNWARD biases on the observed epi/non-epi CFR ratio: (a) suspected-case definition broadens
during surges (the model's own chi_endemic 0.64 -> chi_epidemic 0.75 switch, ~0.85x), and (b) the
flag is thresholded on the CFR DENOMINATOR, so epidemic weeks are enriched for upward case noise
(Berkson). One UPWARD: real care saturation. So observed 1.10 is a lower bound; generous correction
still lands ~1.3 pooled with 1.0 inside the interval.

## Related
[[who_field_semantics_gotchas]] · [[surveillance_revision_is_source_precedence]] ·
[[wb_processed_filename_drift]] (same class: `plot_CFR_by_country.R:26` still hardcodes the stale
`case_fatality_ratio_2014_2024.csv` that the processor no longer writes)
