---
name: datestart-tolerance-findings
description: Per-country build date_start tolerance + 40-country quality scan — coverage/CFR rules, source eras, and the cases-vs-deaths start split
metadata:
  type: project
---

date_start tolerance analysis on `cholera_surveillance_weekly_combined.csv` (multi-source WHO+JHU+AI+SUPP, KEN future-zero fix in). Verified 2026-06-20, local laptop.

**Genuine-reporting-week definition (load-bearing):** count a week as real coverage only if `cases` non-NA AND NOT synthetic. Synthetic = AI rows tagged `fourier_*` or `assumed_zero`. Real = WHO/JHU/SUPP (all untagged) + AI `observed` + AI `documented_zero`. This matches the fit target (fourier is NA-blanked). Of 52,967 AI weekly rows only 998 are `observed`; 45,664 are fourier — never count fourier as coverage.

**Source eras:** JHU = back-history, genuine through 2023-11; WHO weekly = 2023+ (max 2026-05); AI fills recent gaps (max 2026-06, e.g. KEN 2024-25); SUPP through 2021. No fabricated-future artifacts remain post-fix (0 genuine rows dated after today).

**Two structural unfittable-deaths failure modes** (deaths = mu_jt × infections is constant-CFR×cases):
1. CFR too high (case under-ascertainment in early outbreaks): COD JHU 2014-2017 CFR 13-39%.
2. CFR = 0 = deaths simply not reported (NOT a real zero): 55 country-years with >=50 cases & 0 deaths. ETH 2015-2018 is the worst (huge cases, zero deaths). Excluded from deaths-reliable by requiring cfr in [0.005, 0.08].

**Rules used:** cases_ok = >=20 genuine weeks AND (>=5 nonzero weeks OR >=50 cases), first-sustained (>=3 of next 4 yrs). deaths_ok = cases_ok AND cfr in [0.005,0.08].

**5 high-priority verdicts:** COD cases ~2015 / deaths 2018+ (pre-2018 CFR implausible). MOZ cases 2017 / deaths 2017-18 (some zero-death yrs 2020-21). KEN cases 2017 / deaths 2017 (2020-21 zero-death gaps, 2024-25 AI-only). ETH cases 2016 / deaths 2023 ONLY (2015-18 JHU zero deaths + 2019-22 total gap). NGA cases 2014 / deaths 2014 (cleanest of the five, plausible 0.5-3.6% throughout).

**Recommendation:** date_start = **2018** for cases (25/40 reliable, 28 are non-near-zero usable), with **deaths scored on a 2023+ window** (COD-type split — deaths_reliable jumps 12->20 at 2023, and the worst CFR-nonstationarity/zero-death years are pre-2023). 12 countries (BWA ERI GAB GMB GNB GNQ MLI MRT SEN SWZ ZAF + BFA) are near-zero/unusable at any date.

Artifacts (local): `MOSAIC-pkg/claude/datestart_scan/` — analyze.R, by_country_year.csv, country_quality_scan.csv.
See [[ai-source-integration-provenance]] and [[who-field-semantics-gotchas]].
