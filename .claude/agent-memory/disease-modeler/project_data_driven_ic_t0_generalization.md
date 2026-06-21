---
name: data-driven-ic-t0-generalization
description: ic_t0 IC-seeding epoch is now data-driven (active-case-richest month in [date_start, +12mo], +/-8wk window over weekly surv) replacing the hard 2023-02-01 floor (commit 4eb43675, v0.46.2); audit of date_start generalization
metadata:
  type: project
---

make_priors_default.R commit 4eb43675 (MOSAIC v0.46.2, 2026-06-20) made the IC seeding
epoch `ic_t0` data-driven so back-history builds (e.g. 2015) seed era-correct ICs instead
of an 8-yr-mismatched 2023 state. `ic_t0` = active-case-richest month within
[date_start, date_start+365d], scored by unique case-positive countries in a +/-56d window
over `cholera_surveillance_weekly_combined.csv`; falls back to date_start if all-zero.
`date_start` reads env `MOSAIC_BUILD_DATE_START` else installed `config_default$date_start`.

**Why:** the old `max(date_start, 2023-02-01)` floor predated the JHU/AI multi-source
back-history that now populates pre-2023 years; 2015-02 now has ~9-20 active-case countries.

**How to apply / verified facts:**
- ic_t0 resolution (reproduced): 2023-01-01 -> 2023-02-01 (22 active; **TIE** at 22 across
  months 2/3/4, broken to earliest by which.max -> matches old floor, shipped priors inert);
  2015-01-01 -> 2015-01-01 (20 active); 2018 -> 2018 (23); 2027/2030 -> fallback (0 active,
  surv data sparse post-2026).
- All consumers correctly use ic_t0: est_initial_V1_V2 (date_start=ic_t0), est_initial_E_I /
  _R / _S (t0=ic_t0), and the prop_I_initial population-at-t0 match (difftime vs ic_t0). Each
  est_initial_* honours the explicit t0/date_start arg over config$date_start.
- est_initial_R_location only counts infections with recovery_date < t0 -> NO future-case
  leakage at an early t0 (era-correct R seed).
- E_I seeds from `cholera/daily/cholera_surveillance_daily_combined.csv` (DIFFERENT file from
  the weekly file used to PICK ic_t0), 3-day lookback. At 2015-01-01 that window has 28
  countries present / 15 case-positive -> E/I NOT cold-started for an early start. Coverage
  to 2031.
- Population: UN_world_population_prospects_daily.csv covers 1967-2100; annual demographics
  2000-2024 (capped via pmin(year, dem_max_year) for late surv). Early ic_t0 well-covered.
- epidemic_threshold and the mu CFR window (recent_years 2021:2025) are era-agnostic by
  design (regime threshold / CFR level), NOT coupled to date_start. Acceptable.

**Statistician red-team resolved (2026-06-20, this session):**
- Finding 1 (criterion mislabeled): epi decision = KEEP breadth, RELABEL honestly. Breadth
  = COVERAGE (max countries seeded from real surv vs no-data Beta fallback), NOT intensity.
  Volume-weighting REJECTED: it pulls ic_t0 to the outbreak PEAK and over-seeds E/I (the
  est_initial_E_I 21d-volume back-calc still runs per-country at the chosen t0). Confirmed on
  real 2023 data: breadth ties to Feb (~22, earliest), volume would pick April (~30k vs ~22k
  Feb cases). Comments at make_priors_default.R top-note + the .ic_select_epoch helper now
  state this explicitly.
- Finding 2: epoch selection factored into PURE helper `.ic_select_epoch(surv_df, date_start,
  window_days=56L, ...)` returning list(ic_t0, nactive, candidates). Test
  tests/testthat/test-ic_select_epoch.R extracts ONLY that fn def from the build script (parse
  + eval the single assignment, no full build) and tests 2023->2023-02-01, early-start tracks,
  ties->earliest, all-zero->fallback+warn, breadth-not-volume. 12 assertions green. NOTE the
  +/-56d window makes adjacent months tie (a mid-March signal is seen by Feb's window), so
  synthetic-fixture expected months are the EARLIEST window-reaching month, not the signal's
  own month — this is correct earliest-tie-break behavior.
- Finding 3: added `priors_default$metadata$build_date_start <- as.character(date_start)` for
  swe's make_config cross-artifact desync assert.
- Confirmed real-data 2023 default ic_t0 STILL == 2023-02-01 (HARD CONSTRAINT held; shipped
  v15.12 .rda numerically unchanged). DESCRIPTION/document/rda-rebuild left to parent.

**Open gaps (parent to fix in coordinated change):**
1. make_config_default.R:19 hardcodes as.Date("2023-01-01") with NO MOSAIC_BUILD_DATE_START
   override -> the env contract is asymmetric; a back-history rebuild must set BOTH or the
   config and priors disagree on date_start. Comment at make_config_default.R:15 still cites
   the stale `ic_t0 = max(date_start, 2023-02-01)` formula.
2. priors_default description (make_priors_default.R:64) v15.12 (a)-clause still describes the
   OLD floor verbatim ("now use ic_t0 = max(date_start, 2023-02-01)", "2015 ... 0 active-case
   countries", "the floor pins ICs"). Superseded by the code. Next rebuild should bump to
   15.13 and rewrite the (a) clause to the data-driven derivation. .rda NOT rebuilt (2023
   values numerically unchanged), so shipped .rda is self-consistent.
3. which.max tie-break (earliest month) is what preserves the "2023 unchanged" guarantee —
   fragile: a data refresh nudging month-3 above month-2 would silently shift ic_t0 and the
   2023 priors on next rebuild. Consider documenting/pinning the tie rule.
