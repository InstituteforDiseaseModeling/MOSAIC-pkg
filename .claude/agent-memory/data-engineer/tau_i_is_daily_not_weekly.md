---
name: tau-i-is-daily-not-weekly
description: MOSAIC tau_i is a DAILY departure probability (verified from the OAG mean-daily file); MOSAIC-docs and the MOSAIC-OCV E3 notes both call it weekly, which inflates every E3 "overland vs air" amplitude claim by exactly 7x
metadata:
  type: reference
---

`config_default$tau_i` / `model/input/param_tau_departure.csv` is a **per-DAY**
departure probability.

**Verification (re-runnable):** `est_mobility()` reads
`MOSAIC-data/processed/OAG/oag_africa_2017_mean_daily.csv` ("mean DAILY OAG
flight data from 2017") and fits `fit_prob_travel(y = daily trips, n = N_2017)`.
Naive check reproduces the shipped values to 3 s.f.:
- NGA: rowSums(daily OAG)/N_2017 = 7.12e-06 vs `config_default$tau_i["NGA"]` = 7.13e-06
- median over 40 = 2.96e-05 in both
The sibling `oag_africa_2017_mean_weekly.csv` is 7.02x larger and is NOT what
the fit uses.

**Two authoritative-looking sources say "weekly" and are wrong:**
1. `MOSAIC-docs/04-model-description.Rmd` L650 ("fitted to average weekly air
   traffic volume") and the L724 figure caption ("the estimated weekly
   probability of travel"). L753 in the same file correctly says "daily".
   The doc contradicts itself; the code is daily.
2. `MOSAIC-OCV/notes/E3-mobility-*.md` — "Air-fit tau_focal ~ 1-5e-5/wk" and
   the table of air-fit values (COD 1.8e-5, MOZ 2.2e-5, NGA 1.2e-5, ETH 4.7e-5).
   Those are per-day numbers labelled per-week.

**Consequence — the 7x:** E3's overland tau band (1-5e-3/wk) is genuinely weekly
(its conversion is `tau_nat ~ (crossings/day * 7 * outbound_frac * unique_frac)/pop`,
E3-mobility-overland-adjustment.md section 7a). Comparing that *weekly* band to an
air tau they believed was weekly but is daily produced E3's headline
"~100x overland amplitude correction" / "45-561x, median 84x". The honest
like-for-like ratio is **daily-vs-daily ~5-25x (median ~12x)**. Any figure
quoting 84x/100x/561x from the E3 notes is 7x too large.

Related: [[mobility_od_source_quirks]].
