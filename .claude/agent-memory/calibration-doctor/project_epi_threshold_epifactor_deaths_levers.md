---
name: project-epi-threshold-epifactor-deaths-levers
description: How much epidemic_threshold + mu_j_epidemic_factor can move deaths bias (NGA/COD/ETH/MOZ NMME full-metapop); decomposition vs the CFR_target floor
metadata:
  type: project
---

Deaths-bias lever exploration for the four dense-deaths NMME full-metapop runs (laser.cholera
0.16.1 / MOSAIC 0.55.12). Sweep harness + brief: `claude/diagnose_fit/epi_lever_exploration/`
(run_epi_lever_sweep.R, epi_lever_sweep_results.csv, BRIEF_epi_levers.md).

**Why:** quantify how much of each country's deaths bias the epi_threshold + epi_factor levers can
fix ALONE, vs deferring to the separate CFR_target / mu_j_baseline drift fix.

**Mechanism (re-verified infectious.py 0.16.1):** epi_factor moves ONLY deaths; threshold moves
BOTH deaths (epi boost gate) AND cases (chi_endemic/chi_epidemic PPV switch). Confirmed in sweeps:
cases bias flat to 4 sig figs across epi_factor 0->1.

**Key durable findings (symptom -> lever):**
- The lever FLOOR = deaths bias at epi_factor=0 (boost fully off). Threshold cannot push deaths
  below this floor. Floors: NGA 1.60, COD 1.98, ETH 5.50 (degenerate medoid), MOZ 1.23. The floor
  IS the CFR_target/mu_j_baseline story — these two levers cannot reach it.
- epi_factor share of deaths-bias excess-over-1: NGA 47%, COD ~1% (inert), MOZ 87%, ETH ~50%.
- COD: threshold at p99.5 = NEVER-epidemic (0.5% time flagged) -> epi machinery OFF, epi_factor
  inert. COD deaths bias ~100% CFR; do NOT pull these levers for COD.
- MOZ: deaths bias is almost entirely a MEDOID TAIL DRAW of epi_factor (1.68 ~p93). BS-median
  epif 0.52 -> 1.77; epif 0 -> 1.23 (lowest CFR floor of the four). MOZ is the best case for the
  prior-tightening lever.
- NGA: threshold mis-set LOW (p19 -> 81% time / 98% deaths flagged, collapsed to always-epidemic).
  Raising threshold to ~p90 is a WIN-WIN: deaths down AND cases (under at 0.79) toward 1.0. NGA is
  the ONLY country where the threshold lever helps cases (cases under-predict there).
- The threshold 'knee' (time_frac ~0.10) sits near simulated-prevalence p90 for NGA/ETH/MOZ.

**Medoid vs ensemble (always keep distinct):** medoid-sandbox deaths bias matches ensemble for
NGA (2.14 vs 2.22), COD (1.99 vs 2.09), MOZ (2.80 vs 2.46 — medoid tail epif). ETH MEDOID IS
DEGENERATE: sandbox 9.96 vs ensemble 2.96 — read ETH absolute bias off the ensemble summary, use
the medoid only for lever-RESPONSE SHAPE. ETH endemic-deaths bias is NA (no observed endemic deaths
in the data-driven split).

**Implied-CFR ratios (predicted/observed, ensemble median):** NGA 2.75, COD 2.27, ETH 3.15, MOZ
2.35 — this is the dominant deaths-bias driver and the floor these levers cannot touch.

**Proposals (to disease-modeler/statistician, NOT committed):**
1. Tighten mu_j_epidemic_factor prior Gamma(1,2) (p90=1.15) -> e.g. Gamma(2,8) (mean .25, p90 ~.5)
   to stop unidentified tail-draw medoids (chiefly helps MOZ/ETH). Full posterior already low-mean.
2. Re-anchor epidemic_threshold to a fixed ~p90 percentile of SIMULATED Isym/N (not the observed-
   incidence Zheng mapping) so the data-derived threshold lines up with the scale it's compared
   against. Small cases cost via chi (negligible COD/MOZ where chi_endemic~=chi_epidemic).

See [[feedback_deaths_field_naming]] (use reported_deaths) and the central_method ~2x deaths-bias-
by-design note. The CFR_target/mu_j_baseline fix is the separate, dominant track.

**Sandbox gotcha:** run_fit_sandbox does NOT pass config epidemic_threshold to calc_fit_diagnostics
— the endemic/epidemic SCORING split uses a data-driven default (p75 of positive observed), so
endemic/epidemic bias columns are comparable across threshold sweeps. R scripts must use ABSOLUTE
paths (cwd resets between bash calls). raw run_LASER returns flattened (T x 1) numpy arrays for
single-location configs; N channel = S+E+Isym+Iasym+R+V1+V2.
