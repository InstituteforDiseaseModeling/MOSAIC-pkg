---
name: reff-median-aggregation-flattening
description: Cori R_eff near-1 is an aggregation+burn-in artifact, NOT smooth dynamics — per-day cross-member median + IC-spike-driven burn-in both flatten it; report medoid or member-own-peak
metadata:
  type: project
---

The reported time-varying Cori R_eff in `reproductive_numbers.rds` sits near 1 almost
everywhere. Diagnosed (NGA, COD national, full_metapop_nmme, 2026-06-30) as TWO compounding
ARTIFACTS, not faithful dynamics.

**Why:** explosive cholera should show peak R well above 1; near-1 misrepresents the model.

**Mechanism 1 — per-day cross-member MEDIAN (primary).** Production `central` = `.add_reff_recompute_ci`→`.mosaic_reff_resim_ci`'s `central_mat` = `weighted_quantiles(per-member-R, w, 0.5)` per (loc,t). That is "per-member R then per-day weighted median" (method b). Members are phase-MISALIGNED: member-R-peak DAYS spread over IQR ~450d (NGA) / ~100d (COD), SD 661 / 176 days. At any single calendar day only 20-51% of members exceed R=1.5, so the per-day median regresses to ~1. Post-burn-in peaks (daily reported_cases proxy): NGA (b)=1.33 vs member-own-peak wq50=2.63 / wq90=3.46; medoid=2.19. COD (b)=1.53 vs own-peak wq50=3.30/wq90=4.64; medoid=3.27. Renewal on the weighted-MEDIAN incidence (method a) is ALSO flattened by the same phase-misalignment smoothing (a≈b: 1.33/1.67).

**Mechanism 2 — burn-in hides the only un-flattened peak.** The GLOBAL renewal peak is at day 2-3 (IC seeding transient: NGA 2.93, COD 7.57/6793 raw). `.add_reff_recompute_ci` correctly NA-masks first `burn_in_days` (default 30, here 45). But post-mask the only thing left is the flattened plateau → reported peak collapses (NGA 2.93→1.33, COD 7.57→1.67). The day-3 spike IS a genuine IC artifact (correctly excluded); the problem is the median-aggregation leaves nothing explosive behind it. infectiousness_floor=1 gates ~0-1 extra steps (only the t=2 COD R=6793 IC spike) — floor is NOT a meaningful suppressor.

**Mechanism 3 — kernel mean (minor, correct direction).** Moment-matched GI mean is 6.76d (NGA) / 4.98d (COD), not ~5.4. Euler-Lotka: at r=0.10/d, R=1.37(mean3)/1.62(5.4)/1.89(8); at r=0.20, 1.78/2.31/3.84. Larger GI mean RAISES R, so the kernel is NOT the cause of near-1; if anything the two-clock kernel (excludes 1/delta env delay) is a slight under-estimate. Not the lever.

**Implied-R from growth:** median-incidence max 14d log-growth r≈0.06(NGA)/0.11(COD) → implied R 1.33/1.61 — consistent with the FLATTENED curve. Per-MEMBER max growth r wq50≈0.14/0.18 → implied R 1.70/2.02. So the median incidence input is itself already smoothed; the explosive growth lives in individual members.

**How to apply / CORRECT reporting:** for a headline R_t that captures explosive peaks, report the MEDOID single coherent trajectory's R_eff (method d) or the distribution of per-member OWN peaks (method c), NOT the per-day cross-member weighted median. The per-day median/quantile band is valid as an "at this calendar date" statistic but must NOT be sold as the epidemic peak R. Verdict: near-1 = artifact of (1) per-day cross-member median + (2) burn-in removing the only sharp peak; kernel mean is innocent. Repro: /tmp/reff_diag*.R (load_all, reads ensemble_candidate.rds cases_array as daily proxy + trajectories_ensemble incidence median).

**FIX SHIPPED (2026-06-30, R/calc_Reff.R `.mosaic_reff_resim_ci` + `.mosaic_reff_select_medoid_member`; add_reproductive_numbers wires attrs).** Headline `central` redefined = MEDOID trajectory's R_t (method d), reusing run_MOSAIC()'s medoid criterion ON THE SAVED cases_array: param set whose stochastic-MEDIAN reported_cases at loc 1 has min log-MAE (eps=1) to ensemble$cases_median, then within-param stochastic rerun closest to that param's own median → member m=(s-1)*nP+p. NOTE the within-set tiebreak with nS=2 favors the LARGER rerun (log-scale asymmetry — both equidistant in count are not equidistant in log). q2.5/q50/q97.5 KEPT as per-calendar-day cross-member weighted quantiles (now the envelope, attr band_definition). NEW attr `peak_Rt` = per-location data.frame (location, q2.5/q50/q97.5, n_members) of per-member post-burn-in floor-gated TIME-MAX R_t reduced by weighted_quantiles = the explosivity stat (schema pinned with plot_Reff's .reff_peak_table — it expects a DATA.FRAME not a list). burn_in_days now passed INTO the resim fn (NA-mask before time-max so IC spike doesn't dominate). Smoke: MOZ OLD per-day-median peak 1.46 → NEW medoid 4.58, peak_Rt q50 4.62 [2.30,6.98]; COD 1.50 → 2.23, peak_Rt q50 1.88 [1.45,3.15]. Direct calc_Reff() path (renewal on weighted-MEDIAN incidence) is ALSO flattened (method a) — docstring now flags it + points to recompute_ci for the coherent headline.
