---
name: cholera-rt-r0-literature
description: Literature R_0/R_t ranges for cholera by setting + verdict on whether MOSAIC's near-1 national Cori R_t is expected; GI-kernel/burn-in/aggregation diagnosis
metadata:
  type: reference
---

# Cholera R_0 / R_t literature + MOSAIC near-1 national R_t verdict

## Literature R_0 / R_t by setting (with assumed generation/serial interval)
EXPLOSIVE / EPIDEMIC introductions (high initial R_t):
- Haiti 2010: Mukandavire et al. PNAS 2011 mechanistic R_0 ~1.06-2.78 across departments (Artibonite ~2.6-2.8); Tuite et al. Ann Intern Med 2011 ~2-3 early; Andrews & Basu Lancet 2011 ~2.0-2.6. Initial renewal R_t reported well >1 (often 2-4, some early-window estimates higher). GI/SI assumed ~5 days (Kaplan/Hartley waterborne) up to ~10 d in some.
- Yemen 2016-17: Camacho et al. Lancet Glob Health 2018 governorate R_t peaks ~2-3 at wave onsets (some early >4); renewal/EpiEstim, SI mean ~5 d.
- Zimbabwe 2008-09: R_0 ~1.5-2.7 (Mukandavire 2011 companion; ~2.0 typical).
- Goma/DRC 1994 refugee crisis: explosive, effective R_0 high (point estimates up to ~3-4 in compressed early phase; data-poor).
- These are NEW introductions into fully susceptible spatially-localized populations -> initial R_t is the MAXIMUM of the whole trajectory.

ENDEMIC / Sub-Saharan multi-year settings (R near 1):
- Endemic Bengal/Africa renewal R_t oscillates around 1.0 with seasonal excursions to ~1.3-2 at season onset, dipping <1 between seasons. Time-average ~1.
- This is the regime our 27 NATIONAL multi-year (2018-2027) series live in.

METHOD / GI sensitivity: renewal R_t scales ~ exp(r * mean_GI) for growth rate r, so a LONGER assumed GI INFLATES R_t for the same observed growth. Cholera GI/SI estimates range widely (~3-8 d direct human-human; Azman et al., Kahn et al. ~5 d household SI; longer when environmental/waterborne reservoir delay included). Choice of GI is a primary driver of the reported number.

## MOSAIC's R_t setup (calc_Reff.R + get_generation_time_distribution.R)
- Cori (2013) instantaneous R_t on SIMULATED S->E infection incidence (descriptor, not invasion threshold).
- GI kernel: TWO-CLOCK moment-matched Gamma, E[G]=5.4 d (1.4 latent 1/iota + 4.0 infectious), SD~6.5 d. iota=0.714, gamma_1=0.1, gamma_2=0.5, sigma=0.25.
- DELIBERATELY two-clock: excludes the documented THREE-CLOCK environmental-survival delay (1/delta). Spec (04-model-description.Rmd ~L1378-1391) states this makes R_t read SLIGHTLY CLOSER TO 1 in waterborne-dominated settings (shorter-mean-G approximation).
- infectiousness_floor=1: explicitly NA's the IC-seed spike (would read R~1e3) and deep troughs (R~0.01). This SUPPRESSES the explosive seed onset by design.
- National single-location aggregates.

## VERDICT
Near-1 national TIME-MEDIAN R_t is EPIDEMIOLOGICALLY EXPECTED for endemic multi-year national aggregates (endemic equilibrium R~1; phase-misaligned subnational outbreaks average growth+decline toward 1). NOT per se wrong.
BUT the weak MAX (only ~3.7-4.5 point / q50 barely >1) is mildly UNDERSTATED vs explosive-setting literature (onset R_t 2-4+). Most likely culprits, ranked:
1. NATIONAL AGGREGATION (biggest) - phase-misaligned subnational waves cancel; a true onset R_t>>1 in one province is diluted by stable/declining provinces. Legit reason, but it MASKS real local explosive dynamics.
2. 45-day BURN-IN - cuts the highest-R_t initial-growth window of any seeded/introduced outbreak. Defensible for FIT scoring (deaths transient ~28-35 d), INDEFENSIBLE for an R_t diagnostic. Use 0 (or <=7-14 d) for R_t; the infectiousness_floor already guards the IC seed spike.
3. GI KERNEL too SHORT for waterborne - two-clock 5.4 d omits environmental delay; biases R_t toward 1 (documented). A three-clock kernel would lift excursions. 5.4 d human-human is reasonable; it is the OMITTED environmental clock, not the 5.4 itself, that biases low.
4. infectiousness_floor=1 + medoid-of-incidence smoothing further damp peaks.

## Recommendations
- Report SUBNATIONAL / per-outbreak R_t (or per-epidemic-segment) - national aggregate hides onset explosivity.
- Use burn_in=0 (or <=14 d) for the R_t diagnostic specifically (keep 28-45 for fit scoring).
- Present PER-MEMBER peak R_t (max over posterior members), not just q50-of-incidence central, which smooths peaks.
- Optionally compute the three-clock (env-delay) kernel variant as a sensitivity to bound the waterborne-GI effect.

## Hand-offs
- statistician: aggregation math (Jensen/phase-cancellation of subnational R_t), and renewal-on-weighted-median-incidence vs per-member-then-quantile (peak attenuation).
- calibration-doctor: empirical dissection of the 27 outputs - does pre-burn-in window show R_t>>1? per-member max distribution? subnational re-run.
