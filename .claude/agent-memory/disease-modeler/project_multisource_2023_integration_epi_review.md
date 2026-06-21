---
name: multisource-2023-integration-epi-review
description: Epi red-team of config v4.1 / priors v15.12 multi-source fit-target + IC-floor + epidemic_threshold change; key risk = AI documented_zero zero-walls at full likelihood weight
metadata:
  type: project
---

Red-teamed the uncommitted multi-source surveillance integration (config_default v4.1, priors_default v15.12) on 2026-06-19.

**BLOCKING risk — AI `documented_zero` zero-walls enter the fit at full weight.**
Verified against the rebuilt `data/config_default.rda` (NOT the installed namespace, which was still v4.0) and the combined daily file:
- 16 countries (BFA, BWA, CAF, ERI, GAB, GIN, GMB, GNB, GNQ, MLI, MRT, SEN, SLE, SWZ, TCD, ZAF) enter `reported_cases` as an ALL-ZERO WALL (cells present, 0 total positive cases). 8 of them (BWA, ERI, GAB, GMB, GNQ, MRT, SWZ, ZAF) are 100% AI `documented_zero` with NO direct WHO/JHU obs. **All 16 were entirely ABSENT (NA) from the old WHO-only fit file** -> previously excluded from the likelihood; now confirmed-zeros at full weight. 22 countries get deaths zero-walls.
- These 16 all carry the UNTUNED 2e-5 global `beta_j0_tot` default + psi_star_b=+1.0 forcing -> calibration will be pushed to crush beta/psi to suppress all transmission. This is a real bias (Task 1 + Task 4).
- The fix (carried weights 0.8-0.95 on AI cells) is INERT: `calc_model_likelihood` has no per-cell weight slot; only `get_location_config` row-subsets the weight matrix. So "weighting deferred" = AI zeros at effective full trust. **Recommendation: hold `documented_zero` OUT of the fit target until per-cell weighting lands** (gate it like fourier/assumed_zero), OR ship only after the likelihood consumes the weight matrix.

**IC lookahead (Task 2) — defensible but worth a comment.** ic_t0 = max(date_start, 2023-02-01). Default date_start=2023-01-01 so ICs at sim t=1 (2023-01-01) are seeded from the E/I lookback [2023-01-29..01-31], i.e. ~28-30d AFTER sim start. Minor temporal inconsistency at onset (acceptable: 2-3 wks of dynamics is short vs the multi-year run, and avoids the cold-start the floor is designed to prevent). New combined file adds KEN as a positive-case seed country in the lookback; the documented_zero gate does NOT corrupt E/I (zeros contribute 0 infections anyway).

**epidemic_threshold (Task 3) — SOUND.** 28 data-derived / 12 fallback. All means in [6.5e-8, 3.2e-5], well under the 1% cap, biologically plausible (sub-1% daily Isym/N). Derived from positive-case weeks only (cases>0), AI excluded -> zero-walls do NOT affect it. The 12 fallback = exactly the all-AI-zero countries (+BFA), correctly defaulting to Zheng 0.7/100k. No phase-regime flips of concern.

**Seasonality / epidemic_peaks (Task 5) — SOUND.** Seasonal Fourier coeffs are window-independent (fit on full history); shipping old param_seasonal_dynamics.csv is fine. epidemic_peaks is window-filtered at build (`.filter_epidemic_peaks`); new window picks up 2 extra Jan-2023 peaks, none snap to boundaries.

config_default tests (incl. new config_default_weights) PASS. Config params used in threshold derivation: rho=0.423, chi_endemic=0.5, gamma_1=0.1.
