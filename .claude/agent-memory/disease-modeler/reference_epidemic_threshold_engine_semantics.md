---
name: epidemic-threshold-engine-semantics
description: what epidemic_threshold does in production (rng) since CFR v2.1 - switches ONLY the case PPV chi; deaths per onset unaffected; realized reported CFR = mu_jt*chi_eff/chi_epidemic; ZAF v17.1 1.07e-7 verdict; Zheng fallback = permanent endemic regime
metadata:
  type: reference
---
- rng mode: `sim_components.R` reported-cases block, chi_eff = chi_endemic if Isym/N < threshold else
  chi_epidemic; reported = Binom(new_sym, rho)/chi_eff. That is the ONLY production use. mu_j_epidemic_factor
  was removed in v0.96.0 (sample_parameters.R `.MOSAIC_REMOVED_MORTALITY_PARAMS`); the epidemic-flag death
  hazard survives only in replay mode (laser-cholera parity).
- Deaths: p_fatal = mu_jt*rho/(rho_deaths*chi_epidemic) (sim_params.R), deaths likelihood exposure =
  (rho/chi_epidemic)*onsets - both regime-free. So realized reported CFR = mu_jt x chi_eff/chi_epidemic:
  = mu_jt in epidemic regime, ~0.68 mu_jt in endemic regime (prior means 0.52/0.76).
- ZAF v17.0 (Zheng fallback 1.18e-5 = 442 reported cases/wk nationally, above every 2023 week) -> paired sims:
  epidemic regime on 0% of case-days, reported CFR/mu 0.86; v17.1 (1.07e-7 = 4 cases/wk, median of 26 positive
  weeks, 17 of them <=5 cases) -> 100% and 0.99, deaths/onset identical. Verdict 2026-10-01: KEEP (method as
  intended; ZAF sits with CIV/BEN/LBR on the cases/wk scale; robust to the curated curve shape).
- Post-1.0: the 11 fallback countries (BFA/MLI/SEN ... 9-166 reported/wk at threshold) sit in the endemic
  regime at all realistic levels -> their deaths need a ~1.5x mu offset; p_fatal's fixed chi_epidemic couples
  the deaths fit to the threshold.
See [[v0101-quiet-start-review]], [[cfr-v21-mujt-prior-review]].
