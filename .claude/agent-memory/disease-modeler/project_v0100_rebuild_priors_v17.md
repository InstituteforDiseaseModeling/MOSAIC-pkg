---
name: v0100-rebuild-priors-v17
description: 2026-09-30 full data-object rebuild (priors v17.0 / config v6.0, branch rebuild/defaults-v0100) - sigma refit decision, IC magnitude shifts, MC non-reproducibility, ic_t0 vs date_start gap
metadata:
  type: project
---
Built 2026-09-30 in worktree .claude/worktrees/rebuild (branch rebuild/defaults-v0100), 10-seed psi, window 2023-01-01..2027-04-29.

- **sigma REFIT** Beta(4.30,13.51) -> Beta(3.75,7.12) (mean 0.24 -> 0.35). Reason: the hardcoded value WAS
  est_symptomatic_prop() output on the mistranscribed Harris 2008 row (0.184); corrected row 127/202 = 0.629.
  Builder now reads param_sigma_prop_symptomatic.csv; config sigma = prior mean. Haiti sero-surveys (0.21/0.24)
  now sit ~20th pct. Open: Harris 2008 (household contacts, culture) arguably does not belong in a population
  sigma table; the est_symptomatic_prop quantile heuristic is not a meta-analysis (statistician).
- **prop_R_initial means fell ~20x median (3-400x)**: bug fix (old est_initial_R used rho=0.1/chi=0.5 fallback +
  mode-fit inflation), now internally consistent with the reporting chain. S rises to 0.84-0.99. Flag: sero
  evidence in endemic SSA suggests higher immune fractions than a symptomatic-reporting back-calc gives.
- **ic_t0 = date_start since v0.100.1** (selector removed). E/I use a 28-day window STRADDLING date_start
  (est_initial_E_I lookahead_days=14): TZA/ZAF/ZWE/SSD/UGA report nothing just before 2023-01-01 but have
  outbreaks under way. Template only if whole window empty (21/40). P(E+I>=1) >=0.99 wherever cases (COG 1 case
  = 0.14). USER DECISION: quiet-start SEEDING FLOOR (est_initial_E_I quiet_start="seed", Beta(1,1e5) E and I,
  = v16.1 mean 1e-5) for locations with no cases in window but cases later up to date_stop: BFA CAF CIV GHA NAM
  NER RWA SWZ TCD TGO (metadata$quiet_start_seeded; v17.1 adds SSD/TZA/UGA/ZAF/ZWE -> 16, see
  [[v0101-quiet-start-review]]); 11 silent-everywhere keep template. COG (1 case in window)
  later EXTENDED (coordinator, within user intent): also seed when the window-based prior implies
  N*(E[pE]+E[pI]) < 1 AND later cases -> COG only (exp 0.40 -> 124; 60-day ignition 0.03 -> 0.94). AGO/UGA/BEN
  (exp >= 4) keep data-based priors; their P(E+I>=1) ~0.98. AGO's seed rests on AI-reconstructed ~1 case/week.
- Seasonal priors: positive envelope only AT PRIOR MEANS; 33% of independent draws negative (engine clamps
  to 0). SD shrink to <5% needed factor 0.12-0.63 in 31 locs -> rejected; builder reports the share.
- IC MC now SEEDED (est_initial_E_I/R/S `seed`, per-ISO derived; builder ic_seed=20260930); two builds byte-identical.
  MC CV of prior means across seeds: E/I 8% at n=100 -> 2-3% at n=1000 (same runtime, builder uses 1000);
  prop_R 12% at n=100 vs prior CV ~1.0 (kept 100; n=1000 ~ +40 min). User ACCEPTED sigma refit + new prop_R (watch posteriors).
- V2 IC = phi_2*min(r2, phi_1*r1) -> V2 x0.62, paired-campaign V1 rises (NER 3.8x). CORRECTION 2026-10-03:
  this is NOT what the engine realizes over a multi-day round (daily cap + re-dosing gives
  min(phi_2*r2, V1)), so the IC V2 is 12-21% low - see [[ocv-nu-split-v0103-review]].
- Unchanged: mu_jt (GAM input identical), mobility, tau_i, beta_j0_tot, kappa/zeta_1/zeta_2, nu (already deduped).

**How to apply:** when a calibration under v17/v6 looks different, check sigma (IC and beta scale via 1/sigma)
and prop_R (R_eff at t0 ~ R0 now) first. See [[h2-prior-rebuild-prep]], [[inert-params-and-prior-defects]].
