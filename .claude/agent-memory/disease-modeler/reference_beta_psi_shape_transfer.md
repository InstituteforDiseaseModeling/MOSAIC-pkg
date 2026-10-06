---
name: reference-beta-psi-shape-transfer
description: Cross-country transfer/pooling of beta_j0 - the engine's psi/psibar normalisation makes beta level-invariant but NOT shape-invariant (BFA psi q95/mean 13.3 vs west donors 2.4-5.9); zeta-interval screens on national posteriors are near-vacuous; between-country logzeta-logbeta r=-0.52
metadata:
  type: reference
---

Engine fact (R/sim_precompute.R `sim_beta_jt_env`): beta_env,t = beta_env,0 * psi_t / mean_t(psi), so beta_j0_env is the time-mean rate whatever the psi LEVEL. Realised forcing also depends on:
- psi SHAPE: the peak/mean ratio scales peak R_t;
- absolute psi through the decay delta_jt = 1/(fast + pbeta(psi)(slow - fast)).

So "beta needs no psi adjustment" (RULE.md 1.1 section 6) holds for level only. A donor-typical time-mean beta in a spiky-psi country gives several-fold larger peak forcing.

Measured on config 6.2, identity map:
- BFA psi q95/mean 13.3 and max/mean 17.1, against west donors (CMR GHA LBR NER NGA TCD) 2.4-5.9 and 2.4-12.4. This is consistent with BFA's prior predictive staying ~18x over observed even with the pooled beta.
- The psi_star gain matters too. Pooling psi_star_a toward mid-amplitude (TN(0.79, 0.37)) removed the base prior's flat and very spiky draws and made UGA ~50% more explosive. Concentrating forcing amplitude can raise cumulative cases.

zeta screens: national log zeta_1 95% intervals are ~7.7 wide against the prior's 9.7. A "donor interval contains the consensus" screen passes nearly everyone; BEN (median 23.35, non-saturating) passes. Within-run |r|(log beta, log zeta) has median 0.18, but between countries r = -0.52 (p = 0.014, slope -0.62; 22 folded runs, v2026-10.02). Lower-zeta fits carry higher beta (GHA, BEN, CIV).

**How to apply:** when proposing beta pooling, transfer or recentring across countries, check the psi shape (q95/mean) of target vs donors, not just the logit-psi mean and SD. Prefer pooling beta only, not psi_star gains. See [[warmstart-rule11-redteam]].
