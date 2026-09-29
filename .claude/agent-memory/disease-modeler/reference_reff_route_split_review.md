---
name: reff-route-split-review
description: Epi review of v0.92.1 route-decomposed R_eff (R_hum+R_env) - kernel faithful to engine; mortality omission shifts human GI ~1 d at high mu (claim "<0.2 d" false); R_env frozen-at-t carries psi twice (beta_env and 1/delta); p_beta is NOT a route share
metadata:
  type: reference
---

Reviewed PR #126 (merge 96096a659, v0.92.1) on 2026-09-28.

- **Sound:** Isym/Iasym equal weight in human kernel = engine (`total_i = Isym + Iasym`) = spec eq:foi-human. The env shedding weight w_i = zeta_i/(zeta_1+zeta_2) is correct, and theta/kappa/scale cancel. Frozen-at-t R_env = (inc_env / W_hat) x (S_w / delta_t) is Fraser-2007 instantaneous R. It is coherent.
- **Mortality:** it matters only through kernel shape, because the total sits in the numerator. Defaults (gamma_1=0.1, sigma=0.25, iota=0.714) give a human mean GI of 9.12 d. Adding mortality changes it as follows:
  - mu=0.0065: 8.66 d
  - mu=0.0174 (max mu_j_baseline x1.5 epi): 8.02 d
  - mu=0.058: 6.53 d
  - At r=0.05/d this biases R_env +5% (mu 0.0174) and +14% (mu 0.058).
  - The roxygen claim "<0.2 day" holds only at median mu (~0.002).
  - Fate-at-onset CFR v2.1 will require the kernel to be re-derived.
- **psi amplification:** R_env scales with beta_env(psi)/psibar x 1/delta(psi). At s1=5, s2=2.5, psi 0.2 -> 0.9 moves 1/delta from 16.5 d to 190 d (11.5x) with no hazard change. R_env > 1 at high psi means "if this survival held ~200 d". It does NOT mean growth, and it is not a cohort R.
- **Route share:** p_beta = beta_hum/(beta_hum+beta_env) adds rates in incommensurable units. beta_hum multiplies I^a1/N^a2, which is ~1e-3 at national N; beta_env multiplies (1-theta) x a saturating dose, ~0.35.
  - Prior-centre sims give a 6-10% human share (ETH, COD); calibrated runs give 0.1-0.6%.
  - The sim_components.R "~25% near onset" comment holds only at ~1e-6 symptomatic prevalence with 16-d decay.
  - The prior description "proportion of transmission human-to-human" is wrong under the current engine.

Related: [[cholera-rt-r0-literature]], [[alpha-mixing-exponents]].
