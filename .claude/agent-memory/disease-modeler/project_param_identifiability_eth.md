---
name: param-identifiability-eth-v0903
description: OAT likelihood-profile classification of all 43 free-draw parameters on ETH v0.90.3 - 12 class-A (>=1000 nats, data see them, inference recovers nothing), 15 class-D flat, 3 provably INERT (prop_S_initial/rho_deaths/phi_2); HSIC-on-115 is a power failure
metadata:
  type: project
---

Built 2026-09-17 for the inference-methods review. Report:
`MOSAIC-pkg/claude/review_inference/findings/SAMP.md`. Table:
`MOSAIC-pkg/claude/review_inference/scratch/SAMP/classification.csv`.

**Why:** the pipeline's own HSIC diagnostic (run on the 115-draw best subset) reports ZERO
significant parameters, which was being read as "the data are uninformative". That is a power
failure, not a fact about the data.

**How to apply:** before changing any prior on "the parameter is unidentified" grounds, check
the class here. Three prior decisions in `priors_default` were justified by a
non-identifiability premise that this measurement contradicts (see below).

## Method
43 free draws x 7 prior quantiles x 20 engine seeds = 6,060 sims, all other sampled quantities
pinned at the best draw (`sim_id = 7520`) via point-mass priors so the real `sample_parameters()`
derivations (B2.1 mu, beta split, zeta_2, psi* transform, IC integerisation) still run. Per-point
SE ~26 nats. Common random numbers give NO variance reduction here (engine stream diverges on any
state change) -- must average over seeds.

## Results
- **Class A (>=1000 nats, z>=10), 12:** `psi_star_a` (1.44e6), `zeta_1` (1.44e6),
  `beta_j0_tot` (7.4e4), `sigma` (2.8e4), `CFR_target` (1.0e4), `rho` (9.5e3),
  `psi_star_b` (6.2e3), `psi_star_k` (3.7e3), `chi_epidemic` (3.3e3), `chi_endemic` (1.9e3),
  `mu_j_epidemic_factor` (1.3e3), `p_beta` (1.0e3).
  **For all but psi_star_a/b/k and CFR_target the posterior moved <0.1 prior SD** -> inference
  failure, NOT identifiability failure.
- **Class D (flat within the noise floor), 15:** `omega_1/2`, `phi_1`, `gamma_2`, `epsilon`,
  `iota`, `delta_reporting_cases/deaths`, `a_1_j`, `a_2_j`, `b_1_j`, `b_2_j`, `psi_star_z`,
  `prop_V1_initial`, `prop_E_initial`. Genuinely non-identifiable from ETH case/death series.
- **Class E INERT (3):** see [[inert-params-propS-rhodeaths-phi2]].
- A global Spearman/KS sensitivity on ALL 25,000 draws finds **14 Bonferroni-significant**
  vs HSIC-on-115's zero. Use `is_retained`, not `is_best_subset`, for sensitivity.

## Premises now FALSIFIED by this measurement
1. **`mu_j_epidemic_factor`**: priors v15.18 reshaped Gamma(1,2)->Gamma(3,6) on the stated
   grounds that it is "statistically UNIDENTIFIED (calibration leaves posterior ~ prior)".
   It is class A (1,270 nats, z=34.9) and the likelihood wants it **below the 1e-5 quantile**
   of the reshaped prior -- the reshape moved the mode UP (0 -> 0.33), i.e. the wrong way.
2. **`zeta_1` / kappa saturation**: `R/sim_components.R:463-467` and
   `04-model-description.Rmd {#sec:shedding}` say the environmental dose-response saturates so
   "kappa, zeta_1, zeta_2, zeta_ratio and the four decay parameters are flat directions".
   That was TRUE pre-v0.89.0. Under the v0.89.0 per-capita dose (`W/N`), `zeta_1` is the
   **joint-strongest parameter in the model**. Both texts are stale.
3. **`rho_deaths`** tight prior (v15.7) was to pin the mu*rho_deaths sloppy direction. Under
   B2 (v15.15) it is an EXACT cancellation, so the prior does nothing.

## Prior-vs-likelihood conflicts found (ETH)
| param | prior centre (source) | likelihood optimum | channel |
|---|---|---|---|
| `rho` | 0.423 Wiens 2025 | 0.173 (p02) | cases 6,286 nats |
| `chi_endemic` | 0.521 Weins 2023 | >0.963, still improving past p99.999 | cases only |
| `CFR_target` | 0.0121 WHO-GAM | 0.061 (p98) | deaths only, 9,944 nats |
| `theta_j` | 0.632 | >0.733, still improving | - |

The `CFR_target` one is a **deaths-TIMING** signal, not a level signal: the best draw already
over-predicts total deaths 5.16x (5,856 vs 1,136 observed) while the deaths LL still wants a
higher CFR -- i.e. simulated deaths are in the wrong places in time and the level inflates to
cover the observed-nonzero/predicted-zero cells. This reframes the standing "production deaths
bias ~2x" work ([[project_prod_deaths_bias_b2_epi_gap]]).

**Caveat:** all profiles are ETH-conditional and one-at-a-time (blind to ridges). `phi_2` is
inert *because ETH has no second doses*; seasonality is flat *for ETH*. Re-profile per country
before pinning globally.

Related: [[likelihood-noise-floor]], [[required-n-prior-as-proposal]]
