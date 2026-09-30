---
name: psi-star-absorbs-stage1-psi-gains
description: Measured 2026-09-24 — calc_psi_star's 4 free per-location params absorb 69-100% of a 13% stage-1 psi accuracy gain out-of-sample; the absorber is `a` (gain) and `z` (smoothing), NOT `b` (offset); a->0 makes psi* a constant so the transform can discard psi entirely
metadata:
  type: project
---

**The invariant:** a stage-1 improvement in environmental suitability psi does not propagate
downstream unless it lies OUTSIDE the span of `calc_psi_star(psi, a, b, z, k)`. Measure the
absorption before designing any psi A/B.

**Why:** the engine never sees psi. `sample_parameters()` overwrites `config$psi_jt` with
`calc_psi_star()` output (`R/sample_parameters.R:1461`) using four per-location CALIBRATED
parameters. Any psi difference inside that 4-dim family is re-fitted away.

**Measured** (dugong `~/psi_evolve`, scripts `absorb_check{,2,3}.R`, results
`absorb_result{,2,3}.csv`; 16-country pool x 6 selection cutoffs = 89 country-blocks; arms
`psi_cache_P000E` (production LSTM) vs `psi_cache_ND` (DLinear); truth =
`target_D_rate_per_country_floored`; theta fitted MAP on 3y pre-cutoff history, scored on the
held-out OOS window):

| theta freed (others at shipped a=1,b=1,z=1,k=0) | LSTM | ND | gap | absorbed |
|---|---|---|---|---|
| none (raw psi, no transform) | 0.2057 | 0.1784 | +13.3% | — |
| none (shipped default transform) | 0.2147 | 0.1886 | +12.2% | 0% |
| **b only (offset)** | 0.2117 | 0.1925 | +9.1% | **26%** |
| **a only (gain)** | 0.2343 | 0.2228 | +4.9% | **56%** |
| **z only (EWMA)** | 0.2054 | 0.1922 | +6.4% | **50%** |
| k only (lag) | 0.2188 | 0.1935 | +11.5% | 3% |
| **a + b** | 0.2044 | 0.2060 | −0.8% | **106%** |
| all four | 0.2030 | 0.1948 | +4.0% | 69% |

Every paired test on the transformed series is null (t p = 0.11-0.84, Wilcoxon 0.13-0.98,
ND better in 34-47 of 89). The RAW gap is t p=0.011 but Wilcoxon p=0.104 — marginal even
untransformed. Fitting theta per-country-per-block (13 points, 4 params) absorbs **98.5%**;
that is the overfit upper bound.

**Three mechanisms, in order of size:**
1. `a -> 0` sends `psi* -> sigma(b)`, a CONSTANT. The reachable set of the transform contains
   "ignore psi". Median fitted a is 0.85 for the LSTM vs 1.63 for ND — the transform sharpens
   the under-amplifying arm, which is precisely ND's advantage (sd_ratio).
2. `beta_jt_env = beta_j0_env * psi*/mean(psi*)` (`R/sim_precompute.R:192`) is **exactly
   invariant to multiplicative rescaling of psi\***. A pure LEVEL difference (ND bias 0.53 vs
   0.49) is structurally invisible to the environmental transmission route before any
   calibration. Level only reaches the model through the decay channel,
   `1/(fast + pbeta(psi*,s1,s2)*(slow-fast))` (`sim_precompute.R:215`), which `b` then absorbs.
3. **Prior smearing.** 4,000 draws from the shipped psi_star priors applied to MOZ psi:
   normalized amplitude ratio p05/p50/p95 = 0.035 / 0.855 / 1.949; **16.4% of draws cut psi's
   amplitude below 25%** of original, 27.9% below 50%; **49.5% shift psi by >14 days**. The
   posterior ensemble averages psi through this kernel, so two psi variants are made alike
   even with no likelihood-driven absorption.

**How to apply:**
- Do NOT design a psi A/B around pinning `b`. Pinning b alone leaves 74% of the absorption
  capacity. To isolate level from shape, pin **a and z** and free b and k.
- An "identity" arm (a=1,b=0,z=1,k=0) is not a clean control: the shipped config default is
  **b = +1** (`config_default`), b=0 roughly halves the decay channel's dynamic range in
  low-psi countries (NGA survival mean 34.8d -> 21.7d against a 16d floor), and it changes
  four factors at once — which the psi_evolve PROTOCOL's "one change per arm" forbids.
- Run this absorption test (cheap, no simulation) as the gate before spending any calibration
  compute on a downstream psi A/B.

Related: [[ab-endpoint-noise-and-sizing]], [[inference-scaling-measured-v0903]],
[[likelihood-zero-penalty-dominates]].
