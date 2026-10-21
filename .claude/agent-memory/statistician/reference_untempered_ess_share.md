---
name: untempered-ess-share
description: MOSAIC 1.4.0 informativeness f1 = Kish ESS(eta=1)/N is a PRIOR-VOLUME share - N-invariant but dimension-dependent; 1-2 parameter test fixtures get flagged flat; threshold 0.01 evidence; not assessable at N <= 100
metadata:
  type: reference
---

f1 = `ess_untempered_frac` (round-1 D1, `flat_ess_frac` = 0.01) converges to E[u]^2/E[u^2] at eta = 1: the prior
share the raw likelihood leaves plausible. On the 28 national pools (sim-order prefixes 7k/15k/30k/50k) BFA is
0.052-0.054 at every N, the other 27 are <= 2.7e-4 (ESS ~ 1, so f1 ~ 1/N). Script:
claude/redteam/theory/r1/12_flat_frac.R.

**Why it bites:** it is a volume, so it depends on the number of parameters. A 1-D toy likelihood with an 18x
contraction (-2000 (theta - 0.4)^2) has f1 = 0.056 and is "flat"; test-auto-gate's 2-parameter gate_pool was flagged
flat for the same reason. Fixtures must use sharp likelihoods (e.g. -2e5 (g - 0.4)^2 - 5e3 (rho - 0.5)^2 at
n_obs 1e5 -> f1 5e-4, all green).
**How to apply:** any test or example expecting a green informativeness row needs f1 < 0.01; f1 >= 1/N always, so
N <= 1/flat_ess_frac (100) is "not assessed" (yellow, flat NA). Decided 2026-10-20 (coordinator): the stop rule
needs only the three SAMPLING checks green twice (history field `sampling_green`); a flat run stops (BFA ~8.9k) with
stopping_rule_met TRUE and a YELLOW status. Suite cost 2.07M (1.48x) instead of 2.22M.

Round 2 (2026-10-21): flat also needs untempered Kish ESS >= 20 (`.mosaic_flat_min_ess()`); the N <= 100
"not assessed" rule is gone. BFA flat 1k-50k (ESS 56 at 1k; 300 draws ESS 16 -> not flat). A plateau (>= 10 ties)
also turns k-hat "fewer than 5 distinct tail ratios" into tied/green (BFA 20-30k). sims_projected_for_green includes
one confirming batch, so replays that extrapolate from it must subtract it (else cost 2.13M instead of 2.07M).

Related: [[v140-theory-redteam]], [[v140-ws1-statistical-core]].
