---
name: subset-ranking-beats-size
description: Measured @ ETH n=10,000 (2026-09-19) — |B| has an interior optimum at 25-250 so a bigger best subset does NOT predict better; replacing the NB log-likelihood ranking with a normalised training-window MAE cuts held-out WIS 26-39%; mean skill saturates by n~1,000-2,500 and only reproducibility keeps improving with n.
metadata:
  type: project
---

Answers SCALE's open question ("does a larger |B| predict better?") and the user's goal ("make the
calibration improve as more sims are run"). Report:
`MOSAIC-pkg/claude/inference_lab/reports/SUBSET-EXP.md`; figures `…/reports/figures/SUBSET-EXP_*`.

**Why:** "run more simulations" and "take a bigger subset" are the two reflexive levers. Both were
measured directly, on a cached 10,000-trajectory ETH pool with two leak-free holdout windows, and
**both are near-dead**. The live lever is the ranking statistic.

**How to apply:** before proposing more draws or a bigger `|B|`, check which of these binds. Do not
re-derive from the formulas — the effects are empirical.

1. **|B| has an INTERIOR optimum (25-250) under the shipped rule; 115 is inside it.** Beyond ~500,
   held-out WIS skill falls monotonically and held-out bias rises. At |B| = n the weighted median
   **collapses to an all-zero forecast**, because **51% of ETH prior draws predict exactly 0 cases
   on the median day** (34.7% produce <1% of the observed total). The selection step's real job is
   to exclude the dead half of the prior; past |B|~250 it starts re-admitting over-predictors.
2. **The ranking, not the size, is the lever.** Ranking draws by a *normalised training-window MAE*
   (`MAE_cases/mean(obs_cases)`, optionally + the deaths term) instead of by the NB log-likelihood,
   on the same data and the same trajectories, at |B| = 115: held-out WIS −39% (valid) / −26%
   (test); cases bias 1.51 → 0.99; deaths bias 2.19 → 1.10; 95% coverage 0.65 → 0.92. Paired across
   up to 40 disjoint sub-pools, t = −6 to −37. A logL pre-screen to the top 30·|B| changes nothing
   (Δskill < 0.003), so the gain is the criterion, not a two-stage structure.
   **Guardrail:** the cases+deaths variant selects zero-deaths members where deaths are sparse (ETH
   valid: deaths bias exactly 0.000). Default to cases-only; floor any channel with mean < ~1/day.
3. **Why the likelihood loses:** Spearman(logL, per-member **in-sample** R²) = **+0.65** but
   Spearman(logL, per-member **held-out** R²) = **+0.05 to +0.08**; *within* the top 1.15% it is
   **−0.08**. The NB logL ranks in-sample fit well and out-of-sample shape not at all.
4. **Scaling (WIS ∝ n^b), n = 250 → 10,000, disjoint sub-pools:** shipped rule b = −0.30 (valid)
   overall but only **−0.069 ± 0.021 beyond n = 1,000** (test: −0.313 ± 0.034). The MAE rule at
   |B| = 1.15·√n is **flat (−0.011 ± 0.008) because it is already at the ceiling at n = 250**.
   The random-subset null shows no scaling (−0.013 ± 0.016) — control passed.
   **The only thing that keeps improving with n is run-to-run spread:** across-sub-pool SD of
   held-out WIS ∝ n^−0.3…n^−1.1. That is the quantity behind the draw-block instability
   (R²_cases 0.797 / 0.019 / 0.371 over three 10k blocks).
5. **Fixed-ζ fractional posterior (B1) does NOT buy skill.** The best fixed ζ (3.2e-3, chosen on
   `valid`, confirmed on `test`) **ties** the shipped rule while driving Kish ESS to ~20. ESS
   becomes Θ(n) as the reviewers measured, but the ζ that maximises ESS forecasts worse than
   climatology, and exact IS (ζ = 1) is worse still. Corroborates
   [[subset-weights-do-no-work]].
6. **Stochastic reruns are decoration.** `n_ensemble_stochastic_per` 1 → 10 moves ensemble R² by
   **+0.0021** and WIS by −0.4%. A 1-rerun harness reproduces the shipped 10-rerun ensemble
   numbers to 3 decimals. The ensemble is a *parameter* ensemble.
7. **Weighted median > weighted mean** at every |B|, and the gap widens with |B| (bias 1.51 vs 1.68
   at 115; 1.97 vs 2.64 at 3,000) because the prior predictive is wildly right-skewed (member total
   cases: median 18.9k, p90 555k, max 13.3M, vs 88.8k observed). Keep `central_method = "median"`.

Not determined: generalisation beyond ETH (T2 needed); whether a proper score (training WIS/CRPS)
beats MAE; behaviour under adaptive batching (both reference runs are single-batch FIXED).

See also [[best-subset-selection-is-data-free]], [[inference-scaling-measured-v0903]],
[[likelihood-overconcentration-invariants]], and the metric traps in
[[oos-scoring-metric-traps]].
