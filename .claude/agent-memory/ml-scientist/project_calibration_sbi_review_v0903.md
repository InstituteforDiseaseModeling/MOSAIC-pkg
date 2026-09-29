---
name: calibration-sbi-review-v0903
description: Measured facts about MOSAIC's prior-draw IS calibration at v0.90.3 — 946-dim production problem, LL Monte-Carlo noise floor, why proposal refit does not fix ESS, and the free 2x budget screen
metadata:
  type: project
---

Review round 2026-09-17 (agent RESEARCH-SBI) on whether to replace prior-draw importance sampling.
Report: `/Users/johngiles/MOSAIC/MOSAIC-pkg/claude/review_inference/findings/RESEARCH-SBI.md`;
evidence scripts + output: `.../claude/review_inference/scratch/RESEARCH-SBI/` (local laptop).

**Why:** the ensemble is rank-selected top-0.1–0.5% with near-uniform (delta-AIC saturated) weights
and exact IS ESS = 1.00; the question was whether more draws help and what to replace it with.

**How to apply:** these are measured constants, not opinions — reuse them instead of re-deriving,
but re-measure after any engine/likelihood change.

### Durable measured facts (v0.90.3)

- **Production calibration is 946-dimensional** (22 global + ~23 per location x 40), NOT 45.
  45 is the single-country (ETH) case. Anything needing a full-rank target covariance is dead on
  arrival — the best subset has 115 members.
- **Per-draw cost:** ~11.8 core-sec (40 loc), ~5.5 core-sec (1 loc), n_iterations = 3.
- **Monte-Carlo noise floor of the log-likelihood at fixed theta** (fresh re-simulation, 16–20 seeds):
  SD ~3,300–7,700 nats at the top of the 40-location subset; SD 122–423 nats for ETH. Compare:
  stored rank-1→rank-2 gap is 7,039 (prod) and 244 (ETH). **The top of the best-subset ordering is
  ~1–2 SD of seed luck**, and re-measured true means actually invert production ranks 2 and 5.
- **Winner's curse:** production rank-1 stored LL is 30,417 nats (4.5 SD) better than its own
  16-seed mean. `log_mean_exp` over 3 iterations + best-of-100,000 selection compound.
  Consequence: the `pmin(delta, 4)` saturation, criticised as unprincipled, is *accidentally* a
  real noise-robustness device. Do not naively un-saturate.
- **Scaling:** best LL grows ~9,069 nats per e-fold of n (prod) / 364 (ETH), but the fixed-percentile
  subset cut is nearly flat. 10k→100k buys 15,538 nats on the best draw and 1,229 on the cut.
- **A linear screen on theta predicts bad draws before simulating**: held-out AUC 0.859 (prod, 1,267
  cols, `lm.fit`) / 0.978 (ETH, 43 primitives, `glm`). Keeping the 50% least-suspect retains 100% of
  the true top-0.1% and halves the budget. It is a *bad-draw filter only* — AUC for "in the top
  0.1%" is just 0.694 at production scale.

### Two traps that cost real effort

1. **ETH does not generalise to the 40-location model.** ETH's likelihood is sharply bimodal (48.8%
   in a −1.45e6 structural-failure plateau); the 40-location likelihood is **unimodal and smooth** —
   summing 40 locations smears the per-location take-off failure into a continuum. Any claim derived
   from the single-country artefact must be re-checked on `dugong:~/prod100k_v087/`.
2. **A proposal refit fixes draw quality but NOT the weight degeneracy.** Measured one-round
   ABC-SMC/IMIS "move" (defensive mixture, Beaumont 2x kernel, correct pi/q weights): hit rate into
   the retained band 0.5% → 3.5% (7x), catastrophic draws 48.8% → 28.1%, and 1,200 refit draws beat
   the best of 25,000 prior draws. **ESS_IS stayed at exactly 1.00 and k-hat stayed degenerate.**
   Degeneracy is driven by the likelihood's sharpness vs the proposal's width, not by the proposal's
   location. Only tempering (an SMC gamma-ladder) fixes it.

### The thing nobody had noticed

`R/run_MOSAIC_helpers.R:2045-2074` sets the Gibbs inverse temperature as
`eta = -log(weight_floor) / max_delta_aic` — with max delta-AIC = 1.38e7 that is **eta ~ 5e-7**.
MOSAIC is already doing generalized Bayes (`calc_model_weights_gibbs()` cites Bissiri/Holmes/Walker),
with a learning rate chosen by a *flatness target* rather than any statistical criterion. That is why
"adaptive-tempered ESS-all = 60,541" looks healthy — it measures how flat the weights were made.

### Missing primitive that blocks the whole SMC/IMIS family

`sample_from_prior()` exists (10 families) but there is **no prior-density counterpart anywhere in
`R/`**. Every IS-weight correction needs it (~150 lines, no new dependency). Workaround used for the
pilot: map primitives through their empirical prior CDF from the existing draws, then `qnorm` — the
prior becomes N(0,I) in z-space. Approximate (max |off-diag z-cor| = 0.467), fine for a pilot.

Related: [[project_fit_sandbox_scoring_mismatch]] (same lesson — recompute metrics yourself rather
than trusting a reported summary).
