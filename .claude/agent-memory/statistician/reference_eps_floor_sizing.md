---
name: eps-floor-sizing
description: The eps floor on the predicted NB mean is a per-channel tuning constant (cases 0.02, deaths 0.25 of mean(obs)); how it was sized, what it cannot fix, and the traps in measuring it
metadata:
  type: reference
---

# The eps floor on the predicted mean is a per-channel tuning constant

`calc_log_likelihood_negbin/_poisson()` evaluate every cell at
`mu = max(est, max(1e-4, eps_rel * mean(observed)))`. `eps_rel` is a knob, not a
detail: it sets the price of a zero prediction against a positive observation,
and production scores ONE stochastic realisation per draw, so a low-count channel
is full of exactly those cells.

**Measured (2026-09-23, v0.93.0+, 4 countries, real 365-day holdout, per-block
paired, `mu_j_baseline`-scale profile).** Deaths bias at the likelihood optimum,
pooled median over 48 blocks:

| eps_rel_deaths | 0.02 | 0.10 | 0.20 | **0.25** | 0.30 | 0.40 | 0.50 |
|---|---|---|---|---|---|---|---|
| in-sample | 2.34 | 1.74 | 1.12 | **0.97** | 0.87 | 0.50 | 0.35 |
| held-out  | 1.43 | 1.39 | 1.20 | **1.11** | 1.10 | 0.98 | 0.80 |

- **0.5 is PAST the crossing** (in-sample bias 0.35, a 3x *under*-prediction).
  An earlier report (`claude/cfr_review/04_identifiability.md` §6 R1) recommended
  0.5 from a coarser 7-point ETH-only grid; the multi-country sweep corrects it.
- Selection rule: minimise `|log bias_in| + |log bias_out|`. `|bias - 1|` is
  asymmetric and a dead forecast wins it (see [[oos-scoring-metric-traps]]).
- Per-country in-sample crossings: MOZ 0.127, COD 0.274, ETH 0.341.

## Three things the eps floor does NOT do

1. **It is inert on a very sparse channel.** KEN (0.07 deaths/day, every non-zero
   day equal to 1): the deaths bias is IDENTICAL at every eps from 0.02 to 0.50,
   because the floor never binds against integer-rounded `reported_deaths`.
   Replicate-averaging *does* move KEN (0.31 -> 0.98). So for the sparsest
   countries the Jensen gap is mediated by integer granularity, not the floor.
2. **It is a two-sided distortion.** Raising eps makes a zero-vs-positive cell
   cheaper (pushes the optimum down) but also makes a *correctly predicted* zero
   more expensive (`dnbinom(0 | mu = eps) < 0`, pushes it up). The net is
   downward, but this is why it is a tuned constant and not a principled fix.
3. **`mo = mean(observed)` is computed over ALL non-NA cells, including cells
   whose weight is 0** (a holdout mask, or the deaths prefix before
   `deaths_score_start`). At COD the eps denominator is 1.53x the fit-window
   mean. Not a bug in production (no holdout there) but it leaks in any
   holdout experiment, and it means eps is not sized on the effectively-scored
   subset.

## Cross-check: replicate-averaging agrees

Scoring `rowMeans` of n realisations at the UNCHANGED 0.02 floor: pooled
in-sample deaths bias 2.51 (n=1) -> 1.50 (n=3) -> 1.20 (n=6) -> 1.10 (n=24).
Same landing point as eps=0.25. Two independent routes, one answer -- which is
what the Jensen diagnosis predicts. Residual: the replicate route plateaus at
~1.1-1.2, not exactly 1 (MOZ stays at 1.6), so ~10-20% of the deaths bias is
NOT Jensen.

## Measurement traps found while doing this

- **Raw prior draws are mostly useless for a profile.** First pass: 18 of 36
  blocks had a grid-edge argmax because the draw's epidemic was dead; the pooled
  median bias came out as 0.00 and the crossing was NA for 3 of 4 countries. Fix:
  accept a base draw only if simulated CASES are within 3x of observed and
  deaths > 0 (filter on cases so the deaths level is not pre-selected), then
  CENTRE the g grid on that draw's own bias so it brackets 1.
- **eps arms need no interleaving** because eps is a pure post-hoc re-scoring of
  a cached simulation pool -- the pairing is exact by construction. Use this
  wherever possible; it removes the Lesson-17 timing-drift problem entirely.
- "Non-edge blocks only" is a *selection effect*: the number of interior optima
  varies with eps (22 to 31 of 36), so filtering on it biases the crossing.
  Prefer reporting the edge count over filtering.

See also [[likelihood-zero-penalty-dominates]] (the retired `-y*log(1e6)` rule
this floor replaced) and [[cfr-block-identifiability]].
