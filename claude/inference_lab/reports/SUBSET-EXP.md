# SUBSET-EXP — how the ensemble subset is chosen and weighted, and whether it improves with n

**Agent:** SUBSET-EXP (statistician) · **Date:** 2026-09-19 · **Config:** ETH, n = 10,000
**Base code:** `inference-lab` @ `f5461f1b8` (pinned private copy, see HARNESS-01)
**Arms:** `arm/A1b_v2` @ `69aa076c5` (eps-floored NB) and the stock likelihood, both injected explicitly.
**Deliverables:** `figures/SUBSET-EXP_fig1_Bcurve.png`, `figures/SUBSET-EXP_fig1b_Bcurve_deaths.png`,
`figures/SUBSET-EXP_fig2_scaling.png`.

---

## 0. What was built, and why you can believe it

Everything below is computed on **one cached pool of 10,000 simulated ETH trajectories**. The draws
are the *same parameter vectors* the lab's `b0` block used (`sample_parameters(seed = sim_id)`,
`INFLAB_SEED_OFFSET = 0`), each run once through `run_simulation()` at the production stochastic
seed `(sim_id − 1)·3 + 1`. Every selection rule, weighting scheme and combination rule is then pure
arithmetic on that one cache, so **rules are compared on identical simulations** and the only thing
that varies is the estimator.

Three independent validations of the harness, all passed *before* any result was read:

| check | result |
|---|---|
| Reproduce the shipped `A1b2_b0` run's stored per-draw likelihood, from scratch, 20 random draws (3 reruns each, `calc_log_mean_exp` collapse) | max **relative** diff **8.1e-15**, 20/20 |
| Same for `base2_b0` (stock likelihood) | max relative diff **7.9e-15**, 20/20 |
| Reproduce the shipped **ensemble** headline numbers from the cache (|B| = 114/115, production weights, weighted median, day 31 → 2025-09-01) | `r2_cases` 0.7954 vs 0.7962 shipped; `r2_deaths` 0.3723 vs 0.3770; `bias_cases` 1.1245 vs 1.1330; `bias_deaths` 1.4798 vs 1.5254 (baseline arm equally close) |

WIS is my own implementation of Bracher et al. 2021 including the 0.5·MAE coefficient, unit-tested
against a hand-computed value (`y=10`, `q=(5,8,11,12,20)` ⇒ WIS = 0.75 exactly) plus two degenerate
cases. It is *not* the package's `.compute_wis_parametric_row()`, which scores a single trajectory
under an assumed NB; here WIS is computed from the **ensemble's own weighted quantiles**.

**Two genuinely held-out windows**, each with its own likelihood cutoff so the scored data was never
seen by the weights *or* by any selection statistic:

| window | dates | obs cases | obs deaths | likelihood cutoff used |
|---|---|---|---|---|
| `valid` | 2025-03-02 → 2025-09-01 (184 d) | 5,696 | 35 | C1 = 2025-03-01 |
| `test` | 2025-09-02 → 2026-03-01 (181 d) | 1,562 | 21 | C2 = 2025-09-01 (the lab's cutoff) |

**Skill is reported against seasonal climatology** (day-of-year mean of the training window), because
two degenerate forecasts otherwise win silently: on `test` the *all-zero* forecast has MAE 8.63 vs
climatology's 16.52, and the random-subset ensemble **is** the all-zero forecast for |B| ≥ 115
(measured bias exactly 0.000). `valid` is the sound window — there the all-zero forecast is bad
(MAE 30.96 vs 15.06 for the training mean). Where the two windows disagree I lead with `valid`.

---

## SUBSET-EXP-01 — **Does a larger |B| predict better? No.** (SCALE's open question, answered) — HIGH

`figures/SUBSET-EXP_fig1_Bcurve.png`. Top-|B| by likelihood, uniform weights, weighted median,
scored out of sample. **WIS skill vs seasonal climatology** (higher better; 0 = climatology):

| \|B\| | valid, shipped LL (A1b) | valid, shipped LL (base) | test, shipped LL (A1b) | random null (valid) |
|---|---|---|---|---|
| 5 | −0.015 | −0.338 | 0.672 | −0.244 |
| 25 | 0.086 | 0.075 | 0.716 | −0.392 |
| 50 | **0.154** | −0.013 | 0.694 | −0.206 |
| **115 (shipped)** | **0.121** | −0.074 | **0.673** | −0.111 |
| 250 | 0.131 | −0.239 | 0.661 | −0.206 |
| 500 | 0.116 | −0.476 | 0.598 | −0.174 |
| 1,000 | 0.038 | −0.680 | 0.463 | −0.179 |
| 2,000 | −0.016 | −0.935 | 0.277 | −0.202 |
| 5,000 | −1.071 | −1.084 | −0.537 | −0.193 |
| 10,000 | −0.194 | −0.194 | 0.382 | — |

**|B| has an interior optimum at 25–250, and the shipped 115 sits inside it.** Beyond ~500, held-out
skill falls monotonically and held-out cases bias rises monotonically (1.44 → 1.65 → 1.86 at
|B| = 115 → 1,000 → 3,000 on `valid`; 1.88 → 2.81 → 4.00 on `test`). At |B| = n the weighted median
**collapses to an all-zero forecast**: 51% of prior draws predict exactly 0 cases on the median day
(34.7% produce < 1% of the observed total), so the across-member median sits on the 0/non-0 boundary.

So the "take a bigger best subset" lever is **not available** under the current ranking. There is a
real mechanism — the likelihood's job here is to *exclude the dead half of the prior*, and past
|B| ≈ 250 it starts re-admitting over-predictors — and it is the ranking, not |B|, that binds.

Two corollaries worth recording:

- **Correlation-R² is the wrong metric for this question and points the opposite way.** On `test`,
  `r2_corr` rises monotonically with |B| (0.554 at 115 → 0.735 at 5,000) while bias goes 1.9 → 5.0
  and WIS goes 5.5 → 25.6. R² is scale-invariant, so it rewards a subset that tracks the shape while
  over-predicting the level fivefold. The random-subset null also scores `r2_corr` ≈ 0.56 at
  |B| = 115 *while forecasting zero*. Any |B| decision taken on R² will be wrong.
- **The A1b arm's likelihood is a genuinely better ranking**, out of sample: at |B| = 115, valid
  skill +0.121 (A1b) vs −0.074 (stock); test 0.673 vs 0.358. The two arms' top-115 sets overlap by
  only **34/115**.

---

## SUBSET-EXP-02 — **The ranking statistic is worth 3.7–7× more than the subset size** — HIGH

Leak-free (each evaluation window has its own cutoff for *both* the likelihood and every selection
statistic). |B| = 115, uniform weights, weighted median, A1b arm:

**`valid` window, cases** (climatology WIS = 16.84):

| selection rule | WIS skill | WIS | bias | cov50 | cov95 |
|---|---|---|---|---|---|
| top-115 by **train MAE**, cases+deaths normalised (`lm_mae_cd`) | **0.424** | 9.70 | **0.986** | 0.386 | 0.924 |
| top-115 by **train MAE**, cases only (`lm_mae_c`) | 0.380 | 10.44 | 1.059 | 0.348 | 0.810 |
| top-115 by \|log bias\| (`lm_absl`) | 0.340 | 11.12 | 1.213 | 0.228 | 0.766 |
| systematic resampling from `w ∝ exp(ζ·ℓ)` | 0.064 ± 0.023 | 15.75 | 1.432 | 0.253 | 0.585 |
| **top-115 by log-likelihood (SHIPPED)** | **0.062** | 15.80 | 1.511 | 0.272 | 0.647 |
| random 115 (null) | −0.234 ± 0.086 | 20.78 | 0.110 | 0.920 | 1.000 |

**`test` window, cases** (climatology WIS = 16.52): `lm_mae_cd` 0.772, `lm_mae_c` 0.754, systematic
0.698 ± 0.008, **top-K 0.669**, random 0.339 ± 0.072.
**`test` window, deaths**: `lm_mae_c` 0.696 with bias **1.10**, vs top-K 0.594 with bias **2.19**.

Replacing the NB log-likelihood with a **normalised training-window mean absolute error** — computed
on exactly the same data, from the same trajectories — cuts held-out WIS by **39%** on `valid` and
**26%** on `test`, and brings held-out cases bias from 1.51 to 0.99 and deaths bias from 2.19 to 1.10.
Interval calibration improves too (cov95 0.65 → 0.92 on `valid`; nominal 0.95).

A log-likelihood pre-screen to the top M = 30·|B| makes essentially no difference (0.424 vs 0.422
with no screen at all), so the gain is entirely attributable to the **criterion**, not to a two-stage
structure. Three negative controls behaved correctly: random subsets lose decisively;
selecting by in-sample **R²** is catastrophic (skill −0.76 to −1.9, bias 2.5–10.0 — it selects
shape-correlated over-predictors); and selecting by `|bias − 1|` inherits that statistic's asymmetry.

**The likelihood ranks the wrong thing.** Measured over all 10,000 draws: Spearman(logL, per-member
**in-sample** R²) = **+0.65**, Spearman(logL, per-member **held-out** R²) = **+0.08** (`test`) and
**+0.05** (`valid`). *Within* the production subset (top 1.15%) it is **−0.08** — i.e. inside B,
higher likelihood predicts, if anything, slightly worse held-out shape.

---

## SUBSET-EXP-03 — the lab's bias-signal statistic is an artefact of `|bias − 1|` — MEDIUM

The lab reported Spearman(LL, |bias−1|) = −0.090 (baseline) → −0.510 (A1b) and read it as "A1b made
the likelihood 5× more informative about bias". Measured on all 10,000 draws, on the `test` window:

| statistic | baseline LL | A1b LL |
|---|---|---|
| Spearman(LL, **\|log bias\|**) | **−0.788** | **−0.841** |
| Spearman(LL, **\|bias − 1\|**) | +0.573 | +0.440 |

`|bias − 1|` bounds under-prediction at 1 while letting over-prediction run to ∞, so a **dead draw
(bias = 0) scores as nearly perfect**. With ~50% of the prior dead, the statistic mostly counts how
many dead draws an arm promotes. On the symmetric `|log bias|`, **both** arms rank bias strongly and
the A1b advantage is real but modest (−0.84 vs −0.79), not 5×. The reviewer's prediction that
"rules can exploit the bias signal" is **confirmed** — but the exploitable quantity is a direct
training-window level statistic, not the likelihood's implicit bias ordering.

The reviewer's other prediction — "little or no R² gain is available" — is **partly falsified**. On
`test`, cases R² goes 0.551 (top-K) → 0.643 (`lm_mae_c`) → 0.665 (`lm_mae_cd`), i.e. +0.09 to +0.11.
On `valid` it is ~0 under every rule, because that window carries no usable daily shape signal at
all (median *per-member* R² is 0.02–0.03 there, vs 0.47–0.52 on `test`).

---

## SUBSET-EXP-04 — fixed-ζ fractional posterior: ESS scales, **predictive skill does not** — HIGH

`w ∝ exp(ζ·ℓ)` over all 10,000 retained draws, ζ fixed (never solved from an ESS target), weighted
median. A1b arm, cases:

| ζ | Kish ESS | valid skill | test skill | valid bias |
|---|---|---|---|---|
| 0 (uniform) | 10,000 | −0.194 | 0.382 | 0.000 |
| 1e-5 | 6,729 | −0.558 | −0.088 | 1.73 |
| 1e-4 | 3,737 | −0.431 | −0.045 | 2.03 |
| 1e-3 | 262 | 0.053 | 0.623 | 1.56 |
| **3.2e-3** | **19.0 / 21.8** | **0.061** | **0.697** | 1.44 |
| 1e-2 | 2.0 | −0.156 | 0.614 | 1.52 |
| 1 (exact IS) | 1.0 | −0.393 | 0.461 | 1.52 |
| *shipped top-115 + saturated weights* | 87.4 | 0.121 | 0.672 | 1.90 |

The best fixed ζ is a **tie** with the shipped rule (valid 0.061 vs 0.121; test 0.697 vs 0.672), and
it achieves that tie by driving the **effective sample size down to ~20**. The reviewers' measurement
that ESS becomes Θ(n) under fixed ζ is correct, but the ζ that is good *predictively* is the one that
pins the effective subset at the same ~20–260 members the current rule already uses. **B1 does not
buy predictive skill.** The ζ that maximises ESS destroys the forecast (ζ ≤ 1e-4 is worse than
climatology on both windows); exact importance weights (ζ = 1) are worse still.

The ζ optimum was chosen on `valid` (cutoff C1) and confirmed on `test` (cutoff C2) — the same value,
3.2e-3, wins on both, so this is not a tuned-on-the-test-set number.

---

## SUBSET-EXP-05 — reruns buy nothing; members are the whole ensemble — MEDIUM

Production runs `n_ensemble_stochastic_per = 10` reruns of each of ~114 members (1,140 trajectories).
Trading members against reruns at matched trajectory count (A1b arm, scored window = day 31 → C2):

| configuration | trajectories | r2_cases | bias_cases | WIS |
|---|---|---|---|---|
| 114 members × 1 rerun | 114 | 0.7954 | 1.125 | 7.580 |
| 114 members × 2 | 228 | 0.7964 | 1.122 | 7.570 |
| 114 members × 10 (**production**) | 1,140 | 0.7975 | 1.121 | 7.549 |
| 228 members × 1 | 228 | 0.7971 | 1.131 | 7.836 |
| 1,140 members × 1 | 1,140 | 0.7726 | 1.210 | 10.603 |

Going from 1 rerun to 10 moves R² by **+0.0021** and WIS by **−0.4%**. The ensemble is a parameter
ensemble; the stochastic reruns are decoration. (This is also why the 1-rerun harness reproduces the
shipped 10-rerun numbers to 3 decimals.) **`n_ensemble_stochastic_per` is a 10× compute knob with a
0.3% effect** — it can be cut to 2–3 with no measurable loss, though it is not free: it is what the
medoid/CI machinery consumes elsewhere, so this is a measurement, not yet a recommendation.

---

## SUBSET-EXP-06 — median vs mean: the median wins on level, and WIS cannot see the difference — LOW

Weighted median vs weighted mean at matched members and weights (`valid`, cases, A1b):

| \|B\| | bias, median | bias, mean | MAE, median | MAE, mean |
|---|---|---|---|---|
| 50 | 1.47 | 1.56 | 25.2 | 26.7 |
| 115 | 1.51 | 1.68 | 25.6 | 29.4 |
| 500 | 1.65 | 1.94 | 27.9 | 34.7 |
| 3,000 | 1.97 | 2.64 | 34.8 | 53.0 |

The median is better on bias and MAE at every |B|, and the gap **widens with |B|** — the prior
predictive is so right-skewed (member total cases: median 18.9k, p90 555k, max 13.3M against 88.8k
observed) that the mean is dominated by a handful of explosive members. WIS is identical for the two
because it is computed from the same quantiles. **Keep `central_method = "median"`** — this
corroborates the Forecast-Hub result and the existing MOSAIC default.

---

## SUBSET-EXP-07 — **Does calibration improve with n? Only its reproducibility does.** — HIGH

`figures/SUBSET-EXP_fig2_scaling.png`. Disjoint sub-pools of the same 10,000 iid draws
(40/20/10/4/2/1 blocks at n = 250/500/1,000/2,500/5,000/10,000), each rule applied inside each
sub-pool, scored out of sample. Mean **WIS skill** on `valid`, cases:

| n | SHIPPED top-115 by logL (A1b) | logL→train-MAE, \|B\|=115 | train-MAE, \|B\|=1.15·√n | random 115 |
|---|---|---|---|---|
| 250 | −0.968 | −0.158 | **0.414** | −0.185 |
| 500 | −0.157 | 0.095 | **0.431** | −0.141 |
| 1,000 | −0.077 | 0.393 | **0.432** | −0.124 |
| 2,500 | 0.022 | 0.449 | 0.432 | −0.178 |
| 5,000 | 0.033 | 0.435 | 0.426 | −0.136 |
| 10,000 | 0.062 | 0.424 | 0.422 | −0.196 |

**Measured scaling exponents** (OLS of log WIS on log n; WIS ∝ n^b, more negative = improves faster):

| rule | window | b over 250–10,000 | **b over n ≥ 1,000** | b of the across-block SD |
|---|---|---|---|---|
| SHIPPED top-115 by logL (A1b) | valid | −0.295 ± 0.028 | **−0.069 ± 0.021** | −0.68 |
| SHIPPED top-115 by logL (A1b) | test | −0.515 ± 0.022 | **−0.313 ± 0.034** | −0.80 |
| logL screen → train MAE, \|B\|=115 | valid | −0.301 ± 0.017 | −0.042 ± 0.026 | −1.13 |
| logL screen → train MAE, \|B\|=115 | test | −0.186 ± 0.020 | +0.109 ± 0.015 | −0.72 |
| train MAE, \|B\| = 1.15·√n | valid | −0.011 ± 0.008 | +0.006 ± 0.008 | −0.50 |
| train MAE, \|B\| = 1.15·√n | test | +0.027 ± 0.022 | −0.012 ± 0.036 | −0.31 |
| random 115 (**null**) | valid | −0.013 ± 0.016 | +0.028 ± 0.048 | −0.04 |
| random 115 (**null**) | test | −0.014 ± 0.023 | +0.046 ± 0.066 | −0.19 |

Read this carefully:

1. **The shipped rule does improve with n, and the improvement is almost entirely in the pathological
   small-n regime.** At n = 250 it is selecting 46% of the pool and forecasting worse than the random
   null; by n = 2,500 it has merely reached climatology. Past n = 1,000 the exponent is −0.07
   (`valid`): **a 10× increase in n buys a 15% WIS reduction**, and on `valid` that still leaves it
   7× below the alternative.
2. **A better ranking reaches the ceiling at n = 250.** `|B| = 1.15√n` by training MAE scores 0.414
   at n = 250 and 0.422 at n = 10,000 — exponent indistinguishable from zero on both windows. The
   *entire* 40× compute increase is worth +0.008 skill under that rule; the rule change is worth
   +0.36. **The ranking is worth ~45× what the extra simulations are worth.**
3. **What genuinely improves with n is reproducibility.** The across-sub-pool SD of held-out WIS
   falls as n^−0.3 to n^−1.1 for every non-null rule and is flat for the null (−0.04). This is the
   quantity behind the lab's draw-block instability (R²_cases 0.797 / 0.019 / 0.371 across three
   10,000-draw blocks) — and the proposed rule shrinks it by a further 4–8× at matched n
   (SD of skill at n = 1,000: 0.057 vs 0.074 shipped on `valid`; 0.007 vs 0.065 on `test`).
4. The null shows **no** scaling on either window, which is the control this measurement needed.

Paired comparison on identical draws (proposed `logL→MAE` minus shipped, ΔWIS, negative = better),
`valid` cases: −13.6 (n=250, t=−15.1, 40 blocks), −4.2 (n=500, t=−12.3), −7.9 (n=1,000, t=−32.1),
−7.2 (n=2,500, t=−12.8), −6.8 (n=5,000, t=−25.0), −6.1 (n=10,000, 1 block). `test` cases: −19.1
(t=−36.7) → −1.7. Every n clears the protocol's 2×SD gate by an order of magnitude.

---

## SUBSET-EXP-08 — a weak ceiling on what member selection can buy — MEDIUM

Selecting the |B| members that individually score best **on the held-out window itself** (an oracle,
never a usable rule): `valid` cases skill 0.186 at |B| = 115 — *below* the 0.424 achieved by the
honest training-MAE rule. The individually-best members do not form the best ensemble, so this is a
weak bound, not a real ceiling; but it does say that a substantially larger gain from member
selection alone is not obviously sitting there. The remaining error is model error: even the top
likelihood decile over-predicts the `test` window by **3.3×** at member level, because the model does
not produce the observed decline.

---

## HARNESS-01 — the shared lab worktree changed branch mid-run (must go in `STATE.md`) — HIGH

At 13:10 on 2026-09-19 a sibling agent checked out `arm/A1b_v2` in
`…/scratchpad/lab`, the worktree `CONTEXT_TRACKS.md` designates READ-ONLY. Consequences measured:

- `score_ll.R base`, run at 13:34 with **no** likelihood injection, silently produced **A1b**
  values: `ll_base.rds` was byte-identical to `ll_A1b.rds` (max abs diff 0). Caught because the
  baseline and A1b rows of an unrelated table agreed to 5 decimal places, which is impossible
  (sim 358 scores −1,403,318 under the stock penalty and −179,354 under the eps floor).
- Everything that consumed `ll_base.rds` before 14:04 is **void**: the `base_C1`/`base_C2` slices of
  E1, E3 and E5. They were dropped or recomputed; no reported number depends on them. The
  `base_prod` rows (read from the shipped run's `samples.parquet`) were never affected.
- **The trajectory pool is unaffected and this was verified, not assumed.** `inference-lab` and
  `arm/A1b_v2` differ in exactly two files (`DESCRIPTION`, `R/calc_log_likelihood_distributions.R`),
  so the engine is identical; re-simulating sims 1/250/1,200/4,800/7,200/9,999 under the pinned copy
  reproduced the cache with **max abs diff 0** in both channels.

Two fixes are now in this harness and should be adopted lab-wide: (i) analysis runs load a
**private `git archive` copy** of the worktree at a named commit, never the shared checkout;
(ii) the arm's likelihood is **injected explicitly for every arm, including the control**, from a
file extracted with `git show`, with a signature assertion (`eps_j` present / `log(1e6)` absent).
A control arm that relies on "the default code" is not a control when the default code can move.

---

## RECOMMENDATIONS (ranked by expected gain / cost)

**R1 — Replace the ranking statistic, not the subset size. (largest gain, small change)**
Rank retained draws by a normalised training-window absolute error computed from the trajectories
the pipeline already simulates:

```
s_i = MAE_cases(i) / mean(obs_cases)            # SAFE variant (recommended default)
s_i = MAE_cases(i)/mean(obs_cases) + MAE_deaths(i)/mean(obs_deaths)   # best-scoring variant
```

take the top |B| (uniform weights), and keep the weighted median.
**Buys, at |B| = 115, n = 10,000, ETH, leak-free:** held-out WIS −26% to −39%; cases bias
1.51 → 0.99–1.06 (`valid`) and 1.92 → 1.60 (`test`); deaths bias 2.19 → 1.10 (`test`), 3.46 → 0.77
(`valid`); test cases R² 0.551 → 0.643; 95% coverage 0.65 → 0.81–0.92. Paired across 40/20/10/4/2
disjoint sub-pools, t = −6 to −37 at every n.
**Cost:** ~15 lines in `run_MOSAIC.R`'s subset block; the statistic is already computable from the
scored arrays. No new simulations.
**Guardrail (measured, not hypothetical):** the cases+deaths variant selects zero-deaths members on
sparse-deaths countries — on ETH's `valid` window it drives deaths bias to exactly 0.000. Default to
the cases-only variant, and floor any per-channel denominator; a channel whose observed mean is
below ~1/day should not enter the criterion.
**Keep the likelihood in the loop** as a pre-screen to the top M = 30·|B| — it costs nothing
(Δskill < 0.003) and prevents the criterion from being minimised by a pathological member.

**R2 — Leave |B| alone under the current rule; raise it only if R1 ships. (free)**
|B| = 115 is at the interior optimum of the *current* rule (25–250 on both windows). It is not a
universal constant: under R1 the optimum moves to |B| ≈ 500–1,000 (valid skill 0.424 → 0.455 at
|B| = 500; test 0.772 → 0.817 at |B| = 1,000). If R1 ships, set |B| = max(115, round(1.15·√n)) —
that reaches the ceiling at every n tested, including n = 250.
**Do not** make |B| a fixed fraction of n: q = 10% and q = 30% are both worse than q = 1.15% at every
n, and |B| → n is an all-zero forecast.

**R3 — Do not adopt B1 (fixed-ζ fractional posterior) as a skill improvement. (saves work)**
Best fixed ζ ties the shipped rule on both windows while pinning Kish ESS at ~20. It is a legitimate
*reporting* fix (it makes the weights an honest tempered posterior) but not a predictive one, and
choosing ζ to maximise ESS makes the forecast worse than climatology. Exact IS weights (ζ = 1) are
worse still, corroborating the earlier ablation.

**R4 — Stop quoting correlation-R² as the ensemble's headline. (free, high leverage)**
On `test`, R² rises from 0.55 to 0.74 while bias rises 1.9 → 5.0; the random-subset null scores
R² ≈ 0.56 *while forecasting zero*. Report WIS and the bias ratio, with a climatology skill score,
and keep R² as a secondary shape diagnostic. Every |B| or subset decision taken on R² will be wrong.

**R5 — `n_ensemble_stochastic_per = 10` is a 10× cost for a 0.3% effect.** Measure what the medoid
and CI paths actually need before cutting it, but it should not be 10 for the sake of the ensemble
central line.

**R6 — Escalate to the lab: for ETH, more than ~1,000–2,500 simulations does not improve the mean
forecast.** Past n = 1,000 the shipped rule's exponent is −0.07 (`valid`) / −0.31 (`test`); the
proposed rule's is ~0. The return on n is in **run-to-run reproducibility** (SD ∝ n^−0.3…n^−1.1), which
is worth having but should be budgeted as such.

**Next step for the lab (T2):** R1 is a single-country result. Before promotion it needs the T2 ladder
(MOZ, COD, NGA — COD is ψ-saturated, the southern belt is not) with the same leak-free two-window
design, and it needs the sparse-deaths guardrail exercised on a country with real death counts.

---

## WHAT I COULD NOT DETERMINE

- **Whether R1 generalises beyond ETH.** Everything here is one country. The within-ETH replication
  is strong (paired across up to 40 disjoint sub-pools, t = −6 to −37), but a criterion that works on
  a country whose model over-predicts by ~2× may behave differently where the bias is the other way.
- **Whether a proper probabilistic training criterion beats MAE.** I tested MAE, |log bias| and
  in-sample R². A per-member training WIS or CRPS (which needs a within-member predictive
  distribution) was not tested and is the obvious next candidate — MAE is minimised by zero on
  sparse channels, and a proper score would not be.
- **The deaths channel at ETH is too thin to decide anything out of sample** — 35 observed deaths in
  `valid`, 21 in `test`. The deaths numbers above are directionally consistent across both windows
  but each rests on ~20–35 events.
- **Whether the optimum |B| under R1 keeps growing past 1,000 at larger n.** My pool is 10,000, and
  at |B| ≥ 2,000 the median degenerates regardless of ranking. Testing |B| = 2,000–5,000 at n = 10⁵
  needs a bigger pool than I simulated.
- **The interaction with the adaptive-batch sampler.** Both reference runs are single-batch FIXED,
  i.e. i.i.d. from one prior. Under adaptive batching the draws are not exchangeable and the disjoint
  sub-pool design used for the scaling exponents does not directly apply.
- **Whether `n_ensemble_stochastic_per` can actually be cut**, because I did not audit what the
  medoid selection, the R_eff resim-CI path and `trajectories_ensemble.rds` require.

---

### Reproduction

Scripts and cached results (local laptop):
`/private/tmp/claude-501/-Users-johngiles-MOSAIC-MOSAIC-pkg/c26a96f4-20ef-4a2a-8ee4-36b6e863f4c7/scratchpad/subsetexp/`
— `sim_pool.R` (the 10,000-draw trajectory cache, ~25 min on 8 local cores), `score_ll.R`
(per-draw likelihoods under either arm, explicit injection), `lib.R` (WIS + weighted-quantile
ensemble + scoring), `exp0/0b` (discrimination + degenerate baselines), `exp1` (|B| curve + null),
`exp2b` (leak-free selection rules), `exp3` (ζ grid), `exp4` (members × reruns), `exp6` (oracle),
`exp8` (rule × |B|), `exp9` (scaling), `check_prod.R` (reproduces the shipped run),
`verify_pool.R` (pool integrity under the pinned package).
Shipped-run artefacts pulled from `dugong:~/inflab/{A1b2_b0,base2_b0}_ETH_n10000_s0/`.
