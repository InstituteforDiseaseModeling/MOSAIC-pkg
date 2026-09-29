---
name: ab-endpoint-noise-and-sizing
description: Measured noise on MOSAIC's downstream forecast-CV endpoints (within-country SD of log WIS ~1.2, of wis_skill ~0.61) and the resulting MDE table; plus why exact IS ESS=1.00 can never license an n_iter cut
metadata:
  type: reference
---

**The invariant:** any downstream MOSAIC A/B must be sized against a MEASURED same-arm
replicate floor, and `n_iter` must be chosen from that floor — never from ESS.

**Measured endpoint noise** (`claude/forecast_cv_ocv4_q2yr/scores_cells.parquet`, OCV-4:
4 countries x 9 quarterly cutoffs, cases channel, window `OOS<=3mo`, model `ensemble`):

| quantity | COD | ETH | MOZ | NGA | use |
|---|---|---|---|---|---|
| within-country SD of `log(wis)` across cutoffs | 1.18 | 0.96 | 1.53 | 1.01 | sigma_cell ~ **1.17** |
| within-country SD of `wis_skill_persistence` | 0.55 | 0.58 | 0.47 | 0.84 | sigma_cell ~ **0.61** |
| cell SD of `R2_corr` | — | — | — | — | **0.29** |
| cell SD of `log(bias_ratio)` | — | — | — | — | **1.19** |

**MDE** (80% power, two-sided 0.05) on `log WIS`, paired by (country x cutoff):
`MDE = 2.802 * sigma_cell * sqrt(2(1-rho)) / sqrt(N)`.

| rho (paired corr) | N=5 | N=45 (5c x 9) | N=144 (16c x 9) | N=360 (40c x 9) |
|---|---|---|---|---|
| 0.90 | 0.66 | 0.22 | 0.12 | 0.077 |
| 0.95 | 0.46 | 0.16 | 0.086 | 0.055 |
| 0.99 | 0.21 | 0.069 | 0.039 | 0.024 |

Multiply by `sqrt(1 + (m-1)*ICC)` for clustering (9 cutoffs/country, ICC ~0.3 -> x1.84).
Common random numbers (same prior draws, same seeds, only the swapped input differs) is the
only lever that buys rho ~ 0.99 and is therefore worth more than any extra compute.

**Why ESS cannot size n.** `project_inference_scaling_measured_v0903` item 3: exact IS ESS is
pinned at **1.00 at every n from 500 to 100,000** because `log w = LL + const` and
`sd(LL) = 4.2e5`. A statistic that is constant over the entire candidate range carries **zero
information** about that range; "ESS is 1.00 at both 10k and 100k, so use 10k" is the same
argument as "use 500". It is not an argument. Likewise `|B| ~ 1.14 x ESS_best` and
`ESS_B = 107.475` are closed forms in the control, not data.

What *does* bear on n, for an A/B specifically: item 5 — `W1(post_5k, post_100k) = 0.158`
against a two-random-subsets null of 0.146 [0.119, 0.177], i.e. the 5k/100k posterior
difference is ~92% **resampling noise**. That resampling noise IS the A/B's error term. And
run-to-run SD falls as `n^-0.3..-1.1` (`subset-ranking-beats-subset-size`), so a 10x cut in n
inflates the A/B's error SD by **2.0x to 12.6x** and the required cell count by 4x to 160x.
Saturation of the posterior MEAN (n ~ 7-10k) is the wrong criterion for a difference test.

**How to apply:**
1. Pre-register a minimal effect of interest `delta`.
2. Run >=3 same-arm replicate calibrations (identical psi/config, different calibration seed)
   and measure `sigma_rep(n)` on the primary endpoint. Choose the smallest n with
   `sigma_rep(n) <= delta/3`. If no feasible n clears it, the experiment is not runnable —
   say so rather than running it.
3. At fixed compute `C = n x cells`, `MDE ∝ sqrt(n[sigma_between^2 + sigma_rep(n)^2]/C)`, so an
   interior optimum in n exists only if `sigma_rep ∝ n^-beta` with beta > 0.5. The measured
   range spans 0.3-1.1, i.e. both regimes — so the trade must be measured, not assumed.
4. Score ONE pre-registered horizon bucket as primary. The `OOS<=1/2/3mo` buckets are
   **cumulative-nested** ([[forecast-cv-horizon-scoring]]); treating them as three replicates
   is a 3x fake-n inflation.

Related: [[inference-scaling-measured-v0903]], [[subset-ranking-beats-subset-size]],
[[forecast-cv-horizon-scoring]], [[oos-scoring-metric-traps]],
[[psi-star-absorbs-stage1-psi-gains]].
