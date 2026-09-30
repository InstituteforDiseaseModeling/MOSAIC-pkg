---
name: weighting-scheme-invariants
description: Durable invariants of MOSAIC's five weighting schemes (v0.90.3) — the "tempered" switch is 86x SHARPER not softer, the saturated weights have only 2 distinct values, A/CVw are exact functions of ESS, param_ess inflates a point mass by n/n_grid, and ESS-targeting is a free drop-in fix
metadata:
  type: reference
---

Measured 2026-09-17 (agent WEIGHT, inference-methods review) on
`/Users/johngiles/MOSAIC/output/eth25k_v0903/` plus a controlled bias/variance study calibrated to
that regime. Scripts: `MOSAIC-pkg/claude/review_inference/scratch/WEIGHT/0*.R`-`2*.R`.
Report: `.../review_inference/findings/WEIGHT.md`.

## The trap: `best_subset_weighting = "tempered"` is 86x SHARPER, not softer

Its doc strings (`R/run_MOSAIC.R:3612-3616`, `05-model-calibration.Rmd` ~L245) say it "preserves
more of the likelihood ordering". On the real ETH 115-draw subset:

| scheme | ESS_perp | max/min w | mass on best draw |
|---|---|---|---|
| `"saturated"` (default) | **107.48** | 7.39 | 6.09% |
| `"tempered"` (switch) | **1.25** | 1e15 | **96.08%** |

Mechanism: `eta = -log(1e-15)/max(Delta)`, and **within the subset** max(Delta) = 4,215, not the
all-draws 2.88e6 — so eta is 680x larger and the 1e-15 floor lands on the subset's own worst member
by construction. Flipping the switch ships a point-mass posterior.
Corrected in [[inference-scaling-measured-v0903]] item 7, which recommended flipping it while
actually having measured ESS-TARGETED tempering (a different, unreachable-from-config estimator).

## The saturated posterior has exactly TWO distinct weights

Real ETH subset, n=115: **114 of 115 (99.1%) share one value**; only 1 draw has `Delta <= 4`.
Spearman(w, logL) = **0.1608**, Kendall 0.1319. The 114 tied draws span **1,863 log-lik units**
(ratio `e^1863` = numerically Inf) and are weighted identically.

**The free fix:** solve `eta` by bisection so `ESS_perplexity(exp(-eta*Delta)) = target`. At the
*exact* ESS the pipeline already reports (107.4751): **Spearman = Kendall = 1.0000**, 115 distinct
weights, max/min = 4.17 (LESS spread than saturated), posterior median moves **0.033 prior SD**,
median 95% CI width 3.613 vs 3.626 prior SD. Strictly better, ~15 lines, no dependency.

## Sample-invariance (coherence) fails — the schemes are not posteriors

A posterior requires `w_i/w_j` to depend only on draws i and j. Saturated: draws
A=-100, B=-102, C=-110 give `w_A/w_B = 7.3891` (correct); **add a better draw D=-90 and it becomes
1.0000**. Because `min(Delta, 4)` puts the sample max inside a non-linear function, the shift no
longer cancels. Exact IS gives 7.3891 both times. Bissiri et al. (2016) derive the Gibbs form FROM
a coherence requirement and need a loss that is a fixed function of `(theta, data)` — so the spec's
Gibbs-posterior claim (`05-model-calibration.Rmd` ~L289-296) is false for the saturated loss, and
contradicts its own "deliberate regularisation, not importance sampling" at ~L237.

## A and CVw are EXACT functions of ESS — the 3-criterion gate is 1 criterion

```
CVw = sqrt(B/ESS_kish - 1)        max |err| = 2.4e-15
A   = log(ESS_perplexity)/log(B)  max |err| = 0.0e+00   (exact)
```
And with weights capped in `[e^-2, 1]` (both the saturated scheme AND `grid_search_best_subset`'s
`eta = 0.5*(4/range)`), `min A = 0.860 / 0.900 / 0.931` at B = 30 / 115 / 1000 versus a target of
0.70 — **A is mathematically incapable of failing**; CVw's worst case is 1.175 vs target 1.0 and
needs an extremal two-point configuration. So tiers 1-20 of the 30-tier ladder are functionally
identical and the search reduces to `B >= ~115`.
(Extremal bounds `min ESS_kish/B = 0.41997`, `min ESS_perp/B = 0.62221` — the spec's 0.42/0.62 are
correct.) Independently confirmed by [[inference-scaling-measured-v0903]] item 2.

## `param_ess` scores a LITERAL POINT MASS as passing — both branches

`calc_model_ess_parameter.R` builds `w_global = exp(LL - max(LL))` (only 14 of 25,000 non-zero),
then rescales: KDE branch `× n_clean/n_grid`, binned branch `× n_clean/n_occupied` (= 25000/100 =
**250**). Real logL vs shuffled logL vs a literal one-hot point mass (target is 100):

| | KDE (default) | binned |
|---|---|---|
| sigma: real / shuffled / point mass | 1708.1 / 1708.1 / **1708.1** | 263.2 / 263.2 / **263.2** |
| rho | 1911.7 / 1914.7 / **1911.7** | 252.5 / 252.5 / **252.5** |
| uniform weights (no likelihood) | 13971.7 | 14706.8 |

The binned branch's `ess_bins` correctly returns ≈1 for a point mass and the ×250 rescale throws it
away. **Switching `ESS_marginal_method` to `"binned"` does NOT fix it.** And it is *gated*:
`calc_convergence_diagnostics.R:286-289` includes `status_param_ess` in the PASS/WARN/FAIL
aggregation.

## Saturation genuinely beats untruncated IS in MSE — and its intervals are 26x too wide

Controlled study, d_eff=5 calibrated to ETH (`eps_B`=5094, ESS_IS=1.02, khat=116), posterior
displaced 1 prior SD, 150 reps. RMSE on the posterior mean / (95% CI width ÷ truth):

| exact SNIS | **saturated** | adaptive-all | PSIS | Ionides trunc | ESS-targeted(100) |
|---|---|---|---|---|---|
| 0.1176 / 0.07 | **0.0711** / **26.0** | 0.3263 / 62.7 | 0.1171 / 0.11 | 0.1177 / 0.09 | 0.0711 / 28.2 |

So the `pmin(delta,4)` regularisation is doing real work (1.65x RMSE, 2.7x MSE) — **that is the
honest defence and it holds**. But no temperature gives calibrated intervals (best achievable is
~20x too wide at ESS target 20); the width is set by `eps_B`, not by the posterior.
**Do not read the reported 95% "credible intervals" as credible intervals — they are ABC-tolerance
intervals.** Beaumont/Zhang/Balding regression adjustment on the scalar logL does NOT fix them
(tested: de-biases, doubles the SD, CI unchanged 26.5→26.3); a multivariate version would need
per-draw summary statistics MOSAIC does not persist.
Ionides (2008) truncates at `min(r, sqrt(S)*r_bar)` — a level that GROWS with S, which is what
makes it consistent. MOSAIC's fixed `Delta*=4` does not, so the saturated estimator is inconsistent.

## PSIS cannot be rescued at khat = 74

ETH: tail length `M = 3*sqrt(25000) = 475`, of which only **14 (2.95%)** are non-zero exceedances,
spanning 292 orders of magnitude. `khat = 73.879` with `khat_status = "ok"` — not a calibrated
shape estimate (Vehtari et al. calibrate over ~[0, 1.5]; **khat >= 1 means the mean of the ratio
distribution does not exist**). Measured: PSIS RMSE 0.1171 vs SNIS 0.1176 — nothing. PSIS's correct
role here is the diagnostic it already is.

## `eps_B = Delta_(B)` is the only reported-able quantity that improves with n

B is a FIXED COUNT (~115 at every n from 1,000 to 25,000; B/n falls 11.6% → 0.46%), so
`eps_B ~ n^-0.431` (R²=0.9895, d_eff = 2/alpha = 4.64); 25k→100k shrinks it 1.82x. Reaching
`eps_B <= 4` (saturation inactive) needs ~2.6e11 draws. But see
[[inference-scaling-measured-v0903]] items 4-6: the improvement is confined to the ~5 identified
directions and is invisible in marginal summaries over 58 parameters, where the posterior stays
inside the random-115-subset null at every n. **Report `eps_B` AND the random-subset null; neither
alone is sufficient.**

## Adaptive-eta is invariant to the affine group and degrades with n

`eta = -log(weight_floor)/max(Delta)` exactly (the `actual_range` in
`run_MOSAIC_helpers.R:2051`/`:2063` cancels). So `w ∝ exp(-34.5388 * Delta_i/max_j Delta_j)` — a
min-max normalised softmax, invariant to `logL -> a*logL + b` (bit-identical for power-of-2 `a`;
machine precision otherwise). Temperature is set by the WORST draw: make it 100x worse and
`ESS_kish` rises 11,167 → 24,354 of 25,000. Clipping logL at −5e4 instead of −2e4 moves ESS/n from
0.0089 to 0.0000. Synthetic RMSE **rises** 0.291 → 0.354 as n goes 2.5k → 100k, the only estimator
measured to get worse with more draws.

Related: [[weighting-stack-measured-v085]] (nb_k_min, the three weight columns),
[[likelihood-overconcentration-invariants]] (score noise SD 120 vs best-vs-2nd gap 244 — the noise
swamps exactly the top-1 comparison that carries the saturated scheme's only discrimination).

## (v0.93.0 review, 2026-09-29) `"tempered"` never reaches the posterior

`best_subset_weighting` is read ONLY in the gated-metrics block (`run_MOSAIC.R:1852-1880`: ESS_B/A/CVw,
temperature, degeneracy warning). `results$weight_best` (`:1999-2011`) is hard-coded `pmin(Delta,4)`,
eta 0.5, and that is what the ensemble/posterior consume. So flipping the switch changes the gate verdict
(ESS_B ~1.25 on ETH -> FAIL) but not one posterior number. Check this wiring before believing any
"scheme X changes the posterior" claim.
