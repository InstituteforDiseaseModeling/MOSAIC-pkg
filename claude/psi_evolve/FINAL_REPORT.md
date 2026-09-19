# psi_evolve — final report

**Question.** Can changes to the inner-CV structure or the LSTM architecture
improve the **12-week out-of-sample accuracy** of the production environmental
suitability model (ψ), measured as MAE / R² / WIS against the forecast cutoffs
already used to validate production?

**Grid.** The 9 frozen OCV-4 production cutoffs (quarterly 2024-01-01 →
2026-01-01). Six for selection; three sealed for confirmation and read exactly
once, at the end, under a pre-registration written before the read.

---

## 1. The answer

**Yes, but modestly, and only in combination with a persistence baseline — and
the gain is not reliable at the level of an individual country or quarter.**

The best product is **N8 ψ blended with `C12c_C11h`**:

| horizon | selection (6 blocks) | confirmation (3 sealed blocks) |
|---|---|---|
| weeks 1-4 | −3.1% | −2.3% |
| weeks 5-8 | −0.5% | +8.0% |
| **weeks 9-13** | **+5.7%** | **+10.8%** |

(% = MAE improvement over a 4-week persistence baseline; positive = better.)

On the sealed blocks at weeks 9-13: blend **0.1179**, persistence **0.1322**,
raw ψ **0.2042**. The pre-registered success criterion was the **sign** of the
weeks-9-13 gain, with magnitude explicitly not tested. **The sign replicated.**

### The qualification, which must travel with that number

Under the honest unit of analysis — country × block, because cells within one
are strongly autocorrelated — the effect is **a coin flip**:

- **Confirmation:** blend better in **16 of 32** country-block units, exact
  two-sided **p = 1.0000**, median paired gain **+0.0000**.
- **Selection:** 34 of 54, **p = 0.0759**.

And it is dominated by a few blocks. Per-block at weeks 9-13:

| | blocks |
|---|---|
| selection | +0.0%, +10.2%, +6.6%, **−7.6%**, **−3.5%**, **+34.7%** |
| confirmation | +10.8%, **+30.7%**, **−26.2%** |

Two of three sealed blocks positive (the pre-registered expectation), but one is
strongly negative. **The pooled gain comes from magnitude in a minority of
country-blocks, not from consistency across them.**

A readable mechanism for the spread: on the sealed block where the blend lost
badly (2026-01-01), persistence was already excellent (MAE 0.0970 — the easiest
block in the set). The blend helps when persistence is poor and hurts when
persistence is already very good.

Finally, **the blend is inert for a large fraction of cases**: λ = 0 in 176 of
439 selection cells (40.1%), i.e. for 36 of 90 country-blocks the "improved
forecast" *is* persistence, bit for bit. λ ranges from RWA 0.000, SSD 0.011,
MWI 0.059 up to CMR 0.725, MOZ 0.661, SOM 0.554.

---

## 2. What actually improved accuracy

Four stacked changes, in the order they contribute:

| change | what it fixes | evidence |
|---|---|---|
| **N8** country-balanced loss | shape | R²_corr 0.098 → **0.321**, best of any arm |
| **D9b** country embedding initialised from static covariates | level | best raw MAE **0.1918** |
| **C11h** half-strength level anchor to last observation | the OOS decay collapse | — |
| **C12c** horizon-dependent blend weight | lets ψ earn weight as persistence decays (λ 0.05 → 0.50) | — |

**The underlying diagnosis.** ψ decays out-of-sample to **42% of the truth** by
week 9 (0.179 → 0.089 while the truth holds near 0.22), identically in `psi`,
`pred_smooth` and `pred_raw` — so it is the model, not post-processing. In
sample it is well calibrated. R²_corr ≈ 0.27 with R²_sse ≈ −6.4 means the shape
is partly right and the level is badly wrong. Every change above is aimed at one
of those two failures.

---

## 3. What is closed

**The fold ladder — inner-CV geometry is inert.** Four geometries spanning 89 to
912 folds:

| arm | folds | geometry | MAE | R²_corr |
|---|---|---|---|---|
| P000H | 89 | control | 0.1988 | 0.300 |
| F4 | 229 | 84-d stride, window matched to the 12-week horizon | 0.1989 | 0.297 |
| F1 | 677 | 28-d stride (denser, same 4-y window) | 0.2025 | 0.287 |
| F3 | 912 | 2-y grid start (reaches further back) | 0.1982 | 0.318 |

**No monotone relation to fold count**; three of four sit within 0.35%.
**Increasing IS CV folds while preserving the rolling-window structure and
12-week horizon emulation leaves 12-week OOS accuracy unchanged.** The reason is
structural: the inner CV's only output is `round(median(best_epoch))` for the
refit, and the model is insensitive to that epoch over the 16-26 range the
geometries produce.

*A retracted claim.* At wave 30, with only three of the four arms in, I
concluded that more folds drive the epoch to a cutoff-invariant constant and
thereby degrade accuracy. F3 refuted both halves — 35% more folds than F1, yet
its epochs vary again (24 22 26 24 24 23 vs F1's flat 22) and its MAE is the best
of the four. The error was extrapolating a monotone law from three ordered points
while the fourth was still running, on a ladder where fold count was never the
single varying quantity.

**The blend family.** A2 (damped trend) rejected, C13 a wash, C12b ≈ best.
Saturated.

**Architecture arms.** N6 (sign-permissive γ) marginal. N5 (input-FiLM) behind
N8.

**A demoted heuristic.** The wave-27 rule "judge a ψ for blending on R²_corr,
not MAE" explained the one case it was built on (D9b) but does not generalise:
F3 at R²_corr 0.318 blends **worst** (0.1540) while F4 at 0.297 blends **second
best** (0.1515). The robust statement is that **all reasonable ψ variants blend
to the same place, 0.1511-0.1540** — a 1.9% span near the replicate floor —
because the blend takes its level from persistence and uses ψ only for a thin
shape contribution.

---

## 4. Honest bottom line

At the 12-week horizon the blended product beats persistence **on average**, and
that survived a properly sealed holdout. But:

- it does **not** beat persistence at 1-4 weeks (it is slightly worse);
- it is **exactly 50/50** across country-block units on the holdout;
- it is **inert** (λ = 0) for ~40% of country-blocks;
- a single quarter can swing it from +31% to −26%.

So the defensible claim is: *a horizon-weighted blend of the N8 ψ with
persistence reduces pooled 12-week MAE by roughly 5-11% relative to persistence
alone, concentrated in the minority of country-quarters where persistence is a
poor baseline.* It is **not** a uniform improvement, and raw ψ on its own remains
far worse than persistence at every horizon tested (0.2042 vs 0.1322 at weeks
9-13 on the sealed blocks).

The largest remaining lever is not the inner CV and not the blend — both are
closed. It is the **OOS decay of ψ itself** (to 42% of truth by week 9), which
the level anchor patches downstream rather than fixes at source.

---

*Grid: 9 OCV-4 production cutoffs. Pool: 16 countries, burden-weighted. Target:
`target_D_rate_per_country_floored`. Confirmation blocks read exactly once, on
2026-09-19, under `PREREGISTRATION_CONFIRM.md`; the read is recorded in
`confirm_read_log.txt` on dugong at `/home/jgiles/psi_evolve/`.*
