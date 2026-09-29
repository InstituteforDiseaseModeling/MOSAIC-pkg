---
name: oos-scoring-metric-traps
description: Four metrics that silently reward a degenerate forecast when MOSAIC ensembles are scored out of sample — corr-R² (scale-free), |bias−1| (asymmetric), MAE/WIS on a low-incidence tail (won by all-zero), and the random-subset null (which IS the all-zero forecast for |B| >= 115). Always anchor to a climatology skill score.
metadata:
  type: reference
---

Measured on ETH n = 10,000 while answering the |B| question
([[subset-ranking-beats-size]]). Each of these flipped a conclusion during that work.

**1. Correlation-R² is scale-invariant and therefore blind to the thing that is actually wrong.**
On the ETH test window, `r2_corr` for cases rises monotonically with |B| — 0.554 (|B| = 115) →
0.735 (5,000) — while the bias ratio goes 1.9 → 5.0 and WIS goes 5.5 → 25.6. A *random* 115-draw
subset scores `r2_corr` ≈ 0.56 **while forecasting essentially zero**. Any |B| / subset / weighting
decision taken on R² points the wrong way. Report WIS + bias; keep R² as a secondary shape
diagnostic. (`calc_model_R2(method = "sse")` does not have this problem but is dominated by a few
large days.)

**2. `|bias − 1|` is asymmetric and makes dead draws look good.** It bounds under-prediction at 1
while over-prediction runs to ∞, so a draw predicting nothing scores as nearly perfect. On the test
window: Spearman(logL, `|bias−1|`) = **+0.44** (A1b) / **+0.57** (baseline) — i.e. "higher
likelihood is worse" — while Spearman(logL, `|log bias|`) = **−0.84** / **−0.79**. With ~50% of
prior draws dead, `|bias−1|` mostly counts how many dead draws an arm promotes. **Always use
`|log(bias)|`.** This is what made an earlier lab measurement read as "arm A1b made the likelihood
5× more informative about bias" when the symmetric statistic shows −0.84 vs −0.79.

**3. On a low-incidence holdout window, MAE and WIS are won by predicting nothing.** ETH's
2025-09 → 2026-03 window has 1,562 cases over 181 days (8.6/day) against a training mean of
30/day, and the model cannot produce the decline. Measured reference MAE there: **all-zero 8.63**,
seasonal climatology 16.52, training mean 23.90. So a level-sensitive score alone will select
degenerate members. Pick a holdout window where the trivial forecast is bad (ETH 2025-03 → 2025-09:
all-zero MAE 30.96 vs training-mean 15.06) and **always print the all-zero / climatology reference
alongside the ensemble score.**

**4. The random-subset null is not a neutral null once |B| >= ~115 — it IS the all-zero forecast.**
Because 51% of ETH prior draws predict 0 cases on the median day, a random |B|-subset's weighted
median sits on the 0/non-0 boundary and collapses: measured bias exactly 0.0000 and MAE exactly
equal to the all-zero MAE at every |B| >= 115. Its apparent "skill" on a low-incidence window is
the zero forecast's skill, not evidence about selection. At small |B| (5-50) it is genuinely
random and the band is wide. Interpret the null band accordingly, and state which regime it is in.

**Corollary for reporting:** use **WIS skill vs seasonal climatology** (day-of-year mean of the
training window) as the headline. It is scale-sensitive, proper, comparable across windows and
channels, and it makes the degenerate forecasts visible as explicit reference lines.
