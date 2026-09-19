# Pre-registration of the confirmation read

Written **before** `confirm_read.R` was run. The holdout is write-once
(PROTOCOL 5.3); this file exists so the bar is fixed before the number is
visible, rather than rationalised after it.

## What is being tested

The selection set (6 blocks, 2024-01-01 .. 2025-04-01) says: N8 psi blended
with `C12c_C11h` beats a 4-week persistence baseline at **weeks 9-13** by
**+5.7%** MAE (blend 0.1658, persistence 0.1758, production psi 0.2186).

The question is whether that sign survives on the 3 blocks
(2025-07-01, 2025-10-01, 2026-01-01) never used to choose anything.

## Power, stated up front

The holdout at weeks 9-13 is **226 cells across 40 country-block units**
(14 / 14 / 12 countries per block) against roughly twice that in selection.
**I do not expect +5.7% to reproduce as a magnitude.** A point estimate from
226 autocorrelated cells has a standard error of the same order as the effect.
Pre-committing to this now so that a smaller number is not read as a failure
and a larger one is not read as a triumph.

## Primary endpoint

Weighted pooled MAE at weeks 9-13, blend vs persistence, 3 locked blocks:
`(MAE_pers - MAE_blend) / MAE_pers`.

**Success = the sign replicates (> 0).** Magnitude is reported, not tested.

## Secondary, all pre-specified

1. **Per-block** % improvement. Expect >= 2 of 3 positive. One block carrying
   the whole effect is a materially weaker result and will be reported as such.
2. **Exact paired sign test** over the 40 country x block units (two-sided).
   Reported for information: at n=40 with cell-level autocorrelation this is
   the honest unit, but it is not the pass/fail gate.
3. **Falsification check** at weeks 1-4 and 5-8. Selection showed -3.1% and
   -0.5% there: the blend is *supposed* to be no better than persistence at
   short horizons, because it is mostly persistence there (lambda ~ 0.05).
   If the holdout shows the blend winning *large* at weeks 1-4, that is
   evidence of a bug, not of a better model, and I will say so.
4. **Raw psi MAE** at weeks 9-13, expected to be far worse than both (~0.22).
   If raw psi is suddenly competitive, the blend machinery is not doing the
   work I claim it is.

## Declared in advance

- No re-run, no re-specification, no alternate blend, no alternate pool after
  the read. Whatever prints is the result.
- The 40-unit count and the 12-14 country coverage are known now and are not
  a post-hoc excuse.
- If the sign flips negative, the programme's conclusion is that the
  selection-set win did not generalise, and FINAL_REPORT.md will say exactly
  that.
