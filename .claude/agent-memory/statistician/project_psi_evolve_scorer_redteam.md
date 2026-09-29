---
name: psi-evolve-scorer-redteam
description: Red-team of the psi 12-week evolution scorer/gates (2026-09-18) — baseline gets 14d of extra data, residual-mode drops the first block, A2/A3 gates reject ~a.s. at n=13 origins, no-regression guard fails open
metadata:
  type: project
---

Independent review of `claude/psi_evolve/` (arms A000/B-CAL/T1/A100, none adopted). The
measurement defects below all bias in the direction of the conclusions that were drawn.

**Why:** an autonomous loop scored 4 arms and 3 controls on a scorer whose residual-interval
path has zero test coverage and whose baseline sees data the model does not. Every headline
number in `WAVES.md` inherits these.
**How to apply:** before quoting any psi-evolve `S`, check which of these has been fixed.

1. **Baseline gets 14 days the model does not.** `score_psi_arm.R` cuts the persistence
   baseline at `fd$test_start` (= cutoff + 14) instead of `fd$train_end` (= cutoff).
   `persistence` = mean of the last 4 weekly obs, so 2 of its 4 anchor weeks are post-cutoff.
   Inflates the baseline, deflates every `wis_skill`. Cancels in arm-vs-arm deltas (same
   baseline) but not in levels — so "psi is 40% worse than persistence" is an upper bound on
   the gap. `folds$train_end` is passed in and ignored.
2. **`interval_mode="residual"` silently drops the earliest block.** Model residual quantiles
   come from `pred[date < test_start]`, which for the first fold in the mode is empty ->
   `length(r) < 8` -> `next`. A000: 199 cells (seed) vs 183 (residual) = exactly 16 = one fold
   x 16 countries. So seed-vs-residual is not the same cell set. Worse under `confirmation`
   (first holdout block dropped, next estimated from ~12 points).
3. **Residual mode is not symmetric with the baseline** despite the roxygen claim. Baseline
   residuals = full pre-block history (hundreds of weekly points, in-sample deviations from the
   terminal level); model residuals = prior evaluation blocks only (12-144 points,
   out-of-sample forecast errors). Different quantity, different n. The full pre-cutoff
   prediction series IS in the cache (`pred_date_start=2014`) and is thrown away by the driver.
   Residual mode also implicitly bias-corrects the interval *centre*, partially neutralising
   the treatment B-CAL tests.
4. **A2 and A3 reject almost surely at this n.** Implied origin-level SD of the paired delta
   = 0.338 over 13 origins -> SE 0.094 -> min detectable effect ~0.26, vs an A1 margin of
   0.0149: **18.7x mismatch**, ~4,000 origins needed. A3 (no top-10 country worse than -0.02)
   with an empirical per-country delta SD of 0.38 fires with **P ~ 0.998** under a zero true
   effect. "Four arms rejected by four different gates" is therefore not four independent
   verdicts.
5. **Percentile vs basic bootstrap flips T1.** point -0.0415, boot-median -0.358, percentile CI
   [-0.978,-0.042] -> basic CI [-0.041,+0.895]. A 0.32 bootstrap bias means the percentile
   interval is not centred on the statistic. Cause: ordinary bootstrap of a
   median-over-origins at n=4 (simulated max |bias| 0.53 with heavy-tailed skill). Using the
   MEAN over folds makes the bias ~0. For 4 origins an exact sign-flip test has min two-sided
   p = 0.125, so **no origin-level test can reach significance on a 4-origin screen** — a
   sharper statement than any CI.
6. **No-regression guard fails open.** `d <- per_iso$wis_skill[...] - incumbent[t10]` then
   `min(d, na.rm=TRUE)`: a top-10 country missing from the `incumbent` vector is silently
   exempt; all-missing gives `min(NA, na.rm=TRUE) = Inf` -> `guard_ok = TRUE`. `S_delta` uses
   `na.rm=TRUE` so a missing incumbent country's skill counts as 0. Same fail-open shape as the
   drop-tail guard fixed one wave earlier.
7. **`wis_skill = 1 - wis_m/wis_b` is unbounded below and unguarded** (only `wis_base <= 0` is
   skipped). NC3 scoring -3.13 is partly an unbounded-scale artifact, so "the scorer punishes
   noise by 2.74" is not evidence it behaves well in the +-0.1 range where decisions are made.
   Prefer log pairwise-WIS-ratio (Hub-style relative WIS) — bounded, symmetric, bootstraps well.
8. **Frozen objective self-contradicts on the split.** `OBJECTIVE.md` 4b counts selection by
   *cutoff* (14/4, 224 blocks) and 5 defines it by *test_start* (13/5). `EVAL_GRID.csv`'s
   `split` column follows 4b, the scorer follows 5. Block 14 (cutoff 2024-12-28, test_start
   2025-01-11) is labelled selection but scored as confirmation. Must be settled before A4.
9. **T1-vs-A000 is confounded by covariate look-ahead, not one change.**
   `.psi_build_sequences(lead)` anchors the input window `lead` weeks before the target on the
   *prediction* path too, so at lead=0 psi for a target in the block uses covariates up to
   cutoff+97 (reanalysis that did not exist at the cutoff) while at lead=12 it uses only up to
   cutoff+13. T1 forecasts; A000 hindcasts. T1 losing by 0.042 under that handicap is evidence
   the concurrent covariates are worth ~nothing at 12 weeks, NOT evidence against a lead target.
10. **No arm was ever replicated**, so the within-arm fit-noise floor required by PROTOCOL 4
    ("a delta smaller than the within-arm seed spread is noise, whatever the bootstrap says")
    has never been measured. B-CAL is the only cache-paired (fit-noise-free) arm and hence the
    only well-identified effect in the ledger; A100/T1 deltas are single draws.
11. **PLAN.md 6.3 pre-registered that MAE "rewards flat, under-predicting psi"** — and MAE is
    20%+ of WIS by construction. So the NC2 result (a flat per-country constant beats psi) was
    pre-registered as a metric artifact and was then read as a substantive finding. Also
    "psi loses to persistence" and "psi loses to a flat constant" are ONE fact, not two:
    `.rcv_baseline("persistence")` is itself a flat per-country constant (last-4-week mean).
