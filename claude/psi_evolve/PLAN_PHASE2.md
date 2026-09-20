# psi_evolve — phase 2 plan

Phase 1 closed with: fold geometry inert, country-conditioning capacity inert in
both directions, blends buying MAE by flattening, and **every arm at coin-flip
directional accuracy**. Phase 2 is built around the one explanation phase 1
never cleanly tested.

---

## The reframing that drives this plan

**The model is not trained to forecast.** `lead = 0` everywhere: the target is
`X_t -> y_t`, anchored at the last timestep of its own input window. The 12-week
"forecast" is a side-effect of feeding a nowcaster covariates that extend past
the cutoff.

The inner CV inherits this. It scores concurrent reconstruction, so
`median(best_epoch)` is chosen for the wrong task — which is the most economical
explanation for why four geometries spanning 89-912 folds all landed within 2.2%
and why nothing moved `dir_acc` off 0.5.

This *was* tested once (T1) and failed, but that test was confounded three ways:
it was `A100 + lead-12` where A100 was itself rejected; it was scored on
WIS-skill, later measured at **28.4% replicate noise** against MAE's 0.09%; and
it ran on the superseded v2 grid before N8/D9b existed. The conclusion drawn
from it — "the 12-week signal is not in these covariates" — may be right, but it
is not yet earned.

---

## Arms

### Tier 1 — run first, decisive

| arm | one change | vs | cost |
|---|---|---|---|
| **T2** | `lead = 12` (target `X_t -> y_{t+12w}`) | P000 | ~1 h |
| **N9** | `country_static=frozen` + `country_balance=TRUE` | P000 | ~1 h |

**T2** isolates the lead. With `lead = 12` the inner CV automatically begins
scoring the forecast task rather than reconstruction — that is intrinsic to the
change, not a confound.

**T2's prediction is deliberately two-sided and must be registered as such:**
MAE may WORSEN while `dir_acc` and `dcor` IMPROVE. Forecasting 12 weeks ahead is
a strictly harder problem than reconstructing the present, so a model that
finally attempts it can lose on average error while gaining the only thing we
actually want. **T2 must not be judged on MAE alone.** Promotion rule below.

**N9** is the first stacked arm in the programme: D9b fixed the level (best raw
MAE 0.1918, best R2_sse) and N8 fixed the shape (best R2_corr 0.321, and the
only arm that moved cross-country correlation, 0.268 -> 0.189). They repair
different failures and have never been combined. Risk: both touch country
handling and may interact rather than add.

### Tier 2 — inner-CV structure, run alongside

| arm | geometry | folds | inner coverage | overlap |
|---|---|---|---|---|
| **F5** | stride 84 d, test 84 d, gap 2 wk, start 2 y | ~30/cutoff | — | none |
| **F6** | stride 90 d, test 90 d, gap 1 wk, start 270 d (75/25 per fold) | 33-38/cutoff, 213 total | **91%** | **none** |

**F5** completes the 2x2 that F4/F1/F3 imply, matched to F4 in every parameter
but the grid start: F1-vs-F4 said a denser stride hurts (+1.8%), F3-vs-F1 said
an earlier start helps (-2.1%), and stride-84 + early-start is the untested cell.

**F6** is the user's design. Stride == test window, so inner-test folds TILE the
IS timeline with no overlap — each IS observation is held out exactly once —
starting at the first date with a 75/25 per-fold train:test ratio. It is the
missing combination: F4 was non-redundant but shallow, F3 was deep but 3x
redundant. 60/40 was considered and rejected: +3pp coverage for a first fold
training on ~6 sequences per country.

### Tier 3 — conditional, do not pre-commit

- **T2b** = `lead = 12` + N9, only if T2's trend metrics improve.
- **F6L** = F6 geometry + `lead = 12`, only if BOTH are promising. This is the
  coherent end state: non-redundant folds tiling 91% of IS, each scoring the
  actual 12-week task.
- **Trunk axis** (`arch_control$trunk = gru|tcn`). Capability shipped, never
  run; every arm to date is the 3-stack LSTM. Deferred below T2 because the task
  mismatch is a better explanation for coin-flip timing than the encoder is.

---

## Promotion rules, registered in advance

1. **Primary axis for phase 2 is TREND, not MAE.** `dir_acc` and `dcor` against
   the baselines measured in phase 1: every arm 0.45-0.50 / 0.00-0.06;
   climatology 0.57 / 0.097; a constant scores 0 / undefined.
2. **An arm promotes on `dir_acc` > 0.52 sustained across >= 4 of 6 blocks**,
   even if MAE worsens, PROVIDED `sd_ratio` stays in 0.7-1.3 (i.e. it is not
   winning by flattening or by wild over-amplitude).
3. **An MAE-only gain with `dir_acc` <= 0.50 is NOT a promotion.** Phase 1
   established that MAE rewards abandoning trend; we will not re-learn that.
4. Report every arm on the full set: MAE, R2_corr, R2_sse, WIS, bias, sd_ratio,
   dcor, dir_acc, degen (`shape_table.R`), plus per-country and the log-log
   calibration.
5. Replicate floor is 0.1% on MAE from ONE pair (P000/P000R). Treat single-arm
   differences under ~0.5% as unresolved, and prefer agreement across the four
   independent metrics over any one of them.

---

## The holdout problem — needs a decision

**The confirmation blocks are spent.** They were read once, on 2026-09-19, and
re-reading them is a PROTOCOL 5.3 breach that would make every confirmation
number in the programme uninterpretable. Phase 2 therefore has **no sealed
holdout** unless we create one.

Three options, in order of preference:

1. **Extend the outer grid.** The cutoffs are ours to choose. Observed data now
   runs to **2026-06-04**, and the current last cutoff is 2026-01-01. A cutoff
   at 2026-02-15 or 2026-03-01 yields a scorable 13-week window inside the
   available data, giving 1-2 genuinely fresh sealed blocks. Cheap: it needs a
   psi refit per new cutoff per arm, nothing else.
2. **Country-holdout confirmation.** Seal 3-4 countries instead of 3 time
   blocks. Tests generalisation across countries rather than forward in time —
   arguably the more relevant axis given the per-country findings, but it is a
   different claim and should be labelled as such.
3. **Selection-only, pre-registered.** Accept that phase 2 is exploratory and
   say so. Acceptable only if no arm is proposed for production off it.

**DECIDED (user-approved): option 1.** Extend the outer grid with a new cutoff
(~2026-02-15 or 2026-03-01) to create fresh sealed confirmation block(s), fixed
BEFORE any phase-2 arm is scored.

Dependency to resolve at build time: the v3-grid arms (P000/N8/D9b/T2/...) build
from the canonical v7.3 panel, so a new cutoff is straightforward. **P001 is
not** -- it reads a pre-built per-cutoff leak-free `panel_v74_<cutoff>.csv` from
the OCV-4 cache, which does not exist for a new date. If regenerating that panel
is awkward, the comparator at the new cutoff should be **P000** (the
same-pipeline refit incumbent, MAE 0.1996) rather than P001 (0.2022), and the
substitution must be stated in the result.

---

## Sequencing and resources

dugong is currently at ~150 of 176 cores on another agent's MOSAIC calibration.
`launch_arm.sh` refuses to start on top of a workload > 60 cores, and a `STOP`
file is in place. Nothing launches until that clears.

Order once capacity returns, 9 shards each, two arms at a time:

1. **T2 + N9** (Tier 1, ~1 h each) — decisive, cheapest
2. **F5 + F6** (Tier 2, ~2-2.5 h each)
3. Tier 3 only on evidence

Total ~7 h of wall clock at two-arm concurrency, versus ~12 h for phase 1's
fold ladder alone.

## Held constant for phase 2

feature set v7.3 · timesteps 13 · 10 seeds · HA-02 epoch decoupling
(`epoch_select_seeds = 2`) · the 16-country burden-weighted pool · the 6
selection blocks · trunk = lstm (until Tier 3).
