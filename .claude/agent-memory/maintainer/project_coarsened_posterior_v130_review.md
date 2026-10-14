---
name: coarsened-posterior-v130-review
description: MOSAIC 1.3.0 coarsened-posterior review (2026-10-13) - optimizer O(K x grid) cost blow-up, batch ratio is ESS in disguise, saturated bit-identity excludes weight_all, golden fixture recipe
metadata:
  type: project
---

Pre-commit review of the 1.3.0 coarsened posterior / AUTO-gate rewrite (branch feat/v1.2.0-optimize-subset-default, HEAD ba71c4e7d).

**Optimizer cost scales with K x grid, not K.** `.optimize_eval_cell_block` costs per cell ~ |N grid| x K x n_iter, with grid = seq(min_n, K, by = stride). Measured (claude/maint_review/bench_opt.R, laptop, mae-like objective): 0.0045 s/cell at K=108, 0.027 at K=300, 0.26 at K=1000. Continental (40 x 3406 cells), serial as run_MOSAIC calls it: ~0.17 h at K=108, ~1 h at K=300, ~10 h at K=1000. National at K=1000: ~15 min (was ~15 s).
**Why:** coarsened K = clamp(Kish ESS, min, max_best_subset) goes to 1000-1500, versus 108 before.
**How to apply:** any change that grows ensemble size must re-benchmark the optimizer and the ensemble arrays (4 arrays of n_loc x n_t x K x n_iter doubles + results_list held at once).

**Seed-batch ratio is roughly c * sqrt(n_batches / Kish ESS).** c is ~1.5 for 40 parameters and ~1.8 for about 1,500 (it is the max over P of chi_9/3). Stub runs: ESS 140 gave 0.41 and ESS 330 gave 0.25, which the formula predicts. The 1.2.1 saturated runs (ESS 81, ratio 0.44-0.57) fit it too. So batch_ratio_max = 0.2 in practice means Kish ESS of about 560-820. With ESS_param = 100 it is the binding criterion, not independent stability evidence.

**Saturated "bit-identical to 1.2.1" holds only for some outputs:** fixed-mode best subset, quantiles, posteriors.json, ensembles and medoid. It does NOT hold for weight_all (adaptive-Gibbs became tempered at eta_ref), parameter_ess.csv, summary fields, or the auto-mode gate. Under the new gate, saturated runs go to their budget because the ESS is capped near the subset size. The golden snapshot's keep-list omits weight_all.

**Golden fixture regen recipe (~30 s):** `git archive ba71c4e7d | tar -x -C /tmp/v121`, copy helper-stub-run.R into `/tmp/new/tests/testthat/`, then `Rscript claude/weights_investigation/scripts/19_golden_stub_fixture.R /tmp/v121 /tmp/new`, then compare the RDS with identical(). It was verified identical on 2026-10-13.

Related: [[reviewer-checklist]], [[testthat-runtime-profile]]

**Follow-up applied in the same review (2026-10-13, approved by the user via the coordinator):**
- The parameter posterior again comes from the prediction members under every scheme: `.mosaic_posterior_cols()` returns `is_best_subset`/`weight_best`, or the `_opt` columns. The draft had pointed "coarsened" at `is_finite`/`weight_all`. Convention: posterior draws = prediction draws. `weight_all` feeds only weight health, the gate and parameter_ess.csv.
- New key `targets$max_ensemble_members` (default 300) caps K. `optimize_subset` defaults to FALSE again, in both control and `run_rolling_cv`.
- The saturated golden test now passes `optimize_subset = TRUE` explicitly, because that was the v1.2.1 default.
- Not changed, belongs to ai-architect: `.claude/skills/forecast-cv/SKILL.md:65` still says `optimize_subset = TRUE` and lists `ensemble_opt` in the default models.
