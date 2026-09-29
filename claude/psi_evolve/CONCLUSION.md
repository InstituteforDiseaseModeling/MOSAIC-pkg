# psi_evolve — CONCLUSION (closed 2026-09-28)

**Outcome: no psi variant beat the production LSTM once psi was pushed through
calibration. The experiment is closed; none of its architectures ship.**

## What was tried

Waves 0–34 (psi-level, scored on the 9 OCV-4 quarterly cutoffs 2024-01 → 2026-01),
then a 72-cell full-pipeline A/B on dugong: 6 psi arms × COD/ETH/MOZ/NGA × cutoffs
2024-10-01 / 2025-04-01 / 2025-10-01, fixed 5,000 sims per cell with common random
numbers (arms differ only in `psi_jt`).

| Arm | What it is |
|---|---|
| `p000` | LSTM as on main (production proxy) |
| `p000r` | `p000`, disjoint seeds → LSTM noise floor |
| `prod` (P000E) | LSTM + epoch fix |
| `n9` | branch defaults: D9b (static-covariate country embedding) + N8 (per-country loss balance) + epoch fix |
| `nd` / `nd_rep` | DLinear trunk, and its seed replicate → DLinear noise floor |

## Downstream result (cases, ensemble; log(WIS+1) paired difference, lower = better)

| Contrast | In-sample | OOS ≤3 mo | Best-subset Jaccard |
|---|---|---|---|
| LSTM floor `p000r − p000` | +1.7% | +4.1% (SE .036) | 0.86 |
| DLinear floor `nd_rep − nd` | +1.0% | −0.6% (SE .025) | 0.80 |
| Epoch fix `prod − p000` | **+6.0%, worse 11/12** | −4.4% (null) | 0.73 |
| Branch defaults `n9 − p000` | **+12.5%, worse 12/12** | +3.4% (null) | 0.64 |
| DLinear `nd − p000` | **+87.6%, worse 12/12** | −3.5% pooled (NGA −0.47, ETH +0.25) | 0.30 |

- psi changes *do* propagate (Jaccard well below the ~0.8 floors); the posterior is not
  simply absorbing them through `psi_star`.
- Every variant fits in-sample worse than main's LSTM, beyond the floor. None shows an
  OOS gain. DLinear's psi is ~40% of the LSTM's amplitude, which gives better level and a worse fit.
- OOS |log bias| contrasts are dominated by MOZ noise (the floor alone moves MOZ 0.40) and
  are uninformative.
- The psi-level wins that motivated promotion (D9b MAE −3.9%, N8 R²_corr +6.6%, DLinear
  "+13% raw MAE", blend +10.8% vs persistence at wk 9–13) **did not predict downstream fit**.
  The sealed holdout had already shown the blend gain is a coin flip per country-block.

Scoring script: `claude/psi_ab/compare_wave2.R` → `claude/psi_ab/out/compare_wave2_results.rds`.

## What shipped (branch `fix/psi-evolve-closeout`, v0.92.3)

Only the correctness defects found along the way:
- the deployed psi model was refit at the *stop* epoch (best + patience) → `.psi_epoch_from_history()`
- the leak-free v7.4 panel's target anchors used post-cutoff rows → `compile_suitability_data(target_anchor_stop=)`
- ISO-8601 week labelling; day-based RW-CV stride; RW-CV lead/context, fold predictions,
  loud drop-tail guard, manifest provenance (v0.90.6–v0.90.11 commits)

The epoch fix worsens in-sample fit (11/12) and is OOS-neutral. It ships as a correctness
fix: the old behaviour trained past the validated optimum.

## Not shipped

Trunk registry (GRU/TCN/DLinear + knobs), HA-02 epoch-select decoupling, N5/N6 FiLM
variants, D9b, N8, and the D9b+N8 default promotion. Full history under git tag
**`archive/psi-evolve`**.

## Where things are

- Laptop: this directory; `claude/psi_ab/` (harness, `out/` incl. 372 MB per-arm run outputs, gitignored);
  `dugong_final/` = reports/tables/scripts/figures and all A/B cell logs copied off dugong.
- dugong: `~/psi_evolve` (25 psi caches, 4.9 GB), `~/psi_ab*`, `~/psi_timing` and
  `~/MOSAIC/MOSAIC-pkg/claude/psi_ab` **deleted** after the copy. The OCV-4 production
  psi_cache is separate and untouched.

## If psi is revisited

The remaining lever identified was OOS psi decay (psi falls to ~42% of truth by week 9).
Any new candidate should be judged by the downstream CRN A/B against **both** noise floors,
not by psi-level MAE, which has now failed to predict downstream fit three times.
