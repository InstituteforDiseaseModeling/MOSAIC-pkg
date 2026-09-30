---
name: default-path-target-anchor-leak
description: v0.92.3 target_anchor_stop fix only reaches the opt-in v7.4 prefit path; default v7.3 forecast-CV still trains on full-window cp99r anchors (AGO pre-2024 targets deflated ~9x); prefit manifest drops earlier cutoffs on incremental calls
metadata:
  type: project
---

Found in the v0.93.0 production-readiness review (2026-09-29, report at claude/review_v093/ml_report.md).

- `target_anchor_stop` is applied ONLY when the spec sets `feature_set = "v7.4"` (prefit_rolling_cv_psi builds a per-cutoff panel only then). The default spec (v7.3 + target_D) and `run_rolling_cv(psi_cache = NULL)` read the canonical panel, whose target_D anchors span 2000-2027. At a 2024-01 cutoff the log-anchor ratio exceeds 1.25 for 11/40 countries (AGO 9.2x, RWA 8.4x, COG 4.5x). AGO pre-2024 nonzero targets have median 0.076, against 0.699 when the anchor is bounded at the cutoff.
- The psi cache key is not versioned. Caches built before v0.92.3 (over-trained epochs, pre-DA-01 panel) hash identically and are silently reused.
- `prefit_rolling_cv_psi()` writes its manifest as `prior_by_cut[cutoffs]`, so an incremental call erases the earlier cutoffs. Reproduced with a mocked est_suitability.
- The validation-context widening in `.psi_slice_rw_step` is unconditional, not "optional", and changes production best_epoch.
- CORRECTION to [[psi-artefact-provenance-v077]]: the current `config_default$psi_jt` equals the shipped CSV `psi` exactly (max |diff| 0, all 40 locations). The 98-day pred_raw fill tail is still present.

**Why:** leak-freedom has to hold on the path people actually run, not only on the opt-in one.
**How to apply:** before trusting any default forecast-CV psi result, confirm per-cutoff anchoring (or `response_var = "transmission_intensity"`, which anchors train-only in `.psi_build_data`). Treat any psi_cache made before v0.92.3 as stale.
