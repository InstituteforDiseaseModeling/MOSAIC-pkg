---
name: v130-reporting-audit
description: 1.3.0 weighting/ESS/convergence reporting audit (2026-10-15) - sibling set of reporting surfaces, three disagreeing verdicts, weight_retained still load-bearing, downstream readers of dead keys
metadata:
  type: project
---

Read-only audit of how the 1.3.0 coarsened-posterior metrics are reported. Deliverable: claude/reporting_audit/PKG_INVENTORY.md (laptop), renders in figs/.

**The reporting sibling set (change together):** calc_convergence_diagnostics() -> convergence_diagnostics.json -> calc_model_convergence_status() (table + convergence_status.csv) -> plot_model_convergence_status() (PDF); .mosaic_write_summary_json() + .mosaic_weight_health_fields(); run_MOSAIC.R log block ~2040-2140 and Run Summary/[RUN_SUMMARY] ~3449-3510; subset_selection_summary.csv + convergence_results.parquet (no in-package reader); downstream: run_rolling_cv .rcv_read_ess_best (reads metrics$ess_best), deploy_v0103 evaluate_suite M-CONVERGENCE-STATUS/M-IS-DIAGNOSTICS, MOSAIC-results promote_model/build_registry/MODEL_CARD (reads `converged`, and nonexistent r2_cases).

**Key findings:**
- Three verdicts disagree: `converged` (AUTO rule, always FALSE in FIXED), `posthoc_criteria_met` (= weight_health ESS+batch), and PDF Overall (also gates param-ESS proportion; calc_convergence_diagnostics.R:358). Docs say ESS+batch only.
- PASS says nothing about fit: the real Kenya counties 1.3.0 runs (MOSAIC-Kenya/model/output/counties-v0-20xx) PASS with R² 0.07/0.015, and with Kish ESS at 64% of draws (posterior close to prior).
- weight_retained (legacy Akaike, trunc 25) is still LOAD-BEARING: calc_model_parameter_sensitivity picks samples by its ESS, and plot_model_posteriors_detail draws a "Retained (weighted)" panel from it.
- The verdict is on weight_all, but the written quantiles come from K<=300 members. The members' batch ratio was 0.36 when weight_all's was 0.13.

**Why:** the user saw the 1.3.0 convergence PDF and said "fields different, don't all make sense".
**How to apply:** any reporting redesign must update the whole sibling set plus the 3 downstream readers in lockstep. Real 1.3.0 run dirs exist on the laptop under MOSAIC-Kenya/model/output. Stub runs need set_root_directory() before helper-stub-run.R.

Related: [[coarsened-posterior-v130-review]], [[plots-gated-data-writes]]
