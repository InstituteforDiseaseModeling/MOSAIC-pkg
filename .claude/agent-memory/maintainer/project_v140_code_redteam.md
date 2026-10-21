---
name: v140-code-redteam
description: MOSAIC 1.4.0 convergence-status code red team (2026-10-20) - rolling-CV all-red trap, n_valid<2 no-status path, eta*-NA false green, gate-vs-members status split, reusable stub harness
metadata:
  type: project
---

Code-execution red team of the uncommitted 1.4.0 convergence pipeline (feat/v1.4.0-convergence). Verdict REQUEST-CHANGES.

**Traps worth re-checking on any later convergence/status change:**
- The status gate in `evaluate_rolling_cv()` is always on once a cutoff has `convergence_status`. The default cheap control (fixed 2,000 draws) is RED for every national run: the earliest GREEN in the 28-run replay was 7,136 draws. Result: `$summary` has 0 cells and all-NA metrics, with no warning.
- `n_valid < 2`, or no varying sampled parameter, gives `conv_health = NULL` / an empty MCE. The `params` frame in `.mosaic_calibration_diagnostics()` then has 0 columns, so neither JSON is written, and the report shows the "pre-1.4.0" notice.
- `flat <- isTRUE(eta_star >= 0.999)`: η* is NA whenever n_valid < ESS_all target, which turns informativeness GREEN with a false "data inform" reason. Small/fixed runs and a tied likelihood show it.
- The gate always scores the best-fit selection, but the final status is re-scored on the actual members (legacy schemes, optimiser). So `stopping_rule_met=TRUE` can come with yellow/red and a "run more simulations" remedy. Under "saturated", `ESS_best` is reset to 100 and the gate uses it as the ensemble target.
- Re-purposed `ESS_best`: a 1.3.x `control.json` (`ESS_best` 100) silently halves the target. Detect pre-1.4.0 controls by the absence of `status_thresholds`.

**Reusable harness (laptop):**
- `claude/redteam/code/rt_stub.R`: a generic stubbed `run_MOSAIC()` for ANY tree (main/HEAD/working), with env knobs `RT_LL` (mostbad/onegood/allbad/tie), `RT_ISO`, `RT_NO_DEATHS`, `RT_KILL_AT` (quit mid-batch) and `RT_RESUME`.
- `rt_validate_json.R`: a programmatic SCHEMA_v2 §1/§2 check.
- About 30-80 s per run.
- Do not edit `rt_stub.R` while a batch is running: Rscript reads the file incrementally after the run.

**Held up:** 1.3.0→1.4.0 resume, a mid-batch kill and resume (byte-identical posterior), RNG isolation of the KL estimator, the goldens, R CMD check (1 NOTE = baseline), and `calc_weight_health` at 150k×300 in about 1 s (linear).

Related: [[v130-reporting-audit]], [[coarsened-posterior-v130-review]], [[test-suite-audit-traps]]

**Round 2 (2026-10-20):** C1-C20 essentially fixed. New lesson-#13 regression: the round-1 ESS_best shim reads the "raw" control, but the dominant pattern `ctrl <- mosaic_control_defaults(); ctrl$targets$posterior_weighting <- "saturated"` carries the pristine 200 in raw. So "saturated" gets ESS_best 200 plus a spurious warning. Treat a raw value that equals the pristine default as unset.
- **Harness:** the r2 copies are in `claude/redteam/code/r2/`.
- **Full check recipe:** `MOSAIC_RUN_INTEGRATION=1 NOT_CRAN=true R CMD check` takes about 20 min and runs the goldens and the auto stubs.
