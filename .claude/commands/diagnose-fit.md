---
description: Run the active fit-diagnostic workflow on a calibration run
argument-hint: "[path to calibration run dir or medoid config]"
---
Use the **calibration-doctor** subagent with its **diagnose-fit** skill to actively diagnose the
fit of this calibration run and produce a calibration-actionable brief:

    $ARGUMENTS

Steps the doctor should follow:
1. Locate the run's medoid config (e.g. `<run>/2_calibration/best_model/config_medoid.json`).
2. Establish a baseline with `MOSAIC::run_fit_sandbox(config, full_metrics = TRUE)` and read the
   PASS/WARN/FAIL scorecard.
3. Prioritise **bias → shape → variance**; hypothesize before each sweep; run targeted
   `run_fit_sandbox(config, params = list(...))` experiments and follow the leads.
4. Save experiments under `claude/diagnose_fit/` and return the DIAGNOSE-FIT BRIEF (parameter
   targets + recommended prior changes). Prior-change recommendations are proposals — hand the
   actual prior/config edits to the disease-modeler / statistician.
