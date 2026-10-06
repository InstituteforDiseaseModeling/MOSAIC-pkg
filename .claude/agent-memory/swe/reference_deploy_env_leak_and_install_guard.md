---
name: deploy-env-leak-and-install-guard
description: Suite-deploy traps from red-teaming deploy_v0103 (2026-10-03): install guard missed idle lanes; lanes inherit the operator shell and BASH_ENV/R_ENVIRON_USER re-inject knobs after an unset; stage2 assembler inherits INFLATE_FACTOR etc.; name-keyed gates; watchdog bare-PID identity; macOS setsid shim
metadata:
  type: reference
---

- **"No R process alive" is not "no suite running".** `install_mosaic.sh` refuses only on
  `pgrep -af "exec/R|r-mosaic-Rscript"`. While a suite waits for its stage-2 warm start, its lanes
  are alive and sleeping (`bash .../_control/lane.sh N` + `sleep 60`) with no R at all, so an
  install then succeeds and the waiting regional/continental jobs load the NEW MOSAIC (they die at
  preflight: fail-closed, but the suite stalls until the old version is restored from
  `~/R/lib_backup_MOSAIC_<ver>/`). Guard on `pgrep -f '/_control/lane.sh'` too.
- **Lanes inherit the caller's whole environment.** `launch.sh` sources `_control/env.sh` and then
  `setsid nohup bash lane.sh`, so only the knobs env.sh exports are pinned (v0103: SUITE_DIR,
  CORE_BUDGET, N_CORES, MOSAIC_LIB, SAMPLE_ALPHA_1, CONT_*, BUDGET_SCALE, G3). SMOKE,
  CASES_SCORING, FORCE_FRESH, EXPECT_*, EXPECT_WS_*, SKIP_*, RUBRIC_JSON, RSCRIPT exported in the
  operator's shell reach every job (demonstrated with a fake RSCRIPT). CASES_SCORING is invisible
  to the evaluator; `run_job.R` unlinks OUT under FORCE_FRESH even when `3_results/summary.json`
  exists. Fix shape: unset the knob list in launch.sh before sourcing env.sh; refuse FORCE_FRESH
  over a finished calibration.
- **`unset` in launch.sh is not the whole fix** (verified on the v0103 fix, 2026-10-03): each lane
  is a new non-interactive bash that sources a caller-exported `BASH_ENV`, and every R process reads
  `R_ENVIRON_USER` / `~/.Renviron`, so knobs re-enter after the unset (demonstrated). Close the
  silent ones at the consumer (run_job.R refusing CASES_SCORING; FORCE_FRESH refused over a finished
  calibration). Same class one level over: `stage2_handoff.sh` runs the assembler with
  `env K=V ... cmd`, which ADDS to the operator's environment, so INFLATE_FACTOR (RULE.md fixes x2),
  N_VALIDATE_DRAWS, SAMPLE_ALPHA_1, ALLOW_*, MOSAIC_PKG reach it and its provenance check passes.
- **Name-keyed gates are bypassed by renaming**: a `case v2026-10.04*` registration gate let
  `v2026-10.05`/`v1.0-candidate` launch unregistered; gate every launch of release-specific tooling.
- **Watchdog identity is a bare PID** from `claims/<JOB>/owner`: a stale PID reused by another
  group leader gets that group killed; a dead lane (job re-parented, "keeping claim") reads as
  "job not running" and nothing is protected. Key on the group containing
  `_control/scripts/run_job_<JOB>.R` instead (`pgrep -g <owner pid>` works after the leader dies).
- **Rehearse launch.sh/lane.sh/watchdog end to end on the laptop** with a perl `setsid` shim on
  PATH (`use POSIX qw(setsid); fork if getpgrp()==$$; setsid(); exec @ARGV`) plus a fake RSCRIPT
  exported from env.sh; the warm-start file must be >2 min old (`touch -t`). Harness:
  `claude/v0103_redteam_deploy/e2e_rerun/run_e2e.sh`.

**Status (2026-10-03): fixed in claude/deploy_v0103** - launch.sh unsets the knob list before
sourcing env.sh, refuses a differing `_control/run_job.R`/`lane.sh` on relaunch (cp over a script live
bash lanes are reading corrupts them), and refuses `v2026-10.04*` suites until rubric 1.0.3's
PREREGISTRATION.sha256 exists; run_job.R refuses FORCE_FRESH over summary.json; the watchdog keys on a
group member running `run_job_<JOB>.R` and resets its count when the job is not visible. Laptop tests:
`claude/v0103_rebuild/deploy_tests/test_launch.sh` (incl. a no-unset mutant that must leak),
`test_stage2_handoff.sh` (fake HOME), `test_run_job_modes.sh`.
- **Second red-team pass (same night).** Scrubbing the lanes is not enough: (i) `stage2_handoff.sh`
  passed the operator's shell straight into the warm-start assembler (INFLATE_FACTOR, N_VALIDATE_DRAWS,
  ALLOW_*, SAMPLE_ALPHA_1, MOSAIC_PKG all honoured) and the provenance check still said OK -> run it
  under `env -u <knobs>`, take SAMPLE_ALPHA_1/MOSAIC_LIB from env.sh read with `env -i`, and assert
  the manifest's tempering/draws/alpha/allow_* fields; (ii) a caller BASH_ENV is sourced by `bash
  launch.sh` itself, so unsetting it protects only against re-sourcing by the lanes: a knob that file
  exports and the unset list misses still arrives; (iii) R reads ~/.Renviron regardless, so run_job.R
  must refuse the dangerous knobs (CENTRAL_METHOD, CASES_SCORING) after R starts; (iv) a gate keyed
  on the suite NAME (`v2026-10.04*`) lets `v2026-10.05` through: gate every launch of a bundle that
  only ever launches one kind of run.
- **Laptop bash is 3.2.** `"${a[@]}"` on an empty array under `set -u` is "unbound variable" there
  (dugong's bash 5 is fine): write `${a[@]+"${a[@]}"}` in harnesses. In zsh a word starting with `=`
  is `=cmd` expansion, so `echo ====` fails.

Related: [[suite-lane-kill-and-check-traps]], [[production-suite-protocol]].
