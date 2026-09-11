---
name: hedgehog-run
description: >
  Launch, monitor, and retrieve MOSAIC calibration runs on the hedgehog Azure VM
  (120 cores / 448 GB). Covers the GLIBCXX R wrapper, surviving SSH disconnect
  (nohup+logfile default / tmux for humans), the control recipe, monitoring by
  tailing the log, and pulling results (tar-then-pull_results.sh/scp).
  Use when the user wants to run, monitor, or fetch a MOSAIC calibration on hedgehog.
---

# hedgehog-run — run MOSAIC on the hedgehog Azure VM

Canonical reference: `vm/HEDGEHOG.md`. Helpers: `vm/launch_mosaic.R` (single/multi-country),
`vm/launch_mosaic_individual.R` (per-country loop with resume + compression), `vm/pull_results.sh`,
`vm/setup_mosaic.sh` / `vm/setup_mosaic_minimal.sh`, `R/presets.R`.

These `vm/` templates are correct as of MOSAIC v0.43.0 — use canonical control names, `NULL` (not
`'auto'`) for adaptive mode, and the resume check points at the real `2_calibration/` artifacts. Copy
them or use the inline recipe in §3.

## 0. Prerequisites (verify; set up only if missing)
- **SSH:** `ssh -o BatchMode=yes hedgehog 'whoami'` → `jgiles`. If the alias is missing, add the
  `Host hedgehog` block from `vm/HEDGEHOG.md` to `~/.ssh/config`. Unreachable = likely deallocated:
  `az vm start -g <rg> -n hedgehog`, then re-check (FQDN follows the VM).
- **MOSAIC version on VM:** `ssh hedgehog '~/bin/r-mosaic-Rscript -e "cat(as.character(packageVersion(\"MOSAIC\")))"'`
  (note the wrapper — §1). Expect to match `main`. If stale, update in tmux:
  `install_git(... ref="main", force=TRUE)` then `install_dependencies(force=TRUE)`.

## 1. The R wrapper — always use it
hedgehog is Ubuntu 20.04; its system `libstdc++` lacks `GLIBCXX_3.4.29` needed by the venv's
compiled wheels (pyarrow/numba/laser_core). Run R via `~/bin/r-mosaic-Rscript` (batch) or
`~/bin/r-mosaic-R` (interactive). Plain `Rscript` makes `check_dependencies()` report "BROKEN" and
laser won't import. Invoke the wrapper **normally** (shebang repaired 2026-06-16; setup scripts now
guard it). Only if you hit `Syntax error: "(" unexpected`: check `head -1 ~/bin/r-mosaic-Rscript`
is `#!/usr/bin/env bash` at column 0, or prefix `bash` as a stop-gap.

## 2. Execution model — local PSOCK only
Simulations AND post-processing run on hedgehog's local cores; nothing leaves the VM. A single knob,
`control$parallel$n_cores`, IS the sim parallelism (118 of 120 cores ≈ comfortable on 448 GB).

The Coiled hybrid backend (remote Dask workers with hedgehog as client) has been **removed** from
the package. It was already scientifically invalid (issue #113: the worker image lagged
laser-cholera, so runs completed but gave low R²/unconverged results), and the pure-R engine
migration removes the reason it existed. `dask_spec`, `check_coiled_workspace()` and `mosaic_dask_presets()`
now raise an error rather than being ignored.

**This skill is on notice.** Once the R engine's real throughput and per-worker memory are measured
(migrate-laser-r.md phase A-3a), a 120-core VM may no longer be needed for calibration at all. Keep
it for wide sweeps and psi/LSTM training until those numbers exist.

## 3. Stage and launch (run MUST survive SSH disconnect)
1. Stage the script: `scp my_run.R hedgehog:~/`.
2. Launch **detached with a logfile (agent default)** — survives disconnect, captures all output:
   ```bash
   ssh hedgehog 'cd ~ && nohup ~/bin/r-mosaic-Rscript ~/my_run.R </dev/null >run.log 2>&1 & echo "PID $!"'
   ```
   Interactive humans may prefer `tmux` instead: `ssh hedgehog`, `tmux new -s mosaic` (detach
   Ctrl-b d, reattach `tmux attach -t mosaic`). Do NOT run a multi-hour job in a bare foreground SSH
   session.

### Single-country recipe (local PSOCK) — canonical control names
```r
.libPaths(c('~/R/library', .libPaths()))
library(MOSAIC); MOSAIC::attach_mosaic_env(silent = TRUE); set_root_directory("~/MOSAIC")
iso    <- "MOZ"
config <- get_location_config(iso = iso); priors <- get_location_priors(iso = iso)
ctrl <- mosaic_control_defaults(
  calibration = list(n_simulations = 25000L,   # integer = fixed; NULL = adaptive (NOT the string 'auto')
                     n_iterations  = 3L),
  parallel    = list(enable = TRUE, n_cores = 118L),
  paths       = list(plots = TRUE)
)
# Single-location: disable multi-location params
ctrl$sampling$sample_tau_i <- ctrl$sampling$sample_mobility_gamma <-
  ctrl$sampling$sample_mobility_omega <- FALSE
run_MOSAIC(config, priors, dir_output = file.path("~/MOSAIC/output", iso), control = ctrl)
```

### Hybrid variant (only once #113 is fixed)
```r
ctrl  <- mosaic_control_defaults(
  calibration = list(n_simulations = 25000L, n_iterations = 3L),
  parallel    = list(enable = TRUE, n_cores = 100L))   # CLIENT cores
run_MOSAIC(config, priors, dir_output = "~/MOSAIC/output/MOZ", control = ctrl)
```

### Canonical control names (use these — the deprecated forms migrate WITH A WARNING)
| deprecated        | canonical               |
|-------------------|-------------------------|
| `batch_size`      | `batch_size_adaptive`   |
| `min_batches`     | `min_batches_adaptive`  |
| `max_batches`     | `max_batches_adaptive`  |
| `target_r2`       | `target_r2_adaptive`    |
| `max_simulations` | `max_simulations_total` |
| `ess_method`      | `ESS_method`            |

## 4. Monitor (tail the log)
```bash
ssh hedgehog 'tail -n 40 ~/run.log'                                    # progress
ssh hedgehog 'pgrep -af r-mosaic-Rscript || pgrep -af Rscript'         # alive? (-af shows full cmd)
ssh hedgehog 'ls ~/MOSAIC/output/MOZ/2_calibration/diagnostics/ 2>&1'  # got past calibration?
```
A "RUNNING" pgrep hit right after launch can be pgrep matching your own check command; confirm with
`-af` before concluding anything.

## 5. Retrieve results (tar, then pull)
Outputs land in `~/MOSAIC/output/<DIR>/`. `vm/pull_results.sh` only fetches a pre-existing
`*.tar.gz` — **it does NOT compress** (the `launch_mosaic*.R` scripts compress at end). For a plain
`run_MOSAIC`, tar first, then pull:
```bash
ssh hedgehog 'tar -czf ~/MOSAIC/output/MOZ.tar.gz -C ~/MOSAIC/output MOZ'
HEDGEHOG_REMOTE_DIR=MOSAIC/output vm/pull_results.sh --dest ./output MOZ
# or by hand: scp hedgehog:~/MOSAIC/output/MOZ.tar.gz ./output/
```
`pull_results.sh` defaults `HEDGEHOG_REMOTE_DIR=MOSAIC/output/individual` (correct for
`launch_mosaic_individual.R`); override it for a plain run, which writes one level up.
`vm/pull_results.sh --list` lists remote archives without downloading.

## 6. Output structure (verify a run succeeded)
- `1_inputs/` — config/priors/control/environment JSON.
- `2_calibration/` — `samples.parquet`, `posterior/`, `diagnostics/` (`convergence_results.parquet`
  + `convergence_diagnostics.json`), `state/`. Presence of `2_calibration/diagnostics/` = past calibration.
- `3_results/` — `summary.json` (final R²/bias/ESS verdict), `predictions/`, `figures/`.

## 7. Smoke test before any large run
Stage a tiny job (`iso="MOZ"`, `n_simulations=100L`, `n_iterations=1L`, `n_cores=8L`), launch via §3,
confirm it reaches a completion summary and writes `2_calibration/diagnostics/`, pull and inspect.
Only then launch the full calibration.

## 8. Troubleshooting
| Symptom | Cause | Fix |
|---|---|---|
| `GLIBCXX_3.4.29 not found` / check_dependencies "BROKEN" | ran via plain `Rscript`, not the wrapper | use `~/bin/r-mosaic-Rscript` (§1) |
| `Syntax error: "(" unexpected` | wrapper shebang corrupted + invoked bare | check `head -1` shebang at col 0, repair, or prefix `bash` |
| hybrid run finishes but R² implausibly low | #113 worker-image version mismatch | use local PSOCK until #113 fixed |
| `Permission denied (publickey)` after it worked | VM redeployed (fresh host key) | `ssh-copy-id` from the Mac |
| VM unreachable | deallocated | `az vm start …`, FQDN follows the VM |

