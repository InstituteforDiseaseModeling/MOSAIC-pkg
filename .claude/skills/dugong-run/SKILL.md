---
name: dugong-run
description: >
  Launch, monitor, and retrieve MOSAIC calibration runs on the dugong Azure VM
  (176 cores / 1.5 TiB, Ubuntu 24.04, persistently allocated). Covers the libexpat
  LD_PRELOAD R wrapper (NOT hedgehog's GLIBCXX problem), the IP-pinned SSH alias,
  surviving SSH disconnect (nohup+logfile default / tmux for humans), the local-PSOCK
  control recipe (Coiled hybrid is #113-invalid), monitoring by tailing the log, and
  pulling results (tar-then-scp / pull_results.sh with HEDGEHOG_HOST=dugong).
  Use when the user wants to run, monitor, or fetch a MOSAIC calibration on dugong.
---

# dugong-run — run MOSAIC on the dugong Azure VM

Canonical reference: `vm/DUGONG.md`. Helpers shared with hedgehog: `vm/launch_mosaic.R`
(single/multi-country), `vm/launch_mosaic_individual.R` (per-country loop with resume + compression),
`vm/pull_results.sh` (parameterized — set `HEDGEHOG_HOST=dugong`), `R/presets.R`, `R/check_coiled.R`.
Provisioning scripts that built this node: `claude/dugong_setup/` (`install_dugong*.sh`,
`build_venv_dugong.sh`, `make_wrappers_dugong.sh`, `smoke_dugong.R`).

**dugong vs hedgehog — the differences that bite:**
| | dugong | hedgehog |
|---|---|---|
| Size | 176 cores / 1.5 TiB | 120 cores / 448 GB |
| OS | Ubuntu 24.04 | Ubuntu 20.04 |
| Allocation | **persistent** (never deallocated) | deallocated between runs (`az vm start`) |
| SSH target | **IP-pinned** `52.183.44.192` (no DNS/PTR) | FQDN `hedgehog.westus2.cloudapp.azure.com` |
| Wrapper reason | **libexpat** symbol clash | **GLIBCXX_3.4.29** missing |

## 0. Prerequisites (verify; set up only if missing)
- **SSH:** `ssh -o BatchMode=yes dugong 'whoami'` → `jgiles`. If the alias is missing, add the
  `Host dugong` block from `vm/DUGONG.md` to `~/.ssh/config` (keyed on the IP `52.183.44.192` —
  no DNS resolves). dugong stays allocated, so unreachable usually means a redeploy (fresh host
  key / wiped `authorized_keys`) → re-register from the Mac: `ssh-copy-id -i ~/.ssh/id_ed25519.pub dugong`.
- **MOSAIC version on VM:** `ssh dugong '~/bin/r-mosaic-Rscript -e "cat(as.character(packageVersion(\"MOSAIC\")))"'`
  (note the wrapper — §1). If stale, update via §1a.
- **Engine version (laser-cholera):** `ssh dugong '~/.virtualenvs/r-mosaic/bin/python -c "import laser.cholera as lc; print(lc.__version__)"'`.
  Keep this in lockstep with the wheel pinned in `inst/python/environment.yml`. (Updated to 0.16.0 on 2026-06-22.)

### 1a. Updating MOSAIC / the engine on dugong
Public repo — no GITHUB_PAT needed. Run via the wrapper (`~/bin/r-mosaic-R` in tmux for a long install):
```r
.libPaths(c("~/R/library", .libPaths()))
remotes::install_github("InstituteforDiseaseModeling/MOSAIC-pkg",
                        ref = "main", upgrade = "never", force = TRUE, lib = "~/R/library")
MOSAIC::install_dependencies(force = TRUE)   # rebuilds ~/.virtualenvs/r-mosaic from inst/python/environment.yml
```
`install_dependencies()` requires conda configured **conda-forge-only** or it dies on the Anaconda
commercial-channel ToS (see `vm/DUGONG.md` Provisioning gotchas). To bump ONLY the engine wheel
without a full rebuild:
```bash
ssh dugong '~/.virtualenvs/r-mosaic/bin/pip install --no-deps --force-reinstall \
  https://github.com/InstituteforDiseaseModeling/laser-cholera/releases/download/v0.16.0/laser_cholera-0.16.0-py3-none-any.whl'
```

## 1. The R wrapper — always use it
dugong is Ubuntu 24.04 (modern `libstdc++`, so NO GLIBCXX problem), but R links the older *system*
`libexpat 1.9.1` at startup; when reticulate later imports the conda env's `pyexpat` (built against
`libexpat 1.12.1`) the loader reuses the system copy and **PSOCK workers die with `undefined symbol:
XML_SetAllocTrackerActivationThreshold`**. Only an `LD_PRELOAD` of the venv libexpat fixes it. Run R
via `~/bin/r-mosaic-Rscript` (batch) or `~/bin/r-mosaic-R` (interactive). `check_dependencies()` and
TensorFlow-only work PASS without the wrapper (they skip the laser/pyexpat worker path) — which masks
the bug, so use the wrapper anyway. Recreate it with `claude/dugong_setup/make_wrappers_dugong.sh` if lost.

## 2. Choose a backend
### (a) Local PSOCK — everything on dugong. RECOMMENDED.
LASER sims AND post-processing run on dugong's local cores; nothing leaves the VM. Enabled by
**omitting `dask_spec`**. `control$parallel$n_cores` IS the sim parallelism — 1.5 TiB RAM at ~2 GB/worker
means you can run very wide (170+ of 176 cores is comfortable). Engine = dugong's laser-cholera
end-to-end → VALID.

### (b) Coiled hybrid — sims on a Coiled cloud cluster, dugong is the Dask client.
**Currently scientifically INVALID** — the worker image lags laser-cholera (issue #113): runs complete
but give low R²/unconverged results. Use (a) until #113 is resolved. `save_simresults` is rejected on
this path; you may not switch backends across a `resume`.

## 3. Stage and launch (run MUST survive SSH disconnect)
1. Stage the script: `scp my_run.R dugong:~/`.
2. Launch **detached with a logfile (agent default)** — survives disconnect, captures all output:
   ```bash
   ssh dugong 'cd ~ && nohup ~/bin/r-mosaic-Rscript ~/my_run.R </dev/null >run.log 2>&1 & echo "PID $!"'
   ```
   Interactive humans may prefer `tmux`: `ssh dugong`, `tmux new -s mosaic` (detach Ctrl-b d, reattach
   `tmux attach -t mosaic`). Do NOT run a multi-hour job in a bare foreground SSH session.

### Single-country recipe (local PSOCK) — canonical control names
```r
.libPaths(c('~/R/library', .libPaths()))
library(MOSAIC); MOSAIC::attach_mosaic_env(silent = TRUE); set_root_directory("~/MOSAIC")
iso    <- "MOZ"
config <- get_location_config(iso = iso); priors <- get_location_priors(iso = iso)
ctrl <- mosaic_control_defaults(
  calibration = list(n_simulations = 25000L,   # integer = fixed; NULL = adaptive (NOT the string 'auto')
                     n_iterations  = 3L),
  parallel    = list(enable = TRUE, n_cores = 170L),   # dugong has 176
  paths       = list(plots = TRUE)
)
# Single-location: disable multi-location params
ctrl$sampling$sample_tau_i <- ctrl$sampling$sample_mobility_gamma <-
  ctrl$sampling$sample_mobility_omega <- FALSE
run_MOSAIC(config, priors, dir_output = file.path("~/MOSAIC/output", iso), control = ctrl)
```

### Canonical control names (use these — deprecated forms migrate WITH A WARNING)
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
ssh dugong 'tail -n 40 ~/run.log'                                    # progress
ssh dugong 'pgrep -af r-mosaic-Rscript || pgrep -af Rscript'         # alive? (-af shows full cmd)
ssh dugong 'ls ~/MOSAIC/output/MOZ/2_calibration/diagnostics/ 2>&1'  # got past calibration?
```
A "RUNNING" pgrep hit right after launch can be pgrep matching your own check command; confirm with
`-af` before concluding anything.

## 5. Retrieve results (tar, then pull)
Outputs land in `~/MOSAIC/output/<DIR>/`. `vm/pull_results.sh` only fetches a pre-existing `*.tar.gz`
— it does **NOT** compress (the `launch_mosaic*.R` scripts compress at end). For a plain `run_MOSAIC`,
tar first, then pull. `pull_results.sh` is parameterized — point it at dugong with `HEDGEHOG_HOST`:
```bash
ssh dugong 'tar -czf ~/MOSAIC/output/MOZ.tar.gz -C ~/MOSAIC/output MOZ'
HEDGEHOG_HOST=dugong HEDGEHOG_REMOTE_DIR=MOSAIC/output vm/pull_results.sh --dest ./output MOZ
# or by hand: scp dugong:~/MOSAIC/output/MOZ.tar.gz ./output/
```
`HEDGEHOG_REMOTE_DIR` defaults to `MOSAIC/output/individual` (correct for `launch_mosaic_individual.R`);
override it for a plain run, which writes one level up. `--list` lists remote archives without downloading.

## 6. Output structure (verify a run succeeded)
- `1_inputs/` — config/priors/control/environment JSON.
- `2_calibration/` — `samples.parquet`, `posterior/`, `diagnostics/` (`convergence_results.parquet`
  + `convergence_diagnostics.json`), `state/`. Presence of `2_calibration/diagnostics/` = past calibration.
- `3_results/` — `summary.json` (final R²/bias/ESS verdict), `predictions/`, `figures/`.

## 7. Smoke test before any large run
Stage a tiny job (`iso="MOZ"`, `n_simulations=100L`, `n_iterations=1L`, `n_cores=8L`), launch via §3,
confirm it reaches a completion summary and writes `2_calibration/diagnostics/`, pull and inspect.
Only then launch the full calibration. (`claude/dugong_setup/smoke_dugong.R` is a ready template.)

## 8. Troubleshooting
| Symptom | Cause | Fix |
|---|---|---|
| PSOCK workers die `undefined symbol: XML_SetAllocTrackerActivationThreshold` | ran via bare `Rscript`, not the wrapper | use `~/bin/r-mosaic-Rscript` (§1, libexpat preload) |
| `check_dependencies()` reports a Python problem | env drift | `~/bin/r-mosaic-Rscript -e 'MOSAIC::install_dependencies(force=TRUE)'` (uses existing `~/miniconda`) |
| `install_dependencies()` dies on Anaconda ToS | conda not conda-forge-only | write `~/.condarc` `channels:[conda-forge]` + `default_channels:[conda-forge]` + `channel_priority: strict` |
| `fs`/`sensitivity`/`shiny` build fails (`uv.h`) | no `libuv1-dev`, sudo needs password | `export USE_BUNDLED_LIBUV=1` before the R install |
| hybrid run finishes but R² implausibly low | #113 worker-image version mismatch | use local PSOCK until #113 fixed |
| `Permission denied (publickey)` after it worked | VM redeployed (fresh host key) | `ssh-copy-id -i ~/.ssh/id_ed25519.pub dugong` from the Mac |
| `rjags`/`mobility` fail to load | no JAGS (Suggests-only) | `sudo apt install jags` only if you need the `mobility` tools |
