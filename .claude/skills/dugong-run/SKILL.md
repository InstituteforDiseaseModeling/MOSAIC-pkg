---
name: dugong-run
description: >
  Start, stop, launch, monitor, and retrieve MOSAIC calibration runs on the dugong Azure VM
  (176 cores / 1.5 TiB, Ubuntu 24.04, deallocated when idle). Covers starting and stopping it
  (vm/dugong_power.sh up|down|status, through the admin server's ./dugongup and ./dugongdown),
  the libexpat LD_PRELOAD R wrapper (NOT hedgehog's GLIBCXX problem), the IP-pinned SSH alias,
  surviving SSH disconnect (nohup+logfile default / tmux for humans), the local-PSOCK
  control recipe, monitoring by tailing the log, and
  pulling results (tar-then-scp / pull_results.sh with HEDGEHOG_HOST=dugong).
  Use when the user wants to start or stop dugong, or run, monitor, or fetch a MOSAIC
  calibration on it. dugong is billed for every hour it runs: an agent that starts it must stop it
  (vm/dugong_power.sh down) as soon as its job is done or it is no longer needed.
---

# dugong-run — run MOSAIC on the dugong Azure VM

Canonical reference: `vm/DUGONG.md`. Helpers shared with hedgehog: `vm/launch_mosaic.R`
(single/multi-country), `vm/launch_mosaic_individual.R` (per-country loop with resume + compression),
`vm/pull_results.sh` (parameterized — set `HEDGEHOG_HOST=dugong`), `R/presets.R`.
Provisioning scripts that built this node: `claude/dugong_setup/` (`install_dugong*.sh`,
`build_venv_dugong.sh`, `make_wrappers_dugong.sh`, `smoke_dugong.R`).

> **Cost rule (mandatory).** dugong is billed for every hour it is running, busy or idle, and an
> idle dugong is the most common waste. An agent that starts dugong owns stopping it:
> 1. **Start it only when you are about to use it**, never "in case".
> 2. **Stop it as soon as your work on it is done**: after the run finishes and its results are
>    pulled (§5), or straight away if you started it and then did not need it, or a launch failed.
>    Run `vm/dugong_power.sh down` and confirm it ends with `dugong is DOWN`.
> 3. **A run that will outlive your session**: before you finish, tell the user plainly that dugong
>    is still running (and billed), which job is on it, roughly when it should finish, and the exact
>    command to stop it afterwards. Never end a session leaving dugong up without saying so.
> 4. **Do not stop a dugong you did not start for your own job** (the user's interactive session,
>    someone else's run) unless the user asks. `down` refuses while R runs or anyone is logged in;
>    never use `--force` to get past that without the user's say-so.
> 5. **At the start of any dugong task, run `vm/dugong_power.sh status`.** If it is up and idle and
>    nobody is expecting it, tell the user it is running unused and offer to stop it.

**dugong vs hedgehog — the differences that bite:**
| | dugong | hedgehog |
|---|---|---|
| Size | 176 cores / 1.5 TiB | 120 cores / 448 GB |
| OS | Ubuntu 24.04 | Ubuntu 20.04 |
| Allocation | **deallocated when idle**: `vm/dugong_power.sh up` / `down` (§0b) | deallocated between runs (`az vm start`) |
| SSH target | **IP-pinned** `52.183.44.192` (no DNS/PTR; kept across 3 stop/start cycles) | FQDN `hedgehog.westus2.cloudapp.azure.com` |
| Wrapper reason | **libexpat** symbol clash | **GLIBCXX_3.4.29** missing |

## 0. Prerequisites (verify; set up only if missing)
- **Is it up?** `vm/dugong_power.sh status` (exit 0 = up). If not, `vm/dugong_power.sh up` (§0b).
- **SSH to dugong:** `ssh -o BatchMode=yes dugong 'whoami'` → `jgiles`. If the alias is missing, add the
  `Host dugong` block from `vm/DUGONG.md` to `~/.ssh/config` (keyed on the IP `52.183.44.192` —
  no DNS resolves). If dugong is up but refuses your key, it was redeployed (fresh host key / wiped
  `authorized_keys`) → re-register from the Mac: `ssh-copy-id -i ~/.ssh/id_ed25519.pub dugong`.
- **SSH to the admin server** (only to start/stop): `ssh -o BatchMode=yes dugong-admin true`. The
  `Host dugong-admin` block (its address and your user there come from the team) lives only in your
  `~/.ssh/config`; never put them in this public repo. Set it up once: `ssh dugong-admin` (accept the
  host key, log in with the password), `exit`, then `ssh-copy-id -i ~/.ssh/id_ed25519.pub dugong-admin`,
  run in your own terminal (both prompt).
- **MOSAIC version on VM:** `ssh dugong '~/bin/r-mosaic-Rscript -e "cat(as.character(packageVersion(\"MOSAIC\")))"'`
  (note the wrapper — §1). If stale, update via §1a.
- **Engine version:** there is no separate one to check. The transmission engine is pure R as of
  MOSAIC v0.68.0, so the MOSAIC version above *is* the engine version. (The old check here imported
  `laser.cholera` and compared it against the wheel pinned in `inst/python/environment.yml`; the
  wheel left that file in v0.69.0.)

### 1a. Updating MOSAIC on dugong
Public repo — no GITHUB_PAT needed. Run via the wrapper (`~/bin/r-mosaic-R` in tmux for a long install):
```r
.libPaths(c("~/R/library", .libPaths()))
remotes::install_github("InstituteforDiseaseModeling/MOSAIC-pkg",
                        ref = "main", upgrade = "never", force = TRUE, lib = "~/R/library")
MOSAIC::install_dependencies(force = TRUE)   # rebuilds ~/.virtualenvs/r-mosaic from inst/python/environment.yml
```
`install_dependencies()` requires conda configured **conda-forge-only** or it dies on the Anaconda
commercial-channel ToS (see `vm/DUGONG.md` Provisioning gotchas). It is also no longer on the
critical path for a calibration: the Python env it builds now holds only numpy + TensorFlow, which
`est_suitability()` needs and `run_MOSAIC()` does not. Updating the R package is the whole update.
(There used to be a `pip install --force-reinstall <laser_cholera wheel>` recipe here for bumping
the engine without a full rebuild. The engine ships inside the R package now.)

## 0b. Start and stop dugong
dugong is deallocated when idle (no compute billing) and started for a run. The admin server holds
the Azure login; its scripts `./dugongup` (`az vm start`) and `./dugongdown` (`az vm deallocate`)
in your home directory there do the work. Drive them only through the tracked wrapper:
```bash
vm/dugong_power.sh status          # exit 0 up, 1 not reachable
vm/dugong_power.sh up              # ./dugongup (~2.3 min), then waits until SSH answers (~10 s more)
vm/dugong_power.sh down            # idle check, ./dugongdown (~1.6 min), waits until SSH stops answering
```
- **`up`** does nothing if dugong already answers.
- **`down` refuses (exit 2)** while any R process runs on dugong (anyone's) or anyone is logged in,
  and when dugong cannot be reached to check. Deallocating kills every running job, so never add
  `--force` to get past a busy check without knowing whose work it is.
- **Stop it when you are done** (the cost rule at the top): after pulling results (§5),
  `vm/dugong_power.sh down`. A run still going needs dugong up until it finishes; check with
  `status` before stopping, and if you will not be there when it ends, tell the user to stop it.
- **Safe by construction:** no hosts or credentials in the script (SSH aliases only), key login only
  (`BatchMode`, never prompts), alias names validated, one up/down at a time (lock in `$TMPDIR`),
  fixed remote commands (`./dugongup`, `./dugongdown`), every result checked over SSH, not assumed.
- Call the scripts as `./dugongup` / `./dugongdown`. The `dugongup` / `dugongdown` on the admin
  server's PATH (`/usr/local/bin`) are a broken earlier install (`sudo: /usr/local/sbin/dugong-ctl:
  command not found`) — do not use them.
- **What can go wrong** (see §8): the admin server's Azure login expires (only its owner can renew
  it); a VM this size can fail to start for lack of Azure capacity (`AllocationFailed`, retry later);
  the IP could change (it has not so far); your network may not be allowed to reach dugong.

## 1. The R wrapper — always use it
dugong is Ubuntu 24.04 (modern `libstdc++`, so NO GLIBCXX problem), but R links the older *system*
`libexpat 1.9.1` at startup; when reticulate later imports the conda env's `pyexpat` (built against
`libexpat 1.12.1`) the loader reuses the system copy and **PSOCK workers die with `undefined symbol:
XML_SetAllocTrackerActivationThreshold`**. Only an `LD_PRELOAD` of the venv libexpat fixes it. Run R
via `~/bin/r-mosaic-Rscript` (batch) or `~/bin/r-mosaic-R` (interactive). `check_dependencies()` and
TensorFlow-only work PASS without the wrapper (they skip the pyexpat worker path) — which masks
the bug, so use the wrapper anyway. Recreate it with `claude/dugong_setup/make_wrappers_dugong.sh` if lost.

> **Unverified, worth testing:** this whole `LD_PRELOAD` dance exists because PSOCK workers imported
> Python. Since v0.68.0 calibration workers are pure R and import nothing, so the wrapper may be
> unnecessary for `run_MOSAIC()` — and still required for `est_suitability()`, which does import
> Python. Nobody has tested this on the VM. Keep using the wrapper until someone does; it is
> harmless when unneeded.

## 2. Execution model — local PSOCK only
Simulations AND post-processing run on dugong's local cores; nothing leaves the VM.
`control$parallel$n_cores` IS the sim parallelism — 1.5 TiB RAM at **~1.0 GB/worker** (v0.73.0
measurement: VmHWM 992 MB, flat from 1 to 19 workers) means you can run very wide. **`n_cores = 170L`
is correct and now actually granted** — but only because the wrapper raises R's connection ceiling.

### R's connection ceiling (why the wrapper carries `--max-connections`)
R allocates a FIXED connection table at startup — 128 slots, 3 taken by stdin/stdout/stderr — and
every PSOCK worker holds one for its lifetime. So a default R build caps a cluster at ~125 workers
**regardless of core count**. Measured on dugong at `n_cores = 170`:

| | default R | wrapper (`--max-connections=512`) |
|---|---|---|
| calibration cluster | silently clamped to **123** | **170 granted** |
| ensemble cluster | **threw**, ensemble stage lost | **170**, 0.3 s |
| startup / parent fds / R RSS | 13.6 s / 129 / 84 GB | 16.6 s / 176 of 1024 / 121 GB of 1511 |

`~/bin/r-mosaic-Rscript` sets `--max-connections=512` (generated by the tracked
`vm/make_wrappers.sh`; regenerate with `bash vm/make_wrappers.sh` if lost). Three consequences:
- **Use the wrapper, not bare `Rscript`** — bare `Rscript` costs you 28% of the box, silently.
- It is a **startup-only** option: no env var, nothing in-session can raise it. A caller may
  override with a *later* flag (`r-mosaic-Rscript --max-connections=1024 my_run.R`); a flag placed
  after the script filename is ignored.
- If you ever run without it, `run_MOSAIC()` logs a `[WARN]` naming the budget and the remedy at
  startup, and every cluster clamps rather than throwing (MOSAIC >= v0.83.0).

The Coiled hybrid backend has been **removed** from the package, along with the worker image and its CI (v0.70.0). It was already scientifically
invalid (issue #113: the worker image lagged laser-cholera, so runs completed but gave low
R²/unconverged results), and the pure-R engine migration removes the reason it existed. `dask_spec`,
`check_coiled_workspace()` and `mosaic_dask_presets()` now raise an error rather than being ignored.

**This skill is on notice.** Per-worker RSS has now been measured (~1.0 GB at v0.73.0, above; the
old ~2 GB figure was the *Python* engine) and the R engine is ~2.4x faster than its first port, so a
176-core VM may no longer be needed for calibration. What is still unmeasured is end-to-end
calibration throughput on dugong itself. Keep the VM for wide sweeps and psi/LSTM training.

## 3. Stage and launch (run MUST survive SSH disconnect)
1. Stage the script: `scp my_run.R dugong:~/`.
2. Launch **detached with a logfile (agent default)** — survives disconnect, captures all output:
   ```bash
   ssh dugong 'cd ~ && nohup ~/bin/r-mosaic-Rscript ~/my_run.R </dev/null >run.log 2>&1 & echo "PID $!"'
   ```
   The wrapper already supplies `--max-connections=512`, so nothing extra is needed for a
   170-worker run. Confirm with the `[WARN]`/`Connection budget OK` line `run_MOSAIC()` logs at
   startup, or directly:
   ```bash
   ssh dugong '~/bin/r-mosaic-Rscript -e "cat(parallelly::availableConnections())"'   # expect 512
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
  parallel    = list(enable = TRUE, n_cores = 170L),   # dugong has 176; needs the wrapper (§2)
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
| `max_simulations` | `max_simulations_total` |
| `ess_method`      | `ESS_method`            |
| `best_subset_weighting` | `posterior_weighting` |

Retired in MOSAIC 1.4.0 (warned and ignored when set): `max_batches` / `max_batches_adaptive`,
`target_r2` / `target_r2_adaptive`, `max_batches_predictive`, `targets$ESS_param`, `ESS_param_prop`,
`batch_ratio_max`, `weights$floor`, `io$verbose_weights`. Adaptive mode stops on the convergence
status (`targets$ESS_best`, default 200, and `targets$status_thresholds`), after `min_batches_adaptive`
and `min_draws` (5,000), or at `max_simulations_total` (150,000). `ESS_best` now means the ensemble
ESS target (it was the tier target, 100, in 1.3.x): setting it warns. `min/max_best_subset` apply
only under `posterior_weighting = "saturated"`; the best-fit ensemble is bounded by
`min/max_ensemble_members` (30 / 1,000).

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
Then stop dugong unless another run needs it: `vm/dugong_power.sh down` (§0b).
`HEDGEHOG_REMOTE_DIR` defaults to `MOSAIC/output/individual` (correct for `launch_mosaic_individual.R`);
override it for a plain run, which writes one level up. `--list` lists remote archives without downloading.

## 6. Output structure (verify a run succeeded)
- `1_inputs/` — config/priors/control/environment JSON.
- `2_calibration/` — `samples.parquet`, `posterior/`, `diagnostics/` (`convergence_results.parquet`
  + `convergence_diagnostics.json`), `state/`. Presence of `2_calibration/diagnostics/` = past calibration.
- `3_results/` — `summary.json` (final R²/bias/ESS verdict; a FIXED-mode run reports `converged = FALSE`
  with `convergence_evaluated = FALSE` and status `completed_fixed` — expected, see run-mosaic §3),
  `predictions/`, `figures/`.

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
| `ssh dugong` times out | dugong deallocated (normal when idle) | `vm/dugong_power.sh up` (§0b) |
| `up`: `./dugongup failed`, output mentions login / `AADSTS` / `az login` | the admin server's Azure login expired | ask its owner to run `az login` on the admin server; nothing to fix locally |
| `up`: `AllocationFailed` / `ZonalAllocationFailed` | no Azure capacity for this VM size right now | retry later; dugong stays deallocated meanwhile |
| `up`: started, but `SSH did not answer` | still booting, IP changed, or your network not allowed | `status` again in a minute; if still down, check the IP with the admin; it is billed while up, so `down --force` if you cannot reach it |
| `down` refuses: `dugong is in use` | an R process runs or someone is logged in | wait, or confirm with its owner; `--force` kills their work |
| `Host key verification failed` / `Permission denied` on `dugong-admin` | admin server not set up for key login | §0 (`ssh dugong-admin` once, then `ssh-copy-id`) |
| `sudo: /usr/local/sbin/dugong-ctl: command not found` | called the admin server's PATH `dugongup` | use `vm/dugong_power.sh` (it calls `./dugongup`) |
| `rjags`/`mobility` fail to load | no JAGS (Suggests-only) | `sudo apt install jags` only if you need the `mobility` tools |
