# dugong — Azure compute VM access

`dugong` is a large, **persistently-allocated** Azure VM for MOSAIC calibration runs. This file
documents how to reach it and move results. **No secrets here** — keys live in `~/.ssh/`.

Unlike `hedgehog` (see `vm/HEDGEHOG.md`), dugong stays allocated (not deallocated between runs) and
runs **Ubuntu 24.04** — so the *GLIBCXX* problem does not apply. BUT a **libexpat** mismatch still
requires the `r-mosaic-*` LD_PRELOAD wrapper (see "Running R on dugong" below): always run MOSAIC via
`~/bin/r-mosaic-Rscript`, not bare `Rscript`.

## Specs
- **176 cores, 1.5 TiB RAM** (Ubuntu 24.04.2 LTS). ~936 GB free on `/`.
- Much larger than hedgehog (120 cores / 448 GB) — good for big multi-country or full-metapop batches.

## Connection

| Field | Value |
|-------|-------|
| SSH alias | `dugong` (defined in `~/.ssh/config`) |
| User | `jgiles` |
| Public IP | `52.183.44.192` (stable — VM stays allocated; no FQDN/PTR resolves) |
| Key | `~/.ssh/id_ed25519` |

```bash
ssh dugong                       # via the config alias (recommended)
ssh jgiles@52.183.44.192
```

No DNS name resolves for dugong (the `*.cloudapp.azure.com` guess is NXDOMAIN and there is no PTR),
so the SSH config is keyed on the **IP**. Because the VM stays allocated the IP is stable; if a DNS
name is later assigned, prefer it over the raw IP.

`~/.ssh/config` block:
```sshconfig
Host dugong
  HostName 52.183.44.192
  User jgiles
  IdentityFile ~/.ssh/id_ed25519
  AddKeysToAgent yes
  ServerAliveInterval 60
  ServerAliveCountMax 3
```

## Running R on dugong — use the `r-mosaic-*` wrapper
dugong is Ubuntu 24.04 with a modern `libstdc++`, so the **GLIBCXX** problem hedgehog has does NOT
apply. HOWEVER there is a separate **libexpat** trap: R links the older *system* `libexpat`
(`1.9.1`) at startup, so when reticulate later imports the conda env's `pyexpat` (built against the
venv's `libexpat 1.12.1`) the loader reuses the already-loaded system copy and PSOCK workers die with
`undefined symbol: XML_SetAllocTrackerActivationThreshold`. Only `LD_PRELOAD` of the venv libexpat
fixes it. So **always run MOSAIC via the wrapper**:
```bash
~/bin/r-mosaic-Rscript script.R     # batch
~/bin/r-mosaic-R                     # interactive
```
The wrapper `LD_PRELOAD`s the venv `libexpat.so.1` (and `libstdc++.so.6`, harmless here), sets
`R_LIBS_USER=~/R/library` and `RETICULATE_PYTHON`. `check_dependencies()` and TensorFlow-only work
pass without it (they skip the laser/pyexpat worker path), which masks the problem — use the wrapper
anyway. Recreate it with `claude/dugong_setup/make_wrappers_dugong.sh` if lost.

The MOSAIC R package installs to the user library `~/R/library` (set via `~/.Renviron`
`R_LIBS_USER`). conda lives at `~/miniconda`; the MOSAIC Python env is the conda env
`~/.virtualenvs/r-mosaic` built by `MOSAIC::install_dependencies()`.

## Updating MOSAIC on dugong
The repo is public, so no GITHUB_PAT is needed. Run via the wrapper (`~/bin/r-mosaic-R`):
```r
.libPaths(c("~/R/library", .libPaths()))
remotes::install_github("InstituteforDiseaseModeling/MOSAIC-pkg",
                        ref = "main", upgrade = "never", force = TRUE, lib = "~/R/library")
MOSAIC::install_dependencies(force = TRUE)   # rebuilds ~/.virtualenvs/r-mosaic from inst/python/environment.yml
```
NOTE: `install_dependencies()` needs conda configured for **conda-forge only** (see Provisioning
gotchas) or it dies on the Anaconda commercial-channel ToS.

## Results layout
- Home: `/home/jgiles/`
- Calibration results: `~/MOSAIC/output/<DIR>/` (`1_inputs/`, `2_calibration/`, `3_results/`).
- `vm/pull_results.sh` only **fetches** a pre-existing `*.tar.gz` — it does NOT compress. For a plain
  `run_MOSAIC`, tar first then pull (see hedgehog notes); set `HEDGEHOG_REMOTE_DIR` if not using the
  `individual/` layout.

## Pulling results
```bash
ssh dugong 'tar -czf ~/MOSAIC/output/MOZ.tar.gz -C ~/MOSAIC/output MOZ'
scp dugong:~/MOSAIC/output/MOZ.tar.gz ./output/
```

## Long-running jobs
A multi-hour calibration MUST survive an SSH disconnect:
- **`nohup` + logfile (default for scripted/agent launches):**
  ```bash
  ssh dugong
  nohup ~/bin/r-mosaic-Rscript ~/launch_mosaic.R </dev/null >run.log 2>&1 &
  tail -f run.log
  ```
- **`tmux` (interactive humans):** `tmux new -s mosaic` (detach Ctrl-b d; reattach `tmux attach -t mosaic`).

## Backend
Use **local PSOCK** (omit `dask_spec`); `control$parallel$n_cores` is the sim parallelism — dugong has
176 cores / 1.5 TiB, so you can run very wide. The Coiled hybrid path is currently scientifically
invalid (issue #113); use local PSOCK.

## Troubleshooting
- **`Permission denied (publickey)` after it worked** → VM redeployed (fresh host key + wiped
  `authorized_keys`). Re-register the key **from your Mac**: `ssh-copy-id -i ~/.ssh/id_ed25519.pub dugong`.
- **`check_dependencies()` reports a Python problem** → rebuild the env via the wrapper:
  `~/bin/r-mosaic-Rscript -e 'MOSAIC::install_dependencies(force = TRUE)'` (uses the existing `~/miniconda`).
- **PSOCK workers die with `undefined symbol: XML_SetAllocTrackerActivationThreshold`** → ran via bare
  `Rscript`. Use `~/bin/r-mosaic-Rscript` (libexpat preload).

## Provisioning gotchas (fresh-install lessons, 2026-06-22)
Install scripts that produced a working node: `claude/dugong_setup/` (`install_dugong*.sh`,
`build_venv_dugong.sh`, `make_wrappers_dugong.sh`, `smoke_dugong.R`). Three traps, all resolved:
1. **`fs` 2.1.0 needs libuv** (`uv.h`); dugong lacks `libuv1-dev` and **sudo needs a password**. Build
   `fs` (and its dependents `dtwclust`←`sensitivity`, `shiny`/`ggExtra`) with `export
   USE_BUNDLED_LIBUV=1`. `sensitivity` is a hard Import, so this blocks MOSAIC itself.
2. **conda 26.x ToS block** on Anaconda default channels. Fix (ToS-free, org-clean): write `~/.condarc`
   with `channels: [conda-forge]` AND **`default_channels: [conda-forge]`** + `channel_priority:
   strict`. `nodefaults` alone is NOT enough — conda keeps a built-in `defaults`→repo.anaconda.com.
3. **`rjags`/`mobility` fail** (no JAGS) but are Suggests-only, so MOSAIC still installs. Install JAGS
   (`sudo apt install jags`) only if you need the `mobility` tools.
