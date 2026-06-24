---
name: reference-dugong-vm
description: dugong Azure compute VM — second large MOSAIC run host (176c/1.5TiB, Ubuntu 24.04, persistent IP); where its skill/docs/setup live and the libexpat-wrapper trap
metadata:
  type: reference
---

**dugong** is the second large Azure compute VM for MOSAIC calibration (alongside
[[project_hedgehog_run_infra]]). Provisioned + brought to laser-cholera 0.16.0 in
the 2026-06-22/23 thread.

- **ssh dugong** — user `jgiles`, key `~/.ssh/id_ed25519`, **IP-pinned** `52.183.44.192`
  (no DNS/PTR resolves; config keyed on the IP). **Persistently allocated** — never
  `az vm start`; unreachable = redeploy → `ssh-copy-id` from the Mac.
- **176 cores / 1.5 TiB RAM, Ubuntu 24.04.** Bigger than hedgehog (120c/448GB).
- **R wrapper is mandatory: `~/bin/r-mosaic-Rscript` / `~/bin/r-mosaic-R`.** Reason is
  **libexpat** (NOT hedgehog's GLIBCXX): bare `Rscript` → PSOCK workers die with
  `undefined symbol: XML_SetAllocTrackerActivationThreshold`. `check_dependencies()`
  passes WITHOUT the wrapper (skips the laser worker path), which masks the bug.
- **Backend:** local PSOCK (omit `dask_spec`) **OR Coiled hybrid as the Dask CLIENT**
  (validated 2026-06-24, see [[project_coiled_dugong_client_validated]]). dugong dask/distributed
  = 2026.6.0 matches the rebuilt worker image, so the client↔image skew that stalled the laptop
  client (dask 2026.3.0) is gone here. **Coiled needs an LD_PRELOAD fix**, see that note.
- **Results:** `~/MOSAIC/output/<DIR>/`; tar then `scp`, or `HEDGEHOG_HOST=dugong
  vm/pull_results.sh` (the helper is parameterized — works for dugong unchanged).

**Where the resources live (mirrors hedgehog):**
- Agent skill: `.claude/skills/dugong-run/SKILL.md` (registered in `.claude/agents/README.md`
  + named in `MOSAIC-pkg/CLAUDE.md`).
- Canonical doc: `vm/DUGONG.md`.
- Provisioning scripts (built this node): `claude/dugong_setup/` (`install_dugong*.sh`,
  `build_venv_dugong.sh`, `make_wrappers_dugong.sh`, `smoke_dugong.R`).
- Shared `vm/` helpers (host-agnostic): `launch_mosaic.R`, `launch_mosaic_individual.R`,
  `pull_results.sh`, `presets.R`, `check_coiled.R`.

**Provisioning gotchas (from DUGONG.md):** conda must be conda-forge-only (`~/.condarc`
`default_channels:[conda-forge]` + `channel_priority: strict`) or `install_dependencies()`
dies on the Anaconda commercial ToS; build `fs`/`sensitivity`/`shiny` with
`USE_BUNDLED_LIBUV=1` (no `libuv1-dev`, sudo needs a password); `rjags`/`mobility` are
Suggests-only so MOSAIC installs without JAGS.

Engine bump recipe (no full rebuild): `pip install --no-deps --force-reinstall <wheel-url>`
into `~/.virtualenvs/r-mosaic`.
