---
name: docker-image-update
description: >
  Rebuild and publish the MOSAIC Coiled worker Docker image
  (idmmosaicacr.azurecr.io/mosaic-worker:latest): backup-tag the current latest,
  build (cross-arch on Apple Silicon), push to Azure Container Registry, refresh
  the Coiled software environment, smoke test, and roll back. Use when the user
  wants to update/rebuild/publish the worker image or refresh the Coiled env after
  a MOSAIC version bump.
---

# docker-image-update — rebuild & publish the MOSAIC Coiled worker image

Helpers (turnkey path — prefer these): **`azure/rebuild_image.sh`** (backup → amd64 build → tag →
push → smoke) + **`azure/refresh_coiled_env.py`** (Coiled env refresh). The full by-hand procedure
(7 steps + rollback) is in §3–§4 below; `azure/DOCKER_IMAGE_UPDATE.md` is the same as a human-facing
prose walkthrough. Registry provenance: `azure/ACR_SETUP.md`. Build recipe:
`azure/Dockerfile`. Coiled/storage wiring: `azure/CONFIGURE_COILED_WS_FOR_AZURE_STORAGE.md`,
`azure/DOCKER_COILED_SUMMARY.md`. This skill is the operational index over those scripts and docs —
the gotchas below are what those docs do **not** say and the scripts already bake in.

## Quick reference

| Item | Value |
|------|-------|
| ACR login server | `idmmosaicacr.azurecr.io` |
| Image | `mosaic-worker` (tag `latest` + `v<pkg-version>`) |
| Coiled software env | `mosaic-acr-workers` |
| Coiled workspace | `idm-coiled-idmad-r2` |
| Resource group / subscription | `rg-coiled-tting` / IDM Research 2 |
| Dockerfile | `azure/Dockerfile` (no `COPY` from context — installs via `install_github`, clones data via git) |
| `LABEL version=` | `azure/Dockerfile` **line 5** |

## Turnkey path (default)
After the prerequisites (§0), the whole update is two commands from the repo root:
```bash
azure/rebuild_image.sh                 # backup → amd64 build → tag :latest+:v<ver> → push → smoke
python azure/refresh_coiled_env.py     # delete+recreate the mosaic-acr-workers Coiled env
```
`rebuild_image.sh` derives the version from `DESCRIPTION`, syncs the Dockerfile `LABEL`, always lands
a backup tag first, forces `--platform linux/amd64`, and prompts before pushing (`--yes` to skip).
Flags: `--ref <git-ref>` (default `main`), `--no-backup`, `--skip-smoke`, `--yes`. The gotchas in
§1–§2 explain *why* the script does what it does; read §3 only for the by-hand path or to debug a
failed step.

## 0. Prerequisites (verify; set up only if missing)
- **Docker running:** `docker info >/dev/null` succeeds.
- **Azure CLI + right subscription:** `az account show --query name -o tsv` → IDM Research 2; else
  `az login` then `az account set --subscription "IDM Research 2"`.
- **ACR auth:** `az acr login --name idmmosaicacr`. The token **expires after a few hours** — if a
  `docker push` later fails with auth errors, just re-run this.
- **Coiled (only for Step 6):** `coiled` importable in the active Python env and logged in to
  workspace `idm-coiled-idmad-r2`.

## 1. Cross-architecture build — the #1 gotcha (Apple Silicon)
The session host is Apple Silicon (arm64); **Coiled workers are x86_64**. A plain `docker build` on a
Mac produces an arm64 image that pulls fine but **fails to start on Coiled**. ALWAYS build for amd64:
```bash
docker build --platform linux/amd64 -f azure/Dockerfile -t mosaic-worker:latest .
```
A bare `docker build` omits `--platform`; `rebuild_image.sh` always sets it (on a native x86_64
builder it's a harmless no-op). Build takes ~15–25 min; confirm the verify layer prints its **three `✓`**
assertions (laser-cholera version, `calc_model_likelihood` importable, MOSAIC version).

## 2. Version is derived, never hand-typed
The tag and the Dockerfile `LABEL` must track the package, not a number typed from memory:
```bash
grep '^Version:' DESCRIPTION        # source of truth, e.g. 0.44.1
```
- Tag the build `v<that version>` in addition to `latest` (§3 Step 4).
- If `LABEL version=` on `azure/Dockerfile` line 5 differs, update it to match (label is metadata
  only — the build installs MOSAIC via `remotes::install_github(ref=MOSAIC_REF)`, default `main`; the
  label does not affect what code ships). To pin a non-`main` build, pass
  `--build-arg MOSAIC_REF=<tag-or-sha>`.

## 3. Procedure (by hand — `azure/rebuild_image.sh` automates steps 1–5 & 7; `refresh_coiled_env.py` does step 6)
Use this only for the manual path or to debug a failed script step.
1. **Backup current `latest` first** — pull it, retag `backup-YYYY-MM-DD-v<old-label>`, push. This is
   your only rollback anchor and is never overwritten by future `latest` pushes. (`rebuild_image.sh`
   appends `-HHMMSS` so two rebuilds on one day don't collide; find the exact tag via §4's list.)
2. (Optional) bump `LABEL version=` (§2).
3. **Build** `--platform linux/amd64` (§1).
4. **Tag** for ACR: `:latest` (required) and `:v<pkg-version>` (traceability).
5. **Push** both tags (`az acr login` first; re-auth if it expires).
6. **Refresh the Coiled env** — delete-then-recreate, NOT `force_rebuild`: because the tag name
   (`latest`) is unchanged, Coiled often keeps the old cached digest. Run
   `python azure/refresh_coiled_env.py` (it does `coiled.delete_software_environment` then
   `coiled.create_software_environment`, name `mosaic-acr-workers`, workspace `idm-coiled-idmad-r2`).
   Safe while no calibration is actively queuing workers; already-running clusters are unaffected.
7. **Smoke test** the published image:
   ```bash
   docker run --rm --platform linux/amd64 idmmosaicacr.azurecr.io/mosaic-worker:latest \
     R -e "library(MOSAIC); MOSAIC::check_dependencies(); cat('OK\n')"
   ```
   `.github/workflows/smoke-test.yml` also runs on the next push to `main`.

## 4. Rollback (new image is broken)
`az acr login` → pull `backup-YYYY-MM-DD-v<label>` → retag it `latest` → push → re-run Step 6 so
Coiled picks up the restored digest. List tags: `az acr repository show-tags --name idmmosaicacr
--repository mosaic-worker -o table`.

## 5. When NOT to rebuild
- A code-only change already on `main` does **not** need a rebuild for *new* clusters if you instead
  pin `MOSAIC_REF` at run time — but the published `latest` still lags until rebuilt. Rebuild when the
  worker image must carry new MOSAIC code, new system/R/Python deps, or a laser-cholera bump (cf.
  hybrid-backend invalidity, issue #113, in the `hedgehog-run` skill).
- Coordinate timing: don't recreate the Coiled env mid-calibration.

## Safety
This publishes to a shared registry and mutates a shared Coiled environment other people's jobs use.
Confirm with the user before pushing `latest` or deleting/recreating the Coiled env, and always land
the backup tag (Step 1) first.
