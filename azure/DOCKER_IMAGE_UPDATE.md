# Updating the MOSAIC Worker Docker Image

How to rebuild `idmmosaicacr.azurecr.io/mosaic-worker:latest`, preserve a rollback tag, push to ACR, and refresh the Coiled software environment.

---

## Registry & environment quick reference

| Item | Value |
|------|-------|
| ACR login server | `idmmosaicacr.azurecr.io` |
| Image | `mosaic-worker` |
| Coiled software env | `mosaic-acr-workers` |
| Coiled workspace | `idm-coiled-idmad-r2` |
| Dockerfile | `azure/Dockerfile` |
| Resource group | `rg-coiled-tting` (IDM Research 2 subscription) |

---

## Step 1 — Backup the current `latest`

Do this **before** building so you always have a working rollback.

```bash
# Pull the current latest locally
docker pull idmmosaicacr.azurecr.io/mosaic-worker:latest

# Tag it with today's date + the image label version (visible in Dockerfile LABEL version=)
docker tag idmmosaicacr.azurecr.io/mosaic-worker:latest \
           idmmosaicacr.azurecr.io/mosaic-worker:backup-YYYY-MM-DD-v<label>
# e.g.:
# docker tag idmmosaicacr.azurecr.io/mosaic-worker:latest \
#            idmmosaicacr.azurecr.io/mosaic-worker:backup-2026-06-16-v0.17.33

# Push the backup tag to ACR
az acr login --name idmmosaicacr
docker push idmmosaicacr.azurecr.io/mosaic-worker:backup-YYYY-MM-DD-v<label>
```

The backup tag is never overwritten by subsequent pushes to `latest` — it is your permanent rollback anchor.

---

## Step 2 — (Optional) Update the Dockerfile label

If the MOSAIC R package version changed, update the `LABEL version=` in `azure/Dockerfile` line 6 to match:

```dockerfile
LABEL version="0.43.0"
```

The build pulls MOSAIC from GitHub `main` by default. To pin to a specific ref, pass `--build-arg MOSAIC_REF=<tag-or-sha>` in Step 3.

---

## Step 3 — Build the new image

From the repo root (`MOSAIC-pkg/`):

```bash
# Build from main (default)
docker build -f azure/Dockerfile -t mosaic-worker:latest .

# OR pin to a specific ref
docker build -f azure/Dockerfile \
  --build-arg MOSAIC_REF=main \
  -t mosaic-worker:latest .
```

Build takes ~15–25 minutes. Confirm the three assertions at the end of the verify layer all print `✓`.

---

## Step 4 — Tag for ACR

```bash
# Required: latest tag
docker tag mosaic-worker:latest idmmosaicacr.azurecr.io/mosaic-worker:latest

# Recommended: versioned tag for traceability
docker tag mosaic-worker:latest idmmosaicacr.azurecr.io/mosaic-worker:v<package-version>
# e.g.:
# docker tag mosaic-worker:latest idmmosaicacr.azurecr.io/mosaic-worker:v0.43.0
```

---

## Step 5 — Push to ACR

```bash
az acr login --name idmmosaicacr   # token expires after a few hours; re-run if push fails

docker push idmmosaicacr.azurecr.io/mosaic-worker:latest
docker push idmmosaicacr.azurecr.io/mosaic-worker:v<package-version>
```

---

## Step 6 — Refresh the Coiled software environment

`force_rebuild=True` alone sometimes keeps the old cached image digest when the tag name (`latest`) hasn't changed. Delete + recreate is the reliable path:

```python
import coiled

print("Deleting old env...")
coiled.delete_software_environment(
    name="mosaic-acr-workers",
    workspace="idm-coiled-idmad-r2"
)

print("Recreating env...")
coiled.create_software_environment(
    name="mosaic-acr-workers",
    container="idmmosaicacr.azurecr.io/mosaic-worker:latest",
    workspace="idm-coiled-idmad-r2"
)

print("Done")
```

Safe to run as long as no calibration jobs are actively queuing workers (running clusters are unaffected — they already have the image).

---

## Step 7 — Smoke test

```bash
docker run --rm idmmosaicacr.azurecr.io/mosaic-worker:latest \
  R -e "library(MOSAIC); MOSAIC::check_dependencies(); cat('OK\n')"
```

The GitHub Actions smoke test (`.github/workflows/smoke-test.yml`) also runs automatically on the next push to `main`.

---

## Tags in ACR after a full update

| Tag | Contents |
|-----|----------|
| `latest` | New build |
| `v<package-version>` | Same new build, versioned |
| `backup-YYYY-MM-DD-v<label>` | Previous `latest` (rollback anchor) |

---

## Rollback: restore old `latest` from a backup tag

If the new image is broken and you need to revert:

```bash
# 1. Login
az acr login --name idmmosaicacr

# 2. Pull the backup
docker pull idmmosaicacr.azurecr.io/mosaic-worker:backup-YYYY-MM-DD-v<label>

# 3. Retag as latest
docker tag idmmosaicacr.azurecr.io/mosaic-worker:backup-YYYY-MM-DD-v<label> \
           idmmosaicacr.azurecr.io/mosaic-worker:latest

# 4. Push it back
docker push idmmosaicacr.azurecr.io/mosaic-worker:latest
```

Then re-run Step 6 to force Coiled to pick up the restored digest.

To delete a backup tag once it's no longer needed:

```bash
az acr repository delete --name idmmosaicacr \
  --image mosaic-worker:backup-YYYY-MM-DD-v<label> --yes
```

---

## List all tags currently in ACR

```bash
az acr repository show-tags --name idmmosaicacr --repository mosaic-worker --output table
```
