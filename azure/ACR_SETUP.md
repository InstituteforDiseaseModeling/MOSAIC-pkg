# Azure Container Registry (ACR) Setup for MOSAIC

## Why ACR?

Docker Hub enforces pull rate limits for unauthenticated users (100 pulls / 6 hours per IP). When Coiled spins up multiple workers in the same Azure region, they share an IP range and quickly exhaust this limit, causing `"You have reached your unauthenticated pull rate limit"` errors.

By hosting the image on Azure Container Registry:
- **No rate limits** — ACR has no pull throttling
- **Faster pulls** — traffic stays within Azure (same region as workers)
- **No authentication needed** — anonymous pull is enabled

## Registry Details

| Field | Value |
|-------|-------|
| Registry name | `idmmosaicacr` |
| Login server | `idmmosaicacr.azurecr.io` |
| SKU | Standard |
| Region | westus2 |
| Resource group | `rg-coiled-tting` |
| Subscription | IDM Research 2 |
| Anonymous pull | Enabled |
| Admin access | Enabled |
| Image | `idmmosaicacr.azurecr.io/mosaic-worker:latest` |
| Coiled software env | `mosaic-acr-workers` |

## How It Was Set Up

### 1. Create the ACR

```bash
az acr create --name idmmosaicacr --resource-group rg-coiled-tting --sku Standard --output table
```

> Note: Basic SKU does not support anonymous pull. Standard is required.

### 2. Enable anonymous pull

```bash
az acr update --name idmmosaicacr --anonymous-pull-enabled true --output table
```

### 3. Log in, tag, and push the image

```bash
az acr login --name idmmosaicacr

docker tag ttingidmod/mosaic-worker:latest idmmosaicacr.azurecr.io/mosaic-worker:latest
docker push idmmosaicacr.azurecr.io/mosaic-worker:latest
```

### 4. Create Coiled software environment

```python
import coiled

coiled.create_software_environment(
    name="mosaic-acr-workers",
    container="idmmosaicacr.azurecr.io/mosaic-worker:latest"
)
```

### 5. Code defaults updated

In `R/run_MOSAIC_dask.R`, the default `dask_spec$software` was changed from `"mosaic-docker-workers"` to `"mosaic-acr-workers"`.

## Usage

### Running the orchestrator locally (in Docker)

```bash
docker run --rm \
  -v ~/MOSAIC/MOSAIC-pkg:/src/MOSAIC-pkg \
  -v ~/MOSAIC/MOSAIC-data:/workspace/MOSAIC/MOSAIC-data \
  -v ~/output:/workspace/output \
  -v ~/.config/dask:/root/.config/dask:ro \
  idmmosaicacr.azurecr.io/mosaic-worker:latest \
  bash -c "R CMD INSTALL /src/MOSAIC-pkg && Rscript /src/MOSAIC-pkg/azure/mosaic_dask_test.R"
```

### Calling from R

```r
# Uses ACR image by default (no change needed)
run_MOSAIC_dask(config, priors, dir_output)

# Or explicitly:
run_MOSAIC_dask(config, priors, dir_output,
                dask_spec = list(software = "mosaic-acr-workers"))
```

## Updating the Image

After rebuilding the Docker image locally:

```bash
# Tag for ACR
docker tag ttingidmod/mosaic-worker:latest idmmosaicacr.azurecr.io/mosaic-worker:latest

# Login (token expires after a few hours)
az acr login --name idmmosaicacr

# Push
docker push idmmosaicacr.azurecr.io/mosaic-worker:latest

# Update Coiled env (force rebuild to pick up new image digest)
python3 -c "
import coiled
coiled.create_software_environment(
    name='mosaic-acr-workers',
    container='idmmosaicacr.azurecr.io/mosaic-worker:latest',
    force_rebuild=True
)
"
```

## Admin Credentials (if needed)

Admin access is enabled for manual operations. Retrieve credentials with:

```bash
az acr credential show --name idmmosaicacr --output table
```

These are not needed for normal operations since anonymous pull is enabled.

## Cost

ACR Standard tier: ~$0.667/day (~$20/month). Includes 100 GB storage. Data transfer within the same Azure region (westus2) is free.

## Reverting to Docker Hub

If needed, switch back by passing the old software environment:

```r
run_MOSAIC_dask(config, priors, dir_output,
                dask_spec = list(software = "mosaic-docker-workers"))
```

Or change the default in `R/run_MOSAIC_dask.R` line 375 back to `"mosaic-docker-workers"`.
