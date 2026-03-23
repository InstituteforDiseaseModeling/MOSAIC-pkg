# Provisioning the MOSAIC Orchestrator VM on Azure

This VM runs the R orchestrator (likelihood computation, parquet I/O) while Coiled workers handle LASER simulations. The L-series VM provides local NVMe SSD for fast I/O.

## VM Details

| Property | Value |
|----------|-------|
| Name | `mosaic-orchestrator` |
| Resource Group | `rg-coiled-tting` |
| Subscription | `IDM Research 2` (`a3836717-7ed1-469b-a629-aeed26f5766d`) |
| SKU | `Standard_L4s_v4` (4 vCPUs, 32 GB RAM, 2×447 GB local NVMe) |
| Location | westus2, zone 2 |
| OS | Ubuntu 24.04 LTS |
| Static IP | `20.83.234.43` |

## 1. Provision the VM

```bash
# Check L-series availability
az vm list-skus --location westus2 --size Standard_L --output table

# Create VM
az vm create \
  --resource-group rg-coiled-tting \
  --name mosaic-orchestrator \
  --image Canonical:ubuntu-24_04-lts:server:latest \
  --size Standard_L4s_v4 \
  --admin-username tinghf \
  --generate-ssh-keys \
  --os-disk-size-gb 128 \
  --location westus2 \
  --zone 2 \
  --public-ip-sku Standard \
  --nsg-rule SSH

# Set static IP to avoid IP changes on restart ($3.65/month)
az network public-ip update \
  --resource-group rg-coiled-tting \
  --name mosaic-orchestratorPublicIP \
  --allocation-method Static
```

## 2. Install Docker

```bash
ssh tinghf@20.83.234.43

curl -fsSL https://get.docker.com | sudo sh
sudo usermod -aG docker $USER
# Log out and back in for group change to take effect
exit
ssh tinghf@20.83.234.43
```

## 3. Install Azure CLI

```bash
curl -sL https://aka.ms/InstallAzureCLIDeb | sudo bash
az login
az acr login --name idmmosaicacr
```

## 4. Mount Local NVMe SSD

The L-series has ephemeral NVMe drives for high-speed I/O. **Data is lost on VM deallocation.**

```bash
lsblk
# nvme0n1 = OS disk (DO NOT format)
# nvme1n1, nvme2n1 = local NVMe SSDs (447 GB each)

sudo mkfs.ext4 /dev/nvme1n1
sudo mkdir -p /mnt/nvme
sudo mount /dev/nvme1n1 /mnt/nvme
sudo chown $USER:$USER /mnt/nvme
mkdir -p /mnt/nvme/output
```

> **WARNING:** Do NOT add NVMe to `/etc/fstab` — these drives are ephemeral and wiped on dealloc. Re-format and mount after each VM restart.

## 5. Set Up Coiled Authentication

```bash
sudo apt install -y python3-pip
pip install coiled --break-system-packages
echo 'export PATH="$HOME/.local/bin:$PATH"' >> ~/.bashrc
source ~/.bashrc
coiled login
# Follow the browser auth link

# If permission error on ~/.config/dask:
sudo chown -R $USER:$USER ~/.config/dask
coiled login
```

## 6. Clone Repositories

```bash
git clone https://github.com/tinghf/MOSAIC-pkg ~/MOSAIC/MOSAIC-pkg
cd ~/MOSAIC/MOSAIC-pkg && git checkout validate_dask_local_sim_take2

git clone https://github.com/InstituteforDiseaseModeling/MOSAIC-data ~/MOSAIC/MOSAIC-data
```

## 7. Pull Docker Image

```bash
az acr login --name idmmosaicacr
docker pull idmmosaicacr.azurecr.io/mosaic-worker:latest
```

## 8. Run MOSAIC

```bash
docker run --rm \
  -v ~/MOSAIC/MOSAIC-pkg:/src/MOSAIC-pkg \
  -v ~/MOSAIC/MOSAIC-data:/workspace/MOSAIC/MOSAIC-data \
  -v /mnt/nvme/output:/workspace/output \
  -v ~/.config/dask:/root/.config/dask:ro \
  idmmosaicacr.azurecr.io/mosaic-worker:latest \
  bash -c "R CMD INSTALL /src/MOSAIC-pkg && Rscript /src/MOSAIC-pkg/azure/mosaic_dask_fixed_test.R"
```

Output is written to `/mnt/nvme/output` (local NVMe) for fast parquet I/O.

## VM Lifecycle

### Stop (saves cost, keeps disk)
```bash
az vm deallocate --resource-group rg-coiled-tting --name mosaic-orchestrator
```
> **IMPORTANT:** Copy results off `/mnt/nvme/` before deallocating — NVMe data is wiped.

### Start
```bash
az vm start --resource-group rg-coiled-tting --name mosaic-orchestrator

# Re-mount NVMe after restart (data was wiped)
ssh tinghf@20.83.234.43
sudo mkfs.ext4 /dev/nvme1n1
sudo mkdir -p /mnt/nvme
sudo mount /dev/nvme1n1 /mnt/nvme
sudo chown $USER:$USER /mnt/nvme
mkdir -p /mnt/nvme/output
```

### Delete (removes everything)
```bash
az vm delete --resource-group rg-coiled-tting --name mosaic-orchestrator --yes
# Clean up orphaned resources
az network nic delete --resource-group rg-coiled-tting --name mosaic-orchestratorVMNic
az network public-ip delete --resource-group rg-coiled-tting --name mosaic-orchestratorPublicIP
az network nsg delete --resource-group rg-coiled-tting --name mosaic-orchestratorNSG
```

## Adding New Users (@gatesfoundation.org)

### Step 1: Add SSH Access to the VM

Get the new user's public SSH key (they can generate one with `ssh-keygen -t rsa -b 4096`):

```bash
az vm user update \
  --resource-group rg-coiled-tting \
  --name mosaic-orchestrator \
  --username <their-username> \
  --ssh-key-value "<their-ssh-public-key>"
```

They can then SSH in: `ssh <their-username>@20.83.234.43`

### Step 2: Grant Docker Access

SSH into the VM and add them to the docker group:

```bash
sudo usermod -aG docker <their-username>
```

### Step 3: Set Up ACR Access

The new user needs Azure CLI and ACR login on the VM:

```bash
# As the new user on the VM:
az login
az acr login --name idmmosaicacr
```

They also need the **AcrPull** role on the ACR registry. An admin grants this from any machine with Azure CLI:

```bash
az role assignment create \
  --assignee <someone@gatesfoundation.org> \
  --role AcrPull \
  --scope /subscriptions/a3836717-7ed1-469b-a629-aeed26f5766d/resourceGroups/rg-coiled-tting/providers/Microsoft.ContainerRegistry/registries/idmmosaicacr
```

### Step 4: Set Up Coiled Authentication

Each user needs their own Coiled token:

```bash
# As the new user on the VM:
pip install coiled --break-system-packages
echo 'export PATH="$HOME/.local/bin:$PATH"' >> ~/.bashrc
source ~/.bashrc
coiled login
```

### Step 5: Clone Repos and Run

```bash
git clone https://github.com/tinghf/MOSAIC-pkg ~/MOSAIC/MOSAIC-pkg
cd ~/MOSAIC/MOSAIC-pkg && git checkout validate_dask_local_sim_take2

git clone https://github.com/InstituteforDiseaseModeling/MOSAIC-data ~/MOSAIC/MOSAIC-data

# Mount NVMe for their output (shared mount point)
mkdir -p /mnt/nvme/output

# Run
docker run --rm \
  -v ~/MOSAIC/MOSAIC-pkg:/src/MOSAIC-pkg \
  -v ~/MOSAIC/MOSAIC-data:/workspace/MOSAIC/MOSAIC-data \
  -v /mnt/nvme/output:/workspace/output \
  -v ~/.config/dask:/root/.config/dask:ro \
  idmmosaicacr.azurecr.io/mosaic-worker:latest \
  bash -c "R CMD INSTALL /src/MOSAIC-pkg && Rscript /src/MOSAIC-pkg/azure/mosaic_dask_fixed_test.R"
```

## Benchmark Results (100 workers, 10K sims, 5 iterations)

| VM SKU | Type | vCPUs | RAM | Mean sim | Max sim | Total | $/hr |
|--------|------|-------|-----|----------|---------|-------|------|
| Standard_D2s_v6 | General | 2 | 8 GB | 3.9-4.1s | 4.8-5.8s | 13.2-13.4 min | ~$0.096 |
| **Standard_D4s_v6** | General | 4 | 16 GB | 4.0s | 8.8-10.2s | 12.8-13.3 min | ~$0.192 |
| Standard_D8s_v6 | General | 8 | 32 GB | 3.9s | 4.7s | 13.5 min | ~$0.384 |
| Standard_F4s_v2 | Compute | 4 | 8 GB | 6.8s | 15-40s | 17.6 min | ~$0.169 |

**Recommendation:** Standard_D4s_v6 for workers (best total time). Standard_D2s_v6 is viable for cost savings with nearly identical performance.
