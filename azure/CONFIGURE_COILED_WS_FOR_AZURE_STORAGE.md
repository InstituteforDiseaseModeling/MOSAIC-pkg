# Configure Coiled Workspace for Azure Blob Storage

Step-by-step guide to give Coiled workers managed-identity access to Azure Blob Storage, so worker code can authenticate with `DefaultAzureCredential` (no embedded keys or SAS tokens).

> Audience: Tony (initial setup) and future onboarders for the IDM Coiled workspace.

---

## TL;DR

We use **one shared User Assigned Identity (UAI)** attached to all Coiled worker VMs in the workspace. Each researcher gets **their own storage account** (per-team / per-project), with **multiple containers inside** to organize their workload. The shared UAI is granted scoped RBAC on each account as researchers onboard. Worker code uses `DefaultAzureCredential` and "just works."

```
                     ┌─── RBAC: Contributor → mosaictonypilot     (containers: inputs, outputs, checkpoints)
                     │
   id-coiled-worker-idm ──── RBAC: Contributor → mosaicresearcher2  (per-team account)
   (one shared UAI)  │
                     └─── RBAC: Reader      → mosaicsharedinputs   (shared read-only data)
```

---

## Decisions and rationale

These decisions are **load-bearing** for the steps below. Re-read this section before changing anything.

### 1. One storage account per researcher/project (not one shared account with per-team containers)

| Driver | Why per-account wins |
|---|---|
| **Performance isolation** | A single storage account caps at ~20K IOPS / 60 Gbps egress, shared across all its containers. Two concurrent calibration bursts on one account will throttle each other invisibly. |
| **Cost attribution** | Per-account billing maps cleanly to project/team for the IDM Coiled bill split. |
| **Lifecycle independence** | Each team sets their own retention, tier, soft-delete, versioning. |
| **HNS (ADLS Gen2) choice** | Locked at account creation — per-account lets each team pick. |
| **Network rules** | Firewall and private-endpoint rules are account-wide. Per-account allows tightening one team without affecting others. |
| **Subscription limit is generous** | 250 storage accounts per region (raisable to 500) — plenty of headroom. |

Trade-off: more accounts to manage, more `az role assignment` calls during onboarding. Acceptable.

### 2. Multiple containers within each account (organize the workload, not the people)

A typical researcher account looks like:

```
mosaictonypilot/
├── inputs/         # read-mostly, retain forever
├── outputs/        # written by Coiled workers
├── checkpoints/    # ephemeral, lifecycle clears periodically
└── archive/        # Cool tier after 90d via lifecycle policy
```

Containers are cheap (~500K per account, effectively unlimited).

### 3. One shared UAI for the whole workspace (not per-team UAIs)

Coiled attaches UAI **per workspace** — all clusters in `idm-coiled-idmad-r2` run with the same identity. Since multiple projects (MOSAIC, future workstreams) share this workspace, the natural choice is one shared UAI granted RBAC on each team's account as it onboards.

| Driver | Why shared UAI wins for this setup |
|---|---|
| **Operational simplicity** | New researcher onboarding = one `az role assignment create`. No Coiled-side changes after initial setup. |
| **Workspace-level UAI is what Coiled exposes** | Per-cluster or per-job identity selection is not a first-class feature. |
| **`DefaultAzureCredential` just works** | Multi-UAI VMs require explicit `client_id` in every Azure SDK call — breaks `azcopy`, `adlfs`, `fsspec` ergonomics. |
| **Trust model fits a collegial research team** | RBAC scoping is sufficient isolation; identity-level isolation is overkill. |

**What we give up** (be aware of these):

- **No identity-level isolation.** A buggy script with the wrong account name could write to another team's account. Code discipline (config-driven storage account names) is the guard.
- **Audit logs are noisy.** Azure storage logs show one `principalId` for every write. Application-level tagging (blob metadata) is needed to attribute activity to a researcher.
- **Broader blast radius if the UAI were compromised** (very unlikely with MSI, but in theory).

These are acceptable for collegial research workloads with no PHI / embargoed / regulated data. See [When to expand to multiple workspaces](#when-to-expand-to-multiple-workspaces) for the escape hatch.

### 4. UAI name is project-neutral

Naming the UAI `id-coiled-worker-idm` (not `id-coiled-worker-mosaic`) since it will serve every project that runs on this workspace.

---

## Environment facts

| Item | Value |
|---|---|
| Azure subscription | `a3836717-7ed1-469b-a629-aeed26f5766d` (IDM Research 2) |
| Resource group | `rg-coiled-tting` |
| Region | `westus2` |
| Coiled workspace | `idm-coiled-idmad-r2` |
| Coiled server | `https://cloud.coiled.io` |
| Shared UAI (to create) | `id-coiled-worker-idm` |

> The subscription ID `0af07825-9e93-4b01-9d44-6e3e0753b6bd` shown in Nat's example role JSON is his sandbox — substitute the IDM value above when editing the Coiled Resource Group Role.

---

## Prerequisites

- Owner or User Access Administrator on the subscription (needed to update the Coiled Resource Group Role and create role assignments).
- `az` CLI installed and `az login` completed against the IDM tenant.
- Access to the [Coiled support channel](mailto:support@coiled.io) — Nat Tabris is the contact for workspace UAI configuration.

```bash
az account set --subscription a3836717-7ed1-469b-a629-aeed26f5766d
az account show --query "{Sub:name, ID:id, Tenant:tenantId}" -o table
```

---

## Shell variables used throughout

Every `az` command in this guide references shell variables instead of hardcoded names. **Set these once in your shell session** and the rest of the document is copy-pasteable.

### Workspace-level variables (set once for the whole workspace)

```bash
# IDM Coiled environment — do not change unless the workspace itself moves
export SUBSCRIPTION=a3836717-7ed1-469b-a629-aeed26f5766d
export RG=rg-coiled-tting
export LOCATION=westus2
export UAI_NAME=id-coiled-worker-idm
export COILED_WORKSPACE=idm-coiled-idmad-r2

az account set --subscription $SUBSCRIPTION
```

### Per-researcher variables (set per onboarding)

```bash
# Change these for each researcher being onboarded
export ACCOUNT=mosaictonypilot                    # storage account name — lowercase, 3-24 chars, globally unique
export RESEARCHER=tony                            # short name for tags
export OWNER_EMAIL=tony.ting@gatesfoundation.org  # email for tags

# Derived (computed from above — set after the account exists)
export STORAGE_ID=$(az storage account show -n $ACCOUNT -g $RG --query id -o tsv 2>/dev/null)
export UAI_PRINCIPAL=$(az identity show -n $UAI_NAME -g $RG --query principalId -o tsv 2>/dev/null)
```

Every command below assumes these are set. If you open a new shell, re-export them before continuing.

---

## One-time setup (workspace-level)

These steps happen **once** for the workspace. After Nat confirms the workspace is configured, you never touch this again — onboarding new researchers becomes a per-account operation only.

### Step 1 — Create the shared UAI

```bash
az identity create \
  --name $UAI_NAME \
  --resource-group $RG \
  --location $LOCATION
```

Capture the Resource ID (what Nat calls "the ARN") and the `principalId` (used for RBAC assignments):

```bash
UAI_ID=$(az identity show -n $UAI_NAME -g $RG --query id -o tsv)
UAI_PRINCIPAL=$(az identity show -n $UAI_NAME -g $RG --query principalId -o tsv)
UAI_CLIENT=$(az identity show -n $UAI_NAME -g $RG --query clientId -o tsv)

echo "UAI Resource ID : $UAI_ID"
echo "UAI principalId : $UAI_PRINCIPAL"
echo "UAI clientId    : $UAI_CLIENT"
```

Save all three values to your password manager / secure notes. You'll reuse the `principalId` on every researcher onboarding (it's also exported as `$UAI_PRINCIPAL` for the rest of this guide).

### Step 2 — Update the Coiled Resource Group Role

Coiled needs the `Microsoft.ManagedIdentity/userAssignedIdentities/assign/action` permission to attach the UAI to worker VMs. This is the line highlighted in Nat's email screenshot.

**Easiest path — Azure Portal:**
1. Portal → Subscriptions → `IDM Research 2` → Access control (IAM) → Roles
2. Search for "Coiled Resource Group Role"
3. Edit → JSON tab
4. Add `"Microsoft.ManagedIdentity/userAssignedIdentities/assign/action"` to the `actions` array
5. Review + update

**CLI alternative:**

```bash
# 1. Export current role definition. Note: `list` returns a JSON ARRAY ([...])
#    but `update` expects a single OBJECT ({...}). Use jq to unwrap.
az role definition list --name "Coiled Resource Group Role" \
  | jq '.[0]' > coiled-role.json

# 2. Edit coiled-role.json (in your editor): add the action to the actions array
#      "Microsoft.ManagedIdentity/userAssignedIdentities/assign/action"

# 3. Apply
az role definition update --role-definition coiled-role.json
```

> If you skip the `jq '.[0]'` unwrap, `az role definition update` errors with
> `Invalid role definition. A valid dictionary JSON representation is expected.`
> The file needs to start with `{` (object), not `[` (array).

Verify:

```bash
az role definition list --name "Coiled Resource Group Role" \
  --query "[0].permissions[0].actions" -o tsv | grep userAssignedIdentities
# expected: Microsoft.ManagedIdentity/userAssignedIdentities/assign/action
```

### Step 3 — Email Nat with the UAI Resource ID and workspace name

Generate the email body with the variables substituted:

```bash
cat <<EOF
To: nat@coiled.io
Cc: support@coiled.io
Subject: UAI for Coiled workspace $COILED_WORKSPACE

Hi Nat,

Following up on UAI-attached workers for Coiled. Here are the details:

  UAI Resource ID : $UAI_ID
  Coiled workspace: $COILED_WORKSPACE

Coiled Resource Group Role has been updated with
Microsoft.ManagedIdentity/userAssignedIdentities/assign/action.

Could you wire the UAI into the workspace config and let me know
when it's live so we can pilot?

Thanks,
Tony
EOF
```

**Block on Nat's confirmation** before testing — until the workspace is configured, the UAI is created but not attached to anything.

### Step 4 — Verify after Nat confirms

After Nat replies that the workspace is configured, spin up a one-worker cluster and verify the UAI by asking it to mint a token for Azure Storage. A successful token issuance is the conclusive proof that the UAI is attached **and** functional.

> Note: IMDS does not expose a clean "list attached UAIs" endpoint. `/metadata/instance` has no `compute.identity` field, and `/metadata/identity/info` is for system-assigned identities only. The token check below is the right verification.

```python
import os, coiled
WORKSPACE       = os.environ["COILED_WORKSPACE"]
EXPECTED_UAI_ID = os.environ.get("UAI_CLIENT", "")  # optional — for cross-check

cluster = coiled.Cluster(workspace=WORKSPACE, n_workers=1)
client = cluster.get_client()

def check_token():
    """Ask the worker's UAI to mint a token for Azure Storage."""
    import requests
    r = requests.get(
        "http://169.254.169.254/metadata/identity/oauth2/token",
        params={
            "api-version": "2018-02-01",
            "resource": "https://storage.azure.com/",
        },
        headers={"Metadata": "true"}, timeout=5)
    r.raise_for_status()
    data = r.json()
    return {k: v for k, v in data.items() if k != "access_token"}  # strip secret

token_info = client.submit(check_token).result()
for k, v in token_info.items():
    print(f"  {k:14s} : {v}")

if EXPECTED_UAI_ID:
    assert token_info["client_id"] == EXPECTED_UAI_ID, \
        f"client_id {token_info['client_id']} != expected {EXPECTED_UAI_ID}"
    print(f"\nOK — UAI client_id matches.")

cluster.close()
```

Cross-check the returned `client_id` against the UAI you created in [Step 1](#step-1--create-the-shared-uai):

```bash
export UAI_CLIENT=$(az identity show -n $UAI_NAME -g $RG --query clientId -o tsv)
echo $UAI_CLIENT   # should equal the client_id from the Python output above
```

**Expected output (success):**

```text
  client_id      : 82f0ebba-d17f-4310-ad5b-f428c0de083a   # matches $UAI_CLIENT
  expires_in     : 86400
  resource       : https://storage.azure.com/
  token_type     : Bearer
  ...
```

**Failure modes:**

| What you see | Means | Fix |
|---|---|---|
| `check_token` raises 400 (`identity_not_found`) | No UAI attached to worker VM | Workspace not configured yet — ping Nat |
| `check_token` raises 400 (`multiple_matching_identities`) | More than one UAI is attached | Unexpected for our setup — confirm with Nat |
| Token returns, but `client_id` ≠ `$UAI_CLIENT` | Wrong UAI attached | Confirm Nat wired the correct UAI Resource ID |
| `Unclosed client session` warning from aiohttp at script end | Coiled/dask cleanup is racy with aiohttp — cosmetic only | Ignore |

The full smoke test (blob round-trip + token check, with labelled output and optional `$UAI_CLIENT` assertion) is in [smoke_test_coiled_azure_storage.py](smoke_test_coiled_azure_storage.py).

---

## Per-researcher onboarding

Every new researcher follows the same recipe. Adapt the name and HNS choice to their needs.

> Before starting, set the **per-researcher variables** from [Shell variables used throughout](#shell-variables-used-throughout) — `$ACCOUNT`, `$RESEARCHER`, `$OWNER_EMAIL`. Workspace variables (`$RG`, `$LOCATION`, `$UAI_NAME`, `$UAI_PRINCIPAL`) should already be set from the one-time setup session.

### Step 1 — Pick a storage account name

Convention: `mosaic<researcher><purpose>` lowercase, 3–24 chars, globally unique.

Examples:
- `mosaictonypilot` — Tony's pilot account
- `mosaictonyprod` — Tony's production account (if separated)
- `idmcholeramalaria` — project-named accounts also fine

Once chosen, set `$ACCOUNT` accordingly and test availability:

```bash
export ACCOUNT=mosaictonypilot   # change for the researcher you're onboarding
az storage account check-name --name $ACCOUNT --query nameAvailable
```

### Step 2 — Create the storage account

Defaults below assume ADLS Gen2 (`--hierarchical-namespace true`). **HNS is locked at creation — pick deliberately.**

| Decision | Recommendation |
|---|---|
| HNS (Gen2) | `true` — gives directory semantics, `abfs://` URIs, better for analytics |
| SKU | `Standard_LRS` for pilot; `Standard_ZRS` if zone redundancy is needed |
| Kind | `StorageV2` |
| Tier | `Hot` (default) — switch via lifecycle later |
| Allow public access | `false` — set explicitly |
| Min TLS | `TLS1_2` |

```bash
az storage account create \
  --name $ACCOUNT \
  --resource-group $RG \
  --location $LOCATION \
  --sku Standard_LRS \
  --kind StorageV2 \
  --hns \
  --allow-blob-public-access false \
  --min-tls-version TLS1_2 \
  --tags owner=$OWNER_EMAIL researcher=$RESEARCHER

# Refresh derived variable now that the account exists
export STORAGE_ID=$(az storage account show -n $ACCOUNT -g $RG --query id -o tsv)
echo "STORAGE_ID = $STORAGE_ID"
```

### Step 3 — Create containers inside the account

Workload organization, not access boundary:

```bash
for c in inputs outputs checkpoints archive data; do
  az storage container create \
    --account-name $ACCOUNT \
    --name $c \
    --auth-mode login
done
```

> `--auth-mode login` uses your `az login` identity; works regardless of whether you have account keys.

### Step 4 — Grant the shared UAI access (scoped to this account)

```bash
az role assignment create \
  --assignee-object-id $UAI_PRINCIPAL \
  --assignee-principal-type ServicePrincipal \
  --role "Storage Blob Data Contributor" \
  --scope $STORAGE_ID
```

**Tighter scoping options** (use these when you want least privilege):

```bash
# Read-only on the inputs container only
az role assignment create \
  --assignee-object-id $UAI_PRINCIPAL \
  --assignee-principal-type ServicePrincipal \
  --role "Storage Blob Data Reader" \
  --scope "$STORAGE_ID/blobServices/default/containers/inputs"

# Read-write on outputs only
az role assignment create \
  --assignee-object-id $UAI_PRINCIPAL \
  --assignee-principal-type ServicePrincipal \
  --role "Storage Blob Data Contributor" \
  --scope "$STORAGE_ID/blobServices/default/containers/outputs"
```

**RBAC propagation** can take 1–10 minutes. If immediate test fails with 403, wait and retry.

### Step 5 — Smoke test from a Coiled worker

The Python smoke test reads `$ACCOUNT` and `$COILED_WORKSPACE` from the environment so it stays in sync with the shell variables you set above:

```python
import os, coiled

WORKSPACE = os.environ["COILED_WORKSPACE"]   # idm-coiled-idmad-r2
ACCOUNT   = os.environ["ACCOUNT"]            # e.g. mosaictonypilot
CONTAINER = "outputs"

cluster = coiled.Cluster(workspace=WORKSPACE, n_workers=1)
client = cluster.get_client()

def smoke_test(account_name, container):
    from azure.identity import DefaultAzureCredential
    import adlfs, pandas as pd

    cred = DefaultAzureCredential()
    fs = adlfs.AzureBlobFileSystem(account_name=account_name, credential=cred)

    df = pd.DataFrame({"x": [1, 2, 3]})
    path = f"{container}/smoke/test.parquet"
    with fs.open(path, "wb") as f:
        df.to_parquet(f)
    return f"wrote {path} to {account_name}"

print(client.submit(smoke_test, ACCOUNT, CONTAINER).result())
cluster.close()
```

If you get a 403, double-check:
- RBAC has propagated (wait 5–10 min)
- `principalId` used for assignment matches the UAI's actual `principalId`
- The UAI is attached to the worker VM (Step 4 of one-time setup)

### Step 6 — Tell the researcher

Send them:
- Storage account name (`$ACCOUNT`, e.g. `mosaictonypilot`)
- Container layout convention (`inputs/`, `outputs/`, `checkpoints/`, `archive/`)
- Reference to [Worker code patterns](#worker-code-patterns) below
- Tell them to `export MOSAIC_STORAGE_ACCOUNT=<their-account>` in their orchestrator shell so worker code stays config-driven

That's it. No Coiled-side changes.

---

## Worker code patterns

### Python environment (`environment.yml`)

Add to the Coiled software environment:

```yaml
dependencies:
  - python=3.11
  - pip
  - pip:
      - azure-identity        # DefaultAzureCredential / MSI
      - adlfs                 # fsspec backend for ADLS Gen2
      - azure-storage-blob    # optional: lower-level SDK
```

### Config-driven account name (read this first)

**Never hardcode storage account names in worker code.** Pass them through environment / orchestrator config so the same code runs against any researcher's account without edits. Every example below assumes this convention:

```python
import os
STORAGE_ACCOUNT = os.environ["MOSAIC_STORAGE_ACCOUNT"]   # e.g. mosaictonypilot
```

Set it in the orchestrator shell before launching the cluster:

```bash
export MOSAIC_STORAGE_ACCOUNT=$ACCOUNT   # mirrors the variable from onboarding
```

### Round-trip test with `adlfs` filesystem object

This block creates its own test data, writes it, reads it back, asserts equality, and cleans up:

```python
from azure.identity import DefaultAzureCredential
import adlfs, pandas as pd

cred = DefaultAzureCredential()
fs = adlfs.AzureBlobFileSystem(account_name=STORAGE_ACCOUNT, credential=cred)

# Self-contained test data — no external file needed
df = pd.DataFrame({"x": [1, 2, 3], "y": ["a", "b", "c"]})
test_path = "outputs/_test/roundtrip.parquet"

# Write
with fs.open(test_path, "wb") as f:
    df.to_parquet(f)

# Read back
with fs.open(test_path, "rb") as f:
    df_back = pd.read_parquet(f)

assert df.equals(df_back), "round-trip mismatch"
print(f"OK: round-trip {len(df_back)} rows at abfs://{test_path}")

# Clean up the test artifact
fs.rm(test_path)
```

### Round-trip test with fsspec URI (`abfs://`)

Same round-trip via the URI form — useful when you want pandas/dask to handle the storage backend transparently:

```python
from azure.identity import DefaultAzureCredential
import pandas as pd

df = pd.DataFrame({"x": [1, 2, 3], "y": ["a", "b", "c"]})
test_uri = "abfs://outputs/_test/uri_roundtrip.parquet"

storage_options = {
    "account_name": STORAGE_ACCOUNT,
    "credential": DefaultAzureCredential(),
}

# Write
df.to_parquet(test_uri, storage_options=storage_options)

# Read back
df_back = pd.read_parquet(test_uri, storage_options=storage_options)

assert df.equals(df_back), "round-trip mismatch"
print(f"OK: round-trip via URI — {len(df_back)} rows")

# Clean up
import adlfs
adlfs.AzureBlobFileSystem(
    account_name=STORAGE_ACCOUNT,
    credential=DefaultAzureCredential(),
).rm("outputs/_test/uri_roundtrip.parquet")
```

> Both examples write under `outputs/_test/` so the test artifacts are easy to spot and bulk-delete if cleanup fails (e.g. the script errors mid-run).

### Passing the account name to worker tasks

```python
# orchestrator-side
client.submit(task, storage_account=STORAGE_ACCOUNT, ...)

# worker-side
def task(storage_account, ...):
    fs = adlfs.AzureBlobFileSystem(account_name=storage_account, credential=DefaultAzureCredential())
    ...
```

### Listing what the UAI can see (debugging)

```python
def list_accessible_containers(account):
    from azure.identity import DefaultAzureCredential
    from azure.storage.blob import BlobServiceClient
    svc = BlobServiceClient(
        f"https://{account}.blob.core.windows.net",
        credential=DefaultAzureCredential())
    return [c.name for c in svc.list_containers()]
```

---

## Operational guidance

### Mitigations worth applying

1. **Always scope RBAC to a storage account or container, never to the resource group.** Avoids the UAI gaining auto-access to future accounts added to the RG.

2. **Tag every storage account** at creation with `project`, `owner`, `researcher`, `created` for cost analysis and audit (the [Step 2 create command](#step-2--create-the-storage-account) already does this):

   ```bash
   --tags project=mosaic owner=$OWNER_EMAIL researcher=$RESEARCHER created=$(date +%Y-%m-%d)
   ```

3. **Application-level write attribution.** Since storage logs show one `principalId` for all workers, tag each blob with metadata identifying the writer:

   ```python
   fs.set_metadata(f"outputs/run_001.parquet", {"writer": RESEARCHER, "run_id": "abc123"})
   ```

4. **Storage firewall** once you have Coiled's egress VNet/subnet from Nat — lock each account to that source so a leaked token from outside the network is useless.

5. **Lifecycle policy on the `checkpoints/` and `archive/` containers** — automate cleanup so storage doesn't grow unbounded.

### Naming conventions

| Resource | Pattern | Example |
|---|---|---|
| UAI (shared) | `id-coiled-worker-<org>` | `id-coiled-worker-idm` |
| Storage account | `<project><researcher><purpose>` | `mosaictonypilot` |
| Containers | descriptive nouns | `inputs`, `outputs`, `checkpoints`, `archive` |
| Tags | `project`, `owner`, `created` | as above |

Storage account names: 3–24 chars, lowercase + digits only, globally unique across all of Azure. Build a project prefix in (`mosaic`, `idm`) so it's obvious in bills and audit logs.

### Restrictions and limits to know

**Per storage account:**
- ~20,000 IOPS total (across all containers/blobs)
- 60 Gbps egress, 50 Gbps ingress (US regions)
- 500 TB max (soft, raisable)
- ~500,000 containers (effectively unlimited)

**Per subscription:**
- 250 storage accounts per region (soft, raisable to 500)

**Locked at storage-account creation (cannot change later):**
- Name, region, kind, hierarchical namespace (HNS), account type

**Changeable after creation:**
- SKU/replication (within constraints), access tier, network rules, encryption, tags

**RBAC propagation:** 1–10 minutes after assignment; sometimes longer for new principals.

### When to expand to multiple workspaces

The shared-workspace, shared-UAI pattern works until one of these becomes true. When it does, carve the affected team into their own workspace + own UAI:

- A team handles PHI, embargoed publication data, or anything regulated.
- Compliance requires per-team identity-level audit trails.
- A team's Coiled spend is large enough that you want it billed separately at the Coiled workspace level.
- A team needs cluster defaults / software envs that diverge significantly from the shared workspace.

Migration path:

1. Create new workspace with Coiled (set `$COILED_WORKSPACE` to e.g. `idm-coiled-<team>`).
2. Create a new dedicated UAI (set `$UAI_NAME` to e.g. `id-coiled-worker-<team>` and rerun [Step 1 of one-time setup](#step-1--create-the-shared-uai)).
3. Grant the new UAI access to that team's existing storage accounts (same as [Step 4 of per-researcher onboarding](#step-4--grant-the-shared-uai-access-scoped-to-this-account)).
4. Revoke the old shared UAI's access to those accounts: `az role assignment delete --assignee <old-principal> --scope <storage-id>`.
5. Ask Nat to wire the new UAI into the new workspace.
6. Researchers in that team point `coiled.Cluster(workspace=...)` at the new workspace (update `$COILED_WORKSPACE` in their environment).

No data migration needed — only identity and workspace change.

---

## Why we did **not** choose alternative patterns

Recorded for future onboarders so we don't relitigate.

### Why not one storage account, container per researcher

- All researchers compete for the same 20K IOPS budget — concurrent calibration bursts throttle each other.
- HNS choice is locked account-wide; one team can't choose differently.
- Cost attribution requires custom tag queries instead of native per-account billing.
- Lifecycle policies must use prefix filters and stay synchronized across teams.

### Why not per-team UAI within the same workspace

- Coiled attaches UAI **per workspace**, not per cluster/job. Multiple UAIs per workspace is not how the platform is designed.
- Even if Azure supports multi-UAI VMs (it does), worker code would need explicit `client_id` in every Azure SDK call — `DefaultAzureCredential()` returns HTTP 400 from IMDS when multiple UAIs are attached without disambiguation.
- Tooling (`azcopy`, `az` CLI, `adlfs`, `fsspec`) assumes "the" managed identity. Multi-UAI breaks this ecosystem-wide.
- Audit attribution from an env-var-selected `client_id` is not cryptographically meaningful — any script in the shared runtime could rewrite the env var.

### Why not per-team workspace from day one

- Coiled-admin overhead per workspace (Nat must configure each one).
- Coiled billing splits into N invoices.
- Researchers move between projects; managing membership across N workspaces is annoying.
- For a collegial research team with <5 active projects, the friction outweighs the isolation benefit. Revisit when conditions in [When to expand](#when-to-expand-to-multiple-workspaces) hit.

---

## Reference

### Useful commands

All commands assume the shell variables from [Shell variables used throughout](#shell-variables-used-throughout) are exported.

```bash
# Show the shared UAI
az identity show -n $UAI_NAME -g $RG -o json

# List all storage accounts in the RG
az storage account list -g $RG -o table

# List role assignments granted to the shared UAI
az role assignment list --assignee $UAI_PRINCIPAL --all -o table

# List containers in a specific account (uses your az login, not the UAI)
az storage container list --account-name $ACCOUNT --auth-mode login -o table

# Check what RBAC roles exist that mention "Blob"
az role definition list --query "[?contains(roleName, 'Blob')].roleName" -o tsv
```

### Common errors

| Error | Likely cause | Fix |
|---|---|---|
| `403 AuthorizationPermissionMismatch` | RBAC not propagated, wrong scope, or wrong role | Wait 10 min; re-check `--scope` and `--role`; verify with `az role assignment list` |
| `400 identity_check_failed` from IMDS | Multiple UAIs attached without `client_id` | Should not happen with one shared UAI — if it does, ask Nat |
| `AzureCliCredential.get_token failed` from `DefaultAzureCredential` on a worker | UAI not attached to worker VM | Verify workspace configuration with Nat; check IMDS from worker (Step 4 of one-time setup) |
| `ContainerNotFound` | Container doesn't exist or wrong account name | `az storage container list --account-name <acct> --auth-mode login` |
| `403` on a container that worked yesterday | RBAC assignment removed, or storage firewall added | Re-check role assignments and network rules on the account |

### Related documents in this directory

- [PROVISION_ORCHESTRATOR_AZURE.md](PROVISION_ORCHESTRATOR_AZURE.md) — original Azure VM provisioning for the orchestrator
- [ACR_SETUP.md](ACR_SETUP.md) — Azure Container Registry setup for worker images
- [MOSAIC_CLOUD_SETUP_GUIDE.md](MOSAIC_CLOUD_SETUP_GUIDE.md) — end-to-end MOSAIC cloud setup
- [MOSAIC_DASK_GUIDE.md](MOSAIC_DASK_GUIDE.md) — Dask/Coiled execution patterns
- [check_coiled_workspace.py](check_coiled_workspace.py) — workspace visibility check

### External references

- [Coiled docs: managed identity on Azure](https://docs.coiled.io/user_guide/azure/index.html)
- [Azure docs: User Assigned Managed Identity](https://learn.microsoft.com/en-us/entra/identity/managed-identities-azure-resources/overview)
- [Azure docs: Storage RBAC roles](https://learn.microsoft.com/en-us/azure/storage/blobs/assign-azure-role-data-access)
- [adlfs](https://github.com/fsspec/adlfs) — fsspec backend for ADLS

---

*Last updated: 2026-05-27 — refactored to use shell variables ([Shell variables used throughout](#shell-variables-used-throughout)) so commands are copy-pasteable across onboardings without manual substitution.*
