# hedgehog — Azure compute VM access

`hedgehog` is the Azure VM used for large MOSAIC calibration runs. This file documents how to
reach it and move results. **No secrets here** — just the access pattern. Keys live in `~/.ssh/`.

## Specs
- 120 cores, 448 GB RAM (Ubuntu).
- Good for large parallel `run_MOSAIC()` batches.

## Connection

| Field | Value |
|-------|-------|
| SSH alias | `hedgehog` (defined in `~/.ssh/config`) |
| User | `jgiles` |
| FQDN | `hedgehog.westus2.cloudapp.azure.com` |
| Public IP | `20.3.249.183` (dynamic — can change on restart; prefer the FQDN) |
| Key | `~/.ssh/id_ed25519` |

```bash
ssh hedgehog                 # via the config alias (recommended)
ssh jgiles@hedgehog.westus2.cloudapp.azure.com
```

Prefer the **FQDN over the IP**: the VM is often deallocated/`TEMP`, and a dynamic public IP can
change on restart while the DNS name follows it.

`~/.ssh/config` block:
```sshconfig
Host hedgehog
  HostName hedgehog.westus2.cloudapp.azure.com
  User jgiles
  IdentityFile ~/.ssh/id_ed25519
  AddKeysToAgent yes
  ServerAliveInterval 60
  ServerAliveCountMax 3
```

## Results layout
- Home: `/home/jgiles/`
- Calibration results: `~/MOSAIC/output/individual/*.tar.gz`
- `vm/launch_mosaic.R` and `vm/launch_mosaic_individual.R` print the exact `scp` pull commands.

## Pulling results
Use the helper (runs from the repo root):
```bash
vm/pull_results.sh              # pull ALL *.tar.gz to ./output/individual
vm/pull_results.sh ZWE ZMB      # pull specific ISO3 archives
vm/pull_results.sh --list       # list remote archives, download nothing
vm/pull_results.sh --dest ~/r   # custom local destination
```
Or by hand:
```bash
scp hedgehog:~/MOSAIC/output/individual/ZWE.tar.gz .
```

## Long-running jobs
Run inside `tmux` so a dropped SSH session doesn't kill a multi-hour calibration:
```bash
ssh hedgehog
tmux new -s mosaic      # detach: Ctrl-b d   |   reattach later: tmux attach -t mosaic
```

## Troubleshooting
- **`Permission denied (publickey)` after it used to work** → the VM was likely redeployed (fresh
  host key + wiped `authorized_keys`). Log in with your password, then re-register the key **from
  your Mac** (not from the VM):
  ```bash
  ssh-copy-id -i ~/.ssh/id_ed25519.pub jgiles@hedgehog.westus2.cloudapp.azure.com
  ```
- **VM deallocated / not reachable** → start it (needs a current `az login`):
  ```bash
  az vm start -g <resource-group> -n hedgehog
  az vm list-ip-addresses -n hedgehog -o table   # confirm the (possibly new) IP
  ```
