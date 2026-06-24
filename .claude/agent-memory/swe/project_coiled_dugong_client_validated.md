---
name: project-coiled-dugong-client-validated
description: Coiled hybrid validated with dugong as the Dask client (skew fixed) — GO for production Coiled fits; plus the dugong OpenSSL-3.3.0 LD_PRELOAD trap that blocks `import coiled` under reticulate
metadata:
  type: project
---

**Coiled hybrid is GO with dugong as the Dask CLIENT** (validated 2026-06-24,
EA 10-loc warm-start smoke). Supersedes the old "#113-invalid on dugong" note —
see [[reference_dugong_vm]], [[project_coiled_hybrid_smoke_validated]].

**Why:** the laptop client (dask/distributed 2026.3.0) stalled at `sims_done=0`
for 29+ min against the rebuilt 2026.6.0 worker image — a client↔scheduler dask
version skew (VersionMismatchWarning table in the run log). dugong's client dask
= **2026.6.0**, matching the rebuilt image (`idmmosaicacr.azurecr.io/mosaic-worker:latest`,
laser-cholera 0.16.1). Result with dugong client: sims climb immediately
(~5 sims/s on 12× D2s_v6), **ZERO** VersionMismatchWarning, completes end-to-end.

**Smoke numbers (1-iter fixed, n_sims=1500, 12 workers):** 1483/1500 successful,
0 errored; cases R²=0.643 (≥0.6 gate → #113 RESOLVED), deaths R²=0.355; bias
cases 1.44 / deaths 2.02; runtime 13.3 min. cases R² near the dugong-local EA
pilot (~0.76) given the tiny fixed budget. **GO** for production regional 100k +
continental Coiled fits with dugong as client.

**How to apply:** for production Coiled fits, run the client on **dugong** (not the
laptop) until the laptop's dask is bumped to match the image. Keep client dask
== worker-image dask or sims silently stall (0/N, no error).

## dugong OpenSSL-3.3.0 LD_PRELOAD trap (separate from libexpat)
`import coiled` under reticulate dies with:
`libcrypto.so.3: version 'OPENSSL_3.3.0' not found (required by _ssl.cpython-312…)`.
Cause: R links the SYSTEM libcrypto (Ubuntu 24.04 ships only up to OPENSSL_3.0.9),
but the venv's `_ssl` needs 3.3.0. The venv HAS a modern libcrypto at
`~/.virtualenvs/r-mosaic/lib/libcrypto.so.3`, but it's loaded too late —
LD_LIBRARY_PATH does NOT help because the system libcrypto is already mapped into
the R process before `_ssl` imports. **Fix = LD_PRELOAD the venv libcrypto+libssl**
(same trick the `r-mosaic-Rscript` wrapper uses for libexpat). The wrapper does
NOT yet do this — launch Coiled runs with:
```
LD_PRELOAD="$HOME/.virtualenvs/r-mosaic/lib/libstdc++.so.6:\
$HOME/.virtualenvs/r-mosaic/lib/libexpat.so.1:\
$HOME/.virtualenvs/r-mosaic/lib/libcrypto.so.3:\
$HOME/.virtualenvs/r-mosaic/lib/libssl.so.3" \
LD_LIBRARY_PATH="$HOME/.virtualenvs/r-mosaic/lib" \
RETICULATE_PYTHON="$HOME/.virtualenvs/r-mosaic/bin/python" R_LIBS_USER="$HOME/R/library" \
Rscript <script>
```
PSOCK-local (no Coiled) runs don't hit this — only the `import coiled` path. A
permanent fix is to add the libcrypto/libssl preload to `~/bin/r-mosaic-Rscript`
(flag to user/maintainer; it's a shared-wrapper infra change other runs depend on).
