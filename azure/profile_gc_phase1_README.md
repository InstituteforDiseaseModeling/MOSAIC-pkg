# Phase 1 — Profile MOSAIC GC behavior

End-to-end instructions to collect three matched datasets in one run:

1. **`gc_events.csv`** — every R `gc()` call with timing + heap delta, written
   by instrumentation in `profile_gc_phase1.R`
2. **A `py-spy` flamegraph** — system-level CPU sampling (catches both R and
   Python frames; this is the same tool that produced the original 33%/7%
   number)
3. **A `samply` profile** — alternative system-level profiler (better stack
   resolution; can be opened in the Firefox Profiler UI)

After all three are collected we know:

- **Which gc() call sites cost the most wall time** (from `gc_events.csv`)
- **What R/Python is doing while gc() runs** (from py-spy / samply)
- **Whether the original 40% GC figure reproduces** in our scenario

If the diagnosis confirms, the same outputs become the baseline for Phase 2
A/B testing.

---

## 1. Prerequisites

- Docker image `idmmosaicacr.azurecr.io/mosaic-worker:latest` (or a newer build)
- `~/MOSAIC/MOSAIC-pkg` and `~/MOSAIC/MOSAIC-data` checked out
- Linux host with `perf_event_paranoid` ≤ 1 (needed for py-spy on processes
  it didn't spawn). Run `cat /proc/sys/kernel/perf_event_paranoid` — if > 1:
  ```bash
  sudo sysctl kernel.perf_event_paranoid=1
  ```
- Disk space: ~200 MB per run

## 2. One-time profiler install (inside the Docker image)

The base image doesn't ship py-spy or samply. Easiest path is to build a
**single-shot profiling image** that adds both tools on top of the base.

```bash
cat > /tmp/Dockerfile.profile <<'EOF'
FROM idmmosaicacr.azurecr.io/mosaic-worker:latest

# py-spy: pure Rust binary, available via pip
RUN /root/.virtualenvs/r-mosaic/bin/pip install --no-cache-dir py-spy==0.4.0

# samply: precompiled binary from GitHub releases.
# Note: the release asset is `samply-x86_64-unknown-linux-gnu.tar.xz`
# (no version-prefix duplication in the asset filename).
RUN set -eux \
 && curl -fsSL -o /tmp/samply.tar.xz \
      https://github.com/mstange/samply/releases/download/samply-v0.13.1/samply-x86_64-unknown-linux-gnu.tar.xz \
 && tar -xJf /tmp/samply.tar.xz -C /tmp \
 && cp "$(find /tmp -name samply -type f -executable | head -1)" /usr/local/bin/samply \
 && chmod +x /usr/local/bin/samply \
 && rm -rf /tmp/samply.tar.xz /tmp/samply-* \
 && samply --version

# gdb helps py-spy resolve symbols when stack unwinding native code.
# samply uses the perf_event_open syscall directly, so the `linux-perf`
# package is NOT required (and isn't available on Ubuntu Noble anyway).
RUN apt-get update && apt-get install -y --no-install-recommends gdb \
 && rm -rf /var/lib/apt/lists/*

# Smoke-test both binaries at the end so a broken install fails the build,
# not later at runtime.
RUN /root/.virtualenvs/r-mosaic/bin/py-spy --version && samply --version

# py-spy needs CAP_SYS_PTRACE; samply needs CAP_SYS_ADMIN for perf_event_open.
# Both are added at `docker run` time via --cap-add, no image changes needed.
EOF

docker build -f /tmp/Dockerfile.profile -t mosaic-worker:profile .
```

Verified working build outputs:
```
py-spy 0.4.0
samply 0.13.1
```

The image takes ~3 minutes to build and is ~50 MB larger than the base.

## 3. Run the profiling script

The container needs three things to profile correctly:

- `--cap-add SYS_PTRACE` so py-spy can attach to processes
- Volume mounts for the package source, data, and output
- The instrumentation script as the command

### 3.1 Plain run (no profiler — sanity check)

First make sure the instrumented script runs end-to-end and produces
`gc_events.csv`. Take ~10 minutes.

```bash
docker run --rm \
  -v ~/MOSAIC/MOSAIC-pkg:/src/MOSAIC-pkg \
  -v ~/MOSAIC/MOSAIC-data:/workspace/MOSAIC/MOSAIC-data \
  -v ~/output:/workspace/output \
  -e RETICULATE_PYTHON=/root/.virtualenvs/r-mosaic/bin/python \
  --cap-add SYS_PTRACE \
  mosaic-worker:profile \
  bash -c "R CMD INSTALL /src/MOSAIC-pkg >/dev/null 2>&1 && \
           Rscript /src/MOSAIC-pkg/azure/profile_gc_phase1.R"
```

Verify:
- `~/output/gc_profile_<stamp>/gc_events.csv` exists and has thousands of rows
- `gc_summary.csv` shows per-site totals
- Final stdout prints "GC as fraction of run: XX%"

That XX% on its own already answers the question "is 40% GC reproducible
in our scenario?" — if it comes back at 5%, the original report was on a
very different setup and Phase 1 is done.

### 3.2 py-spy run

`py-spy` samples the kernel of the target process at ~100 Hz and resolves
both Python and (with `--native`) C/R stacks. The recommended way is to
launch the Rscript under py-spy directly:

```bash
docker run --rm -it \
  -v ~/MOSAIC/MOSAIC-pkg:/src/MOSAIC-pkg \
  -v ~/MOSAIC/MOSAIC-data:/workspace/MOSAIC/MOSAIC-data \
  -v ~/output:/workspace/output \
  -e RETICULATE_PYTHON=/root/.virtualenvs/r-mosaic/bin/python \
  --cap-add SYS_PTRACE \
  mosaic-worker:profile \
  bash -c "R CMD INSTALL /src/MOSAIC-pkg >/dev/null 2>&1 && \
           /root/.virtualenvs/r-mosaic/bin/py-spy record \
             --native \
             --rate 100 \
             --idle \
             --format speedscope \
             --output /workspace/output/pyspy_phase1.json \
             -- Rscript /src/MOSAIC-pkg/azure/profile_gc_phase1.R"
```

What each flag means:

- `--native` — capture R/C frames in addition to Python. Critical for
  attributing time to R's gc().
- `--rate 100` — 100 samples/sec. Default is 100; bump to 250 for finer
  detail at the cost of overhead.
- `--idle` — include threads waiting on IO; useful for finding lock waits
- `--format speedscope` — output is a JSON loadable at
  <https://www.speedscope.app/>. Drag-drop the file to view.
- `--output` — writes inside the output mount so the file persists

The same `gc_events.csv` is still produced because the instrumentation
is independent of the profiler.

Open the JSON at speedscope and switch to the **Sandwich** view. Look
for stack frames containing:

- `R_gc_internal`, `do_gc`, `gc_inhibit_release_pages` — R's GC
- `collect`, `gc_collect_main` — Python's GC
- `Rf_eval`, `Rf_applyClosure` — generic R interp time (ignore for GC analysis)
- `numba_dispatcher`, `lc.run_model` — useful LASER frames

The **Left Heavy** view ranks functions by inclusive time — that's where
"33% GC" would show up.

### 3.3 samply run

`samply` is a newer profiler with better stack resolution and a richer UI
(Firefox Profiler). Use it as an A/B confirmation against py-spy.

```bash
docker run --rm -it \
  -v ~/MOSAIC/MOSAIC-pkg:/src/MOSAIC-pkg \
  -v ~/MOSAIC/MOSAIC-data:/workspace/MOSAIC/MOSAIC-data \
  -v ~/output:/workspace/output \
  -e RETICULATE_PYTHON=/root/.virtualenvs/r-mosaic/bin/python \
  --cap-add SYS_PTRACE --cap-add SYS_ADMIN \
  mosaic-worker:profile \
  bash -c "R CMD INSTALL /src/MOSAIC-pkg >/dev/null 2>&1 && \
           samply record \
             --rate 200 \
             --output /workspace/output/samply_phase1.profile.json \
             -- Rscript /src/MOSAIC-pkg/azure/profile_gc_phase1.R"
```

`SYS_ADMIN` is needed because samply uses perf_event_open under the hood
for kernel-side sampling.

Open the result at <https://profiler.firefox.com/> → "Load a profile from
file" → drop `samply_phase1.profile.json`. The "Call Tree" tab gives the
same kind of inclusive-time ranking as speedscope's Sandwich view.

## 4. What to look for in the outputs

### From `gc_events.csv` / `gc_summary.csv`

| Field | What it means |
|---|---|
| `site` | `<file>:<line>` of the call to `gc()` |
| `wall_sec` | wall-clock seconds spent inside gc() at this call |
| `mb_freed` | RAM reclaimed (negative = R allocated more during the call) |
| `n_calls` (summary) | total times the site was hit |

Rules of thumb:

- **Pure waste:** high `n_calls` × small `mb_freed_mean` × non-trivial `wall_mean`
  (e.g., `run_MOSAIC.R:332` called 300 times, freeing ~0 MB each — that's
  ~300 × wall_mean seconds of dead weight)
- **Working as intended:** few `n_calls` × large `mb_freed_mean` — keep these
- **Hot but useful:** high `n_calls` × meaningful `mb_freed_sum` —
  candidate for the threshold-triggered GC of Phase 3

### From py-spy / samply

Two specific stack frames worth quantifying:

| Stack frame | Percentage tells you |
|---|---|
| `R_gc_internal` or `do_gc` (inclusive) | Total wall-time fraction in R GC. Compare to the 33% in the original report. |
| `gc_collect_main` (CPython) | Total wall-time fraction in Python GC. Compare to the 7%. |
| `numba_dispatcher` + `lc.run_model` | "real work" baseline — useful work that GC competes against |

If both profilers agree that R GC is ≥ 25% of wall time, the original
report is reproduced and Phase 2 is justified.

## 5. Deliverable to compare against later phases

Save these three files together as the **Phase 1 baseline**:

```
~/output/gc_profile_<stamp>/
├── gc_events.csv                   # raw per-call log
├── gc_summary.csv                  # aggregated per-site
├── pyspy_phase1.json               # speedscope-loadable
├── samply_phase1.profile.json      # firefox-profiler-loadable
└── run.log                         # full Rscript stdout
```

Once Phase 2 changes are applied, re-running this exact script produces a
matched set of artifacts. A direct comparison of `gc_summary.csv` between
the two runs is the cleanest evidence that the fix works (or didn't).

## 6. Troubleshooting

| Symptom | Likely cause | Fix |
|---|---|---|
| `py-spy: Operation not permitted` | container lacks `SYS_PTRACE` | add `--cap-add SYS_PTRACE` |
| `samply: Error: perf_event_open` | kernel paranoia | `sysctl kernel.perf_event_paranoid=1` on host |
| `gc_events.csv` is empty | the trace was inserted before any gc fires | check `instrumented_gc` got swapped in (script prints `Instrumented base::gc — every call will be logged.`) |
| `gc_events.csv` has wrong `site` values | callstack walking failed | the script falls back to `<unknown>`; not fatal, but reduces signal. Try running with `Rscript --vanilla` to rule out interactive overrides. |
| Run is very slow under py-spy | `--native` adds overhead | drop `--native`; you lose R frame names but the percentages stay accurate enough |
| Different gc fraction than reported | the original was on a different scenario | adjust `n_simulations` / `n_iterations` / `parallel$n_cores` to match the original report, then rerun |
