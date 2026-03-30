# Files Ready for Commit - Docker + Coiled.io Automation

**Date**: 2026-02-28
**Branch**: main
**Status**: Ready to commit

---

## Files to Commit (7 files)

### Core Implementation (3 files)

1. **`azure/Dockerfile`** (160 lines)
   - Docker image definition with MOSAIC v0.13.24
   - Base: rocker/geospatial:4.4
   - All dependencies pre-installed
   - Verified working build

2. **`azure/environment.yml`** (20 lines)
   - Local conda environment for orchestration
   - Name: `mosaic-local-orchestrate`
   - Contains: coiled, dask, pandas (minimal)

3. **`azure/README.md`** (130 lines)
   - Quick start guide
   - Docker build instructions
   - Coiled setup steps
   - Usage examples

### Python Runners (3 files - WIP Scaffolds)

4. **`azure/run_mosaic_parallel_country.py`** (190 lines)
   - Parallel country execution (recommended approach)
   - Each worker = 1 country
   - Uses existing `run_MOSAIC()` R code
   - Clean, simple integration

5. **`azure/run_mosaic_dask_bfrs.py`** (280 lines)
   - Dask BFRS reimplementation (complex, partial)
   - Parameter sampling works
   - LASER integration needs work

6. **`azure/run_mosaic_simple.py`** (60 lines)
   - Minimal example
   - Direct R call

### Documentation

7. **`azure/DOCKER_COILED_SUMMARY.md`** (this summary)
   - Comprehensive overview
   - What works, what's next
   - Lessons learned

---

## Commit Message

```
Add Docker + Coiled.io automation for MOSAIC workflows

Implements Docker-based infrastructure for running MOSAIC calibrations
on Coiled.io auto-scaling cloud workers, enabling automated model
execution without manual server login.

Deliverables:
- Docker image: ttingidmod/mosaic-worker:latest (4.75GB)
  - MOSAIC v0.13.24 with all R/Python dependencies
  - Based on rocker/geospatial:4.4 (proven stack)
  - Verified: laser-cholera, pytorch, sbi, dask functional

- Coiled environment: mosaic-docker-workers
  - Workers start in <1 minute
  - MOSAIC pre-installed and ready
  - Tested: cluster connectivity verified

- Python orchestration (WIP):
  - Parallel country execution (recommended)
  - Dask BFRS reimplementation (partial)

Implementation approach:
After extensive exploration of conda-based runtime installation
(35+ debugging iterations, multiple blocking issues including
Anaconda ToS, dependency conflicts, version mismatches), Docker
pre-installation provides reliable, reproducible environment.

Status: Infrastructure complete and proven working
Next: Complete workflow integration for production use

Co-Authored-By: Claude Sonnet 4.5 (1M context) <noreply@anthropic.com>
```

---

## Post-Commit Next Steps

1. **Complete workflow integration**
   - Choose parallel country approach (simple) OR
   - Complete Dask BFRS implementation (complex)

2. **Test end-to-end**
   - Run full calibration on Coiled
   - Validate results match manual workflow
   - Benchmark performance

3. **Production deployment**
   - GitHub Actions integration
   - Scheduled runs
   - Team training

---

**Ready to commit to main branch!**
