---
name: engine-tests-ci-gap
description: All engine-backed (laser-cholera) testthat tests SKIP in CI because CI never installs the Python engine — they only run locally/on the VM where the venv exists
metadata:
  type: project
---

The R-CMD-check GitHub workflow (.github/workflows/R-CMD-check.yaml) sets up Miniforge and
`RETICULATE_PYTHON` but NEVER installs laser-cholera into reticulate's env (no
`MOSAIC::install_dependencies()` / `pip install laser-cholera` step). So
`skip_if_no_python_likelihood()` (helper-skips.R, reads option `mosaic.test.has_likelihood`
from setup-python.R) always skips on CI.

**Consequence:** every engine-backed parity/regression test — including the v0.48.0
`test-dask_worker_score_window_parity.R` (guards the loc_idx peak-sourcing fix + per-cell weight
parity), the python-parity suite, and run_LASER smokes — NEVER runs in CI. They run only on the
local dev machine / hedgehog / dugong where the r-mosaic venv exists. Local skip count ~21 (engine
present); CI skip count ~32+ (engine absent).

**Why it matters:** a CRITICAL-path regression guard that CI can't execute is a guard you only get
when a human remembers to run `devtools::test()` locally. The loc_idx fix is exactly the silent-
shape-term-collapse class (Lesson #12 family); its only guard is local-only.

**How to apply:** flag on any PR that adds an engine-backed regression test as the SOLE guard for a
production-path fix. The real remediation (install the engine in CI, or a nightly engine job) is an
infra decision for the user / swe, not a maintainer edit. Note v0.47.4 only fixed CI to install
*Suggests* (so the testthat step runs at all) — it did NOT add the Python engine.
