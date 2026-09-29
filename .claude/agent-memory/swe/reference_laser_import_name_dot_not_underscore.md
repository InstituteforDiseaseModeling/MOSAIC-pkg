---
name: laser-import-name-dot-not-underscore
description: HISTORICAL — the Python laser-cholera engine and its reticulate bridge were removed in v0.68.0/v0.69.0; no R code imports it any more. Kept only because the dotted `laser.cholera` name still matters when reading the read-only oracle repo.
metadata:
  type: reference
---

**STATUS: obsolete for the engine path as of MOSAIC v0.69.0.** Re-verified
2026-09-16: `grep -rn "laser_cholera\|laser\.cholera" R/ inst/examples/` returns a
single *comment* in `calc_model_ensemble.R:670`. `R/run_LASER.R` and
`R/lock_python_env.R` no longer exist, and `inst/python/environment.yml` records
that laser-cholera, laser-core, numba and llvmlite were dropped. The transmission
engine is pure R (`run_simulation()` in `R/sim_engine.R`); the only remaining
reticulate consumer is the keras3 suitability model. **A worker that imports
Python on the simulation or calibration path is a bug.** The two broken-underscore
call sites this note used to flag (`R/prefit_rolling_cv_psi.R:489`,
`inst/examples/forecast_cv_experiment.R:334`) are gone.

## The one part still worth keeping

The distribution/import name is **dotted**, not underscored, and that is still
live knowledge when you go read the read-only oracle repo:

- `laser-cholera/pyproject.toml` has literally `name = "laser.cholera"`;
  `src/laser/cholera/__init__.py` does `version("laser.cholera")`.
- `import laser.cholera` works; `import laser_cholera` raises `ModuleNotFoundError`.
- Source paths are therefore `src/laser/cholera/metapop/*.py`.

Any surviving `laser_cholera` (underscore) string in docs or comments is wrong on
two counts now — wrong separator *and* describing a dependency that is gone.

For reading the oracle correctly — including the fact that the working checkout is
at the **wrong version** — see [[reference_engine_oracle_version_trap]].
