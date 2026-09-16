---
name: laser-import-name-dot-not-underscore
description: The laser-cholera engine imports as DOTTED laser.cholera in the r-mosaic env; laser_cholera (underscore) FAILS. CLAUDE.md/memory notes saying underscore are stale.
metadata:
  type: reference
---

The laser-cholera Python engine is importable **only** as the dotted namespace
package `laser.cholera` in the current `~/.virtualenvs/r-mosaic` env. Verified
2026-07-09:

- `import laser.cholera` -> works; `laser.cholera.__version__` = "0.16.1"
- `import laser_cholera` (underscore) -> `ModuleNotFoundError`
- Distribution name in `laser-cholera/pyproject.toml` is literally `name = "laser.cholera"`;
  `src/laser/cholera/__init__.py` sets `__version__ = version("laser.cholera")`.

**Why:** the root `CLAUDE.md` "LASER Model Integration" snippet and the user's
auto-memory both show `reticulate::import("laser_cholera")` (underscore). That is
**stale** — it triggers false "this is a bug" reviews (e.g. PR #117's parity guard,
which correctly used `import("laser.cholera")[["__version__"]]`). The
production engine bridge (`run_MOSAIC.R`, `calc_model_ensemble.R`,
`make_mosaic_cluster.R`, `run_LASER.R`, `calc_Reff.R`, `lock_python_env.R`) all
use the dotted form `laser.cholera.metapop.model`.

**How to apply:** dotted `laser.cholera...` is correct. Treat any code using
`import("laser_cholera")` as a latent bug. Two files still use the broken
underscore form: `R/prefit_rolling_cv_psi.R:489` and
`inst/examples/forecast_cv_experiment.R:334` — flag/fix if you touch those paths.
Always verify against the live venv, not the CLAUDE.md snippet.
