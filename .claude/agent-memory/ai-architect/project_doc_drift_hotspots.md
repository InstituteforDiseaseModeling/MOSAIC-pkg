---
name: project-doc-drift-hotspots
description: Where MOSAIC prose docs drift out of sync with code after engine/API changes (found in the v0.99.9 deep review, 2026-09-29)
metadata:
  type: project
---

Places where the docs repeatedly go stale after big refactors (pure-R engine v0.68, Coiled removal v0.67/0.70, CFR v2.1 v0.96, alpha_1 pinned v0.93):

- The root `~/MOSAIC/CLAUDE.md` is not in any git repo, so package PRs never update it. After the port it still told agents to run the engine via `reticulate::import("laser_cholera")` and pointed at `azure/` and the local-only `model/LAUNCH.R`.
- The VM skills (hedgehog-run, dugong-run) and the roster README keep "Coiled hybrid / dask_spec" sections after the backend was removed.
- The diagnose-fit parameter map calls `beta_j0_tot` "dead". That is true only for the engine: calibration samples `beta_j0_tot` + `p_beta` and derives hum/env from them.
- run-mosaic says `alpha_1` is sampled by default. It has been pinned since v0.93.0.
- `mosaic_control_defaults()` roxygen mislabels sampling flags (tau_i, iota, gamma_2) and documents a `percentile_min` target that does not exist.
- NEWS.md has no entries for v0.74-v0.91, including the v0.89.0 engine semantics change.

**Why:** code PRs update roxygen but not the skills, the agent prompts, or the root CLAUDE.md.
**How to apply:** after any engine or API release, grep `.claude/skills`, `.claude/agents` and `~/MOSAIC/CLAUDE.md` for the renamed or removed names. Check every `sample_*` default claim against `mosaic_control_defaults()`.
