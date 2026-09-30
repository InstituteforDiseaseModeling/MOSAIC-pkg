---
name: render-figure-source-traps
description: render_MOSAIC_figures reads some figures from the wrong source (input config / global PATHS / unmasked series) -- check the SOURCE of each figure, not just its field names
metadata:
  type: reference
---

Found in the v0.99.9 plotting deep review (2026-09-29). The renderer is "pure read-render"
from the run dir, but several figures quietly read something other than the run's posterior.
Check where a figure's numbers come from, not just whether its field names are current.

- **Spatial figs 2-4** (`departure_tau`, `mobility_flux_matrix`, `mobility_flux_network`) come
  from `calc_mobility_flux(1_inputs/config.json)`, which is the INPUT config (prior/default
  tau_i, mobility_omega/gamma). Fig 1 prefers the engine `pi_ij_ensemble.rds` posterior median,
  so figs 1 and 3 disagree. The flux-matrix roxygen says "calibrated parameters".
- **psi_star diagnostic** reads `PATHS$MODEL_INPUT/pred_psi_suitability_day.csv` (the global
  current file), not the run's `config.json$psi_jt`. It is wrong for psi-refit runs and does not
  work post-hoc on another machine.
- **plot_model_ensemble caption R2/Bias/totals** mask only `score_idx`. The faceted captions mask
  nothing, and neither applies `.mosaic_mask_central_for_scoring()` (the cases_warmup mask), so
  they disagree with summary.json. The plotted line hides the warm-up transient but the caption
  includes it.
- **mobility_tau_ci.csv** is never written: `isTRUE(control$io)` is always FALSE (see
  [[psock-export-and-dead-guard-traps]]), so the tau CI bars are dead.
- `plot_model_subset_optimization` places the "optimal" point at max(score). On a flat profile
  `optimal_n` is the largest N, so the point floats. Use `subset_opt$optimal_score`.

**How to apply:** when a plot fix is proposed, grep what the renderer passes in (render_MOSAIC_figures.R)
as well as the plot body.
