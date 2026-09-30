---
name: ocv-scenarios-cfr-v21-and-engine-compat
description: MOSAIC-OCV scenario runners under CFR v2.1 + R engine: never pass deaths_integration per counterfactual arm (refit erases averted deaths); every registry model (MOSAIC 0.37-0.55) fades out under the >=0.89 engine
metadata:
  type: project
---
The MOSAIC-OCV scenario runners (`code/R/run_scenarios.R`, `run_E5_burden.R`) were ported off `run_LASER` to
`run_simulation()` in OCV commit 2da28be (2026-09-29).

**Counterfactual CFR trap.** `calc_model_ensemble(deaths_integration=)` redraws deaths from the CFR posterior
CONDITIONAL ON EACH PATH's cases vs the OBSERVED deaths. If you apply it to a scenario arm, a campaign in an
observed year gets refit to a higher CFR, so averted deaths vanish. NGA v0.98 test (20M doses in 2021): 4.8% of
cases but 0.5% of deaths averted. The fix: take one draw per member from its conditional CFR, on a baseline path
(`MOSAIC:::.mosaic_posthoc_deaths`), and write it into that member's `mu_jt` (`MOSAIC:::.mosaic_apply_cfr_posterior`)
BEFORE any arm is built. That gives 5.0% of cases and 6.6% of deaths averted, with baseline deaths at the calibrated
level (3,182 vs 3,330 observed).
**Why:** paired counterfactuals need a CFR fixed across arms. A per-arm refit conditions on data the
counterfactual did not produce.
**How to apply:** any OCV/scenario/averted-deaths code on MOSAIC >= 0.96. The run-level median bake
(cfr_posterior.csv) over-predicts baseline deaths by ~20%, so use it only as a fallback.

**Engine compat.** MOSAIC v0.89.0 (per-capita env dose-response) broke reproduction of older fits. The registry
MOZ v2026-07-01.01 (MOSAIC 0.55.12) re-simulates to ~10 cases vs ~88k in its own medoid predictions. The runners
now refuse models calibrated before v0.89.0, and that is every model in MOSAIC-results as of 2026-09-29. So OCV
scenarios need re-calibration under MOSAIC >= 0.97.
Also: a pre-v0.99 `deaths_integration.rds` uses a different setup (`carry_from`/`carry_to`, no `forecast_years`),
and v0.99.9 reads its forecast years at the location offset. Its forecast shift cannot be rebuilt.

Test rig: a stub registry (`$MOSAIC_RESULTS_REPO` -> a dir with REGISTRY.json + scripts/load_model.R serving a local
run dir), plus a /tmp copy of OCV code/config so `ocv_alloc_version` cannot touch the repo's `current` symlinks.
Run the ensemble with `--no-parallel` under load_all ([[reference_rengine_cost_model]]: PSOCK workers load the
INSTALLED build).
