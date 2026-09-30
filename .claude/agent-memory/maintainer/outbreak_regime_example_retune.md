---
name: outbreak-regime-example-retune
description: inst/examples/simulate_outbreak_settings.R sporadic/rare regimes are dead under the v0.89 per-capita dose; what a 2026-09-29 probe learned before handing to disease-modeler
metadata:
  type: project
---

`sporadic` and `rare` in `inst/examples/simulate_outbreak_settings.R` return zero cases for most seeds since v0.89.0 (per-capita env dose-response). v0.99.x added a loud zero-case warning + header note; the re-tune was handed to disease-modeler (not done).

Probe findings (harness was /tmp/osprobe, not kept):
- The toy configs (and `config_simulation_epidemic/endemic`) use zeta_1=7.5, kappa=1e5: under D=W/N the environmental route is effectively OFF (dose/(kappa+dose) ~1e-8). epidemic/endemic/recurring still work via beta_hum.
- Rescaling zeta to config_default scale (3.29e8, kappa 1e6) or zeta ~ 7.5*N makes outcomes BIMODAL: early extinction or continuous endemic activity. Never "quiet then triggered", because W decay is Poisson and integer-clamped (reservoir goes extinct), there is no importation term, and beta_jt_env is psi/mean(psi)-normalised so base psi level does not quiet the route.
- A genuine "rare single outbreak after years of quiet" likely needs an importation mechanism or a redesigned regime, not lever tweaks.

**Why:** saves re-deriving this when the retune comes back for review. **How to apply:** when reviewing the retune, demand a multi-seed check (>=10 seeds) that each regime shows its documented shape, not one lucky SEED.
