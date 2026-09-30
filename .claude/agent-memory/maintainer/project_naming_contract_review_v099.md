---
name: naming-contract-review-v099
description: v0.99.9 param-name contract trace (engine<->config<->priors<->sampler<->inventory<->spec); rda/json parity is clean, defects live in the inventory, sampler helpers and psi_star gating
metadata:
  type: project
---

Traced every param name at v0.99.9 (2026-09-29, frozen worktree review-main). Result:
config_default rda==json (JSON deliberately drops only `metadata`), priors_default rda==json,
config$mu_jt == make_mu_jt(priors_default$mu_jt) exactly. Engine reads (grep `config$` in R/sim_*.R)
are all in config_default except the legacy CFR_target/delta_reporting_deaths (intended).

Defects found (check whether they have been fixed before re-reporting):
- estimated_parameters still lists alpha_1 as scale "global" (priors moved it to location in v15.16);
  multi-loc columns alpha_1_ISO fall to scale "unknown" in calc_model_posterior_quantiles -> no posterior.
  Also tau_i listed as beta, but the prior is lognormal.
- create_sampling_args()/.get_all_sampling_params() read formals(sample_parameters), which has no
  sample_* formals since the sample_args refactor -> every pattern samples defaults. Reusable trap:
  any helper that introspects formals() breaks silently when args move into a list/`...`.
- .apply_psi_star_calibration skips entirely when all four psi_star flags are FALSE, so config
  psi_star_b=1 is ignored when pinned but applied when any sibling is sampled.
- sample_kappa dual-default drift (TRUE in sample_parameters, FALSE in control) STILL open, as of
  [[v093-release-review]].

**How to apply:** rerun claude/deep_review/x-naming-contract/cmp_cfg.R + cmp_pri.R on any data-object bump;
diff inventory scale column against names(priors$parameters_location).
