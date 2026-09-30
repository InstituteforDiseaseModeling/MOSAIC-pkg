---
name: project-v093-review-dose-ic-psi
description: v0.93.0 prod-readiness review (2026-09-29) — per-capita dose fix only desaturates at config zeta_1 MODE (prior median 416x higher => 62% draws saturated, human share ~0.2%); config_default now 37x under-transmits; 2018 IC seeding = insufficient_data Beta(1,999) fallback (ETH 16.5k sim vs 0 obs); shipped psi_jt unreproducible from committed CSV
metadata:
  type: project
---

Findings from the v0.93.0 review (report: claude/review_v093/dm_report.md, local laptop).

- **Dose-response (v0.89.0 W/N):** response depends only on zeta_1/kappa. config_default zeta_1 = 3.29e8 is the MODE of LN(25.654,2.458); the prior median is 1.4e11 (416x higher). The v0.89/v0.90.3 "A1/A2 resolved, Lambda:Psi within 27%" claim was measured on config_default only. Over 60 prior draws: 62% have median active-cell response >0.9 and the human share is 0.14-0.28% in every draw. config_default itself gives 32k reported cases vs 1.19M observed (old regime 2.55M; zeta at median 1.98M).
  **Why:** the priors for zeta_1, kappa and beta_j0_tot were not re-derived on the per-capita scale.
  **How to apply:** any kappa/zeta/beta prior work must be validated on PRIOR DRAWS, not on the config point. Stage-1 beta recentres (v15.14) were fitted in the saturated regime.
- **IC seeding in 2018 defaults (priors v15.18, build_date_start 2018):** est_initial_E_I's `insufficient_data` branch (rows present, zero cases) assigns Beta(1,999), mean 1e-3, which is 10x the no-data default. That inversion is the root bug. make_priors_default's uncited `adjustment_factors_E_I` table is 2023-epoch hand tuning with ETH/BDI missing. Result: ETH I0 = 114,622 gives 16.5k simulated reported cases in d1-28 vs 0 observed. The v15.18 description omits the IC re-seed. See [[reference-inert-params-and-prior-defects]].
- **psi provenance:** v0.77.0 config_default psi_jt correlates only 0.65 with the psi CSV committed in the same commit (the prior commit's pair matched exactly). config version stayed "4.7" with a stale description; the builder default date_start is 2023, not the shipped 2018.
- nu_2_jt is hard-zeroed in make_config_default.R:396, so 2018-22 two-dose campaigns all enter as first doses and phi_2 is inert. zeta_ratio P(<1) = 16.3% (the sample_parameters comment claiming 1e-6 is stale). Pinning ALL psi_star flags skips the transform, so b falls to 0 instead of the shipped +1.
