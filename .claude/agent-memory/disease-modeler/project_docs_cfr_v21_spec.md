---
name: docs-cfr-v21-spec
description: MOSAIC-docs 04/05 rewritten for CFR v2.1 (2026-09-29, commit 17403b1) - new symbols awaiting maintainer approval, and code/data discrepancies found while verifying
metadata:
  type: project
---

MOSAIC-docs spec now describes the CFR v2.1 engine: p^fatal_jt = mu_jt*rho/(rho_deaths*chi_epi), deaths at onset
(never enter I1), reported on l_cases; mu_jt prior = est_CFR_hierarchical GAM -> make_mu_jt (logit-linear between
1 July anchors); likelihood integrates mu_jt out (xi_j offset + e_{j,yr} yearly levels, 60-day 1-Jan blend,
weekly quasi-Poisson, varphi_j, 2% bg, Laplace); forecast shift bar{e}_j.

**Why:** retired mu_j0/mu_j1/mu_j_epi/CFR_target/l_deaths were still in the spec.

**How to apply:**
- Symbols p^{fatal}_{jt}, mu^0_{jt}, xi_j, e_{j,yr}, bar{e}_j, varphi_j, sd_year/sd_product/sd_country were
  introduced WITHOUT style-guide approval (STYLE-GUIDE.md not updated; it still lists mu_{j,0} etc.). Code uses
  a_j/delta_{j,y}/phi_j/sigma/tau - those collide with seasonality a, decay delta, vaccine phi, sigma, tau_i.
- 05's generic NB mean is also mu_jt (pre-existing collision with the CFR symbol).
- Shipped config/priors tau_i (overland E3 weekly/7, lognormal prior) do NOT match what data-raw builders
  produce (they read param_tau_departure.csv = air, beta prior); shipped omega/gamma = "_blend" gravity params
  from hold/defaults-rebuild-2026-09-28. Provenance gap, not fixed.
- Medoid selection (run_MOSAIC.R ~2790) measures distance on location 1 only.
