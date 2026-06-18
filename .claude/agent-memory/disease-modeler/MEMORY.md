# Disease-Modeler Agent Memory — Index

- [Best-subset preset epi red-team (R1 BLOCK -> R2 LIFTED)](project_best_subset_geometry_epi_review.md) — v1 imposed uncertified A=0.95 geometry (harvest has no bio posteriors, CFR diag postdates runs); v2 DROPS geometry -> spec defaults A=0.70/CVw=1.0 -> epi block LIFTED; lone CONCERN: subset_size floor=30 thins implied-CFR posterior under the 20-member CI floor
- [FOI mixing exponents alpha_1 / alpha_2](reference_alpha_mixing_exponents.md) — alpha_1=numerator exponent (Glass 2003), alpha_2=density-vs-freq (McCallum 2001); spec 0.95/0.95 is STALE placeholder; pin 0.27/0.5 (prior mode + engine default); FIX both in Phase 1 AND Phase 2 (confounded w/ beta + network ridge)
- [CFR / mu_j0 identity (laser-cholera v0.13+)](reference_cfr_mu_j0_identity.md) — mu_j0 = CFR_reported * rho/(rho_deaths*chi); deaths LL identifies the PRODUCT mu_j0*rho_deaths, not the factors; rho_deaths prior Beta(36.95,51.02) pins it ~0.42
