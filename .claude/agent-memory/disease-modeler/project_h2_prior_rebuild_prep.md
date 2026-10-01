---
name: h2-prior-rebuild-prep
description: 2026-09-30 builder decisions for the next priors_default rebuild - E/I variance_inflation uniform 10 (derived), Harris 2008 sigma row corrected to 127/202, zeta_ratio lower=1
metadata:
  type: project
---
- **E/I variance_inflation_E_I = 10 (uniform)**, replacing hand-tuned 30-200 per-country table.
  Derivation: est_initial_E_I keeps MC mean, width = VI only. Reporting chain chi/(rho*sigma) under
  global priors 95% ~ [m/4.3, 3.1m] (log-SD 0.65); iota/gamma_1 sdlog 0.40/0.50; combined x/4.7;
  x2 for lookback/timing/count noise -> 10. VI>=30 gives shape1<1 (density diverges at 0 = favours
  no infection where cases are observed). adjustment_factors_E_I already removed (a12acfc8e).
- **Harris et al 2008 (PMC2271133)**: paper reports 202 definite infections of 944 contacts, 127 (62.9%)
  symptomatic -> row now 0.629 [0.558, 0.695] (exact binomial). Old 0.184 [0.112,0.256] untraceable.
  sigma prior in builder is HARDCODED Beta(4.30,13.51) (mean 0.24), not refit from the table; the
  table-driven est_symptomatic_prop fit moves Beta(3.49,12.06) -> Beta(3.73,7.08) (mean 0.22 -> 0.35).
  DECIDED 2026-09-30: refit to Beta(3.75,7.12) in priors v17.0 (builder reads the CSV) - see [[v0100-rebuild-priors-v17]].
- zeta_ratio truncation: see [[inert-params-and-prior-defects]].
