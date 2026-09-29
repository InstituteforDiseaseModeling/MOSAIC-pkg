---
name: cfr-v21-d7-laplace-review
description: CFR v2.1 D7 integrated-deaths Laplace review (2026-09-28) - math verified exact; risks are the 1e-10 floor (-23 LL/death), 5-35x loss of deaths discrimination vs daily NB, missing priors$mu_jt producer, no feasibility bound mu<rho_d*chi/rho
metadata:
  type: project
---
Review of uncommitted `R/calc_log_likelihood_deaths_integrated.R` (worktree cfr-v21, 2026-09-28).

Verified: grad/Hessian match finite differences to 1e-8, Fisher matches MC E[-H] to 0.1%,
Laplace within 0.006 of 2-D/3-D quadrature, theta_sd calibrated (z sd 0.99-1.03, cov95 0.94-0.96
from 0.3 to 200 onsets/day). Engine alignment s = delta_reporting_cases + 1 is exact.

Durable traps:
- A zero-onset week with observed deaths costs D*log(1e-10), about -23 LL per death. The penalty
  size comes from the arbitrary floor constant (the same class as Lesson #5). ZMB: 7 of the top-20
  sims had such weeks, with penalties of about 160 LL, far above the deaths spread among good sims.
- Weekly scoring plus the integrated-out level shrink the deaths LL range among the top-10 sims to
  6-25 units, down from 56-220 under the old daily NB. Cases spread is 600-2500. Deaths are now
  nearly irrelevant to BFRS selection.
- The integrated level absorbs even a 33x under-prediction of onsets for a cost of only 28 LL, with
  the mode CFR reaching 0.75. There is no plausibility or feasibility bound on the CFR.
- The consumer reads `priors$mu_jt` (`sd_product`, `sd_year`, `location[[iso]]$logit_se`), but no
  code produces it. Every run falls back to sd_shift = 0.424 and sd_year = 0.7.
Scripts: claude/cfr_v21_review/stat/ in the worktree.
