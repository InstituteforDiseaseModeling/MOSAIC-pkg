---
name: deaths-embargo-dwell
description: Infection->reported-death dwell from priors (~15-17d median) => a forecast-CV DEATHS embargo must be >=6wk (not 2wk); 2wk scores fitted-tail deaths, not forecast deaths
metadata:
  type: reference
---

Quantified infection->reported-death dwell from the shipped priors (make_priors_default.R),
for red-teaming forecast-CV embargo/gap choices on the DEATHS channel.

Chain (death is a competing hazard OUT of I1 at rate mu_jt every I1 tick — spec eq mu-jt +
transitions table, per-tick death prob 1-e^-mu_jt — so deaths smear across the whole I1 dwell,
NOT at its end):
- latent 1/iota: median ~1.4d (iota lognormal(-0.337, 0.4))
- onset->death ~ I1 dwell 1/gamma_1: median ~10d, mean ~11.4d, 95% CI 3.7-26.8d
  (gamma_1 lognormal(log(1/10), 0.5))
- death-event->report l_deaths = delta_reporting_deaths: TruncNorm(4,3,1,14), mean ~4d
  (death-event-to-report, NOT onset-to-report; onset-to-death is implicit in gamma_1)

=> infection->reported-death: median ~15-17d, right tail to ~45d (I1 95% ~27d + report to 14d).

IMPLICATION: a DEATHS embargo of 2wk (14d) scores deaths almost entirely determined by
PRE-cutoff infections (fitted end-state), NOT the post-cutoff forecast. The correct embargo is
>=6wk (~42d) to clear the full kernel so scored deaths are driven by post-cutoff transmission.
CASES chain is shorter (onset->report, no death hazard) so ~2wk is defensible for cases.
The "clears ~2 generation intervals" justification is the TRANSMISSION GI (cases) — wrong kernel
for deaths. See [[project_prod_deaths_bias_b2_epi_gap]] for the separate ~2x deaths-magnitude bias
(distinct from this timing issue). Verdict rendered in the OCV-4 forecast-CV red-team (2026-07-06).
