---
name: priors-estimation-review-v0999
description: 2026-09-29 deep review of est_*/fit_*_from_ci at v0.99.9 - confirmed defects in IC priors, seasonal envelope, V1/V2 phi, zeta_ratio channel, CI fitters
metadata:
  type: project
---
Deep review of the priors-estimation component at main dc6d30ba7 (v0.99.9), all confirmed by reproduction:

- fit_beta_from_ci(moment_matching) is broken below ~0.02: approx_mean clamped to [ci_lo+0.01, ci_hi-0.01] (absolute),
  so a requested x100 CI around 1e-6 comes back as x1.6, and mode 1e-3 / x100 gives mean 0.06. Feeds est_initial_E_I/R/S
  and calc_model_posterior_distributions (prop_E/I posteriors). fit_lognormal_from_ci shifts wide CIs up by e^{sdlog^2}
  (CI [1e6,1e12] -> [2e11,9e17]). fit_gompertz mode identity wrong (argmax at 0 when eta>1); dormant, no gompertz prior shipped.
- Seasonal a/b priors: at prior means 1+Fourier goes NEGATIVE for 25/40 countries (NAM trough -8.5); engine clamps
  Lambda at 0. Coefficients are fit on day-of-year but the engine uses t = tick since date_start, so non-Jan-1 starts misphase.
- est_initial_V1_V2 counts raw doses and claims the engine splits V by phi - FALSE; the R engine V1/V2 are effective-immune
  (docs 04 eq: V1 gains phi_1*nu). IC V1/V2 overstated by 1/phi. nu_1 gets ALL doses (make_config_default nu_2=0).
- zeta_ratio: priors_default uses DIRECT channel A (meanlog 4.31, sdlog 4.39; P(ratio<1)=16%) but est_zeta_ratio_prior
  docs + param_zeta_ratio_prior.csv say COMBINED C (10.26, 2.57); sample_parameters.R:630 comment assumes C.
- est_initial_R reads priors$parameters_location$rho/chi/fourier_params which do not exist -> rho=0.1 chi=0.5 fixed
  (4.3x vs rho prior mean 0.43) and disaggregate=TRUE silently uses mid-year placement.
- est_initial_E_I hardcodes rho U(0.2,0.7) parallel vs U(0.05,0.30) sequential (2.9x), ignores rho/chi/delta priors,
  and treats report-minus-delay as time since infection (omits incubation).

**How to apply:** before trusting any IC/seasonality/zeta prior or a staged posterior->prior update, check whether these
were fixed (grep the lines); do not derive new priors via fit_beta_from_ci for small-valued quantities.
