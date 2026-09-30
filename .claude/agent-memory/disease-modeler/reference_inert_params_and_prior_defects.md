---
name: inert-params-propS-rhodeaths-phi2
description: Three provably-inert sampled quantities (prop_S_initial never drawn, rho_deaths cancels algebraically under B2.1, phi_2 dead without second doses) plus the zeta_ratio 16% biology violation and the ETH E/I insufficient_data fallback
metadata:
  type: reference
---

Verified 2026-09-17 against MOSAIC v0.90.3 (worktree) + ETH x 25,000 run. Each is a concrete,
uncited-or-mis-cited biological assumption in my files.

## 1. `prop_S_initial` is NEVER sampled
`R/sample_parameters.R:773-775`:
```r
# Define IC compartments - NOTE: S is calculated/adjusted, not sampled
ic_compartments_sample <- c("V1", "V2", "E", "I", "R")  # Only sample these
```
S is the simplex residual. Setting `prop_S_initial` to its 2nd vs 98th prior percentile produces
a byte-identical config (`S_j_initial = 110,680,428` both ways) and ΔlogL = **exactly 0**.

Three harms: (a) `est_initial_S()`'s per-country Beta (re-derived across all 40 ISOs in priors
v15.17, 208 scalar updates) is discarded at sample time; (b) the realised S for the ETH best draw
is **0.982** vs `est_initial_S`'s **0.885** -- ~11M people, and S multiplies the FOI; (c) the
diagnostics compare the residual-derived S against the unused Beta, producing a nonsense
**47 prior-SD shift / 96x CI ratio** in `parameter_estimates.csv` and the posterior plots.

Fix: drop `prop_S_initial` from `priors_default` and from posterior/sensitivity reporting; use
`est_initial_S()` as a VALIDATION check on the implied residual instead.

## 2. `rho_deaths` cancels exactly out of the deaths channel under B2.1
`mu_j_baseline = CFR_target * (1-exp(-gamma_1)) * rho / (rho_deaths * chi_epidemic)`
(`R/sample_parameters.R:660-690`), and `reported_deaths ~ round(Binom(I1, 1-exp(-mu_j)) * rho_deaths)`.
To first order in mu_j (mu_j ~ 0.005/day, so exact to <0.3%):
`reported_deaths ~ CFR_target * (1-e^-g1) * rho * I1 / chi_epidemic` -- **rho_deaths cancels**.
It appears nowhere in the cases channel. Measured: 119-nat profile range, z=2.69 (below the noise
floor), non-monotone. **PIN it** at the Beta(36.95, 51.02) mean 0.4194 (Routh 2017 / Shikanga 2009
/ Bwire 2013 random-effects pool -- derivation unchanged).
**DONE 2026-09-23** (priors_default v15.19, pinned at 0.42); gate numbers in
[[r2-pins-rho-deaths-delta]]. `delta_reporting_deaths` pinned at 5 in the same change.

## 3. `phi_2` is inert wherever there are no second doses
ETH config `nu_2_jt` sums to **0** over all 3,322 days (`nu_1_jt` = 27.6M over 986 days).
`phi_2` gates only NEWLY delivered second doses. ΔlogL = exactly 0. `omega_2` also flat (90 nats).
Fix: make `sample_phi_2`/`sample_omega_2` data-aware -- pin when `sum(nu_2_jt)==0`, and log it.
Conditional pin, not permanent: phi_2 IS identifiable where two-dose campaigns exist.

## 4. `zeta_ratio`: 16.3% of draws are biologically invalid
Shipped `Lognormal(meanlog=4.3133, sdlog=4.3938)` => `P(zeta_ratio < 1) = 0.1631`, i.e. 1 draw in
6 has **asymptomatics shedding MORE than symptomatics** -- the exact failure the zeta_1/zeta_ratio
reparameterisation exists to prevent (`04-model-description.Rmd {#sec:shedding}` says so
explicitly). 95% CI spans 7.5 orders of magnitude (0.0136 to 4.1e5), the widest prior in the model.
`R/sample_parameters.R:716-720` claims P(violation) ~ 1e-6 based on "meanlog ~10 and sdlog ~2" --
**that is not the shipped prior**. `est_zeta_ratio_prior.R` applies no truncation.
Fix: left-truncate at 1 in `make_priors_default.R`; correct the comment and the spec.
**FIX WRITTEN 2026-09-30 (branch fix/handoff-h2-priors, user-approved):** lognormal family gained
optional `lower`/`upper` in `sample_from_prior()` (inverse CDF; untruncated path still `rlnorm`, same
RNG stream); builder emits `lower = 1`; bounds carried through `update_priors_from_posteriors()` and
`inflate_priors()`. Takes effect only at the next priors_default rebuild (v16.1 .rda has no `lower`).
Truncated: median 75 -> 185, 95% [1.4, 5.7e5]. Rationale: Smith 2026 OR<1 is household transmission,
not per-day shedding.

## 5. ETH `prop_E_initial` / `prop_I_initial` are the `insufficient_data` fallback
Both are `Beta(1, 999)` = the `"insufficient_data"` branch (`R/est_initial_E_I.R:360, 395`),
mean 1e-3 -> **114,622 infectious people at t0** while surveillance records ZERO cases for the
first 77 days. `est_initial_E_I()` ships THREE no-data defaults spanning 4 OOM -- Beta(1,999)
(1e-3), Beta(1,9999)/Beta(0.5,9999.5) (1e-4), vs the CLAUDE.md template Beta(0.01,99999.99)
(1e-7) -- and the largest fires for ETH. `ic_moment_match` defaults FALSE so nothing anchors it.

**BUT measured cost is small**: scoring E=I at 1 / 500 / 78,144 / 410,000 people spans only
**119 nats (SE ~42)**; best level (500) is +89 nats over the prior median. My prior hypothesis
that the IC dominated the likelihood surface was WRONG -- 3,322-day window + 30-day burn-in +
gamma_1 ~ 0.05-0.1/day washes the IC out. So: pin E/I (set `ic_moment_match = TRUE`) for
biological defensibility and 2 reclaimed dimensions, not for fit.

Related: [[param-identifiability-eth-v0903]], [[likelihood-noise-floor]]
