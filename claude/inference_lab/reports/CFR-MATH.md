# CFR-MATH — the case-fatality process: priors, chain, and fidelity to the spec

**Agent:** CFR-MATH (disease-modeler role) · **Date:** 2026-09-19
**Question:** how much deaths fit and predictive accuracy is recoverable in the CFR process, quantified.

---

## 0. Substrate, and a protocol hazard to record

**Code read:** the shared lab worktree
`/private/tmp/claude-501/-Users-johngiles-MOSAIC-MOSAIC-pkg/c26a96f4-20ef-4a2a-8ee4-36b6e863f4c7/scratchpad/lab`
(local laptop). **The branch changed under me mid-session** — it was `inference-lab` (HEAD `f5461f1b8`)
when I started reading and `arm/A1b_v2` (HEAD `69aa076c5`) an hour later. Per PROTOCOL §2 that would
normally void the batch, so I bounded it:

```
git diff --stat inference-lab arm/A1b_v2
 DESCRIPTION                           |  2 +-
 R/calc_log_likelihood_distributions.R | 71 +++++++++++-----------
```

`sim_components.R`, `sim_params.R`, `sim_engine.R`, `sample_parameters.R`, `calc_implied_cfr.R`,
`data-raw/make_priors_default.R`, `data/priors_default.rda`, `data/config_default.rda` are **byte-identical**
across the two commits. Every number below comes from `sample_parameters()` + `run_simulation()` +
the data objects, and my likelihood code is hand-written inside my own scripts (both the baseline
`-y·log(1e6)` rule and the A1b epsilon floor), so **no result here depends on which branch was checked
out**. I verified my A1b hand-implementation is exactly equivalent to the arm's on this data:
the arm floors *every* cell at `eps_j`, mine floors only zero cells, and simulated `reported_deaths`
are integer counts so `0 < est < eps_j = 0.00784` occurs on **0 of 2,898** ETH cells.

**Recommendation to the coordinator:** give each agent its own worktree, or pin a SHA. A shared
worktree that another agent can `git checkout` is the exact failure PROTOCOL §2 exists to prevent,
and here it was silent.

**Evidence used:** local `/Users/johngiles/MOSAIC/output/eth25k_v0903/` (ETH, n=25,000, v0.90.3);
`dugong:~/prod100k_v087/3_results/summary.json` (40 locations, n=100,000);
`MOSAIC-data/processed/cholera/weekly/cholera_surveillance_weekly_combined.csv`;
`MOSAIC-pkg/model/input/param_mu_disease_mortality.csv`. ~2,700 fresh ETH simulations run locally
(8 cores, ~45 min total). The six new `dugong:~/inflab/m_*` 30k calibrations were **still running**
at the time of writing (156 R processes on dugong) — none of my results depend on them, but they
are the right confirmation set for CFR-MATH-01 and are listed in §WHAT I COULD NOT DETERMINE.

**Scripts:** `/private/tmp/claude-501/-Users-johngiles-MOSAIC-MOSAIC-pkg/c26a96f4-20ef-4a2a-8ee4-36b6e863f4c7/scratchpad/cfr/`
(`02`–`32`, plus `scalars_eth.csv`, `cfr_profile.csv`, `epi_factor_countries.csv`, `arms_paired_g18.rds`,
`prod/prod_cfr_audit.csv`).

---

## Executive summary — the five numbers

1. **The `(1 + eps)` divisor gap is live and is the dominant systematic error in the deaths channel.**
   Measured chain residual across 19 production countries with ≥50 observed deaths:
   **median 1.444, IQR [1.30, 1.66]**. `Gamma(3,6)`'s median `(1+eps)` is **1.446**. Fixing the
   derivation multiplies simulated deaths by **0.696** with cases invariant to <0.6%
   → post-A1b deaths bias **1.525 → ≈1.06**. Roughly 10 lines of code.
2. **`mu_j_epidemic_factor`'s +50% prior is uncited and points the wrong way.** In the periods the
   engine actually flags, the observed CFR is **0.41–0.54×** the endemic CFR (k = 3–10 countries,
   robust to case floors). The escalation that does exist is early-outbreak:
   **1.129 [0.860, 1.481]** (k = 20). Recommend `Gamma(1, 8)` (mean 0.125, mode 0, p97.5 0.46).
3. **"The deaths channel wants 5× the CFR while over-predicting 5×" is a zero-cell artifact, not a
   CFR signal.** 70% of ETH's observed deaths fall in daily cells where the simulation predicts
   exactly zero → a **10,970-nat** baseline penalty, versus a 2,462-nat NB-density range. That is
   SAMP's 10,029-nat `CFR_target` profile, exactly. A1b already fixes it; **every CFR change below
   must ship after A1b or the sampler will undo it.**
4. **`rho_deaths` cancels from the *entire distribution* of reported deaths, not just its mean.**
   Binomial thinning composition gives `rd ~ Binom(Isym, (1−e^{−mu})·rho_d)` with `mu ∝ 1/rho_d`;
   residual dependence 0.11%. Measured: **3.99%** over a 3.4× sweep, **1.7%** over the prior 95% CI.
   Pin it.
5. **Deaths R² is nearly maxed out.** An oracle knowing the true daily cases and the true CFR(t)
   reaches **0.588** on ETH; the model is at 0.371. Realistic recoverable range is **0.40–0.46**,
   and *none of it comes from the CFR priors* — B2.2's paired ΔR² is **−0.0001 ± 0.012**. The R²
   levers are the conditional-binomial structure (+0.03 to +0.09) and deaths timing (+0.022 ceiling).

**In one line:** the CFR process has a large, cheap, provable **bias** fix and essentially no
**R²** fix; the deaths R² deficit is an observation-granularity ceiling, not a CFR-model defect.

---

## 1. The chain as actually implemented

Every CFR parameter has **exactly one** engine use site. I grepped for all of them:

| quantity | engine site | form |
|---|---|---|
| `mu_j_baseline`, `mu_j_slope`, `mu_j_epidemic_factor` | `R/sim_components.R:190-192` | `mu_jt = mu_0 · (1 + mu_1·t/T) · (1 + eps·1[epi])` |
| epidemic indicator (deaths) | `R/sim_components.R:177-186` | `Isym[t − delta_reporting_cases] > epidemic_threshold · N_manual[t]` |
| disease deaths | `R/sim_components.R:194` | `dd[t] ~ Binom(Isym_surv[t], 1 − exp(−mu_jt))` |
| `rho_deaths`, `delta_reporting_deaths` | `R/sim_components.R:200-206` | `rd[t] += Binom(dd[t − delta_reporting_deaths], rho_deaths)` |
| `chi_endemic` / `chi_epidemic`, `rho` | `R/sim_components.R:283-289` | `rc[t] += round(Binom(new_sym[t − delta_c], rho) / chi_eff)` |
| B2.1 derivation | `R/sample_parameters.R:671-674, 690` | `mu_0 = CFR_target · (1 − e^{−gamma_1}) · rho / (rho_deaths · chi_epidemic)` |
| implied-CFR readback | `R/calc_implied_cfr.R:126, 172-183, 187-194` | `cfr_* = mu_eff · rho_deaths · chi_* / (rho · (1 − e^{−gamma_1}))` |

**The B2.1 derivation reproduces exactly.** Over the 25,000 ETH draws, max relative error between
the stored `mu_j_baseline` and `CFR_target · (1−e^{−gamma_1}) · rho / (rho_deaths · chi_epidemic)`
is **3.4e-15**; 0 draws hit the `1 − 1e-9` clamp. B2 is live and wired as documented.

**Exact closed form for the realized reported CFR.** Substituting the derivation into the engine
equations and summing over the window gives five multiplicative deviations from `CFR_target`:

```
realized_CFR / CFR_target  =  (1 + eps·f_epi)  ×  (chi_eff_bar / chi_epi)  ×  (1/k_ns)  ×  k_dd  ×  (1/k_rc)
```

where `f_epi` = death-weighted fraction of epidemic-flagged ticks, `chi_eff_bar` = case-weighted
harmonic mean of the effective PPV, `k_ns = sum(new_sym) / ((1−e^{−gamma_1})·sum(Isym))` (the
incidence-vs-recovery-flow / non-steady-state factor), `k_dd` the binomial-vs-rate factor, and
`k_rc` the `round(drawn/chi_eff)` low-count loss. **This decomposition reproduces the simulated
realized CFR to a median 2.8% across all 400 ETH draws I ran** (`03_decomp.R`, `06_analyse.R`).

Measured on the 115 ETH posterior members (median, IQR in the finding below):

| factor | median | reading |
|---|---:|---|
| `(1 + eps·f_epi)` | **1.380** | **the B2.2 gap — omitted from the derivation** |
| `chi_eff_bar / chi_epi` | 0.944 | partial offset from endemic-regime ticks |
| `1 / k_ns` | 1.038 | non-steady-state |
| `k_dd` | 1.008 | binomial vs rate |
| `1 / k_rc` | 1.013 | `round()` loss on cases |
| **product of the four non-eps factors** | **1.000** | **they cancel at the median** |
| realized/`CFR_target` (measured) | 1.419 | |

---

## 2. Findings

### CFR-MATH-01 — CRITICAL — the B2.2 `(1 + eps)` divisor gap is live, and is the single largest systematic error in the deaths channel

**Mechanism.** The derivation at `R/sample_parameters.R:671-674` builds `mu_0` from `CFR_target` and
the reporting chain but **not** from the epidemic multiplier the engine then applies at
`R/sim_components.R:190-192`. The two are keyed on the *same* indicator at the *same* lagged tick
(`sim_components.R:181` for deaths, `:283-285` for cases), so on an epidemic-flagged tick the engine
multiplies `mu` by `(1+eps)` **and** uses `chi_epidemic` — which is precisely the regime the
derivation anchors to. The omitted factor is therefore a clean multiplicative error, not a subtlety.

**The diagnostic column already reports it.** `R/calc_implied_cfr.R:178-183` computes
`cfr_epidemic = mu_eff_epi · rho_deaths · chi_epidemic / (rho · (1−e^{−gamma_1}))`, which under B2.1
collapses algebraically to `CFR_target · (1 + eps)`. Verified over 25,000 ETH draws:
**max absolute error 1.11e-16**. Likewise `cfr_baseline = CFR_target · chi_endemic / chi_epidemic`
(max abs error 5.55e-17). So the shipped implied-CFR columns are *internally consistent with the
derivation* and confirm the gap rather than hiding it.

**This falsifies the claim in the prior and in the spec.** `data-raw/make_priors_default.R:1741`
and `:1779` state "the realized implied CFR == CFR_target for EVERY draw, regardless of where
gamma_1/chi/rho/rho_deaths land". It is true with respect to the *chain* factors and false with
respect to `mu_j_epidemic_factor`, which was never in the identity.

**Production-scale quantification (`dugong:~/prod100k_v087`, 40 locations, n=100,000).** I decomposed
each country's implied-CFR error into a prior-centring term and a chain residual:

```
predicted_CFR / observed_CFR  =  (prior median CFR_target / observed CFR)  ×  chain residual
```

| iso | obs deaths | pred/obs CFR | prior/obs | **chain residual** |
|---|---:|---:|---:|---:|
| NGA | 5804 | 1.47 | 1.01 | **1.46** |
| COD | 5558 | 1.36 | 1.06 | **1.29** |
| MWI | 2228 | 1.74 | 1.03 | **1.70** |
| SSD | 1977 | 1.39 | 1.00 | **1.39** |
| ETH | 1136 | 1.56 | 0.96 | **1.62** |
| AGO | 991 | 1.15 | 0.96 | **1.20** |
| ZWE | 851 | 1.47 | 1.03 | **1.43** |
| ZMB | 808 | 1.84 | 0.94 | **1.94** |
| SOM | 370 | 4.52 | 2.13 | 2.12 |
| MOZ | 347 | 1.58 | 0.93 | **1.70** |
| TZA | 327 | 1.55 | 1.08 | **1.44** |
| CMR | 257 | 2.81 | 2.59 | 1.09 |
| NER | 253 | 1.39 | 0.84 | **1.65** |
| KEN | 180 | 3.26 | 1.95 | 1.67 |
| TCD | 165 | 1.19 | 0.91 | **1.31** |
| COG | 106 | 1.92 | 1.25 | **1.53** |
| UGA | 93 | 1.38 | 0.95 | **1.44** |
| ZAF | 90 | 0.85 | 1.19 | 0.72 |
| GHA | 71 | 1.34 | 1.30 | 1.03 |

(full 26-country table in `scratchpad/cfr/prod/prod_cfr_audit.csv`)

- **chain residual, 19 countries with ≥50 observed deaths: median 1.444, IQR [1.297, 1.659], geometric mean 1.420, sd(log) 0.246.**
- **`Gamma(3, 6)` median of `(1 + eps)` = 1.446. Mean = 1.500.**
- The prior *centre* is right: median `prior CFR_target / observed CFR` = **1.027**, IQR [0.955, 1.220].
  The large `pred/obs` outliers are **prior-centring** errors in the WHO-GAM CFR, not chain errors:
  CMR `pred/obs` 2.81 splits as prior 2.59 × chain **1.09**; BFA 0.05 splits as prior 0.05 × chain
  **1.05**; LBR 4.37 as 3.45 × **1.27**; NAM 3.13 as 2.80 × **1.12**. (SOM 4.52 = 2.13 × 2.12 and
  KEN 3.26 = 1.95 × 1.67 are the two countries where *both* terms are elevated — their raw observed
  CFRs, 0.43% and 0.68%, are the lowest in the set and plausibly under-ascertained, so I would not
  read their chain residuals as evidence either way.)

The chain residual is statistically indistinguishable from the omitted `(1 + eps)` factor, in 19
independent countries, at the production configuration.

**Direct paired arm (common random numbers, 115 ETH posterior members, `32_arms_g18.R`).**
Applying `mu_0 = CFR_target·chain/(1+eps)` and re-simulating on the identical seeds:

| arm | aggregate deaths ×BASE | aggregate cases ×BASE | median member deaths bias |
|---|---:|---:|---:|
| BASE | 1.0000 | 1.0000 | 1.323 |
| **B2.2** | **0.6959** | 0.9992 | **0.886** |
| eps ≡ 0 (B2.1 kept) | 0.7029 | 0.9968 | 1.023 |
| eps ~ Gamma(1,8) (B2.1 kept) | 0.7926 | 1.0276 | 1.107 |
| **B2.2 + Gamma(1,8)** | **0.6973** | 0.9947 | 0.900 |

Analytic expectation for B2.2: `1 / E_deaths-weighted[1+eps] = 0.663`, `1 / E[1+eps] = 0.680`.
Measured **0.696** — the 2–5% shortfall is the `Isym`-depletion feedback (a smaller `mu` removes
fewer symptomatics, lengthening the dwell), the same direction as the `rho_deaths` sweep in
CFR-MATH-06. **Cases are invariant to <0.6% in aggregate** and per-member to ±0.3%
(IQR of the per-member cases ratio [0.995, 1.003]) — `mu_j` never enters the case channel.

*CRN caveat:* the pairing is close but not exact. `rbinom()` consumes a variable number of
uniforms as a function of `size`, so changing `mu` desynchronises the RNG stream downstream;
only 1–4% of members reproduce bit-identically. The per-member cases ratio IQR of ±0.3% **is**
that noise floor, and it is 100× smaller than the deaths effect being measured.

**A consequence worth stating separately: once B2.2 is in, the deaths LEVEL becomes invariant to the
eps prior.** B2.2 with `Gamma(3,6)` gives ×0.6959 and B2.2 with `Gamma(1,8)` gives ×0.6973 — the same
number. That is the whole point of the identity fix: it turns the choice of `mu_j_epidemic_factor`
from a *bias* question into a pure *biology/shape* question. It also means **B2.2 should ship first
and on its own merits**, not as part of a bias-tuning bundle.

**Recommendation — IDENTITY/DERIVATION FIX (B2.2).** In `R/sample_parameters.R:672`, change

```r
chain <- g1_dwell * rho / (rhod * chi)
```
to
```r
eps_j <- config_sampled$mu_j_epidemic_factor          # per-location, already drawn above
chain <- g1_dwell * rho / (rhod * chi * (1 + eps_j))  # B2.2: the engine multiplies mu by (1+eps)
                                                      # on epidemic-flagged ticks (sim_components.R:190-192),
                                                      # and the derivation anchors to the epidemic regime.
```
and mirror it in `R/calc_implied_cfr.R` so the readback stays the exact inverse. `chain` becomes a
length-nL vector rather than a scalar (the verbose print at `:695` needs the same treatment).
Ordering is already correct: the per-location loop at `:472-569` populates
`config_sampled$mu_j_epidemic_factor` before the B2 block at `:597`, and when
`sample_mu_j_epidemic_factor = FALSE` it holds the config default, which is still the right divisor.
Add a regression test asserting `cfr_epidemic_<iso> == CFR_target_<iso>` to machine precision after
the round trip — that is the invariant the current code *claims* and does not satisfy.
Update `data-raw/make_priors_default.R:1729-1746` and `04-model-description.Rmd` eq:mu-baseline-derivation.
Bump `priors_default`; this is **recalibration-gated**.

**Expected effect:** aggregate deaths ×0.696 with cases unchanged. Post-A1b ensemble deaths bias
**1.525 → ≈1.06**. Deaths R²: **≈0** (see CFR-MATH-11). Cost: ~10 lines + a docs pass + a priors rebuild.

**Sizing caveat (honest).** The exact burden-weighted divisor is `(1 + eps·f_epi)`, and
`f_epi` (death-weighted) has median 0.943 / IQR [0.805, 0.990] on the ETH posterior. Using `(1+eps)`
therefore over-corrects by a median 2–3%, which is 10× smaller than the error it removes and is
the only form available in closed form (`f_epi` is a dynamic outcome). Take `(1+eps)`.

---

### CFR-MATH-02 — HIGH — the spec's "irreducible ≈1.3–1.5× dynamics-dependent residual" is not irreducible; it *is* the omitted `(1+eps)`

`MOSAIC-docs/04-model-description.Rmd:1007`:

> "A closed-form identity cannot capture the realized epidemic-regime fraction or the spatial-coupling
> residual exactly, so an irreducible ≈1.3--1.5× dynamics-dependent residual in the realized deaths
> channel remains after this correction."

The same sentence appears at `data-raw/make_priors_default.R:1682-1684` and `R/sample_parameters.R:613-616`.
Measured, the residual is **1.444 (median, 19 countries)** and my five-factor decomposition attributes
it as: `(1+eps·f_epi)` **1.380**, and the product of the four genuinely-dynamic factors
(`chi_eff/chi_epi` 0.944, `1/k_ns` 1.038, `k_dd` 1.008, `1/k_rc` 1.013) = **1.000**. The genuinely
irreducible part is the *scatter*, sd(log) = 0.246, not the *centre*. Recording a correctable
closed-form factor as irreducible is what let it survive three prior revisions (B1 v15.14, B2 v15.15,
B2.1 v0.50.0). **Fix the sentence in all three places when B2.2 lands.**

---

### CFR-MATH-03 — HIGH — `mu_j_epidemic_factor` is anchored to a phenomenon the engine's indicator does not select for; the v15.18 reshape moved it the wrong way

**The prior has no citation.** `04-model-description.Rmd:1017`: "reflecting the approximately 50%
increase in cholera CFR typically observed during outbreak surges" — no reference. The build comment
at `data-raw/make_priors_default.R:1879-1881` cites the spec section, which cites nothing. Under the
hard criterion for biological values this is an uncited prior.

**The spec is also stale**: it prints `mu_epi ~ Gamma(1, 2)` (`:1020`) while the shipped prior is
`Gamma(3, 6)` (`make_priors_default.R:1900-1901`). Lesson #12(e).

**Independent verification of the identifiability premise.** The v15.18 reshape was justified on the
grounds that the parameter is "statistically UNIDENTIFIED (calibration leaves posterior ~ prior)".
The posterior-≈-prior observation is correct (ETH 25k: prior mean 0.5025 → posterior 0.5063,
ratio 1.008), but that is a property of MOSAIC's *inference*, not of the data: SAMP's conditional
profile puts it at 1,270 nats over the prior range, an order of magnitude above the 70–148 nat score
noise floor. I confirm the direction independently and mechanistically, without a profile: `mu_j`
enters the engine at exactly one site, deaths scale linearly in `(1+eps)` (measured: setting
`eps ≡ 0` on the 115 ETH posterior members gives aggregate deaths **×0.7029 = 1/1.4227**, against
`median(1+eps) = 1.422` on that subset — a 0.05% match), and the deaths channel over-predicts. A parameter whose entire effect is a measured 1.4× multiplier on an over-predicted
channel is not unidentified — it is unrecovered.

**The engine's indicator selects the wrong periods.** The flag fires on *high symptomatic prevalence*
(`sim_components.R:181-186`). I tested what the surveillance data say about the CFR in exactly those
periods, applying each country's own `epidemic_threshold` from `priors_default` v15.18 to the
weekly combined surveillance file (2014–2026, AI rows excluded for parity with the priors build),
converting weekly cases to symptomatic prevalence with the engine's own identity
`Isym = (cases/7)·chi/(rho·(1−e^{−gamma_1}))` at the prior centres:

| min weekly cases | k | pooled epidemic/endemic CFR ratio (DerSimonian–Laird) | implied `eps` | countries > 1 |
|---:|---:|---|---:|---|
| 1 | 10 | 0.489 [0.316, 0.758] | **−0.51** | 1/10 |
| 10 | 10 | 0.503 [0.322, 0.787] | −0.50 | 1/10 |
| 20 | 9 | 0.538 [0.328, 0.883] | −0.46 | 1/9 |
| 50 | 5 | 0.414 [0.199, 0.858] | −0.59 | 0/5 |
| 100 | 3 | 0.455 [0.268, 0.771] | −0.55 | 0/3 |

**In the periods the engine flags as epidemic, the observed CFR is roughly half the endemic CFR**,
robustly across case floors. (Caveat, stated plainly: low-incidence weeks can carry deaths that
belong to earlier high-incidence weeks, which biases this ratio down. The result survives a
100-cases-per-week floor, where that artifact is weakest, and ETH alone — my highest-quality series —
gives an epidemic/endemic ratio of 0.94 and a quasi-Poisson log-CFR slope on log weekly cases of
**−0.062 (se 0.036, p = 0.083)**, i.e. flat-to-declining. No stratum supports +0.5.)

**Where the escalation actually lives.** Splitting each country's outbreaks into the first 6 weeks
after onset versus weeks 7+ (runs of consecutive case-positive weeks separated by ≥8 weeks):

```
pooled EARLY/LATE CFR ratio = 1.129   95% CI [0.860, 1.481]   k = 20 countries, 10/20 > 1
  strongest: SDN 3.09, CMR 2.68, SSD 2.14, AGO 1.49, NER 1.44, MWI 1.39
  reversed:  ZWE 0.34, ETH 0.45, MOZ 0.54, ZMB 0.54, SOM 0.66, NGA 0.66
  (GHA's 18.3 is in the pool but carries near-zero DL weight: cfr_late = 0.0003 on 3 deaths)
```
Full per-country table: `scratchpad/cfr/epi_factor_countries.csv` and `17_epi_robust.R`.

This matches the literature: cholera CFR is a function of *treatment access*, and the documented
pattern is a **decline over the course of an outbreak as the response scales**, not a rise at peak
prevalence:

- **Haiti 2010–12** — national CFR ~4–5% in the first weeks after introduction (October 2010),
  falling to ~1% within months as cholera treatment centres were established:
  Barzilay et al., *Cholera surveillance during the Haiti epidemic — the first 2 years*,
  N Engl J Med 2013;368:599-609, [10.1056/NEJMoa1204927](https://doi.org/10.1056/NEJMoa1204927);
  Tappero & Tauxe, Emerg Infect Dis 2011;17:2087-93,
  [10.3201/eid1711.110827](https://doi.org/10.3201/eid1711.110827).
- **Yemen 2016–18** — CFR 0.95% in the first wave (Sep 2016 – Apr 2017) versus 0.22% in the much
  larger second wave: Camacho et al., *Cholera epidemic in Yemen, 2016-18*, Lancet Glob Health
  2018;6:e680-e690, [10.1016/S2214-109X(18)30230-4](https://doi.org/10.1016/S2214-109X(18)30230-4).
  Note the direction: the second wave had **far higher prevalence and a 4× lower CFR**, which is
  the exact opposite of what the engine's prevalence indicator encodes.
- **WHO/GTFCC** — treated CFR target <1%; untreated cholera CFR "up to 50%"
  ([WHO cholera fact sheet](https://www.who.int/news-room/fact-sheets/detail/cholera)).

The engine's prevalence indicator fires at and after the peak, i.e. after the response has mobilised.

**Recommendation — PRIOR CHANGE.** Re-anchor `mu_j_epidemic_factor` to the measured early-outbreak
escalation, which is the only positive signal in the data and the one the literature supports:
**target mean ≈0.125, mode at 0, 97.5th percentile ≈0.46**, i.e.

```r
# mu_j_epidemic_factor — proportional IFR increase on epidemic-flagged ticks.
# Anchored to the measured early-outbreak CFR escalation: DerSimonian-Laird pool of
# first-6-weeks vs weeks-7+ reported CFR across 20 SSA countries, 2014-2026 WHO/JHU
# weekly surveillance = 1.129 [0.860, 1.481]  =>  eps ~ 0.13, 95% upper ~0.48, mass at 0.
# NOT +0.5: in the periods the ENGINE flags (high symptomatic prevalence,
# sim_components.R:181-186) the observed CFR is 0.41-0.54x the endemic CFR
# (DL pool, k=3-10, robust to a 1-100 weekly-case floor), because cholera CFR is
# driven by treatment access, which improves as an outbreak matures
# (Haiti 2010: ~4% -> <1%, Barzilay 2013 NEJM; Yemen: 0.95% -> 0.22%, Camacho 2018 LGH).
distribution = "gamma", parameters = list(shape = 1, rate = 8)   # mean 0.125, mode 0, p95 0.374, p97.5 0.461
```

`Gamma(1, 8)`: mean 0.125, mode 0, median 0.087, p95 0.374, p97.5 0.461, p99 0.576.
(The statistician owns the fit; I own the 0.125 centre and the [0, 0.48] interval. If a mode
strictly above 0 is wanted, `Gamma(1.5, 12)` gives mean 0.125, mode 0.042, p95 0.326 — but the data
put half the countries below 1, so mode 0 is the honest shape.)

**Measured effect, paired CRN:** `Gamma(1,8)` alone gives aggregate deaths ×0.793 and median member
deaths bias 1.323 → **1.107**. Cost: 3 lines + a priors rebuild.

**Important:** B2.2 (CFR-MATH-01) and this reshape are **substitutes for the bias, not complements**.
Measured: B2.2 alone ×0.696; `Gamma(1,8)` alone ×0.793; **both together ×0.697** — i.e. once B2.2
is in, the eps prior contributes nothing to the level. **Ship both anyway**: B2.2 because the
derivation is wrong, the reshape because the prior is uncited and points the wrong way. But do not
count their effects twice, and do not ship the reshape *instead of* B2.2 — that would fix the level
by deleting the mechanism while leaving the identity broken.

---

### CFR-MATH-04 — CRITICAL — the deaths channel's ~10,000-nat "CFR signal" is a daily-resolution zero-cell artifact, and it is what makes the likelihood want 5× the observed CFR while over-predicting 5×

This is the answer to thread 3. It is **not** primarily a timing hypothesis.

**Step 1 — the zero cells.** `R/calc_log_likelihood_distributions.R:423-425` (baseline) applies
`ll = −observed[i] · log(1e6)` = −13.8 nats per observed unit whenever the prediction is 0 and the
observation is positive. Measured on the 115 ETH posterior members:

| | zero-with-obs cells | observed units they carry | baseline penalty |
|---|---:|---:|---:|
| **daily deaths** | **642 / 2,898** | **794 / 1,136 (70%)** | **10,970 nats** |
| daily cases | 213 / 2,968 | 1,615 / 90,371 (1.8%) | 22,312 nats |
| **weekly deaths** | 70 / 414 | 200 (18%) | 2,763 nats |
| weekly cases | 0 / 424 | 0 (0.0%) | 0 nats |

Per observed unit, **the deaths zero-penalty is 39× the cases zero-penalty**.

**Step 2 — why.** The observed daily deaths series is a weekly total spread over the week: mean
run-length of identical values = **5.87** days (cases 3.38), and **25.8% of scored death-days have
`0 < obs < 2`**. The simulated daily deaths are integer Binomial draws (mean run-length 120.75 —
long stretches of exact zeros). MOSAIC is scoring a *smoothed weekly-derived* observation against an
*unsmoothed integer daily* simulation, and the zero rule converts that granularity mismatch into an
11,000-nat force.

**Step 3 — the decisive experiment.** For 12 top-likelihood ETH draws I re-derived `mu_0` at
multiples of each draw's own `CFR_target`, re-simulated, and split the deaths log-likelihood into
its zero-penalty and NB-density parts (`22_cfrprofile.R`; medians over the 12 seeds):

| × CFR_target | deaths bias | zero cells | obs deaths in them | ΔLL total | ΔLL zero-penalty | ΔLL NB density | ΔLL under A1b |
|---:|---:|---:|---:|---:|---:|---:|---:|
| 0.25 | 0.53 | 864 | 1078 | −2045 | −7767 | **0** | **0** |
| 0.50 | **1.03** | 816 | 1016 | −1410 | −7004 | −133 | −10 |
| 1.00 | 2.01 | 728 | 909 | −390 | −5734 | −389 | −289 |
| **2.00** | 3.88 | 625 | 778 | **0** | −4094 | −732 | −337 |
| 3.00 | 5.50 | 556 | 693 | −717 | −3159 | −991 | −370 |
| 5.00 | 8.58 | 452 | 561 | −1345 | −1593 | −1436 | −463 |
| 12.00 | 16.40 | 358 | 432 | −2747 | **0** | −2462 | −462 |

- The **zero-penalty spans 7,767 nats and improves monotonically with higher CFR**; the NB density
  spans 2,462 nats and degrades monotonically. The penalty is **3.2×** the density, so the total
  argmax sits at **2× CFR_target (deaths bias 3.88)** instead of at 0.5× (deaths bias 1.03).
- Raising `mu` is the *only* lever that reduces the zero-cell **count** — a day with expected 0.3
  deaths draws non-zero 26% of the time, a day with expected 3.0 draws non-zero 95% of the time.
  That is why a pure multiplicative rescale of an already-simulated series does **not** reproduce
  the effect: I checked, and under a fixed simulated series both the baseline and the A1b rules
  put the optimal scale at the grid floor (0.2) for 17 of 30 top-LL draws — the zero cells are
  zero at every scale, so the penalty drops out. The effect is a *stochastic* one and only appears
  when `mu` is changed in the simulator.
- **Under A1b the argmax moves from 2× to ≤0.25×**, the profile range collapses from 2,747 to
  472 nats, and it is **flat to −10 nats at 0.5×**, i.e. the unbiased point.

**This is SAMP-09's paradox, resolved.** SAMP measured `CFR_target` at 10,029 nats of profile range,
"deaths only", with the argmax at the 98th prior percentile (0.061, 4.8× the observed ETH CFR) while
the best draw over-predicted deaths 5.16×. The zero-cell penalty at the draw's own CFR is
**10,970 nats** on the same data. The two are the same quantity. `CFR_target` is not measuring the
case-fatality ratio; it is buying back zero-cell penalty.

**Why this also explains the single-country vs joint discrepancy.** ETH's implied CFR is
**2.85× observed in the 25k single-country run** but **1.56× in the 100k 40-country run**. Decomposed:
in the joint run the posterior ≈ prior, so the error is `1.03 (prior centring) × 1.44 (chain) = 1.48`.
In the single-country run the calibration *actively selects high-CFR draws* — posterior-weighted
`CFR_target` = 0.0300 = **2.39× the observed CFR**, times a chain factor of 1.19 = 2.85. The
zero-penalty has full leverage at n=1 location and is diluted at n=40.

**Recommendation.** No new action for the CFR owner — A1b already addresses it and is the lab's
promoted arm. But two consequences are mine to state:

1. **B2.2 and the eps reshape MUST ship after A1b (or after weekly scoring), never before.** Both
   *reduce* `mu`, which *increases* the zero-cell count. Reading B2.2's effective **×0.70** off the
   profile table above (log-interpolating between the 0.5× and 1.0× rows): under the **baseline**
   rule it **costs ≈525 nats** of deaths log-likelihood relative to the unmodified derivation, and
   the sampler will simply re-select higher-`CFR_target` draws to undo it. Under **A1b** the same
   ×0.70 is a **gain of ≈145 nats**. This is a hard dependency, not a preference.
2. **Weekly scoring of the deaths channel is the cheapest structural mitigation** — it cuts the
   zero-carried deaths from 70% to 18% and the penalty by 4×, and it matches the actual
   resolution of the observation. That belongs to the likelihood owner (A5), and A5's reported
   Spearman(LL, R²) of −0.082 is a *selection* metric, not a statement about the zero-cell problem;
   I would re-examine A5 restricted to the deaths channel.

---

### CFR-MATH-05 — MEDIUM — the deaths timing error is real, structurally diagnosable, and small

Thread 3 also proposed a timing hypothesis. It is correct in direction and modest in size.

**Structural prediction.** Reported cases read the `new_symptomatic` **incidence** at
`t − delta_reporting_cases` (`sim_components.R:283-289`), while reported deaths read
`disease_deaths` at `t − delta_reporting_deaths`, and `disease_deaths` is drawn on the `Isym`
**stock**, whose members entered on average `1/(1−e^{−gamma_1}) ≈ 9–10` days earlier. So model
deaths must lag model cases by `dwell + delta_deaths − delta_cases ≈ 9.6 + 4.3 − 2.0 ≈ 12` days
(production posterior means).

**Measured on the ETH ensemble median** (`11_ens_shift.R`; I reproduce the run's headline R² exactly:
cases 0.7855, deaths 0.3710, bias_deaths 2.9231, matching `summary.json` to 4 d.p.):

| shift applied to the simulated series | deaths R² | cases R² |
|---:|---:|---:|
| −35 d | 0.3863 | 0.6889 |
| −28 d | 0.3892 | 0.7260 |
| −21 d | 0.3900 | 0.7555 |
| −14 d | 0.3857 | 0.7764 |
| −7 d | 0.3758 | **0.7862** |
| 0 d | 0.3710 | 0.7855 |
| +14 d | 0.3430 | 0.7615 |

- **deaths argmax: −24 d (R² 0.3927); cases control argmax: −5 d.** The **19-day differential** is the
  predicted `dwell + delta_deaths − delta_cases`.
- The **cases control behaves correctly** (essentially no preferred shift), which is what makes the
  deaths result interpretable.
- **Observed** weekly cross-correlation between cases and deaths peaks at **lag 0 weeks**
  (Spearman on log1p: 0.892 at lag 0, 0.876 at −1 wk, 0.862 at +1 wk). Surveillance reports the
  deaths in the same week as the cases; the model puts them 2–3 weeks later.
- There is **no free parameter that can fix it.** `delta_reporting_deaths` can only *add* lag, and
  its prior floor is 1 day (`make_priors_default.R:701-705`, `Truncnorm(4, 3, 1, 14)`). SAMP's ETH
  profile argmax for it is at the **2nd prior percentile** — the data want the minimum. Shortening
  the dwell means raising `gamma_1`, but `gamma_1` is shared with the cases channel, where SAMP's
  profile argmax is at the **10th percentile** (longer dwell). The two channels pull in opposite
  directions through one parameter.
- **Counter-evidence, stated:** in the 40-location 100k production posterior,
  `delta_reporting_deaths` does **not** move at all (prior mean 4.853 → posterior 4.891, KL 0.24;
  `prod100k_v087/2_calibration/posterior/posterior_quantiles.csv`). SAMP's p02 argmax is a
  single-country *conditional profile*, not a posterior, and at 40 locations essentially nothing
  moves, so that run is uninformative either way. I would not over-read the p02 result — the
  ensemble shift measurement below is the stronger evidence and it does not depend on it.

**Size.** The ceiling from perfect timing is **+0.022 R²** (0.371 → 0.393) on the ETH ensemble. For
scale, the between-draw-block spread of ETH cases R² is 0.797 / 0.019 / 0.371 (lab measurement), so
+0.022 is well inside the harness's own instability. **Real, but not where the deaths R² is.**

**Recommendation.** Do **not** re-anchor `delta_reporting_deaths` to absorb the dwell — that would
re-introduce exactly the mislabelling CLAUDE.md lesson #12(c) fixed (it is a death-event-to-report
lag, and the prior is correctly derived from IDSR notification cycles). Record the 19-day
differential as a known structural lag and revisit it only if the deaths channel gets a
conditional-on-cases structure (CFR-MATH-08), which inherits the cases timing for free.

---

### CFR-MATH-06 — HIGH — `rho_deaths` cancels not just in the mean but in the entire distribution; the prior is doing no work and the spec's justification for it is pre-B2

**Symbolic.** `rho_deaths` has exactly **one** engine use site (`R/sim_components.R:204`); I grepped
`R/sim_*.R` and there is no other. Composition of Binomial thinnings gives, conditional on `Isym`,

```
reported_deaths[t] ~ Binom( Isym[t−l_d],  (1 − e^{−mu}) · rho_deaths )
```

— not merely the same mean, the **same distribution**. Substituting the B2.1 derivation
`mu = c / rho_deaths` with `c = CFR_target·(1−e^{−gamma_1})·rho/chi_epidemic` (which is `rho_deaths`-free):

```
(1 − e^{−c/rho_d}) · rho_d  =  c  −  c²/(2·rho_d)  +  O(c³/rho_d²)
```

The leading term is exactly `rho_deaths`-free; the relative residual is `c/(2·rho_d)`. At the ETH
prior centres `c = 6.44e-4` and `rho_d = 0.42`, so the residual is **0.11%**. SAMP-06 established
this to first order in the mean; the thinning-composition argument is stronger — it holds for the
full observation distribution including its variance, which matters because the NB dispersion is
method-of-moments on the observations and never sees the simulated variance anyway.

**The only real effect is second-order and lands on the CASES channel.** `disease_deaths` are removed
from `Isym` (`sim_components.R:195`), and `disease_deaths ∝ 1/rho_deaths`, so a low `rho_deaths`
depletes the symptomatic pool faster. Measured sweep at fixed `CFR_target` with `mu` re-derived
each time (8 top-LL seeds, `29_ceiling_rhod.R`):

| `rho_deaths` | reported deaths (rel.) | reported cases (rel.) |
|---:|---:|---:|
| 0.25 | 0.9896 | 1.0001 |
| 0.32 | 0.9952 | 0.9994 |
| **0.42** | **1.0000** | **1.0000** |
| 0.52 | 1.0126 | 1.0009 |
| 0.65 | 1.0226 | 1.0015 |
| 0.85 | 1.0295 | 0.9991 |

**3.99% total variation in reported deaths across a 3.4× range of `rho_deaths`; 1.7% across the
actual prior 95% CI [0.319, 0.524]; 0.24% on cases.** Production posterior: 0.4198 → 0.4338, KL 0.089.

**The spec's rationale is stale.** `04-model-description.Rmd:970`:

> "The deaths likelihood identifies the product μ_{j,0}·ρ_deaths rather than the two factors
> separately, so a narrow ρ_deaths prior keeps it pinned near 0.42 during sampling, letting the
> per-country μ_{j,0} posteriors carry the cross-country CFR signal cleanly."

That was true under the pre-B2 schema where `mu_j_baseline` was an independent Gamma draw. Under B2
`mu` is *derived with `rho_deaths` in the denominator*, so the product is identically
`rho_deaths`-free and there is nothing left to pin. The same text is at
`make_priors_default.R:592-596`. Lesson #12(e).

**Recommendation — PRIOR/SAMPLING CHANGE.** Set `sample_rho_deaths = FALSE` in the defaults and hold
`rho_deaths` at the Beta(36.95, 51.02) mean **0.4194** (Routh 2017 Tanzania / Shikanga 2009 Kenya /
Bwire 2013 Uganda random-effects pool — the derivation is unchanged and stays documented, and the
parameter remains meaningful for *reporting* the modelled true-death burden). Rewrite the rationale
paragraph in `04-model-description.Rmd` and `make_priors_default.R` to say *why*: the B2 derivation
cancels it exactly, so the prior is a statement about the true-vs-reported death ratio for
interpretation, not an identifiability aid. Frees one sampling dimension at zero cost to the fit.
I concur with SAMP-06; this finding strengthens it from a mean argument to a distributional one.

---

### CFR-MATH-07 — MEDIUM — the biologically-extreme CFR warnings are a pure prior-predictive artifact, reproduced to within 3% from the priors alone, plus a real identity defect in the diagnostic

**Reproduced by Monte Carlo from the priors, no likelihood involved** (`19_extreme2.R`, n = 100,000
draws from `gamma_1 ~ LN(−2.3, 0.5)`, `rho ~ Beta(5.38, 7.10)`, `rho_deaths ~ Beta(36.95, 51.02)`,
`chi_epi ~ Beta(4.79, 1.53)`, `eps ~ Gamma(3, 6)`, `CFR_target ~ LN(meanlog_iso, 0.787)`):

| iso | P(cfr_clinical_epidemic > 0.5), **predicted** | **observed in the 100k production run** | P(cfr_epidemic > 0.5) |
|---|---:|---:|---:|
| MLI | **6.05%** | **5.893%** (5,893/100,000) | 4.90% |
| TCD | **1.55%** | **1.506%** (1,506/100,000) | 0.93% |
| COG | 5.45% | — | 4.38% |
| ETH | 0.011% | — | 0.000% |
| MOZ | 0.000% | — | 0.000% |

It is **not** a corner of parameter space the likelihood selected, and it is not an inference problem.
It is the `CFR_target` lognormal's own right tail.

**Driver: `CFR_target` itself.** Among MLI draws with `cfr_clinical_epidemic > 0.5`, conditional means
vs overall: `CFR_target` 0.385 vs 0.121 (**ratio 3.17**), `chi_epidemic` 0.653 vs 0.758 (0.86),
`rho` 0.516 vs 0.432 (1.20), `1+eps` 1.63 vs 1.50 (1.09), `gamma_1` 0.97, `rho_deaths` 0.97.
**60.2% of the flagged draws have `CFR_target > 0.30` and 20.7% have `CFR_target > 0.50`.**
A *reported* (suspected-case) CFR above 30% sustained at national-year scale has never been observed
in cholera surveillance; above 50% is arithmetically implausible.

**Where MLI's centre comes from.** `model/input/param_mu_disease_mortality.csv` gives MLI a
hierarchical-GAM CFR of **8.80% (2023), 8.80% (2024), 8.65% (2025)** with Beta shapes
`(9.98, 103.4)`, `(8.52, 88.3)`, `(7.31, 77.2)` — i.e. an effective sample of **~85–113 cases**. The
raw weekly surveillance record for MLI over 2014–2026 contains **zero cases and zero deaths**
(374 week-rows, all NA/0). MLI's 8.9% prior median is a hierarchical extrapolation off essentially
no data, and the global `sdlog = 0.787` is then applied on top of it. COG (8.53% median) is similar;
TCD (5.07%) at least has 5,044 cases / 199 deaths behind it.

**A real identity defect in the diagnostic.** `R/calc_implied_cfr.R:190` computes the per-episode
clinical CFR as `1 − exp(−mu_eff / gamma_1)`. The engine's actual competing-risks structure
(`sim_components.R:194` then `:209`, deaths drawn first, then recovery on survivors) gives a
per-episode death probability of `mu_eff / (mu_eff + (1 − e^{−gamma_1}))`. The two agree to first
order but diverge in exactly the tail the warning fires in: the shipped form crosses 0.5 at
`mu/gamma_1 = 0.693`, where the true per-episode CFR is **0.41**. The comment at `:209-212` even
states the wrong threshold ("death hazard exceeds recovery hazard", which is `mu/gamma_1 = 1`).
It also uses the continuous rate `gamma_1` where the engine uses the per-tick probability
`1 − e^{−gamma_1}` — the same `(1 − e^{−gamma_1})` vs `gamma_1` distinction the v0.88.1 fix made
for the surveillance CFR, applied inconsistently two lines later.

**Counterfactual flag rates for MLI:**

| variant | P(> 0.5) |
|---|---:|
| shipped `1 − exp(−mu/gamma_1)` | **5.81%** |
| exact `mu/(mu + (1 − e^{−gamma_1}))` | 2.83% |
| B2.2 + shipped | 2.09% |
| **B2.2 + exact** | **0.89%** |

**Verdict: prior problem (dominant) + identity problem (a 2.06× amplifier). Not a real corner.**

**Recommendations.**
- **IDENTITY FIX (trivial):** replace `1 - exp(-mu_eff / results$gamma_1)` at
  `R/calc_implied_cfr.R:190` with `mu_eff / (mu_eff + (1 - exp(-results$gamma_1)))`, and fix the
  threshold statement in the comment at `:209-212`. Diagnostic-only; changes no fit. 2 lines.
- **PRIOR CHANGE:** the honest fix is not a blanket cap. Make the `CFR_target` `sdlog` a function of
  the GAM's own effective sample size — MLI's Beta ESS is ~95 and COG's is comparable, so their
  centres deserve *more* width on the *log* scale but must not put mass on a >30% reported CFR.
  The operationally simple version, which I'd take first: **truncate the `CFR_target` lognormal at
  0.25** in `sample_from_prior()`. 0.25 is ~3× the highest credible national reported CFR
  (TCD's WHO-GAM 5.5–5.9%, COG's raw 6.4%) and well above every country's prior median, so it is
  inert for 37 of 40 countries and removes 6.2% / 5.5% / 1.5% of MLI / COG / TCD draws. Sizing of
  any per-country `sdlog` belongs to the statistician; **flagging that MLI and COG rest on ~90
  effective cases is mine**, and I recommend the priors build emit a warning when a country's
  CFR Beta ESS is below ~200.

---

### CFR-MATH-08 — MEDIUM — the conditional-binomial structure is supported by the data, collapses the deaths bias into the cases bias, and buys +0.03 to +0.09 deaths R²

This is thread 5. It is a **model-structure change**, out of lab scope to ship; here is the quantification.

**Structural feasibility on ETH (2,898 scored cells):**
- cells with `obs_deaths > obs_cases`: **0**
- cells with `obs_deaths > 0` and `obs_cases == 0`: **0** (the degenerate case — it never occurs)
- cells with `obs_cases == 0`: 378 (13%), which contribute 0 to the likelihood, correctly
- weekly: 414 cells, 0 violations of either kind

**Distributional support.** Weekly, restricted to weeks with ≥20 cases (n = 276):
binomial **χ²/df = 1.4** — a plain Binomial is *adequate*, which is a stronger result than I expected.
Beta-Binomial MLE: `a = 8.97`, `b = 681.4` → mean CFR 0.0130, **intraclass correlation 0.0014**,
95% between-week CFR interval [0.0060, 0.0227]. A Beta-Binomial with `a + b ≈ 690` would be the
conservative choice; the extra-binomial variation is small.

**What it buys (ETH, in-sample, ensemble):**

| deaths predictor | daily R² | weekly R² | deaths bias |
|---|---:|---:|---:|
| current marginal NB deaths channel | 0.3710 | 0.4811 | **2.923** |
| `CFR_obs × ENSEMBLE cases` | **0.3999** | **0.5113** | **1.215** |
| `CFR_obs × ENSEMBLE cases`, shifted −24 d | 0.3839 | 0.4906 | 1.221 |
| LOESS `CFR(t) × ENSEMBLE cases` | **0.4607** | — | — |
| `CFR_obs × OBSERVED cases` (oracle) | 0.5302 | 0.6720 | 1.000 |
| LOESS `CFR(t) × OBSERVED cases` (oracle) | 0.5880 | — | — |

- **+0.029 daily / +0.030 weekly R²** with a constant CFR; **+0.090** if the CFR is allowed to vary
  smoothly in time and the model can predict that variation.
- **Deaths bias becomes identically the cases bias** (1.215 vs the run's 1.2196 cases bias) — the
  deaths-bias problem *ceases to exist as a separate problem*. That is the real prize, not the R².
- Shifting **hurts** the conditional model (0.400 → 0.384): it already inherits the correct cases
  timing, so CFR-MATH-05 is subsumed for free.

**What it would change.**
1. The deaths log-likelihood becomes `dbinom(obs_deaths_t, size = obs_cases_t, prob = CFR_t)` (or
   Beta-Binomial). The **zero-prediction penalty disappears entirely** for deaths — the mean is
   `CFR_t · obs_cases_t > 0` wherever `obs_cases_t > 0`, and deaths are never observed without cases.
   That removes the 10,970-nat artifact of CFR-MATH-04 by construction, not by patch.
2. `CFR_t` becomes the **directly fitted quantity**. `CFR_target` would be identified by an
   ~690-effective-observation Beta-Binomial rather than by 2,898 cells of zero-inflated NB, which is
   a far more honest statement of the information content.
3. `LL = LL_cases + LL_deaths` stops double-counting the shared error (LIKE-05 measured
   residual log-ratio correlation +0.36 weekly).

**What it would break.**
1. **The deaths channel would no longer constrain transmission at all.** Today `mu_j`, `gamma_1`,
   `epidemic_threshold`, `chi_epidemic` and the IC all move the deaths likelihood. Conditioned on
   *observed* cases, only `CFR_t` does. That is a genuine loss of information — though given
   CFR-MATH-04 it is mostly the loss of a *misleading* constraint.
2. **Forecasting requires care.** Out of sample there are no observed cases to condition on, so the
   forecast must use `deaths_t ~ Binomial(simulated cases_t, CFR_t)` — a different object from the
   fitting likelihood. This is the standard conditional-model pitfall and it must be handled
   explicitly or the OOS deaths R² (currently 0.109–0.140) will not improve.
3. `calc_model_likelihood()`'s shape terms (peak timing/magnitude, cumulative, WIS) are written
   against marginal count series and would need a deaths-channel decision.
4. The engine's `disease_deaths` flow must stay (it depletes `Isym` and feeds `reported_deaths` for
   scenario output); only the *likelihood* changes.

**Recommendation.** Worth doing, and the strongest single change available for the deaths channel —
but it is second in priority to B2.2 + the eps reshape because those are ~15 lines and this is a
likelihood-plus-forecast-path rewrite. **Escalate with these numbers**, do not ship from the lab.

---

### CFR-MATH-09 — MEDIUM — most of the 0.37-vs-0.79 deaths/cases R² gap is a ceiling imposed by the observed deaths series, not a model defect

From the same table:

```
current model deaths channel                      0.371
constant CFR  x  ENSEMBLE cases                   0.400
LOESS CFR(t)  x  ENSEMBLE cases                   0.461
constant CFR  x  OBSERVED cases   (oracle)        0.530   <- ceiling with perfect cases, constant CFR
LOESS CFR(t)  x  OBSERVED cases   (oracle)        0.588   <- ceiling with perfect cases AND perfect CFR(t)
observed deaths vs its own 7-day moving average   0.750   <- daily-granularity noise floor
```

**An oracle that knows the true daily cases and the true time-varying CFR reaches 0.588 on ETH.**
The model is at 0.371, i.e. **63% of the oracle ceiling**. The cases channel reaches 0.786 against a
much higher ceiling because the cases series has 2,590 non-zero days of 2,968 (CV 1.34) while the
deaths series has 907 of 2,898 (CV 1.73).

**This bounds the whole question the brief asks.** Realistic recoverable deaths R² on ETH:
**0.371 → 0.40 (constant-CFR conditional) to 0.46 (time-varying-CFR conditional)**, i.e.
**+0.03 to +0.09**, and the residual gap to cases is the observation, not the model.
Everything above +0.09 would require a *better observed deaths series* (sub-weekly resolution, or
line-list deaths), not a better CFR model. I would put that in the paper rather than chase it.

---

### CFR-MATH-10 — LOW — five spec/code mismatches in the CFR chain

All in `MOSAIC-docs/04-model-description.Rmd` unless noted. Each is a Lesson #12(e) instance.

| # | spec says | code does | ref |
|---|---|---|---|
| a | `mu_epi ~ Gamma(1, 2)` (`:1020`) | `Gamma(3, 6)` | `make_priors_default.R:1899-1902` |
| b | "the +50% increase typically observed during outbreak surges" (`:1017`), no citation | — | uncited value (CFR-MATH-03) |
| c | per-country `mu_{j,0} ~ Gamma(4, rate)`, CV 50%, "e.g. AGO Gamma(4, 173)" (`:1007`) | B2 replaced this with a `CFR_target` lognormal in v15.15; no `mu_j_baseline` location prior exists | `make_priors_default.R:1776-1835` |
| d | tight `rho_deaths` prior pins the `mu·rho_deaths` product (`:970`) | the product is identically `rho_deaths`-free under B2 | CFR-MATH-06 |
| e | epidemic indicator uses `N_{j, t−l_cases}` (`:990`, eq:mu-jt) | uses the **manual compartment sum at `t`**, not the Census `N` at `t−l_cases` | `sim_components.R:177-186` |
| f | — (undocumented) | the **deaths** epidemic flag is lagged by `delta_reporting_cases`, not by any deaths-side lag | `sim_components.R:181` |

(e) and (f) are numerically small (`N` drifts <0.1%/month) and (f) is arguably right — the flag is a
surveillance-visible regime indicator — but neither is written down. Fix (a)–(d) with the B2.2 docs
pass; document (e)/(f) in the eq:mu-jt text.

---

### CFR-MATH-11 — INFORMATIONAL — B2.2 and the eps reshape buy **bias, not R²**, and that must be said plainly

Paired per-member measurement on the 115 ETH posterior members, common random numbers
(`30_armr2.R`, results below). B2.2 rescales each member's deaths series by the member-specific
constant `1/(1+eps_m)`; correlation-R² is scale-invariant, so **per-member deaths R² is exactly
unchanged by construction**. The eps reshape *does* change the epidemic/endemic contrast within a
member and therefore can move R², but the measured effect is at the fourth decimal place.

| arm (vs BASE, paired, n = 115) | median ΔR²_daily | paired SD | median ΔR²_weekly | paired SD |
|---|---:|---:|---:|---:|
| B2.2 | **−0.00013** | 0.0122 | −0.00003 | 0.0216 |
| eps ~ Gamma(1,8) | −0.00008 | 0.0316 | +0.00001 | 0.0415 |
| B2.2 + Gamma(1,8) | −0.00026 | 0.0190 | −0.00001 | 0.0305 |

Every effect is **two orders of magnitude smaller than its own paired SD**. (The absolute per-member
R² is small — median 0.0038 daily / 0.0062 weekly — because individual draws fit ETH badly; the
0.371 headline is the *ensemble*. That is the lab's central finding, not a defect of this
measurement: the paired **difference** is what is being tested here, and it is zero.)

Any claim that a CFR-prior change will move the deaths R² is unsupported. The R² levers are
CFR-MATH-05 (timing, +0.022 ceiling) and CFR-MATH-08 (conditional structure, +0.03 to +0.09).

---

## 3. RECOMMENDATIONS, ranked by (expected gain / implementation cost)

Expected gains are stated against the **post-A1b** ETH baseline (bias_deaths 1.525, R²_deaths 0.377)
because every prior change below must ship after A1b (CFR-MATH-04, hard dependency).

*Two different ETH runs appear in this report — do not conflate them.* The lab's A1b table is
**ETH n=10,000** (baseline bias_deaths 2.847 / R² 0.367; A1b 1.525 / 0.377). My decomposition and
shift analysis use **ETH n=25,000, v0.90.3** (`output/eth25k_v0903`, bias_deaths 2.9231 /
R² 0.3710), which is the run I had locally and whose `summary.json` I reproduce to 4 d.p. The
**multiplicative** effects I measure (×0.696 etc.) are run-independent; the **absolute** bias
projections below are anchored to the lab's A1b number. "Bias" means
the deaths bias ratio; "ΔR²" means deaths R². **Nothing in group A or B moves R² — that is the
honest headline, and it is measured, not assumed (CFR-MATH-11).**

### A. IDENTITY / DERIVATION FIXES (highest ratio — small, provable, no new biology)

| # | change | files | expected gain | cost |
|---|---|---|---|---|
| **A1** | **B2.2: divide the `mu` derivation by `(1 + mu_j_epidemic_factor)`** | `R/sample_parameters.R:672`, `R/calc_implied_cfr.R:126,178-183`, `data-raw/make_priors_default.R:1729-1746`, `04-model-description.Rmd` eq:mu-baseline-derivation | **deaths ×0.696 → bias 1.525 → ≈1.06** (measured paired, 115 members); cases invariant to <0.6%; removes a 1.44× systematic error verified in 19 countries. ΔR² = −0.0001 ± 0.012. | **~10 lines + docs + priors rebuild.** Recalibration-gated. |
| **A2** | **Exact competing-risks per-episode CFR in the diagnostic** | `R/calc_implied_cfr.R:190` and the comment at `:209-212` | Halves the biologically-extreme warning rate (MLI 5.81% → 2.83%); makes the clinical-CFR column mean what it says. No fit change. | **2 lines.** |
| **A3** | **Correct the four stale spec/prior statements** (CFR-MATH-02, -10 a–d) | `04-model-description.Rmd:970,1007,1017,1020`; `make_priors_default.R:592-596,1682-1684,1741`; `sample_parameters.R:613-616` | Zero fit gain; removes the recorded belief that a correctable 1.44× factor is "irreducible", which is what let it survive three revisions. | **Docs pass, ~1 h.** |

### B. PRIOR CHANGES (good ratio; each needs a citation, all supplied)

| # | change | files | expected gain | cost |
|---|---|---|---|---|
| **B1** | **`mu_j_epidemic_factor`: `Gamma(3,6)` → `Gamma(1,8)`** (mean 0.125, mode 0, p97.5 0.46), anchored to the measured early-vs-late outbreak CFR ratio 1.129 [0.860, 1.481] (k=20) and to Haiti/Yemen/GTFCC | `data-raw/make_priors_default.R:1877-1905` | **With A1 the level effect is nil** (A1 alone ×0.696, A1+B1 ×0.697) — B1's value is correctness of an uncited prior, not bias. **Without A1**: ×0.793 → bias ≈1.21. Also removes 1.09× of the MLI extreme-CFR tail. ΔR² ≈ 0. | **3 lines + priors rebuild.** Recalibration-gated. |
| **B2** | **Pin `rho_deaths`** (`sample_rho_deaths = FALSE`, hold at 0.4194) | sampling defaults; rationale text in `make_priors_default.R:592-596` and `04-model-description.Rmd:970` | Frees one sampling dimension at **≤1.7% cost to the deaths channel** (measured sweep over the prior 95% CI). Concurs with SAMP-06 and strengthens it to a distributional result. | **1 flag + a rationale rewrite.** |
| **B3** | **Truncate `CFR_target` at 0.25**, and warn in the priors build when a country's CFR Beta ESS < ~200 | `data-raw/make_priors_default.R:1776-1855`, `sample_from_prior()` | Removes 6.2% / 5.5% / 1.5% of MLI / COG / TCD draws with a >25% national reported CFR; inert for 37 of 40 countries. Makes the MLI/COG "~90 effective cases" provenance visible. | **~10 lines + priors rebuild.** Per-country `sdlog` sizing → statistician. |

### C. MODEL-STRUCTURE CHANGES (out of lab scope — escalate with these numbers)

| # | change | expected gain | cost |
|---|---|---|---|
| **C1** | **`deaths_t ~ Binomial(cases_t, CFR_t)`** (or Beta-Binomial, `a+b ≈ 690`) instead of a second marginal NB | **deaths R² 0.371 → 0.400 (constant CFR) / 0.461 (CFR(t)); deaths bias becomes identically the cases bias, 2.92 → 1.22.** Removes the 10,970-nat zero-penalty by construction and the +0.36 weekly double-count. Data support: χ²/df = 1.4, 0/2,898 degenerate cells. | **Large.** Likelihood rewrite + a forecast path that substitutes simulated cases + shape-term decisions. Breaks the deaths→transmission constraint (mostly a feature, given CFR-MATH-04). |
| **C2** | **Weekly-resolution deaths scoring** | Cuts the zero-carried observed deaths 70% → 18% and the penalty 4×; matches the actual observation resolution. | Small-to-medium (this is arm A5, restricted to deaths). **Re-examine A5 per-channel** — its rejection was on a selection metric, not the zero-cell problem. |
| **C3** | Re-key the epidemic indicator from *prevalence* to *outbreak phase* (weeks since onset) so `mu_j_epidemic_factor` measures what the literature describes | Would make the parameter mean what its prior says; the measured early/late signal is +13% [−14%, +48%], so the gain is small and the heterogeneity large (10/20 countries reversed). | Medium. **I do not recommend this now** — B1 makes the current indicator harmless, and the signal is too weak to justify an engine change. |

### Ship order (non-negotiable)

```
A1b (promoted)  ->  A1 (B2.2) + B1 (eps prior) + A2 + B2 + B3 in ONE recalibration-gated priors bump
                ->  re-run T1/T2, confirm deaths bias ~1.1 and deaths R2 unchanged
                ->  then evaluate C1/C2 with the deaths bias already fixed
```
A1 and B1 **before** A1b will be silently undone: both lower `mu`, which raises the zero-cell count,
which the baseline rule charges at ≈525 nats for A1's ×0.70 — so the sampler re-selects
high-`CFR_target` draws to compensate (measured: the baseline deaths-channel argmax sits at
**2× `CFR_target`**, A1b's at **≤0.25×**). Under A1b the same ×0.70 is a ≈145-nat *gain*.

---

## 4. WHAT I COULD NOT DETERMINE

1. **The six new 30k COD/MOZ/ETH calibrations did not finish in time** (`dugong:~/inflab/m_{prod,A1b}_{COD,ETH,MOZ}_n30000_s0`,
   156 R processes still running at 13:30). They are the right confirmation set: CFR-MATH-01 predicts
   a chain residual of ~1.44 in all six, and A1b-vs-prod should show the `CFR_target` posterior moving
   **down** under A1b and **up** under prod. **Not measured.** Run
   `.mosaic_add_implied_cfr_columns()` on each and compare `cfr_epidemic/CFR_target` — it should be
   exactly `(1+eps)` in all six, which is a 30-second check.
2. **The ensemble-level effect of B2.2 on R² and bias is estimated, not measured.** I could not
   faithfully reproduce `calc_model_ensemble()`'s weighted median (my reconstruction gave
   R²_cases 0.061 against the run's 0.786 — the ensemble does per-member stochastic reruns and
   subset optimisation I did not replicate). The per-member paired effect (aggregate deaths ×0.696,
   cases invariant to <0.6%) is solid and is the mechanism; propagating it through the ensemble's
   weighted median is an inference. **The 1.525 → ≈1.06 figure is a projection, not a measurement.**
   It also holds the selected subset FIXED; a real recalibration would re-select members, and under
   A1b the deaths channel prefers lower `mu`, so the true effect is probably slightly larger.
3. **Out-of-sample.** Everything here is in-sample. The brief's OOS deaths R² of 0.109 is untouched
   by my measurements, and none of A1/B1/B2/B3 has an obvious OOS mechanism (they are level
   corrections). C1 plausibly does, but only if the forecast path substitutes simulated cases
   correctly — I did not test that.
4. **The epidemic/endemic CFR ratio (0.41–0.54) is confounded with a denominator artifact.** Deaths
   reported in low-incidence weeks that belong to earlier high-incidence weeks bias it down. It
   survives a 100-case-per-week floor and ETH's within-country slope is independently negative
   (p = 0.083), so I am confident the sign is not +0.5 — but I would not quote 0.5 as the point
   estimate of a *negative* epidemic effect. The early/late split (1.129 [0.860, 1.481]) is the
   cleaner estimand and is what B1 is anchored to.
5. **Whether `CFR_target`'s per-country `sdlog` should vary with the GAM's effective sample size.**
   I established that MLI and COG rest on ~90 effective cases while TCD rests on ~4,000–7,800, and
   that the shared `sdlog = 0.787` therefore means very different things across countries. The right
   ESS→`sdlog` map is a fitting question → **statistician**.
6. **Whether the 0.588 oracle ceiling generalises.** It is ETH-only. COD (psi-saturated) and MOZ
   (southern belt, anti-phased) have different deaths-series sparsity and could have materially
   different ceilings. One script run over the three new runs would settle it.
7. **`k_rc` (the `round(drawn/chi_eff)` low-count loss) has a fat left tail** — p05 = 0.674 on the
   ETH posterior, i.e. 33% of reported cases lost to rounding on low-incidence draws, which inflates
   the realized CFR by 1.48× on those draws. Median effect is 1.3% so it does not change any
   recommendation, but it is an unremarked discretisation bias in the **cases** channel at low
   counts and someone should look at it. Not mine (engine/likelihood).
