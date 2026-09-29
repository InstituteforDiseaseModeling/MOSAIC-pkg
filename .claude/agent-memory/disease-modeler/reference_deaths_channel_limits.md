---
name: deaths-channel-limits
description: What actually limits MOSAIC's deaths channel — the daily zero-cell scoring artifact (70% of observed deaths, ~11k nats), the 0.53-0.59 oracle R2 ceiling, and the ~19-day structural cases-to-deaths lag. Measured ETH v0.90.3 + 100k production, 2026-09-19.
metadata:
  type: reference
---

Measured 2026-09-19 (CFR-MATH track, inference lab). Report:
`MOSAIC-pkg/claude/inference_lab/reports/CFR-MATH.md`. Evidence: ETH n=25,000
(`/Users/johngiles/MOSAIC/output/eth25k_v0903/`, local laptop) + ~2,700 fresh ETH sims.

## 1. The deaths channel's big "CFR signal" is a DAILY ZERO-CELL ARTIFACT

The observed daily deaths series is a **weekly total spread over ~7 days** (mean run-length of
identical values 5.87; 25.8% of scored death-days have `0 < obs < 2`). The simulated daily
deaths are **integer Binomial draws** (mean run-length ~120). So MOSAIC scores a smoothed
observation against an unsmoothed integer simulation. Consequence, ETH posterior members (n=115):

| | zero-with-obs cells | observed units carried | baseline `-y*log(1e6)` penalty |
|---|---|---|---|
| **daily deaths** | 642 / 2,898 | **794 / 1,136 (70%)** | **10,970 nats** |
| daily cases | 213 / 2,968 | 1,615 / 90,371 (1.8%) | 22,312 nats |
| weekly deaths | 70 / 414 | 200 (18%) | 2,763 nats |
| weekly cases | 0 / 424 | 0 | 0 |

Per observed unit the deaths zero-penalty is **39x** the cases one.

**This explains the "deaths channel wants 5x the CFR while over-predicting 5x" paradox** (SAMP-09).
Profiling `CFR_target` in the SIMULATOR (12 top-LL ETH seeds, 8-point grid): the zero-penalty
spans **7,767 nats** and improves monotonically with higher CFR; the NB density spans **2,462
nats** and degrades. The penalty is 3.2x the density, so the baseline argmax sits at
**2x CFR_target (deaths bias 3.88)** instead of 0.5x (bias 1.03). Under A1b (epsilon-floored mean)
the argmax moves to **<=0.25x** and the profile range collapses 2,747 -> 472 nats.
SAMP's 10,029-nat `CFR_target` profile and the 10,970-nat zero penalty are the SAME quantity.

**Mechanism detail that matters:** raising `mu` is the only lever that reduces the zero-cell
COUNT (a day with expected 0.3 deaths draws non-zero 26% of the time; at 3.0 it is 95%). A pure
multiplicative rescale of an already-simulated series does NOT reproduce this — the zeros stay
zero at every scale, so the penalty drops out of the argmax. It is a stochastic effect and only
appears when mu changes in the simulator. Do not test it by rescaling a fixed series.

**HARD DEPENDENCY:** any change that LOWERS mu (B2.2, an eps re-shape, a CFR re-anchor) must ship
AFTER A1b (or weekly deaths scoring). Under the baseline rule a x0.70 on mu costs ~525 nats and
the sampler re-selects high-`CFR_target` draws to undo it; under A1b the same x0.70 GAINS ~145 nats.

**Why single-country runs over-predict deaths more than joint ones:** ETH implied CFR is
2.85x observed at 25k single-country but 1.56x in the 40-country 100k. Joint = 1.03 (prior
centring) x 1.44 (chain); single = 2.39 (posterior CFR_target actively selected UP by the
zero-penalty) x 1.19. The zero-penalty has full leverage at nL=1 and is diluted at nL=40.

## 2. The deaths R2 CEILING (ETH, daily, corr-R2)

```
current model deaths channel                      0.371
constant CFR x ENSEMBLE cases                     0.400
LOESS CFR(t) x ENSEMBLE cases                     0.461
constant CFR x OBSERVED cases   (oracle)          0.530
LOESS CFR(t) x OBSERVED cases   (oracle)          0.588
observed deaths vs its own 7-day moving average   0.750
```

An oracle knowing the true daily cases AND the true CFR(t) reaches **0.588**. The model is at
0.371 = 63% of that. **Realistic recoverable deaths R2 is 0.40-0.46 (+0.03 to +0.09).** The
0.37-vs-0.79 deaths/cases gap is mostly an OBSERVATION-granularity ceiling, not a model defect —
the cases series has 2,590 non-zero days of 2,968 (CV 1.34), deaths 907 of 2,898 (CV 1.73).
State this in the paper rather than chase it. (ETH-only; COD/MOZ ceilings not measured.)

## 3. The structural cases-to-deaths LAG is ~19 days and nothing can absorb it

Reported cases read `new_symptomatic` INCIDENCE at `t - delta_reporting_cases`; reported deaths
read `disease_deaths` at `t - delta_reporting_deaths`, drawn on the `Isym` STOCK whose members
entered ~`1/(1-exp(-gamma_1))` ~ 9-10 days earlier. Predicted lag = dwell + delta_d - delta_c
~ 9.6 + 4.3 - 2.0 ~ 12 d.

Measured on the ETH ensemble median: **deaths R2 argmax at shift -24 d (0.3927 vs 0.3710);
cases control argmax at -5 d.** The 19-day differential is the structural lag. **Observed** weekly
cases-vs-deaths cross-correlation peaks at **lag 0 weeks** — surveillance reports deaths in the
same week as the cases.

**No free parameter fixes it:** `delta_reporting_deaths` can only ADD lag (prior floor 1 day), and
shortening the dwell means raising `gamma_1`, which is shared with the cases channel where the
profile wants gamma_1 LOWER. Do NOT re-anchor `delta_reporting_deaths` to absorb the dwell — that
re-introduces the mislabelling CLAUDE.md lesson #12(c) fixed. Gain ceiling is only **+0.022 R2**,
inside the harness's own draw-block instability.

Caveat: the ETH profile argmax for `delta_reporting_deaths` at p02 (SAMP) is a single-country
CONDITIONAL PROFILE. In the 40-location 100k posterior it does not move at all (4.853 -> 4.891,
KL 0.24). The ensemble-shift measurement is the stronger evidence; do not lean on the p02 result.

## 4. Conditional-binomial deaths: DATA SUPPORT IS GOOD

ETH, 2,898 scored cells: **0** with `obs_deaths > obs_cases`, **0** with `deaths>0 & cases==0`
(the degenerate case never occurs). Weekly, weeks with >=20 cases (n=276): binomial
**chi2/df = 1.4** — a plain Binomial is adequate. Beta-Binomial MLE a=8.97, b=681.4
(mean CFR 0.0130, intraclass rho=0.0014, 95% between-week CFR [0.0060, 0.0227]).
=> the deaths channel's information about CFR is ~**690 effective observations**, not 2,898 cells.

`deaths_t ~ Binomial(cases_t, CFR_t)` would give deaths R2 0.371 -> 0.400 (const CFR) / 0.461
(CFR(t)), make the deaths bias IDENTICALLY the cases bias (2.92 -> 1.22), and remove the zero-cell
penalty by construction. Costs: the deaths channel stops constraining transmission, and the
forecast path must substitute SIMULATED cases. MODEL-STRUCTURE change — escalate, do not ship.

Related: [[project-prod-deaths-bias-b2-epi-gap]], [[mu-j-epidemic-factor-prior]],
[[cfr-mu-j0-identity]], [[deaths-embargo-dwell]].
