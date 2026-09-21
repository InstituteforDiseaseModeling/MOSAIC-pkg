# Design — wave 2: OOS, de-confounding mixtures, and the structural fixes

## The diagnosis this responds to

Deaths over-prediction is a **smearing** problem, not a level problem. Production predicts
0.17 deaths/day on Mozambique's 2,828 zero-death days (~481 spurious deaths vs 347 observed
total) while being essentially correct on days that HAD deaths (0.96x). A1b and B2.2 fix bias by
scaling the rate down UNIFORMLY, which also crushes the real death-days:

| | % zero days | prod on death-days | A1b | FULL |
|---|---|---|---|---|
| COD | 39.5% | 1.74x | 1.25x | **1.07x** |
| ETH | 68.7% | 1.81x | **1.08x** | 0.57x |
| MOZ | 91.8% | **0.96x** | 0.31x | 0.14x |

Outcome depends entirely on country sparsity. **No current lever separates "too many deaths on
empty days" from "right number on real days".** That is what wave 2 must fix.

---

## WAVE 2A — runs only, no new code (LAUNCHED)

Closes the OOS gap and de-confounds FULL. `t_cut = 2025-09-01`, n = 30,000, 3 countries.

| arm | package | likelihood settings | purpose |
|---|---|---|---|
| `prod` | v0.90.5 | defaults | reference |
| `A1b` | A1b | defaults | validated arm, now OOS |
| `A1bB22` | A1b+B2.2 | defaults | **the missing cell — B2.2 alone** |
| `A1bB22nbk` | A1b+B2.2 | nb_k_min_cases=20 | is nbkC the useful half of FULL? |
| `FULL` | A1b+B2.2 | nbkC=20 + pkT/cum/wis | full stack, now OOS |

15 runs. Scored on R2 and bias for train and each OOS horizon (h1,h2,h3,h4-6), plus the
death-day / zero-day split that produced the diagnosis above.

**Pre-registered predictions.**
1. B2.2 alone will beat FULL on R2 (FULL's R2 loss comes from the shape terms, not B2.2).
2. MOZ will still overshoot under every arm containing B2.2 — sparsity is the driver, and none of
   these arms addresses the smear.
3. OOS bias will be WORSE than in-sample bias for every arm (the held-out window is the recent
   period where all models over-predict).

---

## WAVE 2B — the MAE ranking rule (~15 lines, next)

Replace the NB log-likelihood ranking in the subset block with
`s_i = MAE_cases(i) / mean(obs_cases)` over the TRAINING window, keeping a logL pre-screen to the
top 30*|B|, uniform weights, weighted median. Cases-only by default: the cases+deaths variant was
measured to select zero-deaths members on sparse countries (ETH valid deaths bias -> exactly 0.000).
Offline it gave held-out WIS skill 0.062 -> 0.424, coverage 0.65 -> 0.92. **Never calibrated.**

---

## WAVE 2C — the structural fixes for the smear (needs sign-off)

These are the only two items that change WHERE deaths land rather than how many.

**C1. `deaths_t ~ Binomial(cases_t, CFR_t)`.** Ties deaths to the case time series, so deaths can
only occur when cases occur. CFR-MATH: well supported (chi2/df = 1.4, 0/2,898 degenerate cells);
makes deaths bias IDENTICALLY the cases bias, which is already ~1.0 in all three countries. That is
the anchor every current lever lacks. **MODEL STRUCTURE — outside lab promotion scope.**

**C2. The deaths timing lag.** CFR-MATH measured deaths argmax shifting -24 d against cases at -5 d
(a 19-day structural lag), with 70% of ETH's observed deaths in cells predicted exactly zero.
Candidate levers: `delta_reporting_deaths` (documented as death-event-to-report, not
symptom-onset-to-report) and the dwell structure. Cheaper than C1 and may capture much of it.

Sequencing: C2 first (cheaper, reversible, inference-adjacent), then C1 if C2 is insufficient.
