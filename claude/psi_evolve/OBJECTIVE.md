# FROZEN OBJECTIVE — psi 12-week forecast evolution

**Frozen 2026-09-17, re-anchored to the production validation grid 2026-09-18 (v3).
Do not edit without bumping `objective_version` and re-scoring the incumbent.** Every scoring run
asserts the sha256 of `weights_frozen.csv` matches the value recorded here. An arm scored under a
different objective version is not comparable and must be re-scored, not compared.

```
objective_version: 3
weights_file:      weights_frozen.csv          (UNCHANGED from v1/v2 -- same pool, same sha)
weights_scheme:    w_sqrt   (w proportional to sqrt(reported cases 2021-2025))
evaluation_grid:   EVAL_GRID.csv  (grid == "prod": the 9 OCV-4 production-validation cutoffs)
```

### Version history
- **v1** (2026-09-17) — initial freeze: sqrt-burden weights, 16-country pool, no-regression guard.
- **v2** (2026-09-17) — adds section 4b, separating TRAINING folds from EVALUATION blocks and
  freezing an 84-day/18-cutoff evaluation grid invented for this programme.
- **v3** (2026-09-18) — **re-anchors the evaluation grid onto the cutoffs at which the production
  model has already been validated** (OCV-4), adds the horizon decomposition, the seasonal
  baseline, and the two-stage (psi-level -> downstream) gate. **Authorized by the user**, so §5.1's
  "an agent may not enact an objective change" is satisfied by instruction rather than by
  inference. The weights file and its sha are **unchanged** — the pool is the same 16 countries, so
  per-country weights carry over. **Scores are NOT comparable across v2 -> v3** (different blocks):
  every v2 `S` in `REGISTRY.tsv` is superseded, not refuted. `A000/A100/T1/AR03/B-CAL*` keep their
  rows and their v2 conclusions about *method* (the fit-noise floor, the aggregation artifact, the
  interval-mode sensitivity) but their `S` values do not transfer.

## 1. Primary objective (stage 1, psi-level)

```
S(arm) = sum_j  w_j * wis_skill_j(arm)      over the 16-country scoring pool
```

where `wis_skill_j` is WIS-skill vs **persistence** on the held-out block of each production
cutoff, per country, median across blocks; `w_j` is the frozen sqrt-burden weight.

## 2. The no-regression guard (hard constraint, not a term)

An arm is rejected, whatever `S`, if **any of the top-10 burden countries** regresses against the
incumbent by more than `epsilon = 0.02` WIS-skill:

`COD, NGA, SSD, ETH, MOZ, SOM, MWI, AGO, ZWE, ZMB`

This is what stops the search buying a burden-weighted win by sacrificing breadth. **Measured
caveat (v3):** AGO has observed weeks in only **6 of 9** blocks, so its guard rests on 6 cells.
A guard verdict driven solely by AGO must say so.

## 3. Reported alongside (never optimised)

- `n_beat` — count of the 16 pool countries with `wis_skill > 0`
- **skill against all three baselines**: `persistence` (primary denominator), `seasonal`
  (week-of-year climatology from >= 2 y of pre-cutoff history), `persistence_last`
- **per-horizon skill**: `h1mo` / `h2mo` / `h3mo`, from the same cells (§4a)
- per-country Pearson, bias ratio, coverage
- score vs position-in-window (the 2-week-embargo diagnostic)

### 3b. The seasonal baseline is a must-beat, not just a report

`persistence` is a locally-anchored flat constant (`mean(tail(observed, 4))`), so beating it says
nothing about whether psi's *time variation* is informative — wave 17 measured psi's edge over a
constant at **+0.378 where the target is flat and +0.017 where it moves**. `seasonal` is the
cheapest baseline that actually varies in time.

**An arm that does not beat `seasonal` is not adopted, whatever its `S`.** If a week-of-year
climatology forecasts transmission intensity as well as the model does at 12 weeks, the model's
learned dynamics are not earning their keep, and a better-scoring variant of it is not an
improvement worth shipping. (This supersedes the wave-8 per-country-constant rule, which was the
same idea with a weaker baseline; the constant stays a reported diagnostic.)

## 4. Scoring pool (n = 16)

In `iso_codes_mosaic` AND >= 26 non-zero case-weeks over 2021-2025 (trusted sources WHO/JHU/SUPP,
AI-mined rows excluded from the weight derivation).

| iso | cases 21-25 | nonzero wk | w_sqrt | top-10 guard | blocks with obs (of 9) |
|---|---|---|---|---|---|
| COD | 164,016 | 193 | 0.1321 | yes | 9 |
| NGA | 161,900 | 206 | 0.1312 | yes (flagged: cases ~100% imputed) | 9 |
| SSD |  98,635 | 105 | 0.1024 | yes | 9 |
| ETH |  66,130 | 156 | 0.0839 | yes | 9 |
| MOZ |  57,955 | 153 | 0.0785 | yes | 9 |
| SOM |  48,695 | 155 | 0.0720 | yes | 9 |
| MWI |  42,524 | 109 | 0.0673 | yes | 9 |
| AGO |  36,351 |  51 | 0.0622 | yes | **6** |
| ZWE |  35,329 |  96 | 0.0613 | yes | 9 |
| ZMB |  25,456 | 107 | 0.0520 | yes | 9 |
| TZA |  17,680 | 124 | 0.0434 | no | 9 |
| CMR |  15,093 |  70 | 0.0401 | no | **5** |
| KEN |  11,943 |  41 | 0.0356 | no | **6** |
| BDI |   5,888 | 150 | 0.0250 | no | 9 |
| LBR |     494 |  80 | 0.0072 | no | 9 |
| RWA |     312 |  29 | 0.0058 | no | 9 |

Concentration: top-3 = 36.6%, top-10 = 84.3%, **effective n = 11.9** of 16.
**Measured coverage: 134 of 144 country-blocks carry >= 1 observed week** (AGO 6, CMR 5, KEN 6).
Weights are renormalised over the countries actually scored in a block, so the missing cells shift
weight rather than silently dropping it.

### Why sqrt and not raw case-share
Raw shares put **53.8%** of the objective on COD+NGA+SSD (effective n = 7.9), which makes this a
three-country objective and contradicts the "most countries" half of the goal. Sqrt preserves the
burden ORDERING exactly while letting ~12 countries carry real weight. To revert, set
`weights_scheme: w_raw` and bump `objective_version`.

### Known exclusions, recorded so they are not rediscovered
- **SDN (Sudan) is #3 by reported burden (135,125 cases) and is NOT in `iso_codes_mosaic`.**
  It cannot be scored. This is a model-scope limitation, not an objective choice.
- 13 MOSAIC countries fall below the 26-week floor (COG, NAM, BEN, CIV, BFA, CAF, GHA, GIN, GNB,
  MLI, SEN, TCD, UGA). They remain in the psi TRAINING pool (all 40) but are not scored.
- NGA carries 13.1% of the weight and its cases are essentially fully imputed. It stays in the
  objective (it is real burden) but every report must show the objective with and without NGA.

## 4a. The evaluation grid IS the production validation grid (v3)

**Design decision, 2026-09-18.** v2 invented its own 18-cutoff / 84-day-stride grid. v3 replaces it
with the grid at which the production model has **already been validated end to end** — the OCV-4
quarterly forecast-CV. Three things follow, and they are the whole reason for the change:

1. **The incumbent is (mostly) free.** Production psi is already fitted and frozen at all 9
   cutoffs: `psi_<cutoff>.csv` + `panel_v74_<cutoff>.csv` in
   `/home/jgiles/MOSAIC/MOSAIC-pkg/claude/forecast_cv_ocv4_q2yr/psi_cache/` **on dugong** (1.7 GB,
   built 2026-07-11, MOSAIC 0.63.0, sha256 per cutoff in that dir's `psi_manifest.json`).
2. **Halving the cutoffs funds resolution instead of coverage.** 9 cutoffs rather than 18 pays for
   the seed counts and replicates that v2's ledger lacked, which is what made every refit-arm
   delta uninterpretable (PROTOCOL §3b).
3. **A stage-1 win can be propagated with no new baseline.** The downstream cases/deaths scores for
   this exact grid are committed (`claude/forecast_cv_ocv4_q2yr{,_v2}/`, and
   `output/validation/forecast_cv/ocv4_q2yr/v1_baseline/`), so stage 2 compares against a number
   that already exists.

```
evaluation_grid (FROZEN -- enumerated in EVAL_GRID.csv, grid == "prod"):
  cutoffs:       9 quarterly origins, 2024-01-01 .. 2026-01-01
  embargo_days:  14                      # oos0 = cutoff + 14
  block:         oos0+1 .. oos0+92       # 92 days ~ 13.1 weeks
  horizons:      h1mo = oos0+1..oos0+31   h2mo = oos0+32..oos0+61   h3mo = oos0+62..oos0+92
  selection:     6 cutoffs (<= 2025-04-01) -> 88 country-blocks
  confirmation:  3 cutoffs (>= 2025-07-01) -> 46 country-blocks      (LOCKED)
  phases_covered: 4 of 12  (Jan/Apr/Jul/Oct)
```

The block and horizon-bucket arithmetic is **deliberately identical to
`MOSAIC:::.rolling_cv_label()`** (`R/run_rolling_cv.R:373`), which is what OCV-4 scored with, so a
stage-1 cell and a stage-2 cell refer to the same dates.

### The phase-aliasing cost, accepted with eyes open
v2 chose an 84-day stride precisely **because** a quarterly stride aliases: 4 x 91 = 364 days drifts
1.25 d/yr against the annual cycle, so quarterly origins recur in the same four calendar months.
Measured: v2's grid covered **12 of 12** phase months; this grid covers **4 of 12** (Jan/Apr/Jul/Oct).

That is a real loss and it is accepted, because comparability with the production validation and a
free incumbent are worth more than phase breadth at this budget. Two consequences are binding:

- **No arm may be justified by a seasonal-phase argument** ("it helps in the pre-monsoon window"),
  because three quarters of the phase space is unobserved.
- **A phase-extension set is pre-registered but not built:** off-phase cutoffs at cutoff + 42 d
  (2024-02-12, 2024-08-12, 2025-02-11, 2025-08-12), to be appended to `EVAL_GRID.csv` with
  `grid == "ext"`. It is **not free** — the incumbent has no psi cache there — so it runs only to
  defend an arm that has already won on `grid == "prod"`, and it is scored as a separate,
  clearly-labelled sensitivity, never pooled into `S`.

## 4b. TRAINING folds vs EVALUATION blocks — these are NOT the same thing

**Retained from v2 and still the load-bearing distinction.** The arm ladder varies the inner CV
geometry (stride, window, grid start -> 89 to 1,351 training folds). If `S` were computed on each
arm's own training folds, arms would be scored on different blocks and `S` would not be comparable
— a silent, fatal confound that the one-change-per-arm rule does not catch, because the fold set is
a *consequence* of the change rather than a second change.

| | TRAINING folds (inner) | EVALUATION blocks (outer) |
|---|---|---|
| purpose | epoch selection / hyperparameter choice inside IS | computing `S` |
| varies by arm | YES — that is what the F-arms test | **NO — frozen, identical for every arm** |
| geometry | arm-specific (`step_days`, `test_days`, `min_train_years`, ...) | the 9 production cutoffs above |
| data | strictly `date < cutoff` | `oos0+1 .. oos0+92` |

Each block is scored by a model fitted only on data before its cutoff (via
`prefit_rolling_cv_psi()`), so the evaluation is a genuine rolling-origin backtest.

**It is a conditional hindcast, not a forecast.** Covariates at the target date are realized
reanalysis/CMIP6; no cutoff-dated NMME forecast archive exists (BACKLOG DA-07b). Every report must
carry that sentence. It is a property of the data, not of this harness.

## 5. The two reference arms (v3) — what "the current production model" means

The production validation ran a psi spec that is **not** the package default, so v3 scores both and
takes the incumbent to be whichever wins on selection. Recorded so it is never conflated again:

| arm | spec | cost | provenance |
|---|---|---|---|
| **P001** | `feature_set = "v7.4"` leak-free hazard panels, `n_seeds = 10`, `rw_step_months = 1`, `rw_test_months = 4`, `rw_subsample = 2`, `timesteps = 13` | **ZERO — cache exists** | exactly what OCV-4 v2 validated; `psi_manifest.json` on dugong |
| **P000** | `est_suitability()` package defaults: `feature_set = "v7.3"`, midpoint grid start, `rw_test_months = 5`, `rw_subsample = 6` | one refit, 9 cutoffs | what the package ships today |

`P000` vs `P001` is therefore also the **v7.3-vs-v7.4 data-upgrade contrast**, obtained as a
by-product of establishing the baseline. Note it is a *bundle* (features + panel leak-discipline +
seeds + inner geometry), so it identifies "the validated stack vs the shipped default", not the
feature set alone; decomposing it is arm `D1`/`D2` in `BACKLOG.md`.

## 6. Stage 2 — downstream propagation (gated, not run per arm)

Stage 1 is the gate. **Only an arm that has been adopted at stage 1** pays for stage 2: inject its
frozen psi cache into `run_rolling_cv()` over the same 9 cutoffs x {NGA, MOZ, COD, ETH} and score
cases/deaths WIS / R2 / bias against the committed OCV-4 v1/v2 numbers.

Stage 2 exists because a psi improvement can be invisible downstream: calibration drives
`psi_star_b` to its prior floor, muting psi toward 0 (BACKLOG DS-02). **A stage-1 win that does not
propagate is reported as such and does not become a production change.**

## 7. Selection / confirmation split

- **SELECTION blocks:** cutoffs 2024-01-01 .. 2025-04-01 (6).
- **CONFIRMATION blocks (LOCKED):** 2025-07-01, 2025-10-01, 2026-01-01 (3).
- The scorer takes an explicit `mode = c("selection","confirmation")` and records which was used in
  the registry. **Confirmation may be read only for an arm that has already won on selection.**
  Reading it otherwise is a protocol breach and voids the arm.
- Block 9 (cutoff 2026-01-01) is the thinnest: its block ends 2026-04-17 against per-country
  observed ends of 2026-02-26 (ETH) to 2026-06-04 (MOZ), and CMR/KEN are absent. Confirmation
  therefore rests on ~46 country-blocks, not 48.

```
weights_file_sha256: 255078783c0b8bc582e5128e8f7f8d6d170ccd7d1b0867adf981f8baa5a89a81
```
