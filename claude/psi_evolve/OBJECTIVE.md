# FROZEN OBJECTIVE — psi 12-week forecast evolution

**Frozen 2026-09-17. Do not edit without bumping `objective_version` and re-running the
incumbent.** Every scoring run asserts the sha256 of `weights_frozen.csv` matches the value
recorded here. An arm scored under a different objective version is not comparable and must be
re-scored, not compared.

```
objective_version: 2
weights_file:      weights_frozen.csv
weights_scheme:    w_sqrt   (w proportional to sqrt(reported cases 2021-2025))
```

### Version history
- **v1** (2026-09-17) — initial freeze: sqrt-burden weights, 16-country pool, no-regression guard.
- **v2** (2026-09-17) — adds section 4b, separating TRAINING folds from EVALUATION blocks and
  freezing the evaluation grid. PROTOCOL section 5.1 forbids an agent from *enacting* an objective
  change, so this is versioned rather than amended into v1. The scoring function, the weights file
  and its sha256 are **unchanged**; v2 specifies something v1 left undefined. Nothing had been
  scored under v1 (`A000` was never run), so the mandated re-scoring of the incumbent costs
  nothing and no comparison is invalidated.

## 1. Primary objective

```
S(arm) = sum_j  w_j * wis_skill_j(arm)      over the 16-country scoring pool
```

where `wis_skill_j` is WIS-skill vs **persistence** on held-out 84-day blocks, per country,
median across folds; `w_j` is the frozen sqrt-burden weight.

## 2. The no-regression guard (hard constraint, not a term)

An arm is rejected, whatever `S`, if **any of the top-10 burden countries** regresses against the
incumbent by more than `epsilon = 0.02` WIS-skill:

`COD, NGA, SSD, ETH, MOZ, SOM, MWI, AGO, ZWE, ZMB`

This is what stops the search buying a burden-weighted win by sacrificing breadth.

## 3. Reported alongside (never optimised)

- `n_beat` — count of the 16 pool countries with `wis_skill > 0`
- per-country Pearson, bias ratio, coverage
- score vs position-in-window (the 2-week-embargo diagnostic)

## 4. Scoring pool (n = 16)

In `iso_codes_mosaic` AND >= 26 non-zero case-weeks over 2021-2025 (trusted sources WHO/JHU/SUPP,
AI-mined rows excluded from the weight derivation).

| iso | cases 21-25 | nonzero wk | w_sqrt | top-10 guard |
|---|---|---|---|---|
| COD | 164,016 | 193 | 0.1321 | yes |
| NGA | 161,900 | 206 | 0.1312 | yes (flagged: cases ~100% imputed) |
| SSD |  98,635 | 105 | 0.1024 | yes |
| ETH |  66,130 | 156 | 0.0839 | yes |
| MOZ |  57,955 | 153 | 0.0785 | yes |
| SOM |  48,695 | 155 | 0.0720 | yes |
| MWI |  42,524 | 109 | 0.0673 | yes |
| AGO |  36,351 |  51 | 0.0622 | yes |
| ZWE |  35,329 |  96 | 0.0613 | yes |
| ZMB |  25,456 | 107 | 0.0520 | yes |
| TZA |  17,680 | 124 | 0.0434 | no |
| CMR |  15,093 |  70 | 0.0401 | no |
| KEN |  11,943 |  41 | 0.0356 | no |
| BDI |   5,888 | 150 | 0.0250 | no |
| LBR |     494 |  80 | 0.0072 | no |
| RWA |     312 |  29 | 0.0058 | no |

Concentration: top-3 = 36.6%, top-10 = 84.3%, **effective n = 11.9** of 16.

### Why sqrt and not raw case-share
Raw shares put **53.8%** of the objective on COD+NGA+SSD (effective n = 7.9), which makes this a
three-country objective and contradicts the "most countries" half of the goal. Sqrt preserves the
burden ORDERING exactly while letting ~12 countries carry real weight. To revert, set
`weights_scheme: w_raw` and bump `objective_version`.

### Known exclusions, recorded so they are not rediscovered
- **SDN (Sudan) is #3 by reported burden (135,125 cases) and is NOT in `iso_codes_mosaic`.**
  It cannot be scored. This is a model-scope limitation, not an objective choice.
- 13 MOSAIC countries fall below the 26-week floor (COG, NAM, BEN, CIV, BFA, CAF, GHA, GIN, GNB,
  MLI, SEN, TCD, UGA). They remain in the psi TRAINING pool (all 40) but are not scored: a
  12-week forecast skill estimate on <26 observed weeks is noise.
- NGA carries 13.1% of the weight and its cases are essentially fully imputed. It stays in the
  objective (it is real burden) but every report must show the objective with and without NGA.

## 4b. TRAINING folds vs EVALUATION blocks — these are NOT the same thing

**Design correction, 2026-09-17, found while implementing CV-07. This invalidates a
naive reading of the arm ladder and must be settled before any arm runs.**

The arm ladder varies the CV stride (4 / 8 / 12 weeks -> 164 / 82 / 55 folds). If `S` were
computed on each arm's *own* training folds, the three arms would be scored on three different
sets of blocks and `S` would not be comparable across them — a stride arm could "win" purely by
being evaluated on an easier set of dates. That is a silent, fatal confound of exactly the kind
the one-change-per-arm rule is meant to stop, and the rule does not catch it because the fold set
is a consequence of the change rather than a second change.

**Therefore two grids, with different jobs:**

| | TRAINING folds | EVALUATION blocks |
|---|---|---|
| purpose | epoch selection, seed screening, internal CV | computing `S` |
| varies by arm | YES — that is what S1/S2/S3 test | **NO — frozen, identical for every arm** |
| geometry | arm-specific (`step_days`, `test_days`, ...) | 84-day blocks, 12-week stride, 2-week embargo, from 2014-01 |

The evaluation grid is a **protocol parameter** (frozen under section 5.1), not an arm parameter.
Every arm's fitted model is scored on the same blocks, so `S` differences are attributable to the
arm and not to which dates it happened to be graded on.

`A000` is therefore the incumbent MODEL — v7.3 features, concurrent target, production
architecture — scored on the frozen evaluation grid. It is not "the incumbent's own CV".

```
evaluation_grid:  (FROZEN -- enumerated in EVAL_GRID.csv)
  cutoffs:       18, every 84 days from 2022-01-01 to 2025-11-18
  window_days:   84          # block = cutoff + 14d .. cutoff + 97d
  embargo_days:  14
  selection:     14 cutoffs (< 2025-01-01)  -> 224 country-blocks
  confirmation:   4 cutoffs (>= 2025-01-01) ->  64 country-blocks   (LOCKED)
  phases_covered: 12 of 12
```

**Why 84 days and not quarterly.** A 91-day (quarterly) stride advances 4 x 91 = 364 days per four
cutoffs -- a drift of 1.25 days/year against the annual cycle -- so it aliases onto **4 calendar
months** (Feb/May/Aug/Nov). A model would only ever be graded in those four phases. An 84-day
stride drifts 29.25 days/year and rotates through all 12 while still giving zero block overlap.
Measured: quarterly = 4/12 phases, 5-month = 10/12, **84-day = 12/12**.

Each block is scored by a model fitted on data up to its cutoff (via `prefit_rolling_cv_psi()`),
so the evaluation is a genuine rolling-origin backtest, not an in-sample read.

## 5. Selection / confirmation split

- **SELECTION folds:** all origins with `test_start < 2025-01-01`.
- **CONFIRMATION folds (LOCKED):** origins with `test_start >= 2025-01-01` — approximately the
  most recent 2 origin-years, ~25% of folds.
- The scorer takes an explicit `mode = c("selection","confirmation")` and records which was used
  in the registry. **Confirmation may be read only for an arm that has already won on selection.**
  Reading it otherwise is a protocol breach and voids the arm.

```
weights_file_sha256: 255078783c0b8bc582e5128e8f7f8d6d170ccd7d1b0867adf981f8baa5a89a81
```
