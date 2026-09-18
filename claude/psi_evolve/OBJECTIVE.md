# FROZEN OBJECTIVE — psi 12-week forecast evolution

**Frozen 2026-09-17. Do not edit without bumping `objective_version` and re-running the
incumbent.** Every scoring run asserts the sha256 of `weights_frozen.csv` matches the value
recorded here. An arm scored under a different objective version is not comparable and must be
re-scored, not compared.

```
objective_version: 1
weights_file:      weights_frozen.csv
weights_scheme:    w_sqrt   (w proportional to sqrt(reported cases 2021-2025))
```

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
