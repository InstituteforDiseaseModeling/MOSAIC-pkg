# Data refresh — findings, and why a rebuild is a re-baselining not an update

Investigated 2026-09-21 after the MOSAIC data update was re-run.

## 1. The panel IS stale, and by more than expected

| artifact | built | target/cases run to |
|---|---|---|
| `cholera_country_weekly_suitability_data.csv` (the panel psi trains on) | **2026-09-17 20:39** | 2026-06-18 (2026-06-04 over the 16-country pool) |
| `cholera_surveillance_weekly_combined.csv` (its upstream input) | **2026-09-18 09:51** | **2026-08-10** |

342 processed files are newer than the panel. `compile_suitability_data()` reads
the surveillance file directly (line ~160), so a rerun would pick the new data up.

**218 new country-weeks** are available across the pool. Staleness is very
uneven, and two countries were lagging far more than the ~2 months the headline
suggests:

```
CMR 77 wks   KEN 54 wks   NGA 14   COD 13   SSD 13   AGO 13   RWA 11   MOZ 10
MWI  8 wks   BDI  4 wks   ETH  1   SOM/ZWE/ZMB/TZA/LBR 0
```

Last surveillance week per pool country now spans **2026-03-02 (ETH, ZWE)** to
**2026-08-10 (CMR, MOZ, NGA, SSD)**.

## 2. A rebuild retroactively changes the target — this is the important finding

The target is period-normalised:

```r
cp99r <- max(quantile(anchor_rate, 0.99), cases_eq_5)
target_D_rate_per_country_floored <- pmin(1, log1p(rate) / log1p(cp99r))
```

`cp99r` is the per-country 99th percentile **over the whole panel window**, so
adding weeks changes the divisor and therefore **every historical target value
for that country**. A rebuild is not additive.

Measured impact on `cp99r`:

| country | change | consequence |
|---|---|---|
| **RWA** | **+113.9%** | targets rescaled by ~2x across all history |
| **NGA** | **+71.0%** | large retroactive rescale |
| BDI / COD / CMR | +2.6% / +1.8% / −1.9% | negligible |
| 11 others | ~0.0% | none |

So the damage is **localised to RWA and NGA** but severe there. Both happen to be
special cases already: NGA is flagged exploratory and excluded from pooled
results in the OCV-4 work, and RWA has λ = 0 (psi contributes nothing to its
blend) with the lowest incidence in the pool.

**Every number in `REGISTRY.tsv` is computed against this target**, because the
scorers read it from this file. A rebuild silently shifts the scores of arms that
were never re-run.

## 3. What the refresh buys for the sealed holdout

Recomputed against the updated surveillance (previous figures in brackets):

| cutoff | test_end | cells | isos | **wk9-13 cells** | countries complete |
|---|---|---|---|---|---|
| 2026-02-15 | 2026-06-01 | 181 | 16 | **67** (was 29) | 10/16 |
| 2026-03-01 | 2026-06-15 | 170 | 14 | 62 | 9/16 |
| 2026-04-01 | 2026-07-16 | 131 | 14 | 40 | 7/16 |
| 2026-04-15 | 2026-07-30 | 118 | 12 | 35 | 6/16 |
| 2026-05-01 | 2026-08-15 | 105 | 11 | 30 | 0/16 |

Reference: the **spent** 3-block holdout had 200 wk9-13 cells.

**The overlap catch.** 2026-02-15's scoring window (2026-03-02 .. 2026-06-01)
overlaps roughly 50% with the already-read 2026-01-01 confirmation block. It is
therefore *fresher data* but not a *fully sealed* block. A genuinely disjoint
cutoff must be >= 2026-01-01 + 106 d = **2026-04-17**, which costs coverage.

## 4. Recommendation

**Rerun `compile_suitability_data()` — yes.** The panel is stale by up to 77
weeks for CMR and the refresh materially improves the holdout (29 -> 67 cells).

**But treat it as a re-baselining:**

1. **Archive the current panel first** (`..._frozen_2026-09-17.csv`). Every
   registry number is tied to it.
2. Then choose:
   - **(a) Refit everything** on the new panel — 11 arms x 9 cutoffs. Clean and
     fully comparable, but the most expensive option by far.
   - **(b) Freeze the old panel for phase 2** and use the new one only for phase
     3 / new cutoffs. Cheap, but phase-2 and phase-3 numbers are then NOT
     comparable and must never be tabulated together.
   - **(c) Hybrid:** rebuild, and re-score the *existing* caches against the new
     target without refitting. This is cheap and fixes scoring consistency, but
     the arms were *trained* on old targets, so it measures old models against a
     new yardstick. Defensible only if the cp99r shift is negligible — which it
     is for 14 of 16 countries, but not for RWA/NGA.

**Leaning (c) with RWA and NGA excluded from pooled comparisons**, since they are
the only material movers and both are already treated as special cases. That
keeps one consistent target series, avoids 99 refits, and the exclusion is
declarable up front rather than discovered later.

**Sealed-block choice, pending decision:** 2026-04-15 (35 cells, genuinely
disjoint) is the methodologically clean pick; 2026-02-15 (67 cells, ~50% overlap
with a spent block) is the better-powered but less clean one.

---

# REBUILT 2026-09-21 — what actually changed, and a correction

Panel rebuilt with the exact production parameters (both `full_refresh.R` and
`refresh_psi.R` agree on them), 2.7 min. Frozen copy archived at
`..._frozen_2026-09-17.csv`. Verified with `verify_rebuild.R`.

| | frozen | new |
|---|---|---|
| rows | 56,360 | 56,880 |
| columns | 298 | **298 — identical** |
| target last date | 2026-06-18 | **2026-08-13** |
| new pool country-weeks | — | **218** |

## The prediction in section 2 was WRONG, and for the wrong reason

Section 2 predicted the only material movers would be **RWA (+114%) and NGA
(+71%)** via period-normalisation of `cp99r`. What actually happened:

| iso | cor(old, new) | median \|rel\| | max \|rel\| | revised case rows |
|---|---|---|---|---|
| **RWA** | **0.9011** | 0.003 | **30.7** | 2 of 435 |
| **KEN** | **0.9116** | 0.027 | 1.00 | **86 of 1213** |
| **CMR** | **0.9808** | 0.023 | 10.1 | **109 of 1244** |
| NGA | 0.9993 | 0.089 | 2.03 | 20 of 1371 |
| SOM, ZWE, TZA, LBR, ZMB, AGO, ETH, MWI, COD | ~1.0000 | ~0 | ~0 | 0 |

**Root cause: the surveillance refresh REVISED HISTORICAL CASE COUNTS, it did not
only append new weeks.** 109 rows changed for CMR, 86 for KEN, 20 for NGA. The
countries with zero revisions have `cor` of exactly 1.0000, which is what
identifies revision — not period-normalisation — as the mechanism.

Population was checked and is **unchanged** (0.00% for every country), so the
`rate` denominator is not implicated.

`cp99r` still moved, but as a *consequence* of the revisions rather than of
appending data: RWA's two revised rows are large relative to a target that sits
near zero, which both doubles its `cp99r` and dominates its correlation.

**Also a flaw in my own check:** the first version of `verify_rebuild.R` judged
on median |rel| < 0.01 and therefore labelled RWA "unchanged" while its maximum
relative change was 30.7x. The median cannot see a few large revisions in a
series concentrated near zero. Fixed to decide on **correlation**, reporting the
median and max alongside.

## Consequence for re-scoring

The materially-affected set is **larger than predicted and differently
composed**: `CMR`, `KEN`, `RWA` (series changed, cor < 0.99) plus `NGA` (ordering
kept but ~9% rescaled). Four of sixteen, not two.

That weakens option (c) from section 4 — re-scoring existing caches against the
new target — because CMR and KEN are ordinary pool members, not pre-declared
special cases the way NGA and RWA were. Excluding four countries including two
unremarkable ones is a real cost to the comparison.

Revised recommendation: **keep the frozen panel as the scoring target for every
phase-1/phase-2 arm already fitted**, and use the rebuilt panel only for new
cutoffs and phase-3 arms — i.e. option (b), not (c). The two eras then never
share a table, which is a cleaner rule than a four-country exclusion carried
through every comparison.

## 5. Effect on the CV fold structure

As anticipated: more IS data per cutoff means more inner folds. The effect on
existing cutoffs is small (a 90-day stride gains 0-1 fold from ~2 extra months),
but any NEW cutoff at 2026-04-15 would have ~1.3 years more IS span than
2024-01-01 and so materially more folds. The F5/F6 fold counts in
`PLAN_PHASE2.md` were dry-run against the OLD panel and must be re-verified
after a rebuild.
