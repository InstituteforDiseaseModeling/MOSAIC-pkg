# Inference lab — current state (2026-09-19)

**Base:** `main` @ v0.90.5 (`c947820a3`). Lab branch `inference-lab` (+ `INFLAB_SEED_OFFSET` harness hook).
**Standing decisions:** auto-promote within `inference-lab`, human gates `main`; resumable protocol
(no scheduler); **inference machinery only**. See `../RESUME.md`.

## Arms built (all on branches, none merged toward main)

| arm | branch / sha | change | status |
|---|---|---|---|
| A1 | `arm/A1b` lineage, `77ec00d5d` | eps floor = max(0.5, 0.001*mean) | **RETIRED** — overcorrects, kills OOS deaths R2 (0.140 -> 0.001) |
| **A1b** | `arm/A1b_v2` `69aa076c5` | eps floor = max(1e-4, 0.02*mean), channel-relative | **VALIDATED, 3 countries** |
| A5 | `arm/A5` `adf370f07` | weekly block scoring (INFLAB_BLOCK_DAYS) | built, screened, not calibrated |
| A1b+B2.2 | `arm/A1b_B22` `10da02a96` | divide mu_j derivation by (1 + mu_j_epidemic_factor) | measured; OUT of promotion scope (touches sampled params) |
| FULL | `arm/A1b_B22` + control | + nbkC=20, pkT=0.25, cum=0.50, wis=0.50 | calibrating (3 countries, 30k) |

## Validated result — A1b at 30,000 draws, full calibration, 3 countries

| | R2 cases | bias cases | R2 deaths | bias deaths |
|---|---|---|---|---|
| COD prod / **A1b** | 0.677 / **0.719** | 1.033 / **1.015** | 0.316 / **0.375** | 2.178 / **1.483** |
| MOZ prod / **A1b** | 0.446 / **0.474** | 1.140 / **1.016** | 0.264 / 0.243 | 2.359 / **0.522** |
| ETH prod / **A1b** | 0.797 / **0.802** | 1.230 / **1.123** | 0.368 / **0.378** | 2.870 / **1.479** |

Wins R2-cases 3/3, bias-cases 3/3, bias-deaths 3/3, R2-deaths 2/3. **COD — the psi-saturated, least
favourable country — shows the LARGEST gains.** Not an ETH artefact.
Results: `/Users/johngiles/MOSAIC/output/inflab_multi30k/` (237 MB, 33 figures per run).

## Key measurements (with their caveats)

- **MOSAIC calibration is fully deterministic** given (config, priors, control, n): `run_MOSAIC.R:288`
  sets `seed = sim_id`. Replicates need disjoint draw BLOCKS.
- **Draw-block instability:** 3 blocks, best logL within 4%, gave ETH R2_cases 0.797 / 0.019 / 0.371.
- **|B| does NOT improve with size** (SUBSET-EXP): held-out WIS skill peaks at |B| ~ 25-250; shipped
  115 is already inside the optimum. At |B| = n the weighted median becomes an all-zero forecast.
- **The ranking statistic is the big lever** (SUBSET-EXP): replacing NB logL with normalised
  training-window MAE gives held-out WIS skill 0.062 -> 0.424, cases bias 1.51 -> 0.99, deaths bias
  2.19 -> 1.10, cov95 0.65 -> 0.92. Paired over 40 sub-pools, t = -6 to -37. **Larger than A1b +
  B2.2 + shape terms combined.** ~15 lines. NOT YET TESTED beyond one country.
- **B2.2 and weight_deaths=4 are SUBSTITUTES**, not complements: wd4 helps by +0.043 without B2.2 and
  hurts by -0.030 with it. wd4 was compensating for the mis-levelled deaths channel.
- **CFR (CFR-MATH):** the (1+eps) gap is live (chain residual 1.444 vs Gamma(3,6) median 1.446);
  `mu_j_epidemic_factor` is backwards (observed CFR in flagged periods is 0.41-0.54x endemic);
  `rho_deaths` cancels from the whole distribution -> pin it. **Deaths R2 ceiling: an oracle with
  TRUE cases and TRUE CFR(t) reaches only 0.588** vs the model's 0.371 — judge deaths on bias, not R2.

## CORRECTIONS to earlier lab claims (do not propagate the originals)

1. **"Likelihood carries zero information about R2"** — over-generalised. Measured on the top HALF of a
   holdout-scored run. On the top 20% with full-series LL, Spearman(LL,R2) = 0.246. Discrimination is
   weak and depends strongly on tail depth; it is not zero everywhere.
2. **"A1b makes the likelihood 5x better at ranking bias"** — statistic artefact. `|bias-1|` caps
   under-prediction at 1, so dead draws score near-perfect; ~50% of prior draws are dead. On the
   symmetric `|log bias|` it is -0.84 vs -0.79, a small gain.
3. **"Raising weight_deaths makes deaths bias worse"** (from LIKE) — falsified as a headline. wd=4 is
   the best single setting on R2 discrimination WITHOUT B2.2. It does worsen bias discrimination, so
   LIKE's covariance result survives on that axis only.
4. **Screening individual draws measures the wrong object.** Production scores an ENSEMBLE of selected
   draws; selected-draw R2 is ~0.13 where the ensemble reaches ~0.80. All screens now score the
   ensemble median of the selected set.

## Harness bugs

- **HB-01 (FIXED).** "Three seeds" produced bit-identical runs; MOSAIC is deterministic. The 2xSD gate
  was non-functional (paired SD identically 0). Fixed by `INFLAB_SEED_OFFSET`.
- **HB-02 (FIXED, my fault).** I ran `git checkout` in the SHARED read-only worktree while two agents
  were reading it; both were affected. SUBSET-EXP's control arm silently produced A1b values
  (`ll_base.rds` byte-identical to `ll_A1b.rds`). **Give every agent its own worktree at a pinned
  SHA, and inject the arm's code explicitly for every arm including the control.** A control defined
  as "whatever the default is" is not a control when the default can move.

## Queue

1. **FULL stack calibration** (running): A1b+B2.2+nbkC20+pkT.25+cum.50+wis.50, 3 countries, 30k.
2. **MAE ranking statistic** — the largest untested lever. Needs T2 (MOZ/COD/NGA) before promotion.
3. Per-country eps: MOZ overshoots to 0.522 under A1b; one relative constant may not fit every
   country's deaths scale.
4. A5 (weekly scoring) — screened, never calibrated; must be scored at weekly resolution to be fair.
5. Held-out (t_cut) versions of the 3-country comparison; the 30k runs are full-data fits.
