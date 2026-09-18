# BACKLOG — technique queue (v3 ladder, 2026-09-18)

Status: `PREREQ` (blocks everything) | `BACKBONE` (pre-registered ladder, run in order) |
`OPEN` (candidate) | `RUNNING` | `DONE` | `PARKED` | `PROPOSED` (needs a hypothesis to be runnable).

Class: **C** = cache-paired (free, no fit noise) | **R** = refit (must clear the measured floor,
PROTOCOL §3b).

Cost: S = < 1 h dugong, M = 1-6 h, L = > 6 h or new ingestion. Refit costs are from PROTOCOL §6.

---

## Prerequisites — nothing downstream is interpretable until these clear

| id | technique | class | lane | cost | why |
|---|---|---|---|---|---|
| **P001** | score the **existing** production psi cache (OCV-4, v7.4 leak-free, 10 seeds) on the v3 grid | R | ml/stat | **0 h** | 9 cutoffs already frozen on dugong at `~/MOSAIC/MOSAIC-pkg/claude/forecast_cv_ocv4_q2yr/psi_cache/`. Read-only (§5.7). The first number in the v3 ledger. |
| **P000** | refit `est_suitability()` **package defaults** (v7.3, midpoint grid, sub 6) at the same 9 cutoffs | R | ml | 3.6 h | "current production model" is ambiguous — the shipped default is v7.3, the validated stack was v7.4. Scoring both settles it and yields the v7.3-vs-v7.4 contrast for one refit. |
| **FLOOR-10** | replicate the incumbent from a **disjoint seed block** at `n_seeds = 10` | R | stat | 3.6 h | The v2 floor (0.070) was measured at 3 seeds. Predicted ~0.038 at 10. **Every class-R delta is uninterpretable until this exists** — the single lesson of waves 0-20. |
| **HA-02** | decouple epoch selection from the seed ensemble: `arch_control$epoch_select_seeds = k` (default NULL = unchanged) | R | swe | 2 h build | Today cost is `folds x seeds`, so F1 at 10 seeds is 53 h. Decoupled it is 12.4 h. **This is what makes "more CV folds at the production seed count" affordable at all.** Fit-only path -> §5.6 smoke; run as an equivalence arm (predicted 0). |
| **TRUNK-REG** | trunk registry: `arch_control$trunk = "lstm" \| "tcn" \| "gru"` inside `build_trunk()`, FiLM head and loss held fixed | R | swe/ml | 3 h build | `N` arms do not exist as registered specs until a trunk can be selected. `build_trunk()` (`R/lstm_film_suitability.R:104`) already returns `(B, units_3)` and everything downstream is trunk-agnostic, so this is a contained change — and it is the **only** way to test an architecture against *today's* production model rather than against the April bake-off's retired 3-layer LSTM. |

---

## Backbone F — inner-CV structure (run in order, one change per arm)

All at the incumbent's `n_seeds`, on the frozen 9-cutoff grid. Fold counts are exact
(`.psi_make_rw_cv_steps()` over the 9 cutoffs); costs assume `HA-02`.

| id | arm | one change vs parent | folds | cost | predicted | mechanism (PROTOCOL §3c) |
|---|---|---|---|---|---|---|
| F4 | 84 d stride, `min_train_years = 4`, 84 d window, 2 wk gap | day-based geometry matched to the deployment horizon (bundle: `step_days`/`test_days`/`min_test_days`/`gap_weeks`/`min_train_years`) | 229 | 5.7 h | + | F-epoch + validation windows that are the same length as the thing being forecast |
| F1 | **28 d stride** | stride only, vs F4 | 677 | 12.4 h | 0 | **F-epoch — the honest null.** 3x the folds for the same scalar `median(best_epoch)`. If this moves `S` by more than the floor, the epoch estimate was the binding constraint and that is a real finding. |
| F2 | **14 d stride** | stride only, vs F1 | 1,351 | 22.5 h | 0 | as F1; run only if F1 clears the floor (otherwise the fold count is measured inert) |
| F3 | `min_train_years = 2` | grid start only, vs F1 | 912 | 16 h | + | more *seasons* validated, not just more folds: the midpoint rule never validates the first half of the span |
| F6 | **fold-score selection** (`CV-05`): choose the epoch by held-out WIS/MAE instead of pooled BCE `val_loss` | selection criterion only | as parent | +0 | + | **F-select** — `val_loss` is not the deployment metric, and `.rcv_wis()` / `.rcv_baseline()` already exist |
| F7 | epoch estimator: trimmed mean / per-fold-weighted instead of `median(best_epoch)` | estimator only | as parent | +0 | 0/+ | F-select; cheap, and the median discards the fold spread entirely |
| F8 | **fold-model ensembling** — the RW fold models enter the prediction instead of being discarded | prediction assembly | as parent | M | + | **F-ensemble** — today `n_folds` trained models are thrown away and one refit replaces them; bagging over origins is the standard answer and is free of extra fits |

## PRIORITY (wave 23): country-variability capacity is now the top of the ladder

**Why this family is prioritised — RESTATED at wave 24, because the original argument was wrong.**

The wave-22 motivation was that every arm's benefit splits along the shipped `snf_k5` map
(snf_1 weighted −0.259, all members non-positive; snf_2 +0.498; Wilcoxon p = 0.045).
**That claim is RETRACTED.** Tested against a pure fit-noise null — `P000R` vs `P000`, the *same
spec*, so the delta is only the seed block — region adds `p = 0.1416` (R² 0.181), which is
indistinguishable from what it adds on the two real arms (`p = 0.0804` and `p = 0.1173`). Region
explains about as much of the noise as of any effect, because the floor shows fit instability
concentrated in COD (0.611), SOM (0.212) and BDI (0.145) — **all snf_2**, which is therefore simply
the noisy region. Mean reversion was also tested and is not the explanation.

**What still justifies this family is the architecture audit, whose findings are structural facts
about the model and do not depend on any delta analysis:**

- country/region conditioning acts **only on the trunk's 32-dim output**, while the recurrent
  weights that process the 13-week sequence are identical for all 40 countries;
- `country_balance = FALSE`, so the shared trunk is fitted in proportion to data volume;
- **zero** static country covariates in the feature set, so country identity is a bare ID embedding
  and countries can be pooled only by hard region membership, never by similarity;
- tanh γ is **sign-preserving**, so a country can damp or double a trunk feature but never reverse it.

Those are defects whether or not the deltas show a regional pattern. The per-country *decay-ratio*
spread (0.083 ZWE to 1.387 MWI) is still cited as motivation but is **itself measured on a single
fit and not yet tested against the noise null** — that check is queued and should be done before the
spread is quoted again.

| order | id | change | class | cost | status |
|---|---|---|---|---|---|
| **1** | **N8** | `country_balance = TRUE` | R | ~3.6 h | one flag that already exists and has never been tested; cheapest test of the whole family, and it directly explains the snf_1/snf_2 asymmetry (the trunk is fitted in proportion to data volume, so it learns snf_2's response) |
| **2** | **N5** | `film_input = TRUE` — condition the trunk's INPUTS | R | ~3.6 h | **BUILT, TESTED, SMOKED.** Exact identity at init (verified), so the arm starts at the production model. Restores the input-FiLM branch the gauge_A port dropped |
| **3** | **N6** | `gamma_scale = 2` — let country modulation flip a sign | R | ~3.6 h | **BUILT, TESTED, SMOKED.** tanh gamma is sign-preserving, so a covariate with opposite effects in two regimes cannot be represented |
| **4** | **D9b** | static country covariates as **FiLM conditioning input**, replacing/augmenting the bare country-ID embedding | R | ~3.6 h + small build | **Design corrected by the D9 audit.** 12 candidates are clean (0% NA, all 16 pool countries), but 8 are **exactly constant in time** (per-country temporal CV = 0.000, cross-country CV 0.44–1.18) and the other 4 are near-constant within a 13-week window. So appending them to the 38 *sequence* features (`D9a`) feeds the LSTM a constant channel it can only use as a per-country bias — which `beta_c` already supplies, making `D9a` close to a no-op by construction. `D9b` instead makes a country's modulation **a function of its characteristics**, so RWA (29 non-zero weeks) inherits modulation from characteristically similar countries rather than learning a private 16-dim embedding from almost nothing. Composes with N5: same generator, different conditioning input. |
| — | ~~D9a~~ | the same covariates appended as sequence features | R | — | **DO NOT RUN** — constant-in-time channels duplicate `beta_c`; see the audit row in `REGISTRY.tsv` |
| 5 | N7 | region-specific final trunk layer / gated trunk mixture | R | M | genuinely region-specific dynamics; queued behind N5 because N5 is the cheap version of the same hypothesis |
| 6 | AR-08 | per-region or per-regime models instead of one global fit | R | L | what the data says most directly, and how the downstream calibration already works (per-country) |

All are class R, so all need the fit-noise floor `P000R` is measuring. N5/N6 are launch-ready the
moment it lands.

## The wave-22 finding reorders the ladder

C6 established that psi's out-of-sample failure is a **systematic horizon-growing level collapse**
(0.179 -> 0.089 against a flat truth of ~0.22), that it is model-side, and that six class-C repairs
all fail A3 because the correction helps in sustained-transmission countries and destroys countries
whose epidemic is ending. **The binding constraint is that psi has no information about whether
transmission is currently sustained or ending** — its inputs are climate only. That makes the next
arm a refit, and reorders what is worth running:

| id | arm | class | why it moved |
|---|---|---|---|
| **D8** | **recent-incidence (autoregressive) input channel** | R | THE headline hypothesis. Every serious epidemic forecaster is autoregressive; this one is not. It is the only arm that supplies the information the class-C repairs were trying to infer from the level and could not. **SCOPE CAVEAT, for the user not the agent:** psi is consumed by the engine as an *environmental forcing* term, so an autoregressive psi partly duplicates the engine's own transmission dynamics — the same "changes what psi means" warning `DA-05` and `CV-01` carry. Worth measuring regardless, because it bounds how much of the 12-week gap is a missing-input problem. |
| **N3** | multi-horizon heads (4/8/12 wk) | R | promoted. C6 shows the error is strongly horizon-structured (h1 -0.19, h2 -0.51, h3 -0.30 and a monotone level decay), and the model has ONE head trained on one target. The most direct architectural answer to a horizon-structured error. |
| **AR-07** | probabilistic psi into the engine | R | promoted from PARKED. psi's intervals are seed dispersion with 6.8% zero-width rows, and C7b/C7b_C9d move by ~0.19 in residual mode while losing in seed mode — the interval treatment is doing real work in these verdicts, not just decorating them. |
| C7d | combination weight per country AND per horizon week | C | PARKED as not estimable: ~1 block per (country x horizon-week) cell on this grid. C7c's pooled version scored -0.874. |
| C9e | any further tuning of the C9d gate threshold | C | **DO NOT RUN.** Choosing a threshold that excludes ZMB after seeing ZMB is selection on the evaluation set — the error this ledger already records twice (B-CAL2, C7). |

## Backbone N — architecture (blocked on `TRUNK-REG`)

| id | arm | one change vs parent | cost | predicted | evidence |
|---|---|---|---|---|---|
| N1 | **TCN trunk** in place of the 3-stack LSTM, FiLM/head/features fixed | trunk only | 6 h | + | `tcn_v1` led the April bake-off at 4 wk (0.1476, best NN) **and** 13 wk (0.1178, the only NN to beat persistence at 0.1187). **Caveat now recorded:** that leaderboard's LSTM entry was the *then*-production 3-layer LSTM; `lstm_film_suitability.R` landed 2026-06-07, after the bake-off, so no alternative architecture has ever been compared against today's FiLM model. Also `tcn_v1` ranked **last of six** under the bake-off's v2 (26-week) protocol — the replication is across horizons within one protocol, not across protocols. |
| N2 | **GRU trunk** | trunk only | 6 h | 0 | `gru2_v1` was mid-pack (0.1608 at 4 wk). Cheap once the registry exists, and a second trunk is what distinguishes "TCN is better" from "the LSTM trunk is arbitrary". |
| N3 | multi-horizon heads (4/8/12 wk) on a shared trunk (`CV-02`) | head only | 6 h | + | the only architecture change that targets the horizon directly rather than the sequence encoder |
| N4 | `rec_dropout = 0` (`AR-01`) | regularisation only | -31% | 0 on skill | **measured**: 0.36 vs 0.52 min/fold, `val_loss` indistinguishable across 5 folds. Free speed; run it early and keep it if inert, because it makes the whole F ladder ~30% cheaper. |
| N5 | per-region hyperparameters (`AR-08`) | hyperparameters only | M | ? | the FiLM trunk already conditions on region; never tried |

## Backbone D — data and features

| id | arm | one change vs parent | cost | predicted | evidence |
|---|---|---|---|---|---|
| D1 | **v7.3 -> v7.4 features**, holding inner geometry and seeds at the incumbent's | feature set only | 6 h | ? | decomposes the `P000`/`P001` bundle. The earlier v7.4 head-to-head **lost on cases** (WIS-skill 0.141 -> 0.019), but five defects were found afterwards (positional write-back after re-sort in all 3 imputers, circular drought label, non-convergent drought GAM, 1,560 CMIP6 rows entering as observation, cyclone AUC measuring country ranking not timing). The OCV-4 cache used the **leak-free** per-cutoff panels, which is the fixed version — so this is a re-test, not a repeat. |
| D2 | **forecast-observable features only** (`AR-03`): drop the 20 of 38 v7.3 channels not knowable at a 12-week origin | feature set only | 6 h | + | 53% of the production feature set is unobservable at the origin; past the ERA5 horizon it is free-running CMIP6 with no anomaly skill. At v2 this was INCONCLUSIVE (sign flipped with interval mode, below resolution). **Deliberately a lower bound** — it also drops the ENSO short lags that NMME genuinely forecasts. |
| D3 | restore ENSO `data_source` provenance (`DA-07a`) | ETL only | S | 0 on skill | the column exists upstream (137 `forecast` rows) and is dropped at `compile_suitability_data.R:242`. Lets a run label observed vs projected cells per-cell instead of in a caveat sentence. Does not make the evaluation a forecast; makes it honest about not being one. |
| D4 | loss reweighting toward outbreak-coincident weeks (`AR-09`) | loss only | S | + | recorded mitigation for the flat-flood-window failure; never tested |
| D5 | target reformulation crediting flood-onset lead (`AR-10`) | target only | M | + | `target_D` floors episodic spikes toward 0 in off-years |
| D6 | per-capita target variants per regime (`DA-08`) | target only | M | ? | D lifted SOM 0.344 -> 0.816 and COD 0.513 -> 0.726 but regressed 5 of 15 (RWA 0.40 -> 0.03); never resolved into a per-regime rule |
| **D7** | **target-normalisation window: fix the scale, or make the leak explicit** | target only | ml/etl | M | ? | **FOUND 2026-09-18 while re-anchoring the grid.** `target_D_rate_per_country_floored` is normalised per country over its panel's compile window, so the leak-free per-cutoff v7.4 panels do NOT carry the canonical panel's target values: over the pool x 9 scored blocks 1.23% of rows differ by > 0.01, 0.70% by > 0.05, max 1.00 (TZA 1.00, SSD 0.61, COD 0.35). Two consequences. (a) An arm trained on a per-cutoff panel is scored against a series it never trained on -- run `audit_target_scale.R` with every such score; the bias favours canonical-panel arms, so it is conservative for adoption. (b) The deeper issue is the reverse: the CANONICAL target's denominator is computed over the full series **including post-cutoff data**, so the shipped v7.3 path trains on a mildly look-ahead target scale, and the per-cutoff version is the leak-free one. Resolving this means choosing a fixed, pre-cutoff-only normalisation for BOTH paths -- which changes the observed series and is therefore an objective change (PROTOCOL 5.1), not an arm. |

---

## Class-C arms — free, run any time (PROTOCOL §2.3)

| id | technique | why |
|---|---|---|
| C1 | `interval_mode` decomposition: WIS -> point / width / coverage terms | interval treatment flipped **two** arm verdicts at v2 (B-CAL, AR-03) after D2 was fixed. Decomposing WIS settles it without touching the objective. |
| C2 | NC1 / NC3 negative controls on the v3 incumbent | §5.2, every 5th wave, free |
| C3 | `seasonal` / `persistence_last` baseline panel on the incumbent | establishes the A6 must-beat bar (OBJECTIVE §3b) |
| C4 | per-horizon (h1/h2/h3mo) decomposition of the incumbent | is the 12-week loss a 12-week problem or a 4-week problem that compounds? Never measured. |
| C5 | target-variability tertile conditioning (wave-17 diagnostic) on the v3 grid | the one analysis that reversed a headline; re-run it on the new blocks before trusting any pooled claim |

---

## Downstream (stage 2 — does a better psi propagate?)

| id | technique | lane | why |
|---|---|---|---|
| DS-01 | re-baseline at v0.90.x before stage 2 | swe | the env route changed 2026-09-17 from a saturated step carrying 99.87% of infections to a live channel (saturation 1.0000 -> 0.0018; env/human 1044x -> 1.73x). OCV-4's committed scores predate that, so stage 2's baseline needs a re-run or an explicit caveat. |
| DS-02 | `psi_star` attenuation (near-identity `b`, lag `k`) | doctor/dm | calibration drives `psi_star_b` to the prior floor, muting psi to ~0; forcing near-identity + ~20 d lag gave the largest OOS win on record (WIS 7.4 -> 5.3). **A psi improvement cannot show up through an attenuating transform** — this is the main reason a stage-1 win might not propagate. |
| DS-03 | run stage 2 at n ~ 10k, not 100k | stat | posterior saturates at n ~ 7-10k; exact IS ESS pinned at 1.00 from n=500 to 100,000. 10x cheaper at no measured loss. |

---

## Parked / out of scope, recorded so they are not rediscovered

| id | item | why parked |
|---|---|---|
| AR-06 | torch backend | bitwise reproducible where keras diverges up to 0.98, but lost Tier-2 equivalence (p = 0.0065) and was not better (+0.006). Adopt only if reproducibility is the goal. Branch `feature/psi-torch-port`. |
| AR-07 | probabilistic psi into the engine | `q025/q25/q75/q975` computed then discarded; engine takes a point. Real, but it is an engine change, not a psi arm. |
| DA-07b | NMME reforecast archive keyed by issue date | **the structural blocker**: only the latest forecast is retained (zero duplicate `(variable, year, week)` keys), so a cutoff-dated forecast for 2024 does not exist. Until it does, no psi rolling-origin evaluation can be a true forecast evaluation. New data acquisition, not a code change. |
| DA-04 | conflict / displacement covariates (ACLED, UNHCR/IOM-DTM) | the only item addressing a driver class absent altogether (Borno, Goma, Sudan, Somalia). T2's redirect target. |
| DA-05 | subnational suitability (COD, NGA, SOM, +/- ETH, SSD) | the recurring ceiling: COD 77% saturated nationally, NGA/SOM phase-misaligned. T2's other redirect target. |
| DA-06 | route acute hazards via `theta_j(t)` instead of psi | epi verdict: flood/cyclone act by defeating WASH, not by changing climate suitability. Out of scope here. |
| F5 | 7 d inner stride (2,697 folds) | 43 h even with `HA-02`. Out of budget at any seed count; listed so it is not re-derived. |
| CV-08 | flatten the fold loop into the parallel work queue | measured inert: the box is already core-saturated across shards, so ~2 h is the floor whichever axis is parallelised. `HA-02` is the real fix. |
| INFRA-03 | `est_suitability()` output-directory argument | writes three files to fixed `PATHS$MODEL_INPUT` paths, so N concurrent fits race. Worked around (score from the canonical panel); still live for anything else that fans psi fits out. |

## Explicitly NOT on the backlog

- **Broad architecture search.** The bake-off spread among six architectures was ~15% MAE against a
  ~2.5x gap to the trivial baselines. `N1`/`N2` are in because the TCN result replicated across two
  horizons and because no alternative has ever been tested against the *current* FiLM trunk — not
  as an opening for a search.
- **More simulations downstream.** Measured inert (`DS-03`).
- **Within-subset weight tuning.** Uniform ~ saturated within 0.008 R2, and skill *decreases* as the
  likelihood concentrates.
- **Screening at reduced origin counts.** Retired at v3 (PROTOCOL §2): the sign-flip floor at 4-6
  origins is p = 0.125 and the unchanged-arm subset swing reaches 0.77.
