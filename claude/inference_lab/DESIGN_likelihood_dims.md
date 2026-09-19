# Experiment design — likelihood dimensions on top of A1b

Three untested dimensions: **observation weights**, the **NB dispersion floor**, and the four
**shape terms** that ship OFF. All stacked on `arm/A1b_v2`; running them through the old
`-y*log(1e6)` branch would confound.

## The metric that ranks these

`Spearman(LL, R2)` is ~0 under every arm tested so far (+0.064 baseline, -0.001 A1b, -0.082 A5):
**the likelihood does not rank on predictive shape.** The shape terms are shape measures and are
switched off. That makes `Spearman(LL, R2)` the primary screening metric here, not mean R2 --
we are asking whether a setting makes the likelihood *able to see* shape, which is upstream of
whether any selection rule can exploit it.

Secondary: `Spearman(LL, |bias-1|)` (A1b moved this to -0.510, the gain to protect), then R2 and
bias by window/horizon from a full calibration.

## Phase A - discrimination screen (cheap, screens everything)

**Key efficiency.** Predictions depend on the parameter draw, not on the likelihood settings. So:
simulate K draws ONCE, score each for R2/bias, then evaluate `calc_model_likelihood()` on those
same stored predictions under EVERY configuration. One simulation batch ranks the whole factorial.
No calibration, no reinstall.

K = 1,000 draws stratified across the A1b likelihood range, ETH, scored in-sample and on the
held-out tail (t_cut 2025-09-01).

### Factors (one-at-a-time from the A1b reference)

| factor | levels | why |
|---|---|---|
| `nb_k_min_cases` | 3 (ref), 10, 20, 50 | floor binds on 28/28 locations, median override 24x; a prior sweep found this the best in-sample bias lever; the MOSAIC-Ethiopia example ships 20 |
| `nb_k_min_deaths` | 3 (ref), 10, 20 | same floor, sparser channel |
| `weight_peak_timing` | 0 (ref), 0.10, 0.25, 0.50 | direct shape measure |
| `weight_peak_magnitude` | 0 (ref), 0.10, 0.25, 0.50 | direct shape measure |
| `weight_cumulative_total` | 0 (ref), 0.10, 0.25, 0.50 | trajectory progression |
| `weight_wis` | 0 (ref), 0.10, 0.25, 0.50 | proper scoring rule; NB it is a SCORE not a density |
| `weight_deaths` | 1 (ref), 2, 4 | **falsification test**: LIKE measured the deaths NB has NEGATIVE covariance with total LL and predicted raising this makes deaths bias WORSE |
| `weight_cases` | 1 (ref), 0.5 | the complementary rebalance |

~26 configurations, one simulation batch.

### Known miscalibration to report alongside
LIKE-06: cumulative and WIS are **double-normalised** -- cumulative lands at `N_obs x mean / 4` and
WIS at `N_obs x value / 5` against a documented contract of `T`. Sweeping the weight absorbs this,
but the *effective* weight is 4-5x smaller than nominal. Report both; do not read a nominal 0.25 WIS
as 25% of the NB core.

### Interpretation note
WIS is a scoring rule, not a density. Enabling it makes the target a Gibbs posterior rather than a
Bayesian one. The review already established the implemented object is not a Bayesian posterior, so
this is a change of degree, not of kind -- but it must be stated, not stumbled into.

## Phase B - dose-response
For any factor whose main effect moves `Spearman(LL, R2)` off zero, run the full level set and find
the turning point. Still Phase-A cost (no calibration).

## Phase C - full calibrations (expensive; survivors only)
Top ~4 configurations + the A1b reference, ETH n=10,000, 3 disjoint draw blocks, held-out
t_cut 2025-09-01. Scored on the full panel: R2 and bias for all data / training / OOS h1,h2,h3,h4-6,
plus `ess_is`, `khat`. ~15 runs, 4 concurrent at 40 cores = ~2 h once dugong clears.

## Phase D - generalisation
Best configuration to COD and MOZ at n=30,000, compared against the A1b runs already in flight.

## Pre-registered predictions
1. `nb_k_min_cases` up will improve in-sample bias and may *worsen* OOS (sharper likelihood, more
   overfit to the training window). **Watch the horizon split, not the headline.**
2. At least one shape term will move `Spearman(LL, R2)` off zero. Peak-timing is the most likely,
   cumulative the next. If none does, shape is not recoverable through the likelihood and the
   remaining work belongs in the model, not the inference.
3. `weight_deaths = 2` or `4` will make deaths bias WORSE, per LIKE's covariance result. If it
   improves, LIKE's finding needs revisiting and I will say so.
4. WIS will improve bias discrimination but not R2 discrimination, because it scores calibrated
   magnitude rather than timing.
