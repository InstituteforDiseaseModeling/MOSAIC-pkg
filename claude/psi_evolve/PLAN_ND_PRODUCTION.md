# Putting ND into `est_suitability()` and A/B-ing it through a full calibration

Investigation, 2026-09-24. Every claim below was checked against the source.

---

## 1. The headline: the argument already exists

`est_suitability()` already accepts `arch_control`, and it flows straight
through to the trunk registry:

```
est_suitability(arch_control=)              R/est_suitability.R:264, :332
  -> .est_suitability_lstm_v2(arch_control=)
  -> .psi_load_arch_control()               R/run_rolling_cv_suitability.R:153
  -> ac$trunk                               R/run_rolling_cv_suitability.R:313
  -> .psi_fit_predict_lstm(trunk=)          R/lstm_film_suitability.R:130
```

So **this runs today, on the installed package**:

```r
est_suitability(PATHS, arch_control = list(trunk = "dlinear"))
```

No new plumbing is required to *use* ND. What is missing is that the choice is
invisible where it matters: `architecture` is the argument a user reads, and it
offers only `lstm_v2_hierarchical_film` and `lstm_v1_legacy`.

## 2. The design decision, and my recommendation

**Do NOT add a third way to say "use DLinear".** The repo's recurring failure
(lessons 11, 13, 14) is two parallel mechanisms for one thing drifting apart.
We would then have `architecture="dlinear_v1"` AND `arch_control$trunk="dlinear"`
able to disagree.

Instead: **make `architecture` a front end that RESOLVES INTO `arch_control$trunk`**,
keeping the registry as the single source of truth.

```r
architecture = c("lstm_v2_hierarchical_film",   # unchanged default
                 "dlinear_v1",                  # NEW -> trunk = "dlinear"
                 "lstm_v1_legacy")
```

with, in `est_suitability()`:

- `dlinear_v1` sets `arch_control$trunk <- "dlinear"` plus the pinned DLinear
  settings for the shipped variant;
- if the caller ALSO passes `arch_control$trunk` and the two disagree, **error**
  — do not silently prefer one (this is lesson 13: a shim must key off the raw
  user input, and a conflict must be loud);
- if the caller passes `arch_control$trunk` with `architecture` left at its
  default, honour it, because that is what the psi_evolve harness already does
  for every arm and breaking it would orphan the whole experiment record.

**Which DLinear does `dlinear_v1` pin?** On the evidence, **plain ND**:
`kernel=5, pad="zero", l2=0, individual=FALSE`. Tempting to ship `NDe` (edge
padding) since it scored best — but its replicate `NDeR` erased every
distinguishing number, and the NDe/NDeR pair spans the whole refinement ladder.
Shipping NDe would be shipping noise. Plain ND is the arm with a replicate-free
claim that survives: the FAMILY beats the LSTM by 13-15%.

Open question worth one decision: the padding defect is real even if its effect
is unmeasurable. I lean to pinning `pad="edge"` on correctness grounds while
stating plainly that it is not measurably better.

## 3. Downstream: how psi reaches a calibration

Two paths exist. Only one is usable for an A/B.

### The canonical path (NOT usable for A/B)

`est_suitability()` writes a FIXED filename,
`PATHS$MODEL_INPUT/pred_psi_suitability_day.csv` (`R/est_suitability.R:1380`),
and `data-raw/make_config_default.R:64` reads exactly that path to build
`config_default`. So the canonical route is: refit psi -> rebuild
`config_default.rda` -> calibrate. Two variants cannot coexist; you would have
to swap files between runs, and the config rebuild is heavyweight.

### The injection path (USE THIS)

`run_rolling_cv()` already swaps psi into a config per cutoff
(`R/run_rolling_cv.R:257-258`):

```r
cfg <- MOSAIC::get_location_config(iso = iso, config = base_config)
cfg$psi_jt <- .rolling_cv_psi_matrix(psi_csv, cfg$location_name, cfg_dates)
```

`.rolling_cv_psi_matrix()` (`R/run_rolling_cv.R:396-425`) is what makes this
work: it asserts the canonical `psi` column exists, builds an exact
(location x date) matrix in `location_name` ROW ORDER, and LOCF-fills gaps.
**This is why hand-injecting `psi_jt` produces all `-Inf` likelihoods and this
does not** — the naive version gets the dimensions, the row order or the NAs
wrong. The helper is internal (leading dot, so not matched by
`exportPattern("^[[:alpha:]]+")`).

**Required change:** export a public equivalent, e.g.
`make_psi_matrix(psi_csv, location_names, dates)`, and have
`.rolling_cv_psi_matrix()` call it. One implementation, two callers.

## 4. THE THREAT TO THE WHOLE COMPARISON — read before designing the run

The engine does not consume psi directly. It consumes
`calc_psi_star(psi, a, b, z, k)` — a **per-location four-parameter transform**,
logit-affine with gain `a`, offset `b`, EWMA `z` and lag `k`, all four
CALIBRATED (`sample_psi_star_a/b/z/k`, default TRUE,
`R/sample_parameters.R:183-186`).

ND's measured advantage over the LSTM is mostly **level**: bias 0.53 vs 0.49,
R2sse -5.3 vs -6.36. A free per-location offset `b` can absorb exactly that.
And `BACKLOG.md` DS-02 already records that calibration drives `psi_star_b` to
the prior floor, muting psi toward zero, while forcing near-identity plus a
~20 d lag gave the largest OOS win on record (WIS 7.4 -> 5.3).

**So a naive A/B is likely to return "no difference" regardless of which psi is
better**, because the transform re-fits around whatever it is given. That is a
null with no information in it, and it would be easy to mis-report as "ND does
not help downstream".

### The design that avoids this

Run the A/B at **three psi_star settings**, not one:

| arm | psi_star | what it tests |
|---|---|---|
| **free** | a, b, z, k all sampled (production) | what a user would actually get today |
| **identity** | a = 1, b = 0, z = 1, k = 0, all PINNED | whether a better psi helps when the engine is not allowed to re-fit around it |
| **DS-02** | near-identity + ~20 d lag, pinned | the setting that produced the largest OOS win on record |

Two psi variants x three psi_star settings = six calibration cells. If ND wins
under **identity** but ties under **free**, the finding is "the transform is
absorbing the improvement" — which is actionable and is a result about the
ENGINE, not about psi. If it ties under all three, ND's stage-1 advantage
genuinely does not propagate.

## 5. What to actually build

1. `architecture = "dlinear_v1"` resolving into `arch_control$trunk`, with a
   loud conflict error. ~20 lines in `est_suitability()` + roxygen + tests.
2. Export `make_psi_matrix()`; make `.rolling_cv_psi_matrix()` call it.
3. A harness script (`claude/psi_evolve/ab_calibration.R`) that, for each of
   {prod psi, ND psi} x {free, identity, DS-02}: builds the config via
   `get_location_config()`, injects psi with `make_psi_matrix()`, and calls
   `run_MOSAIC()`.
4. Pin `n_iter` per DS-03: the posterior saturates at n ~ 7-10k and exact IS ESS
   is 1.00 from n = 500 to 100,000, so run **10k, not 100k** — 10x cheaper at no
   measured loss.

## 6. Two things to settle before building

- **Baseline drift (DS-01).** The environment route changed on 2026-09-17 from a
  saturated step carrying 99.87% of infections to a live channel (saturation
  1.0000 -> 0.0018; env/human 1044x -> 1.73x). Any committed pre-2026-09-17
  score is not a valid baseline; both arms must be run fresh under the same
  engine version.
- **Which countries.** `per_country_weakness.csv` says production is weakest in
  SSD, COD, AGO, NGA, SOM, and that ND's per-country effect ranges from -49%
  (NGA) to +598% (CMR). A 3-5 country pilot should deliberately span that
  range rather than take the top burden countries, or the pilot will only
  measure the cases where the two psis agree.
