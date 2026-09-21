# TFT — standalone psi model (arm NF1)

A Temporal Fusion Transformer (Lim et al. 2019, arXiv:1912.09363) in keras3-R,
built **independently** of the production psi model. No production code is
touched; comparability comes from emitting the same artifact the other arms do.

## Why TFT specifically

Phase 1 ended with two *structural* problems that tuning could not reach. TFT
addresses both by construction:

| phase-1 finding | TFT's answer |
|---|---|
| `lead = 0` — the production model is a **nowcaster**; its "12-week forecast" is a side-effect of being fed covariates that extend past the cutoff | multi-horizon by design: trained to predict H steps ahead, with **known-future covariates as a first-class input type** rather than an accident |
| intervals are bolted on (residual-based) | **native quantile head** trained with pinball loss |
| no idea which covariates matter, or whether they carry 12-week signal at all | **variable-selection networks** emit per-variable importances, separately for static / past / future |
| country adaptation only rescales a shared clock (xcorr 0.19-0.27 vs observed 0.014) | **static context gating** injects country metadata into the LSTM initial state and into every temporal step, not just as an output-side affine |

## Status

- `tft_model.R` — model + pinball loss. **Smoke-passed** on dugong: builds at
  L=52 / H=12, output `(B, 12, 5)`, 74,831 params, 2-epoch fit loss
  0.2447 -> 0.1520.
- `smoke_tft.R` — shape/forward/loss/fit test.
- **TODO**: data pipeline (panel -> static/country/past/future tensors),
  rolling-origin training per cutoff, prediction -> `psi_<cutoff>.csv`.

## Output contract (what makes it comparable)

Writes `psi_cache_NF1/psi_<cutoff>.csv` with at least:

```
iso_code, date, psi, q025, q25, q75, q975
```

`psi` = the 0.5 quantile. Every existing tool then works unchanged:
`accuracy_table.R NF1`, `shape_table.R`, `arm_C_transforms.R`,
`plot_psi_evolve.R NF1`.

## Design decisions, and the confound to state up front

- **L = 52, H = 12.** A year of context, forecasting 12 weeks. The production
  model uses 13 weeks of context and H is undefined (it is concurrent).
- **CONFOUND:** NF1 changes architecture AND context length AND the lead
  simultaneously. It is an *exploration*, not a clean single-change arm. The
  controls that disambiguate it already exist in the plan: **T2** (lead=12 on
  the production LSTM) and **W2** (timesteps=52 on the production LSTM). NF1
  should not be promoted on its own; if it wins, T2/W2 say why.
- **Known-future = all 38 covariates** in v1, which is faithful to what the
  production model already consumes. A v2 using the honest known-future /
  observed-past split (see `PLAN_ARCHITECTURES.md` section 2) would test whether
  the distinction matters.

## Risk

74,831 params against ~20k sequences is ~3.7 params per sequence. That is the
regime the DLinear critique targets, and why `ND` (DLinear) is queued as the
cheap control. TFT's gates, dropout and variable selection are its defence, but
overfitting is the thing to watch in the fold diagnostics.

---

# TFT variations — one build, several distinct questions

The marginal cost of most variations below is **a config flag**, not new code:
`build_tft()` already takes `n_static`, `n_past`, `n_future`, `lookback`,
`horizon` as arguments. Only `NF2`, `NF-R` and `NF-Reg` need pipeline work.

| arm | variation | question it answers | cost |
|---|---|---|---|
| **NF1** | baseline: L=52, H=12, all covariates known-future, statics on, global pool | does TFT help at all? | **built** |
| **NF4** | **H = 1 vs H = 12** | does joint-trajectory training fix `dir_acc`? | flag |
| **NF2** | honest known-future / observed-past split | is our measured skill an artifact of future covariates? | needs the split |
| **NF-R** | target = `observed - persistence` | learn the correction rather than the level | pipeline variant |
| **NF3** | statics off | is static context gating what helps? | flag |
| **NF-Reg** | per-region models instead of one global fit | attacks xcorr 0.19-0.27 vs observed 0.014 | pipeline variant |

Core sequence: **NF1 -> NF4 -> NF2**, with `NF-R` as the most promising of the
"different application" ideas.

## NF4 — the highest-value variation, and why it is not the same as T2

This distinction was under-sold when the plan was first written and is the
strongest single argument for TFT over a lead-fix on the existing LSTM:

- **T2** (`lead = 12`) trains `X_t -> y_{t+12}` — **one point**, twelve weeks out.
- **TFT at H = 12** trains `X_t -> (y_{t+1}, ..., y_{t+12})` — the **whole
  trajectory, jointly**.

Our failure mode is *shape over the horizon* (`dir_acc` 0.48, `dcor` 0.02). A
loss that scores the entire 12-week path is the only thing in any proposal that
**directly penalises getting the trajectory's shape wrong**. H=1 vs H=12 with
everything else fixed isolates whether joint-trajectory training is the fix —
and it is a one-argument change.

## NF2 — a validity check, not a performance test

Production psi consumes covariates that extend past the cutoff. In real
deployment some of those are **forecasts, not observations**. If TFT's skill
collapses when only genuinely-known-at-origin covariates are routed as
known-future, then every number in this programme is optimistic — **including
the sealed-holdout +10.8%**.

That makes NF2 worth running *whatever* NF1 shows. It is the one variation not
gated on TFT being good.

## NF-R — learn the correction, not the level

Phase 1 established that the blend takes its **level** from persistence and uses
psi only for **shape**, and that psi's own level is badly wrong (bias 0.46).
Training TFT on `observed - persistence` removes the level problem from the
learning task entirely and asks the model for precisely the quantity the blend
is currently hand-constructing.

## Gating — scope discipline

Six TFT arms on top of T2/N9/F5/F6 and ND/NT is a lot of surface, and phase 1's
lesson is that most single changes land inside the noise floor. So:

- **If NF1 does not beat the production LSTM at all**, drop NF3 and NF-Reg.
- **NF2 still runs regardless** — it is a validity check on results already
  reported, not a candidate for promotion.
- NF4 runs with NF1 (same pipeline, one differing argument), so it is nearly
  free and should not be deferred.
