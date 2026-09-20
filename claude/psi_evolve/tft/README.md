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
