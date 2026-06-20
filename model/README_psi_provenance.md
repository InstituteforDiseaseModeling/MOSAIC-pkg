# Provenance: AI-enhanced suitability training data (config_default psi)

The production default environmental-suitability model carried in `config_default`
(the `psi` field) is fit on the **AI-enhanced** combined surveillance dataset, not
the AI-free WHO/JHU/SUPP-only set. This note records the exact recipe the canonical
`LAUNCH.R` pipeline uses to reproduce that training data.

## Why this exists

Previously, `LAUNCH.R` called `process_cholera_surveillance_data(PATHS)` **without**
`include_ai = TRUE`, and `process_AI_cholera_data()` was never invoked. The shipped
default `psi` was actually fit using an ad-hoc, gitignored sandbox script
(`claude/build_ai_suitability_data.R`) that set `include_ai = TRUE`. Re-running the
canonical pipeline therefore produced a **different (AI-free)** suitability training
set than the one the production `psi` was fit on — a reproducibility gap.

`LAUNCH.R` now wires the AI path into the canonical build (step 2A).

## Reproduction recipe

The canonical pipeline regenerates the AI-enhanced suitability training set as part
of step 2A + 4A. To regenerate it standalone:

```r
library(MOSAIC)
set_root_directory("~/MOSAIC")   # parent dir holding all repos
PATHS <- get_paths()

# 1. Build the AI-mined processed weekly file from the ai-cholera-data-mining repo.
#    Reads:  PATHS$AI_CHOLERA_REPO/data/<ISO>/cholera_weekly_<ISO>.csv (40 countries)
#    Writes: PATHS$DATA_AI_WEEKLY/cholera_country_weekly_processed.csv
process_AI_cholera_data(PATHS)

# 2. Merge AI into the combined WEEKLY surveillance file (WHO > JHU > AI > SUPP).
#    include_ai = TRUE propagates confidence_weight + disaggregation_method.
process_cholera_surveillance_data(PATHS, include_ai = TRUE)

# 3. Compile the LSTM suitability training matrix from the combined weekly file.
compile_suitability_data(PATHS,
                         cutoff = NULL, use_epidemic_peaks = TRUE,
                         date_start = "2000-01-01", date_stop = NULL,
                         forecast_mode = TRUE, forecast_horizon = 9,
                         include_lags = TRUE)

# 4. Fit the suitability model. use_confidence_weight = TRUE (B4 arch_control
#    fixture default) down-weights AI rows in the LSTM loss.
est_suitability(PATHS,
                response_var   = "target_D_rate_per_country_floored",
                architecture   = "lstm_v2_hierarchical_film",
                arch_control   = list(n_seeds = 10L, region_map = "snf_k5",
                                      parallel_seeds = 1L, rw_subsample = 5L),
                bias_correct   = TRUE,
                fit_date_start = "2010-01-01")   # the production "G" config
```

## Field semantics (data-engineer notes)

- **Scope:** `include_ai = TRUE` propagates AI rows + their `confidence_weight` /
  `disaggregation_method` into BOTH the combined **WEEKLY** file
  (`cholera_surveillance_weekly_combined.csv`, consumed by the suitability/LSTM
  path) and — as of the multi-source integration (v0.45.3) — the **daily** combined
  file (the calibration **fit target**) via a TRUST-TIER GATE: AI `observed` and
  `documented_zero` weeks DO reach `reported_cases`/`reported_deaths` (a confirmed
  absence is an informative zero), while `fourier_*` (synthetic) and `assumed_zero`
  weeks are NA-blanked so neither modeled fill nor assumed zeros enter the
  likelihood. (Earlier behavior blanket-NA-blanked every AI row from the daily file.)
- **`confidence_weight`** (unit: trust weight in (0,1]): AI rows ~ median 0.5;
  direct WHO/JHU/SUPP rows = 1.0. Per-week constant; replicates (does not divide)
  across days. Consumed in TWO places: (1) the LSTM multiplies its sample loss
  weight by this when `use_confidence_weight = TRUE`; (2) as of v0.45.3 the
  calibration likelihood consumes it too — `make_config_default` carries it as the
  per-cell `reported_cases_weight` / `reported_deaths_weight` matrices, which
  `run_MOSAIC` (run_MOSAIC.R:429-430) passes to
  `calc_model_likelihood(weights_obs_cases=, weights_obs_deaths=)` and from which it
  derives a per-location `weights_location`.
- **`disaggregation_method`** is the trust-tier discriminator (NOT
  `confidence_weight`, which overlaps across tiers): `observed` = real mined
  weeklies; `documented_zero` = confirmed absence; `fourier_*` = SYNTHETIC
  seasonal reconstructions of annual/quarterly totals (no climate input, so no
  climate->cholera circularity). `assumed_zero` rows are dropped by the
  `process_AI_cholera_data` keep-filter.

## Default vs. flag recommendation

**Recommendation: keep the AI path as the DEFAULT canonical build** (not gated
behind a flag), because:

1. The shipped `config_default` `psi` IS the AI-enhanced fit. Making the canonical
   pipeline reproduce the shipped artifact by default is the whole point of the
   fix; a flag defaulting to OFF would re-open the reproducibility gap.
2. The AI source is fully version-tracked: 40 `cholera_weekly_<ISO>.csv` files in
   the read-only `ai-cholera-data-mining` repo, regenerated deterministically by
   `process_AI_cholera_data()`.
3. The AI-downweighting mitigation (`use_confidence_weight = TRUE`) is on the
   production path; only AI `observed`/`documented_zero` weeks reach the fit target
   (trust-tier gate), synthetic `fourier_*` are NA-blanked, and the per-cell
   `confidence_weight` further down-weights the AI rows that do enter the likelihood.

If a future need arises to A/B the AI-free `psi`, do it via the existing
`include_ai` argument (set `FALSE`) rather than a new flag — that argument already
exists and is the documented switch.
