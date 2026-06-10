# Estimate Environmental Suitability (psi) for Cholera Transmission

This function loads climate data, ENSO data, and weekly cholera cases
data to estimate the environmental suitability for cholera transmission
based on various environmental factors. It scales the climate covariates
and then fits an LSTM-based RNN model to predict cholera outbreaks. The
model is trained using past cholera case data and climate conditions,
and predictions are made for environmental suitability (psi) based on
the climate covariates.

## Usage

``` r
est_suitability(
  PATHS,
  fit_date_start = NULL,
  fit_date_stop = NULL,
  pred_date_start = NULL,
  pred_date_stop = NULL,
  feature_set = "v7.3",
  response_var = "transmission_intensity",
  bias_correct = TRUE,
  architecture = c("lstm_v2_hierarchical_film", "lstm_v1_legacy"),
  arch_control = NULL,
  plot_country_diagnostics = FALSE,
  ...
)
```

## Arguments

- PATHS:

  A list containing paths where the data is stored. Typically generated
  by the
  [`get_paths()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/get_paths.md)
  function and should include:

  - **DATA_CHOLERA_WEEKLY**: Path to the directory containing processed
    combined weekly cholera cases data (WHO+JHU+SUPP sources).

  - **DATA_ELEVATION**: Path to the directory where elevation data is
    stored.

  - **MODEL_INPUT**: Path to save the processed model inputs and
    outputs.

  - **DOCS_FIGURES**: Path to save the generated plots.

- fit_date_start:

  Date string or NULL. Start date for model fitting period. If NULL,
  auto-detects from first cholera case data.

- fit_date_stop:

  Date string or NULL. End date for model fitting period. If NULL,
  auto-detects from last date with both cholera cases and complete ENSO
  data.

- pred_date_start:

  Date string or NULL. Start date for prediction period. If NULL, uses
  fit_date_start.

- pred_date_stop:

  Date string or NULL. End date for prediction period. If NULL,
  auto-detects from last date with complete ENSO data.

- feature_set:

  Named covariate set, shared by both architectures: `"v7.3"` (default;
  the 38 screening-informed features, see
  [`MINFEAT_V7_3_FEATURE_SET`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/MINFEAT_V7_3_FEATURE_SET.md))
  or `"default"` (full production candidate set). This is the SOLE
  public, schema-guarded feature selector. Errors if a `"v7.3"` feature
  is absent from the suitability CSV (schema-drift guard).

- response_var:

  Training target column, shared by both architectures (default
  `"transmission_intensity"`, the v0.34 default). For the lstm_v2 path
  this maps internally to the sandbox `"intensity"` recipe
  (`log1p(cases)/log1p(cases_99th)` with a TRAIN-ONLY `cases_99th`
  anchor). Alternatively name a pre-computed `[0,1]` `target_*` column
  present in the suitability CSV (e.g. `"target_C_rate_global"`).

- bias_correct:

  Logical (default `TRUE`), shared by both architectures. Apply a
  post-hoc per-country affine bias correction
  ([`calibrate_psi_predictions`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calibrate_psi_predictions.md)),
  fit on the training window and applied to all predictions; populates
  the diagnostic `pred_bias_corrected` column and feeds the canonical
  `psi` column. (Renamed from the v0.33 `calibrate`; the old name is
  accepted via `...` with a deprecation message.) This is NOT the
  [`run_MOSAIC()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/run_MOSAIC.md)
  Bayesian calibration.

- architecture:

  One of `"lstm_v2_hierarchical_film"` (default, v0.34) or
  `"lstm_v1_legacy"` (frozen v0.33 path). Validated with `match.arg`.
  See the **Architectures** section.

- arch_control:

  `NULL` (default) or a named list of lstm_v2 hyperparameter overrides.
  `NULL` loads the checked-in fixture
  `inst/fixtures/B4_rolling_cv_spec.yml` and applies the two production
  defaults (`n_seeds = 5`, `region_map = "snf_k5"`). A supplied list is
  merged via `modifyList` (overrides only the keys provided). Key
  fields: `n_seeds` (ensemble size; production 5, fixture 3),
  `region_map` (one of `"snf_k5"` (default), `"csv"`, `"seasonal_v1"`,
  `"hydro_v1"`, `"snf_k4"`), `use_confidence_weight`, `timesteps`,
  `partial_pool_lambda`, `exclude_covariates` (lstm_v2 ablation
  override), and the execution knob `parallel_seeds` (integer, default
  `1L` = serial; not a model parameter). Ignored by the legacy path.

- plot_country_diagnostics:

  Logical (default `FALSE`). Per-country base-R diagnostic plots during
  the smoothing loop (legacy path only).

- ...:

  Absorbs deprecated v0.33 arguments for backward compatibility:
  `calibrate` (mapped to `bias_correct`), and the frozen legacy knobs
  `n_splits`, `seed_base`, `fine_tune_epochs`, `fine_tune_lr`,
  `split_method`, `train_prop`, `exclude_covariates` (ignored with a
  deprecation message).

## Value

This function processes climate and cholera case data, fits an LSTM
model, makes predictions on environmental suitability (psi), and saves
both the predictions and covariate data. It also generates a plot
showing the model fit (MAE and loss over training epochs) and saves it
to a specified directory.

## Details

Common to all architectures, the function:

- Loads processed climate and cholera data.

- Determines appropriate date ranges for fitting and prediction based on
  data availability.

- Validates data completeness within the specified date ranges.

- Scales the climate covariates using training-data statistics.

- Builds temporal LSTM sequences respecting country boundaries and
  temporal gaps.

- Trains an LSTM model, predicts environmental suitability (psi) over
  the full prediction period, and writes the predictions and covariate
  data.

The training/validation regime differs by architecture: the default
`lstm_v2_hierarchical_film` uses expanding-window rolling-origin
cross-validation (no random split) with a hierarchical-FiLM trunk, while
the frozen `lstm_v1_legacy` path uses a random 60/40 split with
sequential fine-tuning. See the **Architectures** section above for the
full description of each path.

## Note

The LSTM model uses climate variables to predict cholera suitability.
The model's predictions are saved as a CSV file and a plot showing the
model fit (MAE and loss) is generated.

## Architectures

**`lstm_v2_hierarchical_film` (default, v0.34) – hierarchical-FiLM LSTM,
rolling-origin CV.** A 3-stack LSTM trunk (128-\>64-\>32 units,
recurrent dropout) maps the weekly covariate window (`timesteps=13`) to
a shared climate-response latent `z`. Country/region identity modulates
`z` via hierarchical FiLM: a region embedding produces
`(gamma_r, beta_r)` and a zero-initialized country *deviation* embedding
produces `(gamma_c, beta_c)`, applied as
`z_r = (1+gamma_r) * z + beta_r` then
`z_c = (1+gamma_c) * z_r + beta_c`. The `gamma` use `tanh` (gain in
`[0,2]`, identity at init); an L2 partial-pool penalty shrinks
data-sparse countries toward their region. A sigmoid head outputs psi in
`[0,1]`. Trained with BCE loss under `balanced_uniform` sample weights
(correcting the zero-week imbalance – ~72% of all-MOSAIC training weeks
are zero, higher per high-incidence country) plus an optional per-row
confidence-weight overlay. Training uses expanding-window rolling-origin
CV (each step validates strictly forward in time with a 4-week embargo,
early-stopping records `best_epoch`); the model is then refit on the
full in-sample data at `median(best_epoch)`. `n_seeds` fits are averaged
on the logit scale to yield the prediction (canonical `psi` column),
plus seed-dispersion quantiles (diagnostic only, NOT predictive
intervals). This regime fixes the v0.33 random-split temporal leak that
collapsed out-of-sample forecasts. Defaults are pinned to the B4
fixture; override via `arch_control`.

*Honest framing.* lstm_v2 converts a structurally-collapsed flat psi
(Pearson ~0.09, ~3\\ directional signal (median cross-country Pearson
~0.22, but a wide per-cell range with a large anti-correlated minority –
psi can be worse than climatology for some countries). It strictly
dominates the incumbent on shape; it is NOT a decision-grade forecaster.
psi reaches the downstream LASER engine through TWO channels – a
fractional deviation `(psi - psi_bar)/psi_bar` (timing/shape) and the
absolute level (environmental-reservoir decay) – so the response anchor
and `bias_correct` can move outbreak magnitude, not just timing. Report
the per-country spread, not just the median. The v0.34.0 defaults
(`response_var="transmission_intensity"`, `region_map="snf_k5"`,
`bias_correct=TRUE`) are **provisional**, gated by a post-merge
psi-\>LASER case-skill experiment; `snf_k5` ships unvalidated as a FiLM
region map and reverts to `"csv"` if it does not beat it.

**`lstm_v1_legacy` (v0.33, frozen) – shared LSTM, random split +
sequential fine-tuning.** A single 3-stack LSTM (128-\>64-\>32) with no
country/region identity (one shared model for all countries). Trained on
a random 60/40 split across all weeks with MSE on the logit-transformed
target, followed by sequential fine-tuning. Frozen at the canonical
v0.33 settings (not tunable; legacy knobs are hard-coded internally).
**Retained only for reproducibility/rollback.** Known limitation: the
random split places temporally-adjacent weeks on both sides of the
train/val boundary, so the out-of-sample forecast collapses in amplitude
– the failure lstm_v2 fixes.

## Migration (reproduce v0.33 production behavior)

The v0.34 defaults flip `response_var` to `"transmission_intensity"` and
`architecture` to `"lstm_v2_hierarchical_film"`. To run the frozen v0.33
shared-LSTM path:


    est_suitability(PATHS, architecture = "lstm_v1_legacy",
                    feature_set = "v7.3", response_var = "target_C_rate_global",
                    bias_correct = TRUE)

## See also

[`layer_lstm`](https://keras3.posit.co/reference/layer_lstm.html),
[`fit.keras.src.models.model.Model`](https://keras3.posit.co/reference/fit.keras.src.models.model.Model.html),
[`ggplot`](https://ggplot2.tidyverse.org/reference/ggplot.html)

## Examples

``` r
if (FALSE) { # \dontrun{
# Set up paths
PATHS <- get_paths()

# Basic usage with default settings (lstm_v2 hierarchical-FiLM, rolling-CV)
est_suitability(PATHS)

# Custom date ranges for fitting and prediction
est_suitability(PATHS,
               fit_date_start = "2015-01-01",
               fit_date_stop = "2023-12-31",
               pred_date_start = "2020-01-01",
               pred_date_stop = "2025-12-31")

# Reproduce B4 exactly (revert the two production overrides)
est_suitability(PATHS, arch_control = list(n_seeds = 3L, region_map = "csv"))

# Larger seed ensemble; alternate region map
est_suitability(PATHS, arch_control = list(n_seeds = 7L, region_map = "snf_k4"))

# Frozen v0.33 shared-LSTM path (reproducibility / rollback)
est_suitability(PATHS, architecture = "lstm_v1_legacy")
} # }
```
