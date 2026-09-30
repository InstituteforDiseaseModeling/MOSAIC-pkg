# Run the cholera transmission model

Simulates the metapopulation SEIR model over the configured window and
returns the result channels. This is the package's only engine entry
point: there is deliberately no `engine =` switch, because there is only
one engine. Prior to v0.68.0 this function was a reticulate bridge to
the Python `laser-cholera` package; it is now pure R.

## Usage

``` r
run_simulation(
  config,
  seed = NULL,
  quiet = FALSE,
  components = SIM_PIPELINE,
  rng = c("rng", "replay"),
  record = NULL,
  ...
)
```

## Arguments

- config:

  Config list, or a path to a `.json` / `.json.gz` file. See
  [`sim_params()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/sim_params.md)
  for the normalisation and validation applied.

- seed:

  Integer seed. If `NULL`, uses `config$seed` when present, otherwise
  `123L`. The engine draws from an isolated RNG stream and restores the
  caller's on exit, so calling it never perturbs the caller's
  `.Random.seed`.

- quiet:

  Logical; suppress progress reporting. Accepted for call compatibility
  – the R engine reports nothing either way.

- components:

  Character vector naming the pipeline subset to run. Defaults to the
  full dynamics pipeline. Used by the parity harness to compare a
  component at a time against the oracle; production callers should not
  set it.

- rng:

  Either `"rng"` (draw normally) or `"replay"` (consume `record` and
  assert every draw matches). Replay is a test mode.

- record:

  Replay fixture from
  [`sim_read_fixture()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/sim_fixture.md).

- ...:

  Reserved. Supplying an argument removed with the Python engine
  (`py_module`, `visualize`, `pdf`, `outdir`) raises an error naming it
  rather than silently ignoring it. See
  [removed_api](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/removed_api.md).

## Value

A list with `params` (the config as supplied; a file path is parsed),
`results` (the 28 result channels as `[patch, time]` matrices, except
`pi_ij` and `coupling` which are `[patch, patch]`) and `seed`.

## Relationship to laser-cholera

Only `rng = "replay"` (the parity test mode) reproduces `laser-cholera`
0.16.1, to the tolerances recorded in
`tests/testthat/fixtures/ORACLE.md`. The default `"rng"` mode, which
every production run uses, deliberately follows the model specification
where the Python engine departed from it, so its output is not certified
against `laser-cholera` and is not expected to match it:

- the symptomatic split of new infections, and of `I_j_initial` at t =
  0, is a binomial draw rather than `round(sigma * n)` (v0.89.0);

- the environmental dose-response uses the per-capita dose \\W/N\\
  rather than the absolute reservoir \\W\\ (v0.89.0);

- each symptomatic onset's fate is drawn at onset from the reported CFR
  `mu_jt`, and deaths are reported on the case lag
  (`delta_reporting_deaths` is not used) (v0.96.0).

The internal `.SIM_RNG_ONLY_CORRECTIONS` registry lists these.

## Examples

``` r
if (FALSE) { # \dontrun{
# Run from a config file:
result <- run_simulation(config = "path/to/sim_params.json", seed = 20250418L)

# Run from a config object (uses config$seed if present, else 123L):
result <- run_simulation(config = config_default, quiet = TRUE)

dim(result$results$reported_cases)   # [locations, days]
} # }
```
