# Named LSTM covariate feature sets for est_suitability()

Curated covariate sets that
[`est_suitability`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/est_suitability.md)
can select via its `feature_set` argument. `"v7.3"` is the
screening-informed 38-feature set (cross-country sign-consistency; see
provenance below); `"default"` is the full production candidate set (no
restriction).

## Usage

``` r
MINFEAT_V7_3_FEATURE_SET
```

## Format

Character vector of covariate column names.

## Details

Provenance: `MOSAIC-Mozambique/claude/minfeat_v7_iterations.md` and the
multi-discipline covariate-screening review. The v7.3 set was selected
by the est_suitability ablation sweep (9 variations x 6 cutoffs x 22
countries).

## See also

[`get_feature_set`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/get_feature_set.md),
[`est_suitability`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/est_suitability.md)
