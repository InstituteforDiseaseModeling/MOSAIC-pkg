# World Bank indicators MOSAIC consumes

Named character vector: names are World Bank indicator codes, values are
the `MOSAIC-data/raw/world_bank/` subdirectory each is stored in. The
indicator code is embedded in every raw filename, which is how
`process_WB_*_data()` locates its input.

## Usage

``` r
MOSAIC_WB_INDICATORS
```
