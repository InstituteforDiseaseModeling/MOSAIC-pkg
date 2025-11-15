# Get Pre-configured I/O Settings

Returns pre-configured I/O settings for common use cases. Choose from
debug, fast, default, or archive presets to optimize for your workflow.

## Usage

``` r
mosaic_io_presets(preset = c("default", "debug", "fast", "archive"))
```

## Arguments

- preset:

  Character. One of "default", "debug", "fast", or "archive"

## Value

Named list with I/O settings (format, compression, compression_level)

## Details

Presets:

- `debug`: CSV format, no compression (easy inspection)

- `fast`: Parquet with low compression (fastest)

- `default`: Parquet with medium compression (balanced)

- `archive`: Parquet with high compression (smallest files)
