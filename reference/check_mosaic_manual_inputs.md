# Report on the MOSAIC data sources that must be refreshed by hand

Checks each source with no automated route, reports its age, and prints
the refresh recipe for anything stale. Called automatically at the top
of
[`update_mosaic_data`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/update_mosaic_data.md);
run it standalone to answer "what do I need to download?" without
starting a pipeline.

## Usage

``` r
check_mosaic_manual_inputs(root = NULL, verbose = TRUE)
```

## Arguments

- root:

  MOSAIC parent directory. Defaults to `get_paths()$ROOT`.

- verbose:

  Print the report. Set `FALSE` for the data frame only.

## Value

Invisibly, a `data.frame`: `source`, `path`, `found`, `modified`,
`age_days`, `stale`, `instructions`.

## Details

Nothing here is fatal. A stale manual input degrades the outputs that
depend on it; it does not stop the pipeline.

## See also

[`update_mosaic_data`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/update_mosaic_data.md)

## Examples

``` r
if (FALSE)  check_mosaic_manual_inputs("~/MOSAIC")  # \dontrun{}
```
