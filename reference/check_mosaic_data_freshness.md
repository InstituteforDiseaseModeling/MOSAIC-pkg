# Report the freshness of every MOSAIC data input and output

Read-only audit across all three layers of the data pipeline: the
external scraper repos, the hand-maintained raw inputs, and the
processed artifacts the pipeline produces. Answers "what is stale?" in
one call, and touches nothing.

## Usage

``` r
check_mosaic_data_freshness(
  root = NULL,
  stale_days = 30L,
  repo_stale_days = 14L,
  verbose = TRUE
)
```

## Arguments

- root:

  MOSAIC parent directory. Defaults to `get_paths()$ROOT`.

- stale_days:

  Age in days beyond which a processed artifact is flagged. Default 30.

- repo_stale_days:

  Age beyond which a source repo is flagged. Default 14, matching
  [`refresh_data_repos`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/refresh_data_repos.md).

- verbose:

  Print the report.

## Value

Invisibly, a list with elements `repos`, `manual`, `outputs` and
`summary`, each a `data.frame`.

## Details

The third layer is the one nothing else covers.
[`refresh_data_repos()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/refresh_data_repos.md)
reports source-repo staleness (and *pulls* as a side effect);
[`check_mosaic_manual_inputs`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/check_mosaic_manual_inputs.md)
covers the manual raw inputs. Neither looks at what the pipeline
*produced*, which is where vintage drift actually accumulates – a source
can be current while the artifact derived from it is two years old.

## Read-only

Unlike
[`refresh_data_repos`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/refresh_data_repos.md),
this performs no `git pull` – it reads `git log` only. Safe to run on a
schedule or before deciding whether a refresh is warranted.

## What "vintage spread" means

The report counts how many distinct year-months the files in a directory
were written in. A healthy directory was produced by one build and shows
1-2. A large spread means the directory is sediment from many partial
runs and no single coherent build produced it – the files are
individually valid but mutually inconsistent in what data they saw.

## See also

[`update_mosaic_data`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/update_mosaic_data.md),
[`check_mosaic_manual_inputs`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/check_mosaic_manual_inputs.md),
[`refresh_data_repos`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/refresh_data_repos.md)

## Examples

``` r
if (FALSE)  check_mosaic_data_freshness("~/MOSAIC")  # \dontrun{}
```
