# Pull latest data from external scraper repos and report coverage

Fast-forward `git pull`s each of the four external scraper repos that
MOSAIC depends on for raw data, then prints a per-repo summary of the
latest commit and (for surveillance repos) the date range of the data
file the `process_*()` functions read. Designed to be called at the top
of `model/LAUNCH.R` so a refresh run starts from a known-current
snapshot.

## Usage

``` r
refresh_data_repos(root = NULL, repos = NULL, stale_days = 14L, verbose = TRUE)
```

## Arguments

- root:

  Path to the MOSAIC parent directory (containing all sibling repos).
  Defaults to `get_paths()$ROOT`, i.e. whatever
  [`set_root_directory()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/set_root_directory.md)
  was last called with.

- repos:

  Optional character vector restricting the refresh to a subset of
  repos. Valid names: `"ees-cholera-mapping"`, `"jhu_cholera_data"`,
  `"enso-data"`, `"open-meteo-pipeline"`. Default refreshes all four.

- stale_days:

  Threshold (days since last commit) for the `STALE` flag in the printed
  summary. Default 14.

- verbose:

  If `TRUE` (default), print a formatted summary to the console.

## Value

Invisibly, a list of per-repo results with fields `repo`, `description`,
`ok` (pull succeeded), `last_commit_date`, `last_commit_msg`,
`days_stale`, `coverage` (named list of coverage stats for surveillance
repos; `NULL` otherwise), and `pull_output`.

## Details

Uses `git pull --ff-only` so the call is non-destructive: a divergent
local branch will fail loudly rather than auto-merge. Uncommitted local
changes will also block the pull and surface in `pull_output`.

For `ees-cholera-mapping` and `jhu_cholera_data`, the function
additionally peeks at the canonical surveillance file (the one the
[`process_WHO_weekly_data()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/process_WHO_weekly_data.md)
/ `process_JHU_data()` functions read) and reports record count, date
range, and country count. ENSO and open-meteo coverage stats are not
extracted because their data is spread across many per-country /
per-source files; commit date is the practical freshness signal.

## Examples

``` r
if (FALSE) { # \dontrun{
set_root_directory("~/MOSAIC")
refresh_data_repos()

# Only the surveillance scrapers
refresh_data_repos(repos = c("ees-cholera-mapping", "jhu_cholera_data"))
} # }
```
