# Batch clean multiple JSON files

Applies clean_priors_json to multiple files, useful for cleaning up
calibration directories.

## Usage

``` r
batch_clean_json(pattern, path = ".", dry_run = FALSE)
```

## Arguments

- pattern:

  File pattern to match (e.g., "\*\*/priors.json")

- path:

  Base path to search in

- dry_run:

  Logical; if TRUE, only shows what would be cleaned (default FALSE)

## Value

Character vector of cleaned file paths (invisibly)

## Examples

``` r
if (FALSE) { # \dontrun{
# Dry run to see what would be cleaned
batch_clean_json("**/priors.json", "local/calibration", dry_run = TRUE)

# Actually clean all priors.json files
batch_clean_json("**/priors.json", "local/calibration")
} # }
```
