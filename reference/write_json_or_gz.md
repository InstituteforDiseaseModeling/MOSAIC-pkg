# Write a list to .json, .json.gz, or both – byte-equal by construction

Serialises `data_list` to JSON text once and writes that single
in-memory string to either the plain `.json` file, the gzipped
`.json.gz` file, or both – as selected by the `write_json` and
`write_gz` arguments. Because both outputs are written from the same
byte string, the `.json.gz` always decompresses to a file byte-
identical to the `.json` (when both are produced); the pair cannot
drift.

Replaces the older `write_json_with_optional_gz()` "sidecar" model. Both
flags are first-class peers: any combination except
`write_json = FALSE, write_gz = FALSE` is allowed.

## Usage

``` r
write_json_or_gz(data_list, file_path, write_json = TRUE, write_gz = FALSE)
```

## Arguments

- data_list:

  Named list to serialise.

- file_path:

  Base output path. Must end in `.json`. The `.gz` output (if
  `write_gz = TRUE`) is written to `paste0(file_path, ".gz")`.

- write_json:

  Logical. If `TRUE` (default), write the plain `.json` file.

- write_gz:

  Logical. If `TRUE`, write the gzipped `.json.gz` file. Default
  `FALSE`.

## Value

The base `file_path` (invisibly).

## Details

Both files come from a single
[`jsonlite::toJSON()`](https://jeroen.r-universe.dev/jsonlite/reference/fromJSON.html)
call. There is no parallel serialisation, no patch-and-rewrite loop, and
no possibility for the `.json` and `.json.gz` to diverge.

At least one of `write_json` / `write_gz` must be `TRUE`; passing both
as `FALSE` errors immediately (the function would otherwise produce
nothing).

## See also

[`write_list_to_json()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/write_list_to_json.md)

## Examples

``` r
if (FALSE) { # \dontrun{
  # Plain JSON only (default)
  write_json_or_gz(my_config, "config.json")

  # gzip only
  write_json_or_gz(my_config, "config.json", write_json = FALSE, write_gz = TRUE)

  # Both, guaranteed byte-equal
  write_json_or_gz(my_config, "config.json", write_json = TRUE, write_gz = TRUE)
} # }
```
