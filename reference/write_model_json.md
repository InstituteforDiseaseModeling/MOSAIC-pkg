# Standardized JSON I/O utilities for MOSAIC model files

These functions ensure consistent JSON formatting across the codebase,
particularly for priors and posteriors files. Write priors or posteriors
to JSON with consistent formatting

## Usage

``` r
write_model_json(
  object,
  path,
  type = c("priors", "posteriors"),
  validate = TRUE
)
```

## Arguments

- object:

  List object containing priors or posteriors

- path:

  Output file path

- type:

  Either "priors" or "posteriors" for validation

- validate:

  Logical; whether to validate structure before writing (default TRUE)

## Value

Invisibly returns the output path

## Details

This function standardizes JSON output across the MOSAIC package,
ensuring consistent formatting with auto_unbox = TRUE to avoid array
wrapping of scalar values.

## Examples

``` r
if (FALSE) { # \dontrun{
# Write priors with standardized formatting
write_model_json(priors_default, "priors.json", type = "priors")

# Write posteriors
write_model_json(posteriors, "posteriors.json", type = "posteriors")
} # }
```
