# Validate priors JSON structure

Checks that a priors object or file has the correct structure and warns
about potential issues like array wrapping.

## Usage

``` r
validate_priors_json(priors_object, verbose = TRUE)
```

## Arguments

- priors_object:

  Priors object (list) or file path (character)

- verbose:

  Logical; whether to print validation messages (default TRUE)

## Value

TRUE if valid (invisibly), otherwise stops with error

## Examples

``` r
if (FALSE) { # \dontrun{
# Validate a priors file
validate_priors_json("priors.json")

# Validate a priors object
validate_priors_json(priors_default)
} # }
```
