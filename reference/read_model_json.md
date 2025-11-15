# Read priors or posteriors from JSON with consistent parsing

This function provides standardized reading of JSON files, with options
for simplification.

## Usage

``` r
read_model_json(path, simplify = FALSE, validate = TRUE)
```

## Arguments

- path:

  Input file path

- simplify:

  Whether to simplify to vectors (default FALSE for consistency)

- validate:

  Logical; whether to validate structure after reading (default TRUE)

## Value

List object containing the parsed JSON

## Examples

``` r
if (FALSE) { # \dontrun{
# Read priors
priors <- read_model_json("priors.json")

# Read with simplification (not recommended for priors/posteriors)
data <- read_model_json("data.json", simplify = TRUE)
} # }
```
