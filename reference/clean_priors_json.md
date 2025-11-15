# Convert array-wrapped JSON to clean format

Reads a JSON file that may have array-wrapped scalar values and rewrites
it with proper formatting (auto_unbox = TRUE). This is useful for
cleaning up JSON files created without auto_unbox.

## Usage

``` r
clean_priors_json(input_path, output_path = input_path, backup = TRUE)
```

## Arguments

- input_path:

  Path to JSON file with potential array wrapping

- output_path:

  Path for cleaned JSON (defaults to overwrite input)

- backup:

  Logical; whether to create backup of original (default TRUE)

## Value

Invisibly returns the output path

## Examples

``` r
if (FALSE) { # \dontrun{
# Clean a priors file in place (creates .bak backup)
clean_priors_json("priors.json")

# Clean to a new file without backup
clean_priors_json("old_priors.json", "clean_priors.json", backup = FALSE)
} # }
```
