# Plot MOSAIC Country Map

This function creates a map of Africa highlighting countries included in
the MOSAIC framework. The map is saved as a PNG file in the directory
specified by `PATHS$DOCS_FIGURES`.

## Usage

``` r
plot_mosaic_country_map(PATHS)
```

## Arguments

- PATHS:

  A named list containing file paths required for output. It must
  include:

  **DOCS_FIGURES**

  :   Path to the directory where the map image will be saved.

## Value

Message

## Examples

``` r
if (FALSE) { # \dontrun{

  PATHS <- list(DOCS_FIGURES = "path/to/figures")
  # Generate and save the MOSAIC country map
  plot_mosaic_country_map(PATHS)

} # }
```
