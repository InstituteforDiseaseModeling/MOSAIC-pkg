# Get Unicode Hex Key for a Greek Letter

This function returns the Unicode hex key for a specified Greek letter
name. The user can choose whether the returned letter should be in
uppercase or lowercase.

## Usage

``` r
get_greek_unicode(letter_name, uppercase = TRUE)
```

## Arguments

- letter_name:

  A character string representing the name of the Greek letter. Accepted
  values are the English names of the Greek letters (e.g., "alpha",
  "beta", "gamma").

- uppercase:

  A logical argument indicating whether to return the uppercase version
  of the Greek letter (default is `TRUE`).

## Value

A character string representing the Unicode hex key for the specified
Greek letter. This can be used in plots, titles, or text where Greek
symbols are needed.

## Details

This function allows users to obtain the Unicode hex representation of
any common Greek letter by its name. The function supports both
uppercase and lowercase Greek letters. The returned hex key can be
inserted directly into strings or titles, for example, in `ggplot2`
plots.

## See also

`ggplot2` for using the Unicode symbols in plot titles.

## Examples

``` r
if (FALSE) { # \dontrun{
# Get the Unicode for uppercase Alpha
get_greek_unicode("alpha", TRUE)

# Get the Unicode for lowercase Omega
get_greek_unicode("omega", FALSE)

# Get the Unicode for uppercase Sigma
get_greek_unicode("sigma", TRUE)

# Use in ggplot2 title
ggplot() + labs(title = paste("Example with Greek letter", get_greek_unicode("Psi", TRUE)))
} # }
```
