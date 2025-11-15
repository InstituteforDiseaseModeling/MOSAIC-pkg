# Check Affine Normalization (Zero-Centered Min-Max Scaling)

Verifies that a numeric vector has been affine normalized such that it
is zero-centered and its minimum value is at least `-1`. Specifically,
the function checks that:

## Usage

``` r
check_affine_normalization(x, verbose = FALSE)
```

## Arguments

- x:

  A numeric vector that has been affine normalized. This vector may
  contain `NA` values.

- verbose:

  Logical; if `TRUE`, the function prints details of the check to the
  console.

## Value

Invisibly returns `NULL` if the check passes. Otherwise, the function
stops with an error message detailing the failed condition(s).

## Details

- The mean of the vector (ignoring `NA`s) is approximately zero.

- The minimum value (ignoring `NA`s) is greater than or equal to `-1`.

- The maximum value (ignoring `NA`s) is above zero.

If any of these conditions fail, the function stops execution and
returns an informative error message.

The check uses a tolerance of `1e-8` to account for floating point
precision. The conditions verified correspond to a transformation
defined as:

\$\$x\_{\text{scaled}} = \frac{x - \mu}{\mu - \min(x)}\$\$

where \\\mu\\ is the mean of `x`. Note that while the transformation
forces a zero-centered result, the minimum value need not be exactly
`-1` but should be no less than `-1`.

## Examples

``` r
# Example vector after affine normalization
x <- c(-1, 0, 0.5, 1.2, NA)

# Check without printing details:
if (FALSE) { # \dontrun{
  check_affine_normalization(x)
} # }

# Check with verbose output:
if (FALSE) { # \dontrun{
  check_affine_normalization(x, verbose = TRUE)
} # }
```
