# Affine Normalization of a Numeric Vector (Zero-Centered Min-Max Scaling)

Transforms a numeric vector by centering it at zero and scaling it so
that the minimum value becomes `-1`. Specifically, the affine
normalization is defined as:

## Usage

``` r
calc_affine_normalization(x)
```

## Arguments

- x:

  A numeric vector to be normalized. This vector may contain `NA`
  values.

## Value

A numeric vector of the same length as `x` that has been affine
normalized.

## Details

\$\$x\_{\text{scaled}} = \frac{x - \mu}{\mu - \min(x)}\$\$

where \\\mu\\ is the mean of `x`.

The transformation produces a vector with a mean that is approximately
zero (ignoring `NA` values), a minimum of `-1`, and a maximum above zero
if the original maximum exceeds the mean.

The function computes the mean and minimum of `x` with `na.rm = TRUE`.
If the mean equals the minimum (or is nearly equal within machine
precision), the function issues a warning and returns a vector of `NA`
values to avoid division by zero.

## Examples

``` r
# Example vector with NA values
x <- c(3, 5, 7, NA, 10, 12)

# Apply affine normalization
normalized_x <- calc_affine_normalization(x)

# The normalized vector should have a mean of approximately zero,
# a minimum of -1, and a maximum above zero (if the original maximum exceeds the mean).
```
