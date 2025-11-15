# Helper Function to Safely Fit Beta Distribution

Fits a Beta distribution to data using method of moments estimation,
with error handling for edge cases.

## Usage

``` r
fit_beta_safe(x, label = "")
```

## Arguments

- x:

  Numeric vector of proportions in (0,1)

- label:

  Character string for error messages

## Value

List with shape1 and shape2 parameters, or NULL if fitting fails
