# Helper Function to Fit Beta Distribution with Variance Inflation for est_initial_R

Fits Beta distribution using CI expansion method with variance
inflation. Uses sqrt(variance_inflation) for CI expansion since variance
scales as the square of standard deviation. Falls back to method of
moments with direct variance scaling.

## Usage

``` r
fit_beta_with_variance_inflation_R(samples, variance_inflation = 0, label = "")
```

## Arguments

- samples:

  Numeric vector of proportions in (0,1)

- variance_inflation:

  Numeric inflation factor

- label:

  Character string for error messages

## Value

List with shape1 and shape2 parameters, or NULL if fitting fails
