# Logit Transformation

Applies the logit transformation to a probability value, mapping it from
the interval (0, 1) to the real line (-Inf, Inf).

## Usage

``` r
logit(p)
```

## Arguments

- p:

  A numeric vector of probabilities, where each value must be in the
  interval (0, 1).

## Value

A numeric vector where each value is the logit of the corresponding
input probability. The output will be in the range (-Inf, Inf).

## Details

The logit function is defined as: \$\$logit(p) = \log\left(\frac{p}{1 -
p}\right)\$\$ This is commonly used in logistic regression and other
models where probabilities need to be mapped to a real-valued scale.

## See also

[`inv_logit`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/inv_logit.md)
for the inverse logit (logistic) function.

## Examples

``` r
logit(0.5)   # Returns 0
#> [1] 0
logit(0.8)   # Positive value
#> [1] 1.386294
logit(0.2)   # Negative value
#> [1] -1.386294
```
