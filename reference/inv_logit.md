# Inverse Logit (Logistic) Transformation

Applies the inverse logit (logistic) transformation to a real-valued
input, mapping it from the real line (-Inf, Inf) to the interval (0, 1).

## Usage

``` r
inv_logit(x)
```

## Arguments

- x:

  A numeric vector of real values.

## Value

A numeric vector where each value is the inverse logit of the
corresponding input, representing a probability in the range (0, 1).

## Details

The inverse logit function is defined as: \$\$inv\\logit(x) =
\frac{e^x}{1 + e^x}\$\$ This is often used to map real-valued
predictions (e.g., from logistic regression) back to probabilities.

## See also

[`logit`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/logit.md)
for the logit transformation function.

## Examples

``` r
inv_logit(0)     # Returns 0.5
#> [1] 0.5
inv_logit(1.39)  # Returns approximately 0.8
#> [1] 0.8005922
inv_logit(-1.39) # Returns approximately 0.2
#> [1] 0.1994078
```
