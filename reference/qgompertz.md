# Gompertz Distribution Quantile Function

Compute the quantile function (inverse CDF) of a Gompertz distribution.

## Usage

``` r
qgompertz(p, b, eta, lower.tail = TRUE, log.p = FALSE)
```

## Arguments

- p:

  Numeric vector. Probabilities in \[0,1\].

- b:

  Numeric. Shape parameter (b \> 0).

- eta:

  Numeric. Rate parameter (eta \> 0).

- lower.tail:

  Logical. If TRUE, probabilities are P(X \<= x).

- log.p:

  Logical. If TRUE, probabilities are given as log(p).

## Value

Numeric vector of quantiles.
