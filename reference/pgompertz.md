# Gompertz Distribution Cumulative Distribution Function

Compute the cumulative distribution function of a Gompertz distribution.

## Usage

``` r
pgompertz(q, b, eta, lower.tail = TRUE, log.p = FALSE)
```

## Arguments

- q:

  Numeric vector. Quantiles at which to evaluate the CDF.

- b:

  Numeric. Shape parameter (b \> 0).

- eta:

  Numeric. Rate parameter (eta \> 0).

- lower.tail:

  Logical. If TRUE, return P(X \<= q), else P(X \> q).

- log.p:

  Logical. If TRUE, return log probability.

## Value

Numeric vector of probabilities.
