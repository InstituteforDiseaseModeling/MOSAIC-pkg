# Generate Random Gompertz Variates

Generate random variates from a Gompertz distribution with parameters b
and eta. Uses the inverse CDF method: Q(p) = (1/b) \* log(1 +
(-log(1-p))/eta)

## Usage

``` r
rgompertz(n, b, eta)
```

## Arguments

- n:

  Integer. Number of random variates to generate.

- b:

  Numeric. Shape parameter (b \> 0).

- eta:

  Numeric. Rate parameter (eta \> 0).

## Value

Numeric vector of length n containing random Gompertz variates.
