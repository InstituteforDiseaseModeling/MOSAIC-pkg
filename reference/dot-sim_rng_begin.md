# Establish an isolated RNG stream and describe how to restore the caller's

Establish an isolated RNG stream and describe how to restore the
caller's

## Usage

``` r
.sim_rng_begin(seed)
```

## Arguments

- seed:

  Integer seed.

## Value

A list with the caller's prior `.Random.seed` (or `NULL` if the caller
had none) and their [`RNGkind()`](https://rdrr.io/r/base/Random.html).
