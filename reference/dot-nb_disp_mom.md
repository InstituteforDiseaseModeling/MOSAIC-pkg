# Moment estimate of NB dispersion about a fitted Poisson mean

Used only to seed `init.theta`. Solves \\E\[(y - \mu)^2 - \mu\] = \mu^2
/ k\\ by weighted pooling.

## Usage

``` r
.nb_disp_mom(X, spec)
```

## Arguments

- X:

  Model frame containing `y` and `.w`.

- spec:

  Character model formula.

## Value

Numeric dispersion estimate, or `NA_real_`.
