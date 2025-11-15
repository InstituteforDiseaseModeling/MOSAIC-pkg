# Compute Diffusion Matrix Based on Gravity Model

Computes a connectivity matrix \\\pi\_{ij}\\ between locations using a
normalized gravity model.

## Usage

``` r
calc_diffusion_matrix_pi(D, N, omega, gamma)
```

## Arguments

- D:

  A square numeric distance matrix between locations. Units are
  kilometers.

- N:

  A numeric vector of population sizes. Length must match `nrow(D)`. May
  be named.

- omega:

  Numeric exponent for population scaling.

- gamma:

  Numeric exponent for distance decay.

## Value

A numeric matrix of diffusion probabilities \\\pi\_{ij}\\, with
diagonals set to `NA`.

## Details

The normalized gravity model has the following form:

\$\$\pi\_{ij} = \frac{N_j^\omega \\ d\_{ij}^{-\gamma}}{\sum\_{j \ne i}
N_j^\omega \\ d\_{ij}^{-\gamma}}, \quad \forall \\ i,\\ j\$\$

where \\d\_{ij}\\ is the distance between locations \\i\\ and \\j\\, and
\\N_j\\ is the population or weight of location \\j\\. The diagonal of
the output matrix is set to `NA`, as \\\pi\_{ij}\\ is intended to be
used conditionally alongside the departure probability \\\tau_i\\.

## Examples

``` r
if (FALSE) { # \dontrun{
set.seed(123)
D <- matrix(runif(100, 1, 10), nrow = 10)
D <- (D + t(D)) / 2; diag(D) <- 0
N <- runif(10, 100, 1000)
names(N) <- LETTERS[1:10]

pi_mat <- calc_diffusion_matrix_pi(D, N, omega = 0.5, gamma = 2)
} # }
```
