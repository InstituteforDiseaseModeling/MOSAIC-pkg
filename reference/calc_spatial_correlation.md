# Calculate spatial correlation between two locations

Computes the Pearson spatial–correlation coefficient
\\\mathcal{C}\_{ij}\\ (Keeling & Rohani 2002) for a pair of locations
*i* and *j* from **single** infection time-series per location.

## Usage

``` r
calc_spatial_correlation(I_i, I_j, N_i = 1, N_j = 1)
```

## Arguments

- I_i:

  Numeric vector. Total infections in location *i* at each reporting
  time.

- I_j:

  Numeric vector. Total infections in location *j* at each reporting
  time. Must be the same length as `I_i`.

- N_i:

  Numeric **scalar or vector**. Population size(s) of location *i*.

  - If a scalar, the value is recycled across all time points.

  - If a vector, it must be the same length as the infection series and
    supplies time-varying population sizes.

  - Default is `1`, implying that infection counts are already expressed
    as prevalences.

- N_j:

  Numeric **scalar or vector**. Population size(s) of location *j*.
  Recycling and length rules mirror those of `N_i`. Default `1`.

## Value

Numeric scalar in the interval \[-1, 1\] giving the spatial correlation
between the two locations, or `NA_real_` if the calculation is not
possible (e.g. all values `NA`, zero variance).

## Details

The prevalence series are \\y\_{it} = I\_{it} / N\_{i,t}\\ and \\y\_{jt}
= I\_{jt} / N\_{j,t}\\, where the population sizes \\N\_{i,t}\\ and
\\N\_{j,t}\\ may be either **scalars** (constant over time) or
**vectors** the same length as the infection series (time-varying).

Rows containing `NA` in either prevalence series are removed
automatically. If fewer than two paired observations remain, the
function returns `NA_real_`. A consistency check stops execution if any
computed prevalence exceeds 1.

## References

Keeling M.J., Rohani P. (2002) *Ecology Letters* **5**, 20–29.

## Examples

``` r
set.seed(1)
t  <- 20
I_i <- rpois(t,  8)
I_j <- rpois(t, 12)
N_i <- round(seq(9.8e4, 1.02e5, len = t))   # growing population
N_j <- 8e4                                   # constant population
calc_spatial_correlation(I_i, I_j, N_i, N_j)
#> [1] 0.1570293
```
