# Calculate spatial importation hazard

Computes the **daily probability that at least one new infection is
introduced** into each destination location *j* on day *t* using a
gravity‐style coupling of infectious prevalence across all locations.
The formulation is adapted from Bjørnstad & Grenfell (2008) but (i)
folds the gravity weights directly into the prevalence term instead of
using a separate coupling constant, and (ii) uses the location-specific
departure probability `tau` (\\\tau_j\\, the daily probability that a
resident of \\j\\ travels outside its home location; see "Spatial
dynamics" in the MOSAIC model description) to split each population into
a stay-at-home share \\1-\tau_j\\ and a travelling share \\\tau_j\\.

Concretely, for time *t* and destination *j* the hazard is \$\$\mathcal
H\_{jt} \\=\\ \frac{\beta\_{jt}\\S^{\\}\_{jt}
\bigl\[1-\exp\\-(S^{\\}\_{jt}/N\_{jt})\\\bar y\_{jt}\\\bigr\]}
{1+\beta\_{jt}\\S^{\\}\_{jt}},\$\$ where

- \\S^{\\}\_{jt} =
  (1-\tau_j)\bigl(S\_{jt}+V^{\mathrm{sus}}\_{1,jt}+V^{\mathrm{sus}}\_{2,jt}\bigr)\\
  is the stay-at-home susceptible pool.

- \\\bar y\_{jt}\\ is the **gravity‑weighted prevalence of infection**
  in the entire metapopulation (including the destination): \$\$\bar
  y\_{jt} = \frac{ (1-\tau_j)(I\_{1,jt}+I\_{2,jt}) + \sum\_{i\neq
  j}\tau_i\\\pi\_{ij}\\(I\_{1,it}+I\_{2,it}) } {\sum\_{k} N\_{kt} }.\$\$

Note: This specification differs from the two‑stage form in the original
paper but has been used in later work where the mobility matrix itself
carries the full coupling strength.

## Usage

``` r
calc_spatial_hazard(
  beta,
  tau,
  pie,
  N,
  S,
  V1_sus,
  V2_sus,
  I1,
  I2,
  time_names = NULL,
  location_names = NULL
)
```

## Arguments

- beta:

  Numeric **J × T** matrix. Human force‑of‑infection parameter
  \\\beta\_{jt}\\ for each location (*rows*) and time step (*columns*).

- tau:

  Numeric vector of length **J**. Departure probability \\\tau_j \in
  \[0,1\]\\ of each location: larger values shrink the stay-at-home
  susceptibles and local infections by \\1-\tau_j\\ and export more of
  location \\j\\'s infections to other destinations.

- pie:

  Numeric **J × J** matrix of gravity‑model mobility weights
  \\\pi\_{ij}\\ from origin *i* to destination *j*. Diagonal entries are
  ignored.

- N:

  Numeric **J × T** matrix of total population \\N\_{jt}\\.

- S:

  Numeric **J × T** matrix of fully susceptible individuals \\S\_{jt}\\.

- V1_sus:

  Numeric **J × T** matrix of vaccine‑derived susceptibles after one
  dose.

- V2_sus:

  Numeric **J × T** matrix of vaccine‑derived susceptibles after two
  doses.

- I1:

  Numeric **J × T** matrix of symptomatic infectives.

- I2:

  Numeric **J × T** matrix of asymptomatic infectives.

- time_names:

  Optional character vector of length **T** for output column names.

- location_names:

  Optional character vector of length **J** for output row names.

## Value

A numeric **J × T** matrix `H` (rows = locations, columns = time steps)
with elements in \[0,1\] giving the spatial importation hazard for each
location–time pair.

## Details

*Diagonal of* `pie` *is set to zero internally.* All matrices must have
matching dimensions; dimension checks throw informative errors.

## Warning

The current formulation intentionally **includes local infectious
individuals** in `\bar y_{jt}`. If you wish to model *importation only*,
set the local contribution to zero in the numerator.

## References

- Bjørnstad, O. N., & Grenfell, B. T. (2008). *Hazards, spatial
  transmission and timing of outbreaks in epidemic metapopulations.*
  Journal of Theoretical Ecology, 1, 145‑153.

## Examples

``` r
if (FALSE) { # \dontrun{
set.seed(123)
T_steps <- 10; J <- 5
beta <- matrix(runif(J * T_steps), nrow = J, ncol = T_steps)
tau <- runif(J, 0, 0.2)
pie <- matrix(runif(J^2), J, J); diag(pie) <- 0
N <- matrix(sample(200:400, J * T_steps, replace = TRUE), nrow = J, ncol = T_steps)
S <- matrix(sample(100:200, J * T_steps, replace = TRUE), nrow = J, ncol = T_steps)
V1_sus <- matrix(sample(0:50, J * T_steps, replace = TRUE), nrow = J, ncol = T_steps)
V2_sus <- matrix(sample(0:50, J * T_steps, replace = TRUE), nrow = J, ncol = T_steps)
I1 <- matrix(sample(0:10, J * T_steps, replace = TRUE), nrow = J, ncol = T_steps)
I2 <- matrix(sample(0:10, J * T_steps, replace = TRUE), nrow = J, ncol = T_steps)

H <- calc_spatial_hazard(beta, tau, pie, N, S, V1_sus, V2_sus, I1, I2,
                         time_names = paste0("day_", 1:T_steps),
                         location_names = paste0("loc_", 1:J))

plot_spatial_hazard(H)
} # }
```
