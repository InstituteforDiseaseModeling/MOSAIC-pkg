# Compute the model-implied mobility flux from a run configuration

Derives the **model's own implied daily mobility flux** from a LASER
configuration's population sizes and calibrated mobility point
estimates. This is the single source of truth for the four mobility
figures in the `"spatial"` group of
[`render_MOSAIC_figures`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/render_MOSAIC_figures.md)
(\\\pi\_{ij}\\ diffusion, \\\tau_i\\ departure, modeled flux matrix,
mobility network).

## Usage

``` r
calc_mobility_flux(config)
```

## Arguments

- config:

  A LASER configuration list (as read from `1_inputs/config.json` or
  produced by
  [`make_LASER_config`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/make_LASER_config.md)).
  Must contain `longitude`, `latitude`, `N_j_initial`, `tau_i`,
  `location_name`, and the scalars `mobility_omega`, `mobility_gamma`.

## Value

A list with elements, all in config order:

- location_name:

  Character vector of location labels (length J).

- coords:

  A J x 2 numeric matrix of `longitude`, `latitude`.

- N:

  Numeric vector of origin populations (length J), named by location.

- tau:

  Numeric vector of daily departure probabilities (length J), named.

- omega:

  Numeric scalar gravity population exponent.

- gamma:

  Numeric scalar gravity distance-decay exponent.

- D:

  J x J Haversine distance matrix (km), config order.

- pi:

  J x J normalized gravity connectivity, diagonal `NA`.

- flux:

  J x J modeled daily flux \\M\_{ij}=N_i\tau_i\pi\_{ij}\\, diagonal
  `NA`.

## Details

The modeled daily flux is \$\$M\_{ij} = N_i \\ \tau_i \\ \pi\_{ij}\$\$
the expected number of travelers per day moving from origin \\i\\ to
destination \\j\\, where \\N_i\\ is the origin population, \\\tau_i\\ is
the (daily) departure probability, and \\\pi\_{ij}\\ is the normalized
gravity connectivity
([`calc_diffusion_matrix_pi`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_diffusion_matrix_pi.md)).
The diagonal is `NA` (no self-travel), so `rowSums(flux, na.rm = TRUE)`
equals \\N_i\\\tau_i\\, the total daily travelers leaving each origin.

This differs from
[`plot_mobility`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/plot_mobility.md),
which visualizes the *observed* OAG flight matrix used to fit the
mobility model. This function returns the model's *implied* flux from
its calibrated parameters, and is therefore run-specific and fully
computable from the run's `config.json` alone (no observed flight data,
no disk, no estimation, no API call).

## Ordering

All outputs are carried in **config order** (the order locations appear
in the configuration, which is the order the engine consumes them). The
`location_name` element is aligned element-wise to every vector and to
both axes of every matrix. This function never sorts.

## See also

[`calc_diffusion_matrix_pi`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_diffusion_matrix_pi.md),
[`get_distance_matrix`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/get_distance_matrix.md),
[`plot_diffusion_pi`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/plot_diffusion_pi.md),
[`plot_departure_tau`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/plot_departure_tau.md),
[`plot_mobility_flux_matrix`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/plot_mobility_flux_matrix.md),
[`plot_mobility_flux_network`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/plot_mobility_flux_network.md),
[`render_MOSAIC_figures`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/render_MOSAIC_figures.md).

## Examples

``` r
if (FALSE) { # \dontrun{
cfg  <- jsonlite::fromJSON(system.file("extdata", "config_default.json",
                                        package = "MOSAIC"), simplifyVector = TRUE)
flux <- calc_mobility_flux(cfg)
# Identity: row sums equal N * tau (daily travelers leaving each origin)
stopifnot(all.equal(rowSums(flux$flux, na.rm = TRUE), flux$N * flux$tau,
                    check.attributes = FALSE))
} # }
```
