# Great-circle distance matrix between patch centroids

Haversine, matching `laser.core.migration.distance()` — which is the
only reason laser-core was a dependency of the engine at all. Earth
radius 6371 km.

## Usage

``` r
sim_distance_matrix(latitude, longitude)
```

## Arguments

- latitude, longitude:

  Numeric vectors of length `npatches`, in decimal degrees.

## Value

An `[npatches, npatches]` matrix of distances in km, zero on the
diagonal.

## Precision vs. the Python oracle

This computes in double throughout. The Python implementation does not:
the config's `latitude` / `longitude` are coerced to `float32` by
`params.py`, and `np.radians` of a `float32` array stays `float32`, so
the whole haversine `a` term (`sin^2(dlat/2) + cos cos sin^2(dlon/2)`)
is evaluated in single precision before being widened for the `arcsin`.
The result is that the oracle's distance matrix carries ~1e-6 relative
error and **this version is the more accurate of the two**.

That was established empirically rather than assumed: mimicking each
float32 truncation in turn (inputs only, then the `a` term)
monotonically closed the observed gap — 1.09e-6 to 8.5e-7 to 5.2e-7 —
which confirms cumulative single-precision arithmetic rather than a
difference of formula. The consequence is that `pi_ij` cannot match the
oracle to better than ~1e-6 (observed worst case 1.17e-6), and its Tier
A tolerance is set accordingly. No other precomputed matrix is affected:
they are element-wise in their inputs and match to 1e-7 or better.
