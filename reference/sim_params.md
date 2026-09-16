# Normalise and validate a config for the R transmission engine

Python's `params.py` is 1,009 lines, and it is tempting to call it "just
coercion" and drop it – but that is only safe if the engine's sole input
is
[`make_simulation_config()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/make_simulation_config.md)
output, and it is not: the engine also accepts raw lists and file paths.
Scalar-to-matrix broadcasting, dimension checking and non-finite
rejection all live there today and have to live somewhere afterwards.
This is that somewhere.

## Usage

``` r
sim_params(config, components = SIM_PIPELINE)
```

## Arguments

- config:

  Config list, or a path to a `.json` / `.json.gz` file.

- components:

  Pipeline subset that will be run; determines which compartments
  `Census` sums and which parameters are required.

## Value

A list of validated engine parameters, including the original
(normalised) config under `$config`.

## Details

Orientation is the single most dangerous thing here. Configs carry
time-varying fields as `[npatches][nticks]` (location-major, matching
the JSON on disk), while the engine's internal state is
`[nticks + 1, npatches]` (time-major, matching Python's frames). This
function transposes once, at the boundary, and every dimension is
asserted rather than assumed – a silently transposed input is the
likeliest way to get plausible-looking wrong answers out of the whole
port.
