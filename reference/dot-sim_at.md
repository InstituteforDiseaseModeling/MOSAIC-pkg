# Stamp the current tick and phase onto a draw controller

Called by the engine loop before each phase so that a replay mismatch
can report where it happened. A no-op in `"rng"` mode – see the body.

## Usage

``` r
.sim_at(ctl, tick, phase)
```
