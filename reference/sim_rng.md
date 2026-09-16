# Stochastic draw sites for the R transmission engine

The engine reaches randomness only through
[`.sim_binom()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/dot-sim_binom.md)
and
[`.sim_pois()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/dot-sim_pois.md).
Routing every draw through two functions is what makes the Tier B replay
strategy possible: in `"replay"` mode they return recorded results from
the Python oracle instead of drawing, while asserting that the R engine
asked for exactly the draw the oracle made, at the same tick, in the
same phase, at the same site.

## RNG contract

A simulation's output is determined solely by its `seed` and `config` –
never by worker identity, batch position, or how much randomness was
consumed earlier in the session. `.sim_rng_state()` establishes an
isolated stream and returns everything needed to restore the caller's
`.Random.seed` afterwards, so calling the engine never perturbs the
caller's stream.
