# Read a Tier B replay fixture

Fixtures are frozen recordings of the Python laser-cholera oracle: every
PRNG call it made, in order, plus the 28 result channels it produced.
They are generated once by `claude/oracle/dump_fixture.py` and
committed, so the replay tests run in pure R with no Python installed.
See `tests/testthat/fixtures/ORACLE.md` for the oracle provenance.

## Usage

``` r
sim_read_fixture(path)
```

## Arguments

- path:

  Directory holding `replay.rds`, or the `.rds` itself.

## Value

A list with `calls`, `values`, `results` and `meta`.
