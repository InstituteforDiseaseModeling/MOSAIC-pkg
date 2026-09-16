# Convert a raw oracle dump into a committed replay fixture

Development-time only: reads the raw-binary dump emitted by
`claude/oracle/dump_fixture.py` and writes the `replay.rds` the tests
consume.

## Usage

``` r
sim_convert_oracle_dump(dump, out)
```

## Arguments

- dump:

  Directory containing the `.bin` files and `meta.json` from the oracle
  harness.

- out:

  Path of the `replay.rds` to write.

## Value

Invisibly, the fixture list that was written.

## Details

The dev-time interchange is flat little-endian binary plus a JSON
manifest, read here with
[`readBin()`](https://rdrr.io/r/base/readBin.html). That needs no
package on either side – the oracle env has no pyarrow, and base R has
no HDF5 or parquet reader – so the Python/R boundary carries zero
dependencies. The committed artefact is a `.rds` so the test suite loads
it in one call and needs nothing extra.
