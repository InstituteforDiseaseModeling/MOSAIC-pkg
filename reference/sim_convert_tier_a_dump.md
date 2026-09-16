# Convert a raw Tier A oracle dump into a committed fixture

Tier A covers the four deterministic precomputed matrices (`pi_ij`,
`beta_jt_human`, `beta_jt_env`, `delta_jt`), which have no RNG in them
and can therefore be compared exactly with no draw alignment. Generated
by `claude/oracle/dump_tier_a.py`.

## Usage

``` r
sim_convert_tier_a_dump(dump, out)
```

## Arguments

- dump:

  Directory containing the `.bin` files and `meta.json`.

- out:

  Path of the `.rds` to write.

## Value

Invisibly, the fixture list that was written.
