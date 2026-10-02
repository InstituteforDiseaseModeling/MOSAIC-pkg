# Weekly block index

The block formula behind every weekly aggregation in MOSAIC (see
`.mosaic_week_blocks`): week number relative to a fixed Monday, shifted
by `offset`.

## Usage

``` r
.nb_disp_block(dates, offset = 0L)
```

## Arguments

- dates:

  Date vector.

- offset:

  Integer 0-6 shifting the block boundary off Monday.

## Value

Integer week index relative to the anchor epoch.
