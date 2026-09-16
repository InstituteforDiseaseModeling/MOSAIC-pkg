# Assert a replay run consumed the record exactly

Exhaustion must be checked in both directions. A port that skips a draw
site passes a naive replay test right up to the point where the offsets
happen to realign, so "R never ran past the end" is not enough on its
own.

## Usage

``` r
sim_assert_replay_complete(ctl)
```

## Arguments

- ctl:

  Draw controller used for the run.

## Value

Invisibly `TRUE`; errors on any shortfall.
