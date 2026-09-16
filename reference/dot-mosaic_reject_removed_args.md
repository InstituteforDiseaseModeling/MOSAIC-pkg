# Reject removed arguments captured by `...`

Reject removed arguments captured by `...`

## Usage

``` r
.mosaic_reject_removed_args(dots, fn)
```

## Arguments

- dots:

  The result of `list(...)` in the calling function.

- fn:

  Name of the calling function, for the error message.

## Value

Invisibly `TRUE`; errors if any removed or unknown argument was
supplied.
