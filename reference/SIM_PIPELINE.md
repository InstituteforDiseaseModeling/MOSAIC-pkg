# The dynamics pipeline, in canonical order

Fixed by `model.py:566-579`. `Analyzer`, `Recorder` and `Parameters`
follow it in the Python pipeline but are I/O and diagnostics, consume no
randomness, and are not part of the R contract.

## Usage

``` r
SIM_PIPELINE
```
