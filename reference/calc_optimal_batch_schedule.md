# Calculate Optimal Batch Schedule

Plans an efficient sequence of batch sizes to reach convergence based on
current ESS accumulation patterns.

## Usage

``` r
calc_optimal_batch_schedule(
  rate_analysis,
  max_batch_size = 5000,
  min_batch_size = 100
)
```

## Arguments

- rate_analysis:

  Output from calc_ess_accumulation_rate

- max_batch_size:

  Maximum allowed batch size

- min_batch_size:

  Minimum allowed batch size

## Value

Vector of recommended batch sizes
