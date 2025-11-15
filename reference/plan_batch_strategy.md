# Plan Multi-Stage Batch Strategy

Plans a multi-stage strategy: calibration -\> large predictive batch -\>
fine-tuning

## Usage

``` r
plan_batch_strategy(
  current_ess,
  target_ess,
  ess_history = NULL,
  stage = "calibration",
  calibration_size = 2000,
  calibration_batch = 500
)
```

## Arguments

- current_ess:

  Current minimum ESS

- target_ess:

  Target ESS

- ess_history:

  ESS history from calibration

- stage:

  Current stage ("calibration", "prediction", "fine_tuning")

## Value

Recommended batch size and stage info
