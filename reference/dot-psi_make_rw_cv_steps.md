# Generate the expanding-window RW step grid.

Generate the expanding-window RW step grid.

## Usage

``` r
.psi_make_rw_cv_steps(
  fit_date_start,
  cutoff_date,
  step_months = 1L,
  test_months = 5L,
  gap_weeks = 4L,
  subsample = 1L,
  timesteps = 13L,
  min_test_days = NULL,
  step_days = NULL,
  test_days = NULL,
  min_train_years = NULL
)
```

## Arguments

- step_days:

  integer. Day-based stride; overrides `step_months`. Month stepping
  cannot tile an 84-day window, and a 91-day (3-month) stride advances 4
  x 91 = 364 days per four folds – a drift of 1.25 days/year against the
  annual cycle, which aliases the validation windows onto ~4 calendar
  months. An 84-day stride drifts 29.25 days/year and rotates through
  all 12.

- test_days:

  integer. Day-based validation window; overrides `test_months`. Set 84
  for a 12-week horizon.

- min_train_years:

  numeric. Replaces the midpoint grid-start rule with "start once this
  many years of training data exist". The midpoint rule discards the
  first half of the span by construction: at fit_date_start = 2010 with
  a 2026 cutoff it never validates before 2018-05-31, so 8 years of
  training data are never validated and psi over them is extrapolative.
