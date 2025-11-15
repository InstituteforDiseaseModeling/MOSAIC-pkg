# Check for nonsensical values in estimated data

Check for nonsensical values in estimated data

## Usage

``` r
check_nonsensical_values(est_cases, est_deaths, max_cases, max_deaths)
```

## Arguments

- est_cases:

  Matrix of estimated cases

- est_deaths:

  Matrix of estimated deaths

- max_cases:

  Maximum reasonable cases per timestep

- max_deaths:

  Maximum reasonable deaths per timestep

## Value

Character vector of violations found
