# Get Template for Parameter Values

This function generates a template long-form data frame for parameter
values, including origin (`i`), destination (`j`), and time (`t`), used
to simulate variables such as sigma.

## Usage

``` r
make_param_df(
  variable_name = NULL,
  variable_description = NULL,
  parameter_distribution = NULL,
  i = NULL,
  j = NULL,
  t = NULL,
  parameter_name = NULL,
  parameter_value = NULL
)
```

## Arguments

- variable_name:

  A character string representing the name of the variable (e.g.,
  'sigma').

- variable_description:

  A character string describing the variable (e.g., 'proportion
  symptomatic').

- parameter_distribution:

  A character string representing the distribution of the parameter
  (e.g., 'beta').

- i:

  A vector representing the origin locations.

- j:

  A vector representing the destination locations.

- t:

  A vector representing the time points.

- parameter_name:

  A character vector of parameter names (e.g., shape1, shape2 for a beta
  distribution).

- parameter_value:

  A numeric vector of parameter values (corresponding to the parameter
  names).

## Value

A data frame containing the long-form template for the parameter values.
If all arguments are `NULL`, an empty data frame is returned.

## Examples

``` r
# Example usage for sigma
prm <- list(shape1 = 2, shape2 = 5)
param_df <- make_param_df("sigma", "proportion symptomatic", "beta",
                          i = "A", j = "B", t = 1, names(prm), unlist(prm))
print(param_df)
#>   variable_name   variable_description parameter_distribution i j t
#> 1         sigma proportion symptomatic                   beta A B 1
#> 2         sigma proportion symptomatic                   beta A B 1
#>   parameter_name parameter_value
#> 1         shape1               2
#> 2         shape2               5
```
