# Set OpenMeteo API Key

This function sets the OpenMeteo API key and stores it in a global
option called `openmeteo_api_key`. If an API key is provided as an
argument, it will be used. If not, the function will prompt for the key
interactively.

## Usage

``` r
set_openmeteo_api_key(api_key = NULL)
```

## Arguments

- api_key:

  A character string representing the OpenMeteo API key. If `NULL`, the
  function will prompt for the key interactively.

## Value

A message confirming that the API key has been set, and instructions for
retrieving the key.

## Details

The API key is stored in a global option called `openmeteo_api_key`
using [`options()`](https://rdrr.io/r/base/options.html). The key can be
retrieved at any time using `getOption("openmeteo_api_key")`.

## Examples

``` r
# Set the OpenMeteo API key interactively
set_openmeteo_api_key()
#> Please enter your OpenMeteo API key: 
#> Error in set_openmeteo_api_key(): API key cannot be empty. Please provide a valid API key.

# Set the OpenMeteo API key programmatically
set_openmeteo_api_key("your-api-key-here")
#> OpenMeteo API key set successfully.
#> Retrieve with getOption('openmeteo_api_key')

# Access the API key
getOption("openmeteo_api_key")
#> [1] "your-api-key-here"
```
