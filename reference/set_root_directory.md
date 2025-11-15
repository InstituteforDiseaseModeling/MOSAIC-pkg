# Set Root Directory

This function sets the root directory to a user-specified path or, if
not provided, prompts the user to enter the root directory
interactively. The directory path is stored in a global option called
`root_directory` and is also returned.

## Usage

``` r
set_root_directory(root = NULL)
```

## Arguments

- root:

  A character string representing the path to the root directory. If
  `NULL`, the function will prompt the user to enter the root directory
  interactively.

## Value

A character string representing the root directory. The root directory
is also stored globally in the option `root_directory`.

## Details

This function uses either a user-specified path or a path provided
interactively to set the root directory. The path is validated to ensure
that it exists. Once validated, the path is stored in the global option
`root_directory`, which can be accessed using
`getOption("root_directory")`. If the path is invalid (i.e., it does not
exist), the function throws an error.

## Examples

``` r
if (FALSE) { # \dontrun{
# Set and display the root directory interactively
root_dir <- set_root_directory()
cat("Root directory:", root_dir, "\n")

# Set the root directory programmatically
root_dir <- set_root_directory("/path/to/root")
cat("Root directory:", root_dir, "\n")

# Access the root directory from the global options
getOption("root_directory")
} # }
```
