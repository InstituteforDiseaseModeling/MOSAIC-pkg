# Load or Install Required Packages (with GitHub handling for mobility, propvacc, and MOSAIC)

This function checks whether the specified packages are installed. If a
package is not installed, it will be installed. Special handling is
included for `mobility`, `propvacc`, and `MOSAIC`, which are installed
from GitHub.

## Usage

``` r
load_or_install_packages(packages)
```

## Arguments

- packages:

  A character vector of package names to load or install.

## Details

For most packages, this function installs them from CRAN if they are not
already installed. Special cases are handled for the following packages:

- **mobility**: Installed from GitHub at
  <https://github.com/COVID-19-Mobility-Data-Network/mobility>.

- **propvacc**: Installed from GitHub at
  <https://github.com/gilesjohnr/propvacc>.

- **MOSAIC**: Installed from GitHub at
  <https://github.com/InstituteforDiseaseModeling/MOSAIC-pkg>.

## Examples

``` r
if (FALSE) { # \dontrun{
load_or_install_packages(c("ggplot2", "mobility", "propvacc", "MOSAIC"))
} # }
```
