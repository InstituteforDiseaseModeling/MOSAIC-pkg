# Plot Combined Cholera Surveillance Data

This function generates a two-panel plot that visualizes daily cholera
cases and deaths for a specific country, based on the combined
downscaled surveillance data from WHO, JHU, and SUPP. The top panel
shows reported cholera cases and the bottom panel shows reported cholera
deaths. Both panels use a log-transformation (with log(count + 1)) while
labeling the y-axis with raw counts. Daily data bars are colored by
their original source (WHO, JHU, or SUPP), and weekly observations are
shown as discrete points without connecting lines.

## Usage

``` r
plot_cholera_surveillance_data(PATHS, iso)
```

## Arguments

- PATHS:

  A named list of file paths. Required elements include:

  - `DATA_CHOLERA_WEEKLY` - Directory containing the combined weekly
    data file `"cholera_surveillance_weekly_combined.csv"`.

  - `DATA_CHOLERA_DAILY` - Directory containing the combined daily data
    file `"cholera_surveillance_daily_combined.csv"`.

  - `DOCS_FIGURES` - Directory in which the output PNG file will be
    saved.

- iso:

  A character string representing the ISO code of the target country
  (e.g., "ETH").

## Value

Invisibly returns `NULL`. The function produces a combined two-panel
plot, which is printed to the active graphics device and saved as a PNG
file.

## Details

The function reads input CSV files from the directories specified in
`PATHS` and saves the resulting combined plot as a PNG file in the
directory specified by `PATHS$DOCS_FIGURES`.
