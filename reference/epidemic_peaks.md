# Cholera Epidemic Peaks Data

A dataset containing identified epidemic peaks from cholera surveillance
data across African countries. Peaks are detected using time series
analysis with smoothing and prominence-based peak detection algorithms.

## Usage

``` r
epidemic_peaks
```

## Format

A data frame with 5 variables:

- iso_code:

  ISO 3166-1 alpha-3 country code

- peak_start:

  Start date of the epidemic period (Date)

- peak_date:

  Date of peak incidence (Date)

- peak_stop:

  End date of the epidemic period (Date)

- reported_cases:

  Number of reported cholera cases at peak (numeric)

## Source

Generated from cholera surveillance data using
[`est_epidemic_peaks()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/est_epidemic_peaks.md)
function on combined daily surveillance data from WHO and other sources.

## Details

Epidemic peaks are identified using the following methodology:

- Time series smoothing with 21-day running mean window

- Local maxima detection with 7-day comparison windows

- Prominence-based filtering (minimum 10\\

- Minimum peak height threshold of 10 smoothed cases

- Minimum 90-day separation between consecutive peaks

- Peak boundaries defined where incidence drops to 1/3 of peak height

Country-specific adjustments are applied for:

- Niger (NER): Lower prominence threshold (2\\

- Cameroon (CMR): Adjusted threshold (5\\

- Ethiopia (ETH): Standard threshold (7\\

Manual corrections have been applied for known issues including:

- Ethiopia 2024: February peak corrected to March (sustained outbreak)

- DRC 2023: Added January peak (filtered due to proximity)

- Nigeria 2024: Added October peak

- Kenya 2022-2023: Added December 2022, removed June 2023 minor peak

- Mozambique: Added February 2024 and March 2025 peaks

- Somalia: Added major April 2017 peak and 2024-2025 peaks

- Zambia: Added January 2018 peak

- Tanzania: Added January 2017 peak

## See also

[`est_epidemic_peaks`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/est_epidemic_peaks.md)
for the function that generates this data

[`plot_epidemic_peaks`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/plot_epidemic_peaks.md)
for visualization

## Examples

``` r
data(epidemic_peaks)
head(epidemic_peaks)
#>     iso_code peak_start  peak_date  peak_stop reported_cases
#> 118      AGO 2018-01-01 2018-01-04 2018-01-31             24
#> 1        AGO 2025-03-29 2025-04-28 2025-05-28            303
#> 120      BDI 2016-08-01 2016-08-21 2016-10-31              9
#> 119      BDI 2017-10-15 2017-10-17 2017-10-19              6
#> 121      BDI 2023-04-06 2023-04-13 2023-04-30              8
#> 2        BDI 2023-09-03 2023-09-11 2023-09-19             13
#>     outbreak_interval_days
#> 118                     30
#> 1                       60
#> 120                     91
#> 119                      4
#> 121                     24
#> 2                       16

# Countries with epidemic data
unique(epidemic_peaks$iso_code)
#>  [1] "AGO" "BDI" "BEN" "CAF" "CIV" "CMR" "COD" "COG" "ETH" "GHA" "GIN" "KEN"
#> [13] "LBR" "MOZ" "MWI" "NER" "NGA" "RWA" "SDN" "SLE" "SOM" "SSD" "TCD" "TGO"
#> [25] "TZA" "UGA" "ZMB" "ZWE"

# Recent peaks (2024-2025)
recent_peaks <- epidemic_peaks[epidemic_peaks$peak_date >= as.Date("2024-01-01"), ]
table(recent_peaks$iso_code)
#> 
#> AGO BDI COD COG ETH GHA KEN MOZ NER NGA RWA SDN SOM SSD TGO TZA UGA ZMB ZWE 
#>   1   2   1   1   2   1   3   2   1   2   1   4   2   2   1   3   3   2   2 

# Peak duration calculation
epidemic_peaks$duration <- as.numeric(
  epidemic_peaks$peak_stop - epidemic_peaks$peak_start
)
summary(epidemic_peaks$duration)
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#>    2.00   14.50   28.00   31.19   45.75  117.00 
```
