# Estimate OCV Vaccination Rates

This function processes vaccination data from WHO or GTFCC,
redistributes doses based on a maximum daily rate, and calculates
vaccination parameters for use in the MOSAIC cholera model. The
processed data includes redistributed daily doses, cumulative doses, and
the proportion of the population vaccinated. The results are saved as
CSV files for downstream modeling.

## Usage

``` r
est_vaccination_rate(
  PATHS,
  date_start = NULL,
  date_stop = NULL,
  max_rate_per_day,
  data_source
)
```

## Arguments

- PATHS:

  A list containing file paths, including:

  DATA_SCRAPE_WHO_VACCINATION

  :   The path to the folder containing WHO vaccination data.

  DATA_DEMOGRAPHICS

  :   The path to the folder containing demographic data.

  MODEL_INPUT

  :   The path to the folder where processed data will be saved.

- date_start:

  The start date for the vaccination data range (in "YYYY-MM-DD"
  format). Defaults to the earliest date in the data.

- date_stop:

  The stop date for the vaccination data range (in "YYYY-MM-DD" format).
  Defaults to the latest date in the data.

- max_rate_per_day:

  The maximum vaccination rate per day used to redistribute doses.
  Default is 100,000 doses/day.

- data_source:

  The source of the vaccination data. Must be one of `"WHO"`, `"GTFCC"`,
  or `"BOTH"`. When `"BOTH"` is specified, the function uses combined
  data from both sources with GTFCC prioritized and unique WHO campaigns
  added.

## Value

This function does not return an R object but saves the following files
to the directory specified in `PATHS$MODEL_INPUT`:

- A redistributed vaccination data file named
  `"data_vaccinations_<suffix>_redistributed.csv"` where suffix is WHO,
  GTFCC, or GTFCC_WHO.

- A parameter data frame for the vaccination rate (nu) named
  `"param_nu_vaccination_rate_<suffix>.csv"`.

## Details

**What nu is.** The output `nu` is the number of doses *shipped* per
request, spread over days at `max_rate_per_day`. It carries no round,
regimen or administered-dose information, and
`data-raw/make_config_default.R` routes all of it to first doses
(`nu_1_jt`; `nu_2_jt = 0`). For simulation windows from 2023 onward this
is close to right, because the ICG suspended the two-dose regimen for
outbreak response in October 2022 (global OCV shortage; WHO news
release, 19 October 2022) and reactive campaigns have been single-dose
since. For earlier windows a two-dose campaign is counted as two rounds
of first doses, roughly doubling the distinct people immunised, and
shipped-but-unused doses are counted as delivered. Pre-t0 campaigns
enter the initial conditions through
[`est_initial_V1_V2`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/est_initial_V1_V2.md),
which does pair rounds.

The function performs the following steps:

1.  **Load Vaccination Data**: - Reads processed vaccination data from
    WHO or GTFCC and filters for relevant columns (`iso_code`,
    `campaign_date`, `doses_shipped`).

2.  **Redistribute Doses**: - Redistributes shipped doses day by day
    based on a maximum daily rate (`max_rate_per_day`). - Ensures no
    duplication of `distribution_date` within `iso_code`.

3.  **Validate Redistribution**: - Checks that the redistributed doses
    sum to the total shipped doses.

4.  **Ensure Full Coverage**: - Ensures all ISO codes have data across
    the full date range (`date_start` to `date_stop`), filling missing
    dates with zero doses.

5.  **Calculate Population Metrics**: - Merges population data for 2023,
    calculates cumulative doses, and computes the proportion of the
    population vaccinated.

6.  **Save Outputs**: - Saves the redistributed vaccination data and the
    vaccination rate parameter data frame.

## Examples

``` r
if (FALSE) { # \dontrun{
PATHS <- list(
  DATA_SCRAPE_WHO_VACCINATION = "path/to/who_vaccination_data",
  DATA_DEMOGRAPHICS = "path/to/demographics",
  MODEL_INPUT = "path/to/save/processed/data"
)
est_vaccination_rate(PATHS, max_rate_per_day = 100000, data_source = "WHO")
} # }
```
