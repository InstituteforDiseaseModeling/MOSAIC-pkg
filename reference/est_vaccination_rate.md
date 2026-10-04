# Estimate OCV Vaccination Rates

This function processes vaccination data from WHO or GTFCC,
redistributes doses based on a maximum daily rate, splits them into
first and second doses, and calculates vaccination parameters for use in
the MOSAIC cholera model. The processed data includes redistributed
daily doses, cumulative doses, and the proportion of the population
vaccinated. The results are saved as CSV files for downstream modeling.

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

  The maximum vaccination rate per day used to redistribute doses (no
  default; the data pipeline uses 20,000 doses/day).

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
  GTFCC, or GTFCC_WHO; its `doses_distributed_dose1` and
  `doses_distributed_dose2` columns split `doses_distributed` into first
  and second doses.

- A parameter data frame for the vaccination rate (nu, all doses) named
  `"param_nu_vaccination_rate_<suffix>.csv"`.

- Parameter data frames for the first-dose (`nu_1`) and second-dose
  (`nu_2`) rates named `"param_nu_1_vaccination_rate_<suffix>.csv"` and
  `"param_nu_2_vaccination_rate_<suffix>.csv"`; on every location-day
  `nu_1 + nu_2 = nu`.

## Details

**What nu is.** The output `nu` is the number of doses *shipped* per
request, administered at up to `max_rate_per_day` a day. A GTFCC request
releases each delivery on its own date (its `delivery_schedule`):
deliveries join a stock, the stock is administered at `max_rate_per_day`
a day while it lasts, and a request whose stock runs out resumes at its
next delivery. A request with one delivery, or whose next delivery
arrives before its stock runs out, is one unbroken run from its first
delivery, as are WHO rows, which have no schedule and start at their
campaign date. Requests that overlap in a location add up.
Shipped-but-unused doses are counted as delivered. (Up to MOSAIC
v0.102.0 every delivery of a request was released from its first
delivery date, which moved later deliveries of multi-delivery requests –
typically second rounds and later campaigns of GTFCC preventive
programmes – up to 2.6 years (961 days) early.)

**First and second doses.** Each day's doses of a request are split into
first doses (`nu_1`) and second doses (`nu_2`) by the request's
`round_sequence` (written by
[`process_GTFCC_vaccination_data`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/process_GTFCC_vaccination_data.md)
from the GTFCC Round events and carried through
[`combine_vaccination_data`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/combine_vaccination_data.md)):
the request's shipped doses are divided across its round blocks in
proportion to the doses administered in each round, and the blocks take
consecutive stretches of the request's daily series in administration
order, so a two-round campaign delivers its second doses after its
first. Only the labels change – the daily totals are those of `nu` – and
the block boundaries are whole doses, so with whole-dose shipments both
series are whole numbers. A request with no round information (no GTFCC
Round events, or a WHO-only shipment) counts entirely as first doses. In
the engine, second doses move `phi_2` of their recipients from V1 to V2
and are capped at the V1 stock, first doses move `phi_1` of theirs from
the eligible compartments to V1
([`sim_phase_vaccinated()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/sim_components.md)).
Second rounds belong mostly to campaigns up to 2022: the ICG suspended
the two-dose regimen for outbreak response in October 2022 (global OCV
shortage; WHO news release, 19 October 2022). Pre-t0 campaigns enter the
initial conditions through
[`est_initial_V1_V2`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/est_initial_V1_V2.md),
which pairs rounds from the same Round events.

The function performs the following steps:

1.  **Load Vaccination Data**: - Reads processed vaccination data from
    WHO or GTFCC and filters for relevant columns (`iso_code`,
    `campaign_date`, `doses_shipped`, `delivery_schedule`,
    `round_sequence`).

2.  **Redistribute Doses**: - Releases each delivery on its date and
    administers the stock day by day at up to a maximum daily rate
    (`max_rate_per_day`), then splits each day into first and second
    doses. - Sums requests that overlap on the same `distribution_date`
    within `iso_code`.

3.  **Validate Redistribution**: - Checks that the redistributed doses
    sum to the total shipped doses and that first plus second doses
    equal the doses distributed.

4.  **Ensure Full Coverage**: - Ensures all ISO codes have data across
    the full date range (`date_start` to `date_stop`), filling missing
    dates with zero doses.

5.  **Calculate Population Metrics**: - Merges population data for 2023,
    calculates cumulative doses, and computes the proportion of the
    population vaccinated (doses over population; this does not feed the
    `nu` parameters).

6.  **Save Outputs**: - Saves the redistributed vaccination data and the
    `nu`, `nu_1` and `nu_2` parameter data frames.

## Examples

``` r
if (FALSE) { # \dontrun{
PATHS <- list(
  DATA_SCRAPE_WHO_VACCINATION = "path/to/who_vaccination_data",
  DATA_DEMOGRAPHICS = "path/to/demographics",
  MODEL_INPUT = "path/to/save/processed/data"
)
est_vaccination_rate(PATHS, max_rate_per_day = 20000, data_source = "WHO")
} # }
```
