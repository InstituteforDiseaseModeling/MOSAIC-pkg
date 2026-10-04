# Process GTFCC Vaccination Data for MOSAIC Model

This function processes raw GTFCC vaccination data that has been scraped
from the GTFCC OCV dashboard by the ees-cholera-mapping repository. It
transforms the event-based data structure into a clean, structured
dataset matching the WHO vaccination data format for use in the MOSAIC
model.

## Usage

``` r
process_GTFCC_vaccination_data(PATHS)
```

## Arguments

- PATHS:

  A list containing file paths for input and output data. The list
  should include:

  - **MODEL_INPUT**: Path to the directory where processed vaccination
    data will be saved.

## Value

The function saves the processed vaccination data to a CSV file in the
directory specified by `PATHS$MODEL_INPUT`. It also returns the
processed data as a data frame for further use in R.

## Details

Each output row is one GTFCC request (`req_id`, e.g. `"2019-I05-D01"`):
`doses_shipped` is the sum of all its Delivery events and
`campaign_date` the first delivery date. Four columns carry the
request's delivery and round structure (MOSAIC v0.103.0); the round
columns are read from its Round events (`round_id`
`C<campaign>-R<round>`, `doses` = doses administered in that round):

- `req_id`:

  The GTFCC request identifier, for tracing a row back to the raw log.

- `delivery_schedule`:

  The request's deliveries as `<date>:<doses>` pairs separated by `;`,
  in date order, with deliveries on the same date summed, e.g.
  `"2019-04-24:835200;2019-09-25:835200"`. Their doses sum to
  `doses_shipped`;
  [`est_vaccination_rate`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/est_vaccination_rate.md)
  releases each delivery on its own date.

- `round_sequence`:

  The request's doses as an ordered sequence of `<dose>:<weight>` blocks
  separated by `;`, e.g. `"1:357200;2:332900;1:562000;2:693900"`.
  `<dose>` is 1 for a first round (R01) and 2 for a second or later
  round (R02+), the blocks run in campaign order with R01 before R02
  inside a campaign (campaign numbers follow administration order),
  consecutive rounds of the same dose are merged, and `<weight>` is the
  doses administered in the block (a campaign-round listed more than
  once counts once, with its reported doses). Weights are relative:
  [`est_vaccination_rate`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/est_vaccination_rate.md)
  splits the shipped doses across the blocks in proportion to them, so a
  single block is written with weight 1. `NA` when the round is unknown.

- `round_basis`:

  How the sequence was obtained: `"rounds"` (every round reports its
  doses, or all rounds are of one dose so the counts are not needed),
  `"rounds_imputed"` (the request has first and second rounds but some
  round dose counts are unreported; each unreported round carries the
  mean reported round of the request, or all rounds weigh equally when
  none is reported) or `"unknown"` (no Round event with a `C##-R##`
  identifier: the doses count as first doses downstream).

This function performs the following steps:

1.  **Load and Transform GTFCC Data**: - Reads the scraped GTFCC data
    from ees-cholera-mapping repository. - Transforms event-based
    structure (Request, Decision, Delivery, Round events) to
    request-based structure.

2.  **Map to WHO Format**: - Aggregates events by request ID to extract
    doses requested, approved, and shipped. - Uses Delivery event dates
    as campaign dates. - Records each delivery's date and doses, and
    attributes the request's doses to first and second rounds from its
    Round events. - Converts country names to ISO codes for consistency.

3.  **Infer Missing Campaign Dates**: - Computes the delay between
    decision and campaign dates. - Infers missing campaign dates based
    on the mean delay where possible. - Fixes missing decision dates for
    grouped request numbers.

4.  **Validate and Filter Data**: - Ensures all rows have valid campaign
    dates. - Removes rows where `doses_shipped` is zero or missing. -
    Filters for MOSAIC countries only.

5.  **Save Processed Data**: - Writes the cleaned and processed dataset
    to a CSV file for further modeling.

## Examples

``` r
if (FALSE) { # \dontrun{
# Example usage
PATHS <- list(
  MODEL_INPUT = "path/to/model/input"
)

processed_data <- process_GTFCC_vaccination_data(PATHS)
} # }
```
