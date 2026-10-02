# Process Combined Weekly and Daily Cholera Surveillance Data with Truly Square Data Structure

This function reads the processed weekly cholera data from WHO, JHU, and
supplemental (SUPP) sources, labels each record by its source, combines
them (removing duplicate country-week entries according to the specified
source preference), creates a truly square data structure with all
country-week combinations from min to max date across the entire
dataset, and downscales the combined weekly totals to a daily time
series using
[`MOSAIC::downscale_weekly_values`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/downscale_weekly_values.md)
with integer allocation.

## Usage

``` r
process_cholera_surveillance_data(PATHS, include_ai = FALSE)
```

## Arguments

- PATHS:

  A list of file paths. Must include:

  - **DATA_WHO_WEEKLY**: Directory containing
    `cholera_country_weekly_processed.csv` from WHO.

  - **DATA_JHU_WEEKLY**: Directory containing
    `cholera_country_weekly_processed.csv` from JHU.

  - **DATA_SUPP_WEEKLY**: Directory containing
    `cholera_country_weekly_processed.csv` from supplemental source (may
    include extra columns).

  - **DATA_CHOLERA_WEEKLY**: Directory where the combined weekly output
    will be saved.

  - **DATA_CHOLERA_DAILY**: Directory where the combined daily output
    will be saved.

  - **DATA_WHO_ANNUAL** (optional): Directory containing
    `who_afro_annual.csv`, the WHO annual totals that imputed rows are
    reconciled against (see Details).

- include_ai:

  Logical (default `FALSE`). When `TRUE`, reads the AI-mined processed
  file (`DATA_AI_WEEKLY/cholera_country_weekly_processed.csv`, produced
  by
  [`process_AI_cholera_data`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/process_AI_cholera_data.md))
  as a fourth source, using its per-row `confidence_weight` and
  `disaggregation_method`. When `FALSE`, only WHO/JHU/SUPP are merged.
  If `include_ai = TRUE` but the AI file is missing/empty, a warning is
  emitted and the merge proceeds with the three direct sources.

  The output always carries a `confidence_weight` and a
  `disaggregation_method` column (stable schema regardless of this
  flag): direct-source observations (WHO/JHU/SUPP) get
  `confidence_weight = 1.0` (fully trusted) and
  `disaggregation_method = NA`; AI rows keep their per-row values; and
  square-grid cells with no observation get `confidence_weight = NA`.

## Value

Invisibly returns `NULL`. Side effects:

- Reads weekly CSVs from all three sources and adds a `source` column.

- Cleans rows with missing key grouping fields (iso_code, year, week).

- Harmonizes columns by taking the union across all sources, NA-filling
  any column missing from a given source (so source-specific columns are
  never silently dropped).

- Applies the cross-source rules (AI aggregates, WHO multi-week windows)
  and deduplicates by `iso_code` and `date_start` (the actual week
  Monday, robust to year-boundary week-1 collisions): observed beats
  reconstructed beats imputed, then the fixed priority WHO \> JHU \> AI
  \> SUPP (see Details), and adds `source_deaths`; then applies the
  curated corrections and limits imputed rows to the gap left by the WHO
  account.

- Saves the adjustment log to
  `PATHS$DATA_CHOLERA_WEEKLY/cholera_surveillance_weekly_adjustments.csv`.

- Creates truly square data structure with all country-week combinations
  from min to max date (missing data = NA).

- Saves the combined weekly data to
  `PATHS$DATA_CHOLERA_WEEKLY/cholera_surveillance_weekly_combined.csv`.

- Applies the trust-tier gate before downscaling: only weeks tagged
  `disaggregation_method` `assumed_zero` (surveillance silence, a pure
  assumption) are NA-blanked. `observed`, `documented_zero`, direct
  WHO/JHU/SUPP rows, AND `fourier_*` (synthetic reconstructions of real
  annual/quarterly totals) all reach the daily fit target carrying their
  per-week `confidence_weight` (lower for fourier, ~0.4-0.5), which
  [`calc_model_likelihood()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_model_likelihood.md)
  consumes as a per-observation weight – so low-confidence reconstructed
  weeks inform the fit at reduced weight rather than being dropped.

- Downscales weekly `cases` and `deaths` to daily counts, preserving
  square structure (keeping days with NA), and carries `source`,
  `source_deaths`, `disaggregation_method`, and `confidence_weight` to
  each daily row (the weight is replicated constant across the week,
  never divided).

- Saves the combined daily data to
  `PATHS$DATA_CHOLERA_DAILY/cholera_surveillance_daily_combined.csv`.

## Details

Duplicate country-week entries across sources are resolved by selecting
one whole row per week: a row carrying a count beats an empty one; then
the trust tier decides, whatever the sources – an observed row
(WHO/JHU/SUPP, or AI `observed`/`documented_zero`) beats a reconstructed
one (a WHO multi-week report spread over the weeks it covers,
`who_catchup_*`, see
[`process_WHO_weekly_data`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/process_WHO_weekly_data.md)),
which beats an imputed one (AI `fourier_*` or any other modelled
method); a row with a case count beats a deaths-only row; and the fixed
priority **WHO \> JHU \> AI \> SUPP** decides the rest. When the
selected row has no death count, the deaths of the highest-priority
other observed row reporting the same case count that week are used (the
same report, compared after half-up rounding) and the week keeps the
lower of the two rows' `confidence_weight`; `source_deaths` names the
source of each death count.

Four cross-source rules keep one outbreak from being counted twice:

1.  **AI aggregates.** An AI `observed` week of at least 20 cases, in a
    week no direct source (WHO/JHU/SUPP) reports a case count for, is an
    aggregate mislabelled as one week, and is dropped, when it is at
    least five times every direct-source count in the four weeks either
    side (two or more of them) – Nigeria 2023 week 21 carries 1,851
    cases, the year-to-date total of the WHO weeks before it – or when
    it is within 15% of a WHO weekly cumulative of its year (the
    year-to-date total before it, or the year's total) with a WHO week
    reporting cases within four weeks of it: Congo 2023 week 29 carries
    63 cases, the running total of the outbreak whose 69 cases the WHO
    dashboard reports in weeks 30 and 34.

2.  **WHO multi-week windows.** Inside the window of a WHO multi-week
    report the report accounts for every week. A non-WHO observed row
    that repeats the dashboard's positive as-published value for its
    week, or the report total, is a copy of the dashboard and is
    dropped. When another source reports a positive count for every week
    of the window, the WHO total is redistributed in proportion to those
    counts (`who_catchup_shaped`, confidence 0.9); otherwise the even
    spread stands. A window shaped by a curated epidemic curve
    (`who_catchup_curated_shaped`) keeps its curve. All non-WHO rows in
    the window are then dropped, so a window never mixes the WHO total
    with another source's partial weeks.

3.  **Curated corrections.** The surveillance curation table
    (`inst/extdata/surveillance_curation.csv`) lists corrections the
    rules cannot derive, each with its evidence and source: WHO windows
    dated by an outbreak report, and timed by its documented epidemic
    curve where there is one (South Africa 2023; anchors in
    `inst/extdata/surveillance_curation_shapes.csv`), applied by
    [`process_WHO_weekly_data`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/process_WHO_weekly_data.md);
    documented absences of cholera, whose imputed weeks are emptied
    (`drop_imputed`: Angola 2023, South Sudan May 2023 - September
    2024); and contested imputed years that are kept but listed
    (`flag_imputed`: Burkina Faso 2025).

4.  **Imputed rows against the WHO account.** AI `fourier_*` rows spread
    an annual (or other multi-week) total over every week of its span,
    and the observed weeks that later overwrite part of that span are
    not netted out, so the surviving imputed rows can duplicate cases
    those weeks already report (Ghana 2024: 937 imputed cases in
    April-August beside the 4,618 WHO-reported cases of an outbreak that
    began on 4 October), or spread a total the WHO account contradicts.
    Per country and ISO year with a WHO account \\A\\, with observed
    plus reconstructed cases \\O\\ and imputed cases \\I\\, the imputed
    rows may only fill the gap to the account: they keep \\\min(I,
    \max(0, A - O))\\ cases, cases and deaths scaled by the same factor.
    Rows are emptied (NA) when less than one case is left in all, and
    individually when rescaled below half a case (the integer daily
    downscale would make them zero-case weeks that keep their weight:
    Cote d'Ivoire 2025 had 8 cases left over 40 weeks). The account is
    the WHO annual total (`DATA_WHO_ANNUAL/who_afro_annual.csv`) or, for
    a country-year without an AFRO annual row, the positive year-to-date
    total of its WHO weekly rows (Somalia 2026: the AI spread the WHO
    epidemiological update's 233 cases, which the three WHO weekly rows
    already report). Skipped, with a message, when
    `PATHS$DATA_WHO_ANNUAL` is not set.

Every week these rules (or the WHO spreading) change is listed, with its
before and after values and the evidence, in
`DATA_CHOLERA_WEEKLY/cholera_surveillance_weekly_adjustments.csv`,
including imputed rows a WHO window supersedes and the flagged weeks of
`flag_imputed` (listed unchanged).
