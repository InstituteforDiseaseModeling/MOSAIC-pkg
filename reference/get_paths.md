# Generate Directory Paths for the MOSAIC Project

The `get_paths()` function generates a structured list of file paths for
different data and document directories in the MOSAIC project, based on
the provided root directory.

## Usage

``` r
get_paths(root = NULL)
```

## Arguments

- root:

  A string specifying the root directory of the MOSAIC project; if
  `NULL`, `getOption("root_directory")` (see
  [`set_root_directory`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/set_root_directory.md)).

## Value

A named list of paths, all built with `file.path(root, ...)`:

- ROOT:

  The root directory.

- DATA_RAW, DATA_PROCESSED:

  `MOSAIC-data/raw` and `MOSAIC-data/processed`.

- DATA_SCRAPE_WHO_VACCINATION:

  `MOSAIC-data/raw/WHO/vaccination`.

- DATA_SCRAPE_WHO_WEEKLY, DATA_SCRAPE_GTFCC:

  WHO AWD and GTFCC scrapes under `ees-cholera-mapping/data/cholera/`.

- DATA_DEM, DATA_EMDAT_RAW, DATA_IDMC_RAW:

  Raw inputs under `MOSAIC-data/raw/` (`DEM`, `EMDAT`, `IDMC`).

- DATA_SHAPEFILES, DATA_SIMILARITY_MATRIX, DATA_ELEVATION, DATA_ENSO,
  DATA_OAG, DATA_UNICEF, DATA_WORLD_BANK, DATA_DEMOGRAPHICS, DATA_WASH,
  DATA_SYMPTOMATIC, DATA_IMMUNITY, DATA_SUSPECTED,
  DATA_VACCINE_EFFECTIVENESS:

  Processed data under `MOSAIC-data/processed/` (`shapefiles`,
  `similarity_matrix`, `elevation`, `enso`, `OAG`, `UNICEF`,
  `world_bank`, `demographics`, `WASH`, `symptomatic`, `immunity`,
  `suspected_cases`, `vaccine_effectiveness`).

- DATA_CLIMATE, DATA_CLIMATE_DAILY:

  `MOSAIC-data/processed/climate/weekly` and `.../climate/daily`.

- DATA_WHO_ANNUAL, DATA_WHO_WEEKLY, DATA_WHO_DAILY:

  `MOSAIC-data/processed/WHO/` `annual`, `weekly`, `daily`.

- DATA_JHU_WEEKLY, DATA_JHU_DAILY:

  `MOSAIC-data/processed/JHU/weekly` and `.../daily`.

- DATA_SUPP_WEEKLY, DATA_SUPP_DAILY:

  Both `MOSAIC-data/processed/SUPP/daily`: the weekly supplemental file
  `cholera_country_weekly_processed.csv` is written and read there.

- DATA_CHOLERA_WEEKLY, DATA_CHOLERA_DAILY, DATA_AI_WEEKLY:

  Combined surveillance under `MOSAIC-data/processed/cholera/`
  (`weekly`, `daily`, `ai/weekly`).

- DATA_GTFCC_VACCINATION:

  `MOSAIC-data/processed/GTFCC/vaccination`.

- DATA_EMDAT, DATA_IDMC:

  `MOSAIC-data/processed/EMDAT/weekly` and `.../IDMC/weekly`.

- ENSO_DATA_REPO, OPEN_METEO_REPO, AI_CHOLERA_REPO:

  Sibling repositories `enso-data`, `open-meteo-pipeline`,
  `ai-cholera-data-mining`.

- MODEL_INPUT, MODEL_OUTPUT:

  `MOSAIC-pkg/model/input` and `MOSAIC-pkg/model/output`.

- DOCS_FIGURES, DOCS_TABLES, DOCS_PARAMS:

  `MOSAIC-docs/figures`, `tables`, `parameters`.

## Details

This function helps organize the directory structure for data and
document storage in the MOSAIC project by generating paths for raw data,
processed data, figures, tables, and parameters. The paths are returned
as a named list and can be used to streamline the access to various
project-related directories.

## Examples

``` r
if (FALSE) { # \dontrun{
root_dir <- "/{full file path}/MOSAIC"
PATHS <- get_paths(root_dir)
print(PATHS$DATA_RAW)
} # }
```
