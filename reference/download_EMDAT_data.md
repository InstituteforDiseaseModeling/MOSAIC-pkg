# Download EM-DAT disaster events from the CRED GraphQL API

Queries the EM-DAT `public_emdat` GraphQL endpoint, pages through the
full result set, renames the API's snake_case fields to the
portal-export column names that
[`process_EMDAT_data`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/process_EMDAT_data.md)
expects, and writes a date-stamped CSV into `PATHS$DATA_EMDAT_RAW`. Also
appends a row to that directory's `PROVENANCE.md` ledger.

## Usage

``` r
download_EMDAT_data(
  PATHS,
  source = c("api", "portal", "dataverse"),
  api_key = NULL,
  cookie = NULL,
  url = NULL,
  file_date = NULL,
  year_start = 2000L,
  year_stop = NULL,
  classif = c("nat-hyd-flo", "nat-met-sto", "nat-cli-dro"),
  include_hist = TRUE,
  page_size = 1000L,
  snapshot_date = Sys.Date(),
  overwrite = FALSE,
  verbose = TRUE
)
```

## Source

EM-DAT, CRED / UCLouvain, Brussels. <https://www.emdat.be>. Licence CC
BY 4.0 – attribution required.

## Arguments

- PATHS:

  List of paths from
  [`get_paths`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/get_paths.md).
  Must include `DATA_EMDAT_RAW`.

- source:

  Which distribution to pull.

  `"api"`

  :   (default) The CRED GraphQL API. Current, CC BY 4.0, **requires a
      key**.

  `"portal"`

  :   The session-authenticated file the web portal mints after a custom
      request (`public.emdat.be/graphql/files/`). Current, CC BY 4.0,
      needs a browser **session cookie** rather than an API key.

  `"dataverse"`

  :   The UCLouvain Dataverse archive. Open, no credentials,
      MD5-verified – but a **static, heavily lagged** release and CC
      BY-NC-ND. See the section below before using it.

- api_key:

  EM-DAT API key for `source = "api"`. If `NULL` (default), read from
  the `EMDAT_API_KEY` environment variable.

- cookie:

  Session cookie string for `source = "portal"`, as taken from a browser
  signed in to <https://public.emdat.be>. If `NULL` (default), read from
  the `EMDAT_SESSION_COOKIE` environment variable.

- url:

  For `source = "portal"`, the exact download URL the portal produced.
  Overrides `file_date`. Paste it straight from the browser.

- file_date:

  For `source = "portal"`, the date stamp in the portal's filename, used
  to build
  `https://public.emdat.be/graphql/files/public_emdat_<date>.xlsx`.
  Defaults to today. The portal mints a file per request, so a guessed
  date will usually not exist – prefer `url`.

- year_start, year_stop:

  Inclusive year bounds passed to the API's `from` / `to` filters.
  `year_stop = NULL` (default) uses the current year.

- classif:

  Character vector of EM-DAT classification keys to request. Defaults to
  the three hazard families MOSAIC models: `"nat-hyd-flo"` (flood),
  `"nat-met-sto"` (storm, which carries the Tropical cyclone / Storm
  surge subtypes) and `"nat-cli-dro"` (drought). Pass `NULL` to request
  **all** classifications, matching the "all natural disasters" scope of
  the manual portal extracts.

- include_hist:

  Passed to the API's `include_hist` filter (`TRUE` by default) so
  historic records are not silently dropped.

- page_size:

  Rows per request (default 1000).

- snapshot_date:

  Date stamp for the output filename. Defaults to
  [`Sys.Date()`](https://rdrr.io/r/base/Sys.time.html).

- overwrite:

  If `FALSE` (default) and the target file already exists, the download
  is skipped.

- verbose:

  If `TRUE` (default), print progress and a summary.

## Value

Invisibly, the path to the written CSV.

## Getting an API key

The endpoint requires an `Authorization` header; an unauthenticated
request returns
`{"errors":[{"message":"Missing API Key, please provide a value for the header: Authorization"}]}`.
Keys are issued by CRED / UCLouvain via <https://public.emdat.be> (free
registration covers non-commercial use; whether that tier includes API
access is not stated in the public documentation – ask CRED). Store it
outside every git tree, e.g. in `/Users/<you>/MOSAIC/.env` as
`EMDAT_API_KEY=...`, and export it before calling.

## The two open alternatives, measured

Both were tested on 2026-09-17 against the committed panels:

- **HDX `emdat-country-profiles-<iso>`** – refreshed daily, but *annual
  aggregates*: one row per (Year, Country, Disaster Type, Disaster
  Subtype) with `Total Events` / `Total Affected` / `Total Deaths` and
  **no event start or end dates**. A country-week panel cannot be built
  from them at all. (This is the key difference from IDMC, whose HDX
  mirrors *are* event-level – see
  [`download_IDMC_data`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/download_IDMC_data.md).)

- **UCLouvain Dataverse** (`source = "dataverse"`) – open, event-level,
  and *structurally a drop-in*: same `EM-DAT Data` sheet, same 47
  columns, processes with no code changes. **But its data stops at
  2024-12-31** while being released 2026-04-30 – a 625-day lag as
  measured. Against the 2026-07-09 portal extract it is missing 102
  events from 2025 and 33 from 2026 (134 DisNo. absent), 36 AFRO floods
  and 3 cyclone/surge events from 2000 onward, and the named cyclones
  **Dikeledi (2025-01), Jude (2025-03) and Gezani (2026-02)**; only
  Chido (2024-12) is present. The resulting panel ends 2024-12-16 versus
  2026-04-27 – 71 country-weeks short per country. It is also **CC
  BY-NC-ND** ("unadapted form only"), which sits badly with deriving
  weekly panels; the portal and API distributions are CC BY 4.0.

**Verdict:** `"dataverse"` is a usable offline/reproducibility fallback
and a clean way to pin a historical build, but it is NOT a substitute
for the API or a portal extract in a pipeline that forecasts the current
year.

**Ranking safety.** The downloaded file is stamped with its DATA CUTOFF
(`public_emdat_dataverse_2024-12-31.xlsx`), not its release date,
because
[`process_EMDAT_data()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/process_EMDAT_data.md)
selects the newest extract by the filename-encoded date. Stamping by
release date (2026-04-30) would let this stale archive outrank a fresher
portal extract and silently truncate the panel. Verified: with both
files present the processor still selects the 2026-07-09 portal extract.

## Field mapping (UNVERIFIED – applies to source = "api" only)

The API path has **not** been executed against a live key, so the
API-to-portal column mapping in `.emdat_api_field_map()` is derived from
EM-DAT's published R API guide and the portal export schema, not from an
observed response.
[`process_EMDAT_data()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/process_EMDAT_data.md)
now hard-fails on a missing required column, so a mismatch surfaces
immediately rather than producing an empty panel. To inspect the real
schema, open the GraphiQL explorer at <https://api.emdat.be/> with your
key and adjust `fields` / the map as needed.

## See also

[`process_EMDAT_data`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/process_EMDAT_data.md)
to build the country-week panels.

## Examples

``` r
if (FALSE) { # \dontrun{
PATHS <- get_paths()

# Current data (needs a key)
Sys.setenv(EMDAT_API_KEY = "...")
download_EMDAT_data(PATHS)

# Open fallback -- warns loudly; data stops at 2024
download_EMDAT_data(PATHS, source = "dataverse")

process_EMDAT_data(PATHS)
} # }
```
