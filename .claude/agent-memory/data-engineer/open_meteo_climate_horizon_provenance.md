---
name: open-meteo-climate-horizon-provenance
description: How processed/climate/weekly/*.parquet is built (R-side ERA5+CMIP6 splice), the future-fill is a real climate-model projection not climatology, and what caps the horizon
metadata:
  type: reference
---

The weekly climate parquets MOSAIC consumes (`MOSAIC-data/processed/climate/weekly/{ISO}.parquet`)
are written by **`MOSAIC-pkg/R/process_open_meteo_data.R`**, NOT by the open-meteo-pipeline's own
`compile.py` (that writes a separate gitignored `compiled/weekly/climate_data_{MODEL}_{ISO}_weekly.parquet`).

Chain:
- Raw pull (sibling repo `~/MOSAIC/open-meteo-pipeline`): ERA5 historical (archive-api, ~5-day lag,
  `ERA5_LAG_DAYS=5`) + CMIP6 climate-model projections (default `MRI_AGCM3_2_S`, climate-api),
  year range `DEFAULT_CLIMATE_START_YEAR=2000`..`DEFAULT_END_YEAR` in `src/openmeteo/config.py`.
- R splice (`process_open_meteo_data.R:116-119`): ERA5 is primary; climate-model rows used ONLY for
  dates > ERA5's last date. Spatial mean over 30 grid pts, then daily + weekly (mean; SUM for
  `precipitation_sum` and `et0_fao_evapotranspiration_sum`).

FUTURE-FILL = real CMIP6 projection (option b), NOT climatology/persistence/forward-fill and NOT
open-meteo's 16-day forecast. Verified: future weeks carry a seasonal cycle AND interannual variation
(e.g. MOZ ISO-week-47 temp varies 24.2-29.6C across years; week-5 precip 24.7-51.1mm) — a climatology
would be identical across years. So extending the horizon adds real signal, unlike a flat fill.

HORIZON CONTROL: end date is set solely by the raw climate-model download ceiling
`DEFAULT_END_YEAR` (config.py). As of 2026-06 it is 2026 -> parquets end 2026-12-31. The R splice has
no end-date param. To extend (e.g. match NMME ENSO horizon 2027-02): bump `DEFAULT_END_YEAR` to 2027,
re-download `--api-type climate`, then `process_open_meteo_data(force=TRUE)`. CMIP6 extends to ~2050 so
data exists. Suitability panel end = min(climate horizon, ENSO horizon), so over-covering climate is safe.

Gotcha: stale older-generation files `MOSAIC-data/processed/climate/climate_data_{ISO}_weekly.parquet`
(Feb 2026) sit alongside the live `weekly/{ISO}.parquet` (Jun 2026) — the `weekly/` subdir is the live one.
Related: [[datestart_tolerance_findings]] (suitability panel coverage).
