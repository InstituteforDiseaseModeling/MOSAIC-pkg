---
name: processed-refresh-audit-gotchas
description: Source revisions and processor quirks found auditing the 2026-09-18 update_mosaic_data() refresh — PSL ENSO moved to ERSSTv6, WDI urban/poverty revisions, WHO-annual tie-break, dbf timestamp noise, orphan outputs
metadata:
  type: reference
---

Found auditing the uncommitted 2026-09-18 processed/ refresh (committed 2026-09-29, MOSAIC-data 986f975..211f159).

- **ENSO history revised upstream.** psl.noaa.gov/data/correlation/nina{3,34,4}.anom.data now = ERSST v6, anomaly vs 1981-2010 (switched between enso-data NOAA pulls 2026-07-27 and 2026-08-10). vs old: ENSO34 mean +0.09, max |d| 0.77, SD 0.89->0.77. IOD (HadISST DMI) unchanged. A psi covariate shift, not a MOSAIC bug. enso-data compiled daily also has 28 ENSO3 rows (2026-11) with blank data_source (compile_suitability drops the column).
- **WDI 2026-07-13 vintage**: urban % (SP.URB.TOTL.IN.ZS) revised on 93% of MOSAIC-40 values (max 61%); GDP Angola rebased (+54%); poverty line rebase, see [[wb-poverty-line-redefinition]]. The WB API files carry a "Last Updated Date" line that differs between same-data pulls, so cmp them ignoring line 3.
- **process_WHO_annual_data tie-break**: dedup on (iso, year) by max coverage_days keeps the FIRST file on ties (alphabetical = OLDEST snapshot), so a WHO revision with no new weeks is ignored (ETH 2026: 53/1 kept over 50/2). process_CFR_data splits CIV into 2 rows ("Cote d'Ivoire" vs "Côte D’ivoire").
- **download_*_shapefile rewrites .dbf headers**: bytes 2-4 (last-update YYMMDD) only, so 55 files show as modified with no content change. Check with `cmp -l`, then `git checkout` them. Don't commit them.
- **Orphan outputs**: processed/mobility/D_traveltime_hours.csv (old unkeyed name; the current code uses D_traveltime_hours_agg<k>_<fun>.csv). make_priors_default reads demographics_mosaic_countries_2000_2024_annual.csv (Feb 2025), which has NO producer. Same class as [[wb-processed-filename-drift]].
- **Frozen suitability panels** (`*_suitability_data_frozen_<date>.csv`, ~224 MB) are not matched by .gitignore and exceed GitHub's 100 MB limit, so never `git add -A` in MOSAIC-data.
- EMDAT/IDMC panel end = last event end date, not the extract date.
- Mobility M_* outputs re-derive bit-identically. Trick: override `P$DATA_PROCESSED` to a temp dir, then re-run process_mobility_od_data + rake_mobility_od_to_tau without touching processed/.
