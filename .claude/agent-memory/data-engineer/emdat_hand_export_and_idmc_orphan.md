---
name: emdat-hand-export-and-idmc-orphan
description: download_EMDAT_data has no local-file ingest mode (hand portal exports have no sanctioned raw/ path); IDMC panels have zero package consumers; shared processed/ is written by parallel worktrees
metadata:
  type: project
---

Found 2026-10-01 during the data-refresh discovery pass (laptop).

- **EMDAT hand exports:** download_EMDAT_data(source = api|portal|dataverse) has NO local-file
  argument. The portal-cookie error text and raw/EMDAT/README tell the user to *move the browser
  file into raw/ by hand + add a PROVENANCE row*, which conflicts with the CLAUDE.md "only
  download_* writers" rule. Don't hand-copy; to evaluate, copy into claude/ and run
  process_EMDAT_data with PATHS$DATA_EMDAT_RAW / PATHS$DATA_EMDAT overridden (both are plain PATHS
  fields). A fix would be a `source = "file"` mode reusing .emdat_download_portal's validation.
- **EMDAT panel end != extract date:** panels end at last *flood/cyclone* event end (2026-06-29)
  even though the extract runs to 2026-09-11 (later rows are Road/Water/etc.).
- **IDMC panels are orphans:** processed/IDMC/weekly/displacement_*_country_weekly.csv have no reader
  in R/ or data-raw/ (only get_paths defines DATA_IDMC). Refreshing them changes nothing downstream.
- **Shared processed/ hazard:** worktrees under MOSAIC-pkg/.claude/worktrees/ (e.g. survfix) run
  processors against the SAME get_paths() root, so they overwrite MOSAIC-data/processed/ in place.
  Check psi_suitability_config.json provenance.source_csv_md5 against the on-disk suitability panel
  to detect drift between the shipped psi and the panel.

**How to apply:** any refresh audit should first diff processed/ vs MOSAIC-data HEAD and attribute
uncommitted changes (check worktrees) before blaming upstream. Related: [[update-pipeline-raw-writer-compliance]],
[[emdat-flood-only-filter-drops-cyclones]].
