---
name: update-pipeline-raw-writer-compliance
description: v0.93.0 review — which download_* writers violate the dated/atomic/logged raw-snapshot rule, the two unlisted raw writers in the default plan, and the spurious est_mobility edge that blocks compile on dugong
metadata:
  type: project
---

Findings from the v0.93.0 production review (2026-09-29, report at
claude/review_v093/etl_report.md on the laptop). Re-verify before acting — may since be fixed.

- UPDATE 2026-10-01: WB, WPP, IDMC and mobility_od now DO append PROVENANCE.md (first rows written
  2026-10-01). WB/WPP/IDMC/mobility do NOT dedupe identical content (each run = new dated copy);
  only get_WHO_vaccination_data + process_WHO_annual_data skip byte-identical fetches.
- (2026-09-29, original) Only download_EMDAT_data appended a PROVENANCE row; the friction cache still logs nothing. EMDAT-api, WB and WPP write.csv straight to the final dated name (non-atomic);
  `.mosaic_download` tempfile() lives in tempdir() so cross-FS it falls back to file.copy into dest.
- IDMC + mobility snapshots are atomic per FILE not per SNAPSHOT; newest-wins resolvers accept a
  partial dated dir, and downloaders never throw, so the driver reports `ok` on total outage.
- Unlisted raw writers run by update_mosaic_data: download_country_DEM (re-downloads + overwrites
  raw/DEM every run — freshness code wrongly claims it skips existing) and get_WHO_vaccination_data
  (overwrites raw/WHO/vaccination/who_vaccination_data.csv).
- compile_suitability_data declares est_mobility as a dep but reads nothing from it; `mobility` pkg is
  NOT installed on dugong (nor mipfp/gdistance/malariaAtlas) → compile permanently BLOCKED there.

**Why:** these are the gaps between the CLAUDE.md snapshot rule and the code; they recur when new
downloaders copy the existing pattern.
**How to apply:** new download_* must temp-in-dirname(dest) → validate → rename → PROVENANCE row;
registry deps must match actual file reads. See [[newest-raw-mtime-resolver-hazard]],
[[wb-processed-filename-drift]].
