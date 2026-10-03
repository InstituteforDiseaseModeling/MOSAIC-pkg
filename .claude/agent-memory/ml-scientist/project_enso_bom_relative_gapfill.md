---
name: enso-bom-relative-gapfill
description: enso-data NMME compile gap-fills NOAA's missing last month with BOM RELATIVE Nino indices (rnino_*), creating a ~1 degC Nino4 dip in Aug-Sep 2026; timing-dependent (v0.100.1 panel used NMME anchors instead); also moves ENSO3/IOD -> flood GAM forecast inputs
metadata:
  type: project
---

Found during the v0.101.0 psi refit (2026-10-01). The input was enso-data 3729c87 as absorbed in MOSAIC-data 3636b57.

**Mechanism.** `compile_noaa_nmme.py` anchors monthly values on the 1st of each month, takes priority NOAA historical > BOM observed > NMME forecast, then interpolates linearly by day. `value_adj` uses one constant mean-bias shift per (variable, source).
- NOAA PSL ended at 2026-08, so the 2026-09-01 anchor fell to BOM.
- BOM's `rnino_4`/`rnino_3` are RELATIVE indices (the tropical-mean anomaly is removed). In 2026, NOAA minus BOM for Nino4 was +0.86/+1.01/+0.94 for Jun/Jul/Aug, but the constant shift was only +0.37.
- The result: the Nino4 Sep-01 anchor was 0.49 instead of 1.876 (the NMME counterfactual, reproduced exactly with enso-data's own baseline function), and monthly means of Aug 0.90 / Sep 1.28 instead of 1.57 / 2.00.

**Timing-dependent.** The v0.100.1 panel used NMME Sep-01 anchors for all four indices, because BOM's September monthly did not exist yet. Its ENSO4 weeks 2026-W31..W40 equal the corrected values exactly. The same pipeline produces a dip or no dip depending on the run date.

**Other indices.**
- ENSO34: BOM 2.78 vs NMME 2.81, so harmless.
- IOD: BOM 0.69 vs NMME 0.96. BOM's IOD is an observation of the same index, which is the documented purpose of the gap-fill ("for NOAA IOD lag"), so this one is legitimate.
- ENSO3: BOM 3.82 vs NMME 3.04 (relative index).
- ENSO3 and IOD are not v7.3 LSTM features. They do feed the flood GAM (s(ENSO3), IOD lags 8/16/24), and through it the v7.3 flood features.

**Observed truth check.** The NOAA CPC weekly OISST file (`https://www.cpc.ncep.noaa.gov/data/indices/wksst9120.for`) is fetchable with curl; WebFetch truncates it to 2012. It shows Nino4 SSTA flat at 0.8-1.1 through Aug-Sep 2026.
- PSL minus CPC for Nino4 is stable (+0.23 to +0.36 over Mar-Aug), which puts a PSL-consistent Sep at 1.25-1.35. That is 55-62% of the way from the BOM anchor to the NMME anchor, so the NMME fill over-corrects by about 0.5.
- For Nino3.4 the PSL-CPC offset swings from +0.24 to -0.69, so it cannot be used.
- For ENSO3, the BOM fill is closer to observations than NMME.

**Why:** the dip is invisible in monthly summaries unless you compare against the previous panel, and the BOM file name `rnino` is the only clue that the index definition differs.
**How to apply:** before any psi refit, check the data_source of the last 2-3 monthly anchors per index in `compiled_ENSO_NMME_daily.csv`. Treat a BOM "observed" anchor on ENSO3/ENSO4 as suspect. The proper fix is upstream in enso-data (data-engineer): restrict the BOM gap-fill to IOD, or rescale relative indices. Scripts: `MOSAIC-pkg/claude/v0101_rebuild/psi/nino4/nmme_gapfill_anchor.py` and `scripts/build_nino4_C.R`. The shipped psi C3 trained on the NMME-anchor variant (enso_C; bundle MOSAIC-data `processed/psi_provenance/v0.101.0_C3/`) while canonical `processed/ENSO` keeps the BOM dip, so a default refit reverts to it ([[psi-refit-v0101-c3]]). Related: [[psi-refit-v0100]], [[psi-refit-v0101]].
