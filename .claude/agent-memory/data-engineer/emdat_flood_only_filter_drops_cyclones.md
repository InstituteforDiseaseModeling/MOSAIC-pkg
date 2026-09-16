---
name: emdat-flood-only-filter-drops-cyclones
description: process_EMDAT_data filters Disaster Type=="Flood" ONLY, discarding 47 Tropical-cyclone Storm events (Chido/Dikeledi/Jude, Kenneth, Freddy) → flood covariate blind to cyclone-driven flooding
metadata:
  type: project
---

`R/process_EMDAT_data.R` line ~105 hard-filters the raw EM-DAT extract to
`Disaster Type == "Flood"`. In the MOSAIC AFRO extract this DISCARDS 157 `Storm`
events, of which **47 are `Storm / Tropical cyclone`** (across 2000-2026), the
exact mechanism behind cyclone-driven cholera outbreaks.

**Composition (raw extract, 1907 AFRO events):** Flood 897 (kept), Storm 157
(dropped, incl. 47 Tropical-cyclone + 2 Storm-surge), Drought 149, Epidemic 564,
etc. Cyclone/surge = 5.2% of the hydromet-flood universe by event count.

**Consequence for the emdat_flood_active target (and thus the imputed
emdat_flood_prob GAM):**
- A cyclone is only captured IF EM-DAT ALSO separately coded a `Flood` event for
  the same window. **CORRECTION to a prior belief: Idai (2019-W10) is the ONLY
  named major cyclone that is double-coded → active=1.** Chido, Dikeledi, Jude,
  **Freddy, Kenneth, Eloise, Gombe, Ana are ALL mislabeled 0** at their cyclone
  week. impute_flood_probability's docstring claim to "detect
  Idai/Kenneth/Eloise/Ana/Gombe/Freddy" holds only for precip-driven Flood twins
  in nearby weeks, NOT the cyclone weeks themselves.
- **Positive-label loss:** including Storm/Tropical-cyclone+Storm-surge adds 62
  NEW active country-weeks (12 of 74 cyclone weeks already double-coded). By
  cyclone-exposed country, % of hydromet-flood weeks currently mislabeled 0:
  ZWE 32.7% (18 added), SWZ 25%, MOZ 12.2% (23 added), MWI 5.3%, SOM 4.1%,
  ZAF 3.4%, TZA 1%, KEN 0%.
- **MDG & COM are NOT in `iso_codes_mosaic` (AFRO 40)** — filtered out ENTIRELY,
  not under-covered. MDG carries 84 events / 55 Tropical-cyclone events in the RAW
  extract (single richest cyclone country) but is dropped by the ISO filter, not
  the Disaster-Type filter. CORRECTION to earlier "no EM-DAT flood coverage"
  framing: data exists; the modeled-country set excludes it. No Disaster-Type
  widening recovers MDG/COM — needs adding them to the modeled ISO set.
- Chido/Dikeledi/Jude coded ONLY as Storm → active=0 every week Dec2024-Mar2025
  for MOZ → imputed emdat_flood_prob only 0.30-0.36 (Jude 0.36) vs 0.85 for
  climate-seasonal MOZ outbreaks. Covariate-SIGNAL gap, NOT a coverage cutoff.

**GAM A-vs-B refit (verified on the compiled panel, both fit in-session):**
Adding cyclone labels (B) vs Flood-only (A):
- `s(wind_speed_10m_max)` smooth: A edf 0.74 (near-shrunk-off, p=0.045) →
  B edf 1.99 (real nonlinear smooth, p=0.0007). The wind/cyclone proxy is
  NEUTERED under A and becomes informative under B.
- Predicted emdat_flood_prob at cyclone weeks (A→B): Freddy 0.365→0.563,
  Gombe 0.335→0.531, Eloise 0.459→0.585, Jude 0.360→0.555, Dikeledi 0.155→0.236,
  Kenneth 0.179→0.215, Chido 0.037→0.058 (Chido stays low even in B — far-north
  landfall diluted by national-mean climate → points to a SUBNATIONAL-resolution
  limit beyond the label fix).
- Rolling-year CV mean AUC ~flat (A 0.814 vs B 0.813): 56 added positives among
  2245 barely move aggregate discrimination, but materially lift cyclone-week
  predictions (the thing that matters). B is not worse — targeted gain.

**NOT a reporting-lag / coverage problem:** raw extract
`public_emdat_custom_request_2026-05-28_*.xlsx` (MOSAIC-data/raw/EMDAT) runs
through 2026; processed `floods_country_weekly.csv` panel extends to 2026-05-03.
The events are present in the raw file — just under the wrong Disaster Type for
the Flood-only filter.

**NGA contrast (Sept-2024 Borno/Alau dam):** that WAS coded as `Flood` (1.377M
affected, 321 deaths) → correctly fired `emdat_flood_active=1` Aug22-Sep15 2024,
imputed prob peaked 0.77, `emdat_flood_prob_12w_max` held 0.77 into late Oct then
decayed to ~0.001 by Dec. So NGA's flood signal is present but its 12w-max window
expires ~Nov; a late-2024 psi-flat window in Nov-Dec is the signal DECAY, not a
data gap.

**Fix options (data-engineer side):** widen the filter to include
`Disaster Type %in% c("Flood","Storm")` (or specifically the cyclone/storm-surge
subtypes) so the impute GAM's target sees cyclone flooding; would require
re-running process_EMDAT_data + compile_suitability_data + est_suitability
(psi rebuild). Alternative: a more timely flood/cyclone source. Handoff:
psi/LSTM feature construction is ml-scientist; prior impact is disease-modeler.

Related: [[open_meteo_climate_horizon_provenance]] (climate covariates confirmed
present + variable through 2026 for both windows — rules out a second artifact).
