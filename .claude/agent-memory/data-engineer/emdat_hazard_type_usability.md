---
name: emdat-hazard-type-usability
description: Per-EMDAT-hazard-type usability as a weekly GAM active-label (AFRO40 extract 2026-05-28); which types can be EMDAT-event-labeled vs need a climate proxy (drought=SPEI)
metadata:
  type: project
---

Audit of the AFRO-40 EM-DAT extract (`public_emdat_custom_request_2026-05-28_*.xlsx`,
raw at `MOSAIC-data/raw/EMDAT`) for a hazard-covariate redesign widening beyond the
current `Disaster Type=="Flood"` filter. Active country-WEEKS = the GAM label unit
(unique iso×ISO-year×ISO-week after process_EMDAT_data date-construction, >=2000).

**Per-type usability verdict:**
- **Flood** (897 ev, 3317 wk, span→2026-05, 40 ISOs): the current label. dur median 6d,
  only 5% >90d. USABLE (already the label).
- **Storm/Tropical cyclone** (47 ev, 70 wk, →2026-02, 8 ISOs) + **Storm surge** (2 ev, 4 wk):
  combined = 49 ev / **74 wk**, 9 ISOs, dur median 2d, crisp dates, 0 events >90d.
  USABLE as its own cyclone GAM/label — dense enough (cf. prior 62-added-positive finding).
- **Drought** (149 ev, **6948 wk**, →2025, 30 ISOs): NOT usable as an EMDAT weekly label.
  dur median **344d**, mean 328d, max 1462d; 71% >90d, 73 events >365d (multi-year:
  84 of 149 have EndYear>StartYear, up to +4yr); 61 missing End Month. Peak **18 of 40
  countries simultaneously "drought-active" in one week**, mean 5.2 concurrent — a
  near-constant background wash, not a discrete event. -> MUST come from a climate index
  (SPEI/SPI), not EMDAT events.
- **Mass movement (wet)** (70 ev; Landslide-wet 67 + Mudslide 3; 83 wk, →2026, 17 ISOs):
  crisp dates (dur median 1d) but flood-correlated/secondary. Labelable but sparse and
  largely redundant with Flood — treat as flood-adjacent, low marginal value.

**Storm subtype triage (157 Storm events) — water-hazard vs wind/hail-only:**
- INCLUDE (water): Tropical cyclone 47, Storm surge 2. (=49, the cyclone label)
- AMBIGUOUS: Storm (General) 43, Severe weather 35 — mixed wind+rain; NOT reliably
  flood-producing; excluding is the clean choice.
- EXCLUDE (wind/hail/electrical): Lightning/Thunderstorms 20, Tornado 5, Hail 4,
  Blizzard/Winter storm 1.

**ISO coverage holes (caps any EMDAT signal regardless of type filter):**
- Under-covered floods (<=5 events): GNQ 1, ERI 2, GAB 2, SWZ 3, GNB 4.
- Cyclone-exposed but thin: SWZ (3 flood-active wk, 1 cyclone), ZWE (37 wk, 7 cyclone).
- MOZ carries 22 of 49 AFRO cyclone events; ZWE 7, MWI 6, SOM 6, ZAF 3, TZA 2, COD/GMB/SWZ 1.
- **MDG & COM excluded entirely** (not in AFRO 40) despite MDG being the richest cyclone
  country (55 TC events raw) — see [[emdat-flood-only-filter-drops-cyclones]].

**Provenance/latency:** extract recency adequate for the fast hazards — latest start-year:
Flood 2026, Tropical cyclone 2026, Storm/Severe weather 2026, Mass movement 2026, Drought 2025.
Wind-only subtypes stale (Tornado 2010, Hail 2022, Blizzard 2002) but those are excluded anyway.
No type-specific reporting LAG that would cap the cyclone label.

**Redesign conclusion:** Flood + Cyclone(TC+surge) can each be EMDAT-event GAM labels.
Drought CANNOT (needs SPEI/SPI climate index). Mass-movement is optional/redundant.
Storm General + Severe weather should stay excluded (not reliably water-producing).
Handoff: psi/LSTM feature construction = ml-scientist; prior impact = disease-modeler.
