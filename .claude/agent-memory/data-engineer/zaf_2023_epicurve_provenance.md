---
name: zaf-2023-epicurve-provenance
description: ZAF 2023 timing evidence + decisions -- sitrep #5 Fig 5 onset curve (1,271 = NDoH 1,073 susp + 198 conf) shifted +2d to report dates (sitrep #4 notification lag), Karachi case 14+2 Jul, own report-dated deaths curve (national = Gauteng Hammanskraal + Benoni + Free State Parys death from 25 May); count gap 1,272 vs WHO 1,390; access notes
metadata:
  type: reference
---

Evidence behind ZAF-2023-AAR (final in MOSAIC fix/v0101-trust 9ec46e414, MOSAIC-data 922ef89):

- **Cases: WHO "Multi-country outbreak of cholera, External situation report #5" (4 Aug 2023),
  Figure 5 right** -- daily suspected + confirmed by SYMPTOM ONSET, as of 15 July (page 8;
  who.int/docs/.../20230803_multi-country_outbreak-of-cholera_sitrep-5.pdf). Digitized from the
  embedded raster: 1,271 cases = exactly NDoH's 1,073 suspected + 198 confirmed of 4 July (the
  NDoH phrase "1073 suspected ... of which 198 confirmed" is loose: categories are disjoint).
- **Report dating (coordinator decision, red-team EVID-02):** WHO rows are report-dated and the
  model has ONE global delta_reporting_cases, so onsets are shifted +2 days (cumulative-curve SSE
  vs sitrep #4 Fig 2 notification curve: 0.143/0.040/0.013/0.060 at 0-3 d) before Mon-Sun binning.
  Rows 15/22/29 May: 220/432/249. The imported Karachi case (NDoH 25 Jul: onset 14 Jul, admitted
  18 Jul, positive 24 Jul) is placed by the same rule on 16 Jul (W28) -> curve 1,272.
- **Deaths: own report-dated national curve** (cumulative_deaths column): 0 to 22 Feb; 1 announced
  23 Feb (Benoni; News24/SAnews); 1 as of 15 May (sitrep #3); Gauteng DoH Hammanskraal counts +1
  Benoni: 11 (21 May, Inside Metros), 16 (22 May, Daily Maverick), 21 (24 May, SAnews 25 May),
  official national toll 24 on 27 May (NDoH via The Citizen 27 May: "latest two fatalities from
  Gauteng"); 25 on 28 May = 23 Hammanskraal (SAnews 29 May) + Benoni + the FREE STATE death of
  25 May (33-yr-old Vredefort woman, Parys Hospital; News24 25 May, OFM 9 Jun); the first
  Mpumalanga death (30 May) falls after; 31 as of 6 Jun (NDoH via SAnews 8 Jun -- the red-team's
  "31 by 8 Jun" is the publication date); 38 as of 15 Jun (sitrep #4); 43 at the 25 Jun report
  and 47 as of 4 Jul (NDoH 5 Jul: "4 new suspected deaths since 25 June"). Sitrep #5's 44 as of
  9 Jul trails NDoH -> not used. Weekly: W8 1, W20 10, W21 14, W22 5, W23 5, W24 5, W25 3, W26 3,
  W27 1. Press counts in May are HAMMANSKRAAL-only: add Benoni AND (from 25 May) the Free State
  death for national. Round 3 (2026-10-01): I first shipped 28 May = 24 (FS death missed; the
  coordinator's verifier caught it); W21 13 -> 14, W23 6 -> 5, totals/peaks/psi untouched.
- **Attribution:** "1 February" is NDoH's (5 Jul statement), WHO sitreps say 29/01/2023; the AAR
  page has NO February date (only 165 days, 18 Jul imported case, 1,380/47 by 31 Jul).
- Count gap: NDoH 1,272 (1,073 + 199) vs WHO 1,380 (AAR) / 1,390 (dashboard W35); the extra WHO
  suspected cases have no dates -> curve rescaled 1,390/1,272.
- JHU cholera DB UID 21415 = WHO sitrep cumulative series by REPORT date (6, 11, 122, 924, 1,274,
  1,388): do not use as an onset shape.
- Access: iris.who.int unreachable/403 (AFRO OEW/monthly bulletins); who.int/docs and
  afro.who.int/sites/default/files PDFs, health.gov.za PDFs, sanews.gov.za curl-able.

Anchor consequence: ZAF's p99 is ~the 3rd-largest of 181 trusted weeks: cp99c 52 (flat) -> 241.6
(round 1 shape) -> 225.8 (report-dated Mon-Sun); cp99r 0.0826 -> 0.3827 -> 0.3577 (x41 vs the
v0.100.1 floor). Related: [[surveillance-curation-table]], [[who-weekly-w53-quirk]],
[[suitability-target-anchor-provenance]].
