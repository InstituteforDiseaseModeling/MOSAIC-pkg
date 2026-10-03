---
name: who-multiweek-catchup-reports
description: WHO AWD dashboard 0 means "no cases" OR "no report"; late/batched/YTD reports land in one week. Rule R1 spreads them; since feat/v0101-data the drop-ratio branch is capped at 4 reported zeros unless >=2 of the next 4 weeks are zero; curated windows override; peaks detector blanks to ZERO and misses humped outbreaks
metadata:
  type: reference
---

**Semantics.** The WHO AWD weekly feed (ees-cholera-mapping cholera_country_weekly.csv,
from 2023-W1) writes 0 for no report as well as no cases and books a late/batched report in
the week it arrives; a country added mid-year can enter its YEAR-TO-DATE total first. The
annual dashboard layer (who_afro_annual 2023+) aggregates the same feed separately (SSD 2024
annual 16,416 vs weekly 15,854) and lists a country only while it is on the dashboard.

**Verified catch-ups.** ZAF 2023-W35 1,390/47 = WHO AFRO AAR 1 Feb-31 Jul total; KEN 2023
W3/W17 (JHU Kenya MoH weekly); GHA 2024-W45 (GHS: began 4 Oct); NGA 2023 H2 batches
(JHU-db/UNICEF weekly counts are non-zero in the WHO zero weeks W35-W52); CIV 2025-W33
(IFRC 491 by 3 Aug vs WHO 389).

**Rule R1 (process_WHO_weekly_data).** Report >= 20, previous week silent in the same epi
year, and a fall: next two reported weeks zero (no reach cap) OR >= 2x max of next 4 reported
(boundary pinned by KEN 2023-W17 878 vs 2x437). The ratio-only branch may cover at most 4
reported zeros unless >= 2 of the next 4 reported weeks are zero (alternating = batch
reporting). Exemption keeps NGA W43 / TZA 2023-W28 / GHA 2025-W31 / KEN 2024-W20 / UGA
2025-W09; it loses NGA 2023-W52 (6 zeros, steady 2024 reporting) -> restored by curation.
Unreported weeks only if retrospective. Curated windows ([[surveillance-curation-table]])
replace the rule per report and may end before the report (ZAF: weeks after 31 Jul get 0);
since fix/v0101-trust ZAF is SHAPED by WHO's epicurve moved +2d to report dates (peak 432 in 22-28 May) with its own deaths curve, not flat 51-52/wk.

**Peaks gotchas.** est_epidemic_peaks blanks non-observed days to 0 (so windows count as
observed via `.surveillance_tier()`). The prominence test (8% of all-time max) misses
humped curves: GHA 2024-25 (humps to 75/day vs required 30/day) and CIV 2025 (two humps 14 d
apart after the W30-33 spread) are now hand-curated peaks (2024-12-08, 2025-07-07).

**Right-censoring.** Successive scrapes revise the newest weeks up by large factors; treat
the last 2-4 weeks of any scrape as provisional. MOSAIC-data surveillance last rebuilt
2026-10-01 on ees 780eb54 (04a6d0f: Mon-Sun weeks, report-dated ZAF, half-up spreads).

How to apply: when a WHO weekly value looks wrong, check the silent run before it and the
fall after it, then corroborate with JHU/AI row-level notes; the adjustments log lists every
changed week. Related: [[ai-fourier-full-total-double-count]], [[who-weekly-w53-quirk]],
[[who-surveillance-pipeline]].
