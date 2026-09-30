---
name: who-weekly-w53-quirk
description: WHO AWD weekly labels are MMWR epi-weeks (not ISO): genuine 2025-W53 dated 2025-12-29 and 2026-W01 = 2026-01-05; fixed in v0.100.0 processor (no W52 fold, NA rows kept) and rebuilt 2026-09-30
metadata:
  type: reference
---

WHO `year/week` in ees-cholera-mapping/data/cholera/who/awd/cholera_country_weekly.csv are WHO
epiyr/epiwk on the MMWR calendar, stamped with the following Monday. Differs from ISO only when
4 Jan is a Sunday: WHO 2025 has a real W53 (27 raw countries, values distinct from W52/2026-W01),
and every WHO 2026 week sits 7 days later than ISOweek2date would place it.

v0.100.0 `process_WHO_weekly_data()` dates weeks with `.who_epiweek_start()`, keeps W53 as its own
row, errors on duplicate (iso,year,week), and keeps rows with one NA field (the old
`aggregate()` na.omit silently dropped SOM 2023-W03, NGA 2023-W21, ZMB 2023-W41, AGO 2025-W32).
Rebuild 2026-09-30 (MOSAIC-data c962d4a): 12 W52 values revised down (COD 2907->1313), 20 W53
rows added, 486 WHO-2026 rows re-dated +7d. Consumers must join WHO on date_start, never
(year, week); compile_suitability_data still has a week<=52 filter (see
[[iso-week-labelling-convention]]). Related: [[who-field-semantics-gotchas]].
