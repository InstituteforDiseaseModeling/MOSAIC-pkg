---
name: ai-fourier-full-total-double-count
description: AI fourier rows spread the FULL annual total over all span weeks and higher-priority weeks overwrite part without netting -> double counts; R3 is now a GAP rule (keep min(I, A-O)) with WHO-weekly-YTD fallback, sub-0.5 residue drop and 0.01 rounding tolerance; true scale ~445k pre-2023 (red-team said 89k)
metadata:
  type: reference
---

Mechanism (ai-cholera-data-mining py/build_weekly_timeseries.py `process_country`): each
non-weekly row's `sch` is multiplied by template weights normalised over ALL eligible weeks
of its span, then WHO weekly rows (higher priority) overwrite some weeks in `try_insert`.
The overwritten share is never subtracted, so surviving fourier mass duplicates the WHO
weeks (ZAF 2023 ramp, GHA 2024 937, SOM 2026 212 = re-spread of the WHO epi-update 233).
Multi-year cumulatives also land in one year (ZWE 2008 122k vs WHO 60k; AGO 2007 75k vs
18k; ZAF 2002 54k vs 10k; KEN 2014 6,292 vs 35).

**R3 as of feat/v0101-data (2026-10-01, MOSAIC-data after fcf7113):** per (iso, ISO year of
the week's Thursday), keep = min(I, max(0, A - O)) imputed cases, cases/deaths scaled alike.
A = AFRO annual total, else the WHO weekly year-to-date total when POSITIVE (zero WHO rows
cannot tell no cases from no report). Rows rescaled below 0.5 case are emptied (the integer
daily downscale makes them weighted zero-weeks: CIV 2025 279 zero days -> NB k Inf). Excess
below 0.01 case is skipped: fourier spreading exactly A sums a few thousandths above it
(4-decimal storage), and without the tolerance the residue rule empties those rows (498 vs
349 country-years).

**Scale gotcha:** the red-team's 89k estimate only covered country-years where the old
O-capped rule already fired. The gap formula also caps fourier-only years (O = 0) at A:
349 country-years (old 68), 280 of them fourier-only; pre-2023 imputed 3.77M -> 3.32M
cases; 2023+ fit target barely moves. Gap rule leaves the AFRO annual-vs-weekly layer
difference to imputed rows ANYWHERE in the year (SSD 2024: 562 inside the listed weeks,
spread into a documented-zero Jan-Sep) -> documented absences need curation
([[surveillance-curation-table]]).

Also: AI `observed` rows can be cumulatives (NGA 2023-W21 YTD; COG 2023-07-17 63 = JHU-db
running total of a WHO-reported 69): R4 (>=5x neighbours) + R4b (within 15% of WHO weekly
YTD-before or year total, a positive WHO week within 28 d). The processed AI snapshot
(2026-09-18) predates the AI epi-week fix; never re-run process_AI_cholera_data against an
in-flight AI run; read AI row-level files via `git show 7e16c61:data/<ISO>/...`.

How to apply: before trusting a fourier week, check the same country-year's observed weeks
and the AI's own documented-absence records. Related: [[who-multiweek-catchup-reports]],
[[surveillance-revision-is-source-precedence]].
