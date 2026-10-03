---
name: who-weekly-w53-quirk
description: WHO AWD epi weeks run MONDAY-SUNDAY (stamped with their Monday) but are NUMBERED like MMWR shifted one day -> genuine 2025-W53 dated 2025-12-29, 2026-W01 = 2026-01-05; I wrongly wrote "Sun-Sat MMWR" in round 1 (red-team EVID-01); fixed v0.100.0 dating + v0.101.0 week-of-date
metadata:
  type: reference
---

WHO `year/week` in ees-cholera-mapping/data/cholera/who/awd/cholera_country_weekly.csv are WHO
epiyr/epiwk. **Content period is Monday to Sunday**, stamped with that Monday (`date_wk`; WHO
dashboard note, hub item c33a803a619c476abb673d41c72a7d62: "The date corresponds to the first day
of the epi-week (from Monday to Sunday)"; WHO AFRO OEW bulletins agree: "Week 10: 3 - 9 March
2025"). Only the NUMBERING follows MMWR, shifted one day: week 1 begins the Monday after the
Sunday that starts MMWR week 1. Differs from ISO only when 4 Jan is a Sunday: WHO 2025 has a real
W53, and every WHO 2026 week sits 7 days later than ISOweek2date would place it.

**Mistake I made (2026-10-01 round 1, caught by red-team EVID-01):** I wrote "WHO weeks are MMWR
(Sun-Sat) stamped with the following Monday" and built `.who_week_of_date()` (pre-existing, from
68528392d) and `.curated_shape_weights()` on it, so Sunday onsets landed in the next row and NAM's
Sunday 2 Mar 2025 first case went to W10. Fixed in fix/v0101-trust a50d44c37: week of date = Monday
on/before; shapes bin Mon-Sun with Sunday anchors; NAM -> W09. `.who_epiweek_start()` (labels ->
Monday stamps) was always right. MOSAIC rows, daily downscale and likelihood week blocks are all
Mon-Sun.

v0.100.0 `process_WHO_weekly_data()` dates weeks with `.who_epiweek_start()`, keeps W53 as its own
row, errors on duplicate (iso,year,week), and keeps rows with one NA field. Consumers must join WHO
on date_start, never (year, week); compile_suitability_data still has a week<=52 filter (see
[[iso-week-labelling-convention]]). Related: [[who-field-semantics-gotchas]],
[[surveillance-curation-table]].
