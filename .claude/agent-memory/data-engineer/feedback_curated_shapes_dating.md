---
name: feedback-curated-shapes-dating
description: Coordinator rules for curated surveillance timing (2026-10-01 round 2) -- date curves like the target (report dates, onset + measured lag), give deaths their own dated curve, verify week conventions against the source's own documentation, and attribute every date to the source that actually states it
metadata:
  type: feedback
---

Rules set by the coordinator after the red-team of the ZAF-2023 shape (fix/v0101-trust round 2):

1. A curated curve must be dated like the series it joins: WHO weekly rows are report-dated and the
   model has ONE global reporting delay (delta_reporting_cases), so an onset-dated epicurve is shifted
   by the measured onset-to-notification lag (ZAF: +2 d, from the notification-vs-onset curve SSE)
   before binning.
   **Why:** an onset-dated location inside a report-dated multi-location fit biases timing and the
   shared delay cannot absorb it per location.
2. Deaths get their own dated curve when dated death reports exist (report dates, no shift); a flat
   CFR along the case curve misdates the first and last deaths.
3. Week conventions come from the source's own documentation, not memory: WHO epi weeks run Mon-Sun
   (dashboard note), I had asserted MMWR Sun-Sat and shipped it.
   **How to apply:** before encoding any calendar rule, fetch the source's note / a bulletin title
   (e.g. AFRO "Week 10: 3 - 9 March 2025") and test a Sunday date explicitly.
4. Every date in an evidence text must be attributed to the source that states it (the AAR has no
   February date; NDoH 5 Jul does). Dated individual cases (e.g. an imported case) are placed, not
   left to proportional scaling.
5. Hand-curated numeric tables get total-vs-report guards (0.5-1.02x) and per-series validation
   with distinct messages; negative-test every guard with a mutant.

Related: [[zaf-2023-epicurve-provenance]], [[who-weekly-w53-quirk]], [[surveillance-curation-table]].
