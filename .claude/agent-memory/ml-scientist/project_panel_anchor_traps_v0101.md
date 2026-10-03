---
name: panel-anchor-traps-v0101
description: compile_suitability_data per-country p99 anchors use non-AI rows only (the 5-case floor is NOT AI-invariant); two v0.101 traps, both FIXED before release - backfilled gap rows counted as trusted (BWA anchor x6.5; now tier 3, backfill off by default) and a curated WHO catch-up plateau set the ZAF anchor (x9.5; window now shaped)
metadata:
  type: project
---

Verified on the v0.101.0 panel (md5 5bd9956a, 2026-10-01). Anchors were recomputed with the exact rules and reproduce target_D to 5.6e-16.

- **Editing AI row values never moves the p99 anchors.** `is_ai <- !is.na(source) & source == "AI"`, and the anchors (cp99c, cp99r, median population) use only `!is_ai` rows. The v0.101 cap on imputed rows removed 85,663 cases in 2015-2022 and 488,594 before 2015, all from AI Fourier rows. It changes training targets on AI rows (mean 0.094 to 0.080) but leaves the p99 terms untouched. CORRECTION (maintainer review, 2026-10-01): changing WHICH rows are AI does move the 5-case floor term, whose median population is taken over non-AI rows, so the 11 floor-bound countries' anchors shift (GNB x1.056 v0.100.1 -> v0.101); see maintainer memory suitability-anchor-floor-traps.
- **Trap 1: backfill rows count as trusted.** `backfill_weekly_case_gaps()` gives interpolated rows `source = NA`, so they count as trusted. When the bounding rows are AI, AI-derived values define the anchor. In BWA, the interpolated values 20 and 30 between AI-observed 10 and 40 moved cp99r x6.5. In 10 other countries the effect is under 0.5%. The fix belongs in compile_suitability_data (my file): inherit "AI" from AI-bounded neighbours, or exclude `cases_interpolated` rows from the anchors. FIXED in dc1048068 (fix/v0101-trust, v0.101.0): filled weeks are labelled `backfill_interpolated` (tier 3, confidence <= 0.5) and excluded from anchors; BWA cp99r back to its floor (0.2330). Round 2 (802fcb062) then turned backfill off by default.
- **Trap 2: curated catch-up plateaus become the anchor.** FIXED by 1cd75b0fd: the curated window is shaped by WHO sitrep #5's onset epicurve, so ZAF's anchor is set by the real outbreak peak (0.3827). Before the fix, the single 1,390-case 2023-08-31 WHO report for ZAF was spread uniformly over 27 weeks (51-52 per week), labelled trusted WHO `who_catchup_curated`. That raised ZAF's p99 from 5 to 52 cases per week (cp99r x9.5) and compressed every other ZAF target about 9x (a 5-case week goes from 0.90 to 0.10). The documented Hammanskraal outbreak was concentrated in May-June 2023.
- **cases_binary and epidemic peaks are inert for lstm_v2 psi.** The response is target_D, and the output `cases_binary` is just `cases > 0`.

**Why:** an anchor that moves 6-10x silently rescales a country's entire psi level, and nothing in the log flags it.
**How to apply:** on every panel rebuild, diff the per-country cp99r against the previous panel (`claude/v0101_rebuild/psi/scripts/panel_diff.R`, section 2) and explain any change above 5%. (The ZAF window hand-off to the data-engineer is done: 1cd75b0fd.) Related: [[default-path-target-anchor-leak]], [[psi-refit-v0101]].
