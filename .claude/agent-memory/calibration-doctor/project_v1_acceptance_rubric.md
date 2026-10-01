---
name: project-v1-acceptance-rubric
description: MOSAIC v1.0 go/no-go = pre-registered rubric (frozen 2026-09-30, thresholds sha256 fea5cc66...) + evaluate_suite.R in MOSAIC-pkg/claude/deploy_v0100/acceptance/; how the old models land on it
metadata:
  type: project
---

The user promotes MOSAIC to v1.0 ONLY if the v0.100.1 production suite (28 national + 4 regional + 1
continental, window 2023-01-01..2027-04-29, CFR v2.1, mean central line) passes the pre-registered
rubric. Built and frozen 2026-09-30 before any suite output existed.

- Files: `MOSAIC-pkg/claude/deploy_v0100/acceptance/` (gitignored scratch): RUBRIC.md,
  rubric_thresholds.json (sha256 fea5cc66669a202dc4417fa650ad0392077f3b536aece442a0d0eb5bf791568a,
  hard-checked by the evaluator), evaluate_suite.R, test_evaluator.R, PREREGISTRATION.sha256
  (manifest; report stamps `[UNREGISTERED]` if any file changed), expected_tiers_config_v6.0.csv,
  make_test_views.sh, out/ (baseline + arm reports).
- Run on the FULL pull only: `--pull full --arrays yes` (coverage needs member arrays, see
  [[reference-weekly-coverage-needs-member-arrays]]); otherwise verdict = NOT EVALUABLE.
- Design: per-location tiers (A/B/C/Z by positive weeks + totals), quiet-start locations lose one
  R2 tier, deaths R2 report-only (CFR v2.1 decision: judge deaths on level + coherence), two-level
  scale criteria (block / target; target miss = MAJOR), MAJOR budget 5, no-regression vs
  production on the overlap window with like-for-like daily-sum intervals and an
  observation-revision exclusion band [0.80, 1.25] (KEN cases, UGA, GHA deaths excluded).
- Where old models land (fit-only shadow verdict): production v2026-09-18 NO-GO (deaths 2.0x,
  regional pooled cases bias 1.29-1.50, low tier-A pass shares); arm N (v0.97 median line) NO-GO
  (deaths 0.65x); arm B GO-with-caveats (3 MAJOR); arm D (v0.99 mean line) GO-with-caveats (1 MAJOR:
  deaths 0.78x = asymmetric absorption); arm C GO. Open risks for v0.100.1: 20 small/quiet-start
  countries, exact coverage, regional/continental pooled over-prediction.

**Why:** the user wants the v1.0 decision immune to post-hoc tuning.
**How to apply:** when the suite lands, run the evaluator unchanged and triage from criteria.csv; a
threshold change after results exist must be disclosed (it breaks the hash). Do not re-tune.
