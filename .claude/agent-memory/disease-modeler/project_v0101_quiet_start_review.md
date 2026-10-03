---
name: v0101-quiet-start-review
description: priors v17.1 (2026-10-01) five new quiet starts SSD/TZA/UGA/ZAF/ZWE - AI-window removal verified correct, paired prior-predictive shows the Beta(1,1e5) seed is LL-neutral vs v17.0 and vs an importation-scale seed; verdict KEEP, with post-1.0 flags
metadata:
  type: project
---
Reviewed 2026-10-01 on worktree v0101-trust (74a328565), MOSAIC-data 04a6d0f, rlib at claude/v0101_rebuild/rlib.
Scratch + logs: claude/v0101_rebuild/dm_review/ (06_sims.R paired arms, 07 extra arms, 08 summary, 09 ZAF threshold).

- **Removal of AI Fourier window rows was correct for all five**: each 2023 WHO annual total is fully met by
  observed weeks (SSD 1471/1473, TZA 1068/1068, UGA 80/87, ZAF 1391/1391, ZWE 14517/14517). WHO dashboard
  first_epiwk: TZA 2023-01-16 (5 cases), ZAF shaped from 2023-01-30, ZWE 2023-02-13 (Chegutu onset), SSD
  2023-02-20 (99/wk; ICG OCV request 15 Mar 2023), UGA 2023-07-24.
- **Classification**: ZWE/SSD/ZAF are true quiet starts; UGA quiet for 7 months (Jan-Jul cells NA, unscored);
  TZA is a DATA GAP (no rows before week 3, then 5/2/2/13 cases) - quiet only because all-NA windows count.
- **Paired prior-predictive (50 draws, swap only E/I)**: median paired cases-LL diff v17.1 vs v17.0 ~0 in all
  five (2023 and first 8 scored weeks); vs absolute seed (E,I each mean 10) ~0 too, but abs seed goes extinct
  by day 45 in 12-26% (v17.1 <=2%). Data-implied TZA prior (lookahead 21/35 -> E+I ~3) extinct 38-44%, worse
  top-5 LL. Only measurable cost: ZAF first 8 scored weeks (v17.1 worse than abs seed by >5 nats in 42% of
  pairs) - the seed pulse leaks into the quiet Feb-Apr weeks. Template/no seed: -85 to -1978 nats over 2023.
- Seed in incidence terms: E = 1e-5 N in balance ~ 1 reported case /100k/wk (~the Zheng outbreak-week median);
  week-1 pulse median 30 (SSD) to 106 (TZA) reported cases; growth after is driven by the beta prior.

**Why:** v1.0 critical path; user approved Beta(1,1e5) as "weak seeding floor" (range then 25-680 people,
now 25-1,332). **How to apply:** keep v17.1; post-1.0 candidates = count-scaled seed for large N, distinguish
all-NA windows from observed-zero windows, NA-vs-zero for dashboard-absent weeks (UGA), 45-day burn-in hides
observed contradicting weeks (TZA wk 3-6). Docs 04 Rmd (~line 1443) now lists the 16 quiet starts (MOSAIC-docs 280d837).
See [[v0100-rebuild-priors-v17]], [[epidemic-threshold-engine-semantics]].
