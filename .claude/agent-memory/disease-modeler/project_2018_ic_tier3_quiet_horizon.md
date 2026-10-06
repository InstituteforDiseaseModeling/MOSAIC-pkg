---
name: 2018-ic-tier3-quiet-horizon
description: priors v18.0 (2018-01-01 build, MOSAIC v0.103.0) IC decisions 2026-10-03 - D8 tier-3 window rule (observed precedence, country-Fourier fallback, regional never, tier-aware trigger, counted-day rate) and KEEP the unbounded quiet-start horizon; unanchored-Fourier data facts
metadata:
  type: project
---
Decided 2026-10-03 on the laptop; scratch + patch in claude/plan_2018_start/ic_decisions/
(D8_tier3_ic_window.patch, for the swe to apply at the v18.0 rebuild; verified with git apply --check on
v0103 191892bb9 and main 9f3ea4f1b; 8 E/I test files pass under load_all, new tests fail on old code).

- **D8 rule (est_initial_E_I metadata 1.3.0):** tier-3 (AI Fourier) rows are not dated reports. Set aside
  where the 28-day window holds any tier-1/2 count; a window with none reads fourier_country_* only
  (metadata imputed_window_fallback), never fourier_regional_*; the quiet-start trigger counts tier-1/2
  later cases only; est_initial_E_I_location's onset rate is the mean over COUNTED days (NA/absent =
  unobserved, filled at lambda before t0). The old code counted missing days as zeros (SLE x0.5 at 2018).
- **Why:** Fourier rows spread an annual (residual) total along a seasonal shape, rescaled per year, so
  they jump at Jan 1 (ETH 46/day -> 5/day) and sat 2-14x off the bracketing observed weeks (ETH ~300/wk
  after an observed 61, then 13 and 4/wk in Mar 2018; KEN ~290 between 44 and 101; BDI 10-14 between
  zeros). ETH 2017/18 and RWA 2018-21 have NO WHO annual, so the reconciliation never scaled them (RWA is
  a constant 38/yr placeholder, cw 0.35). The PLAN's "they fill gaps to WHO totals" holds only for BDI/KEN/ZMB.
- **Effect at 2018 (exact):** BDI 33 -> 234 (quiet), RWA 1.2 -> 247 (quiet), MLI 403 -> 4 (template:
  later cases imputed only), KEN 1102 -> 666, ZMB 778 -> 1105, SLE 10.9 -> 34.8, ETH 1022 unchanged
  (fallback). Quiet set 17 -> 18. Rejected: x confidence_weight (a precision, not a magnitude), and
  tier-3-as-quiet (ETH is active, would get a 2,224 seed plus an unearned tier-A R2 leniency).
- **Evaluator coupling (disclose):** union(plan list, priors quiet_start_seeded) gives BDI the R2
  downgrade; its 2023-start national R2 was 0.550, between the 0.4/0.6 bars, so it can flip FAIL -> PASS.
- **Quiet-start horizon: KEEP unbounded.** H=365 (tier-aware dates) would template 8 national models
  (BEN BFA NAM RWA SSD TCD TGO ZAF; 2023-start EVAL obs ~120k incl. SSD 112k); the template gives
  P(E+I >= 1) of only 7-10%. 2023 posteriors bridged gaps with DETECTABLE low-level transmission through
  observed-zero weeks (RWA ~6/wk over 8 zero quarters at 99% weight), not sub-detection smouldering.
  At 2018 the gaps hold 2-4x more observed zeros (BFA 393, CAF 339, CIV 271, GHA 216). extinct_wt at 2023
  understates die-out for small totals (the seed pulse falls inside EVAL); at 2018 it is cleaner.
- Data flags: ZAF's 2018 "first later case" (2018-10-04) is imputed (first observed 2023-02-02). BFA's
  whole EVAL total (481, 2025) is a flagged Fourier spread of an unconfirmed UNICEF figure.

Shipped v18.0 confirms: 18 quiet starts (all by criterion a, none by the <1-infection rule), 9 templates
(8 zero-case + MLI), 13 data-based; docs 04 updated (MOSAIC-docs 6784c23, 2026-10-03).
**How to apply:** Post-1.0:
estimate unobserved windows from bracketing observed weeks; separate all-NA windows from observed-zero
windows; add an importation term (D1). See [[v0101-quiet-start-review]], [[v0100-rebuild-priors-v17]].
