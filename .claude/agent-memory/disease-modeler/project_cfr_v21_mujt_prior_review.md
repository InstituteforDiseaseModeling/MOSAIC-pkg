---
name: cfr-v21-mujt-prior-review
description: CFR v2.1 mu_jt prior reviews (2026-09-28, worktree cfr-v21, v0.96.1): median centre is empirically RIGHT (keep), in-progress WHO year + own-last-year carry-forward need the est_CFR_hierarchical patch, tau unidentified, sd_product mislabelled
metadata:
  type: project
---

The est_CFR_hierarchical() GAM is faithful to spec §8.4 and re-runs bit-identically (tau 0.616, sigma 0.696). It
was reviewed twice on 2026-09-28: a prior review, then a red-team pass at v0.96.1 with config v5.0 and priors
v16.0. The scratch work is in claude/cfr_v21_review/redteam/dm/ in the worktree.

**Durable conclusions:**
- **Keep mu_jt at the logit MEDIAN.**
  - It is the exact prior centre for the D7 integrated likelihood.
  - It is also the best point value for engine-direct deaths. The logit residual of annual CFR is negatively
    correlated with log cases (Spearman -0.34), so the median line applied to observed cases reproduces WHO
    deaths at 0.90-0.95 (pooled, 2000-25 and 2018-25).
  - The logit-normal mean (x1.25) over-predicts by 13-18%.
  - Against the scored product (2023-26, 17 dense countries) the geomean is 0.93.
- **The "1.12 WHO-vs-scored offset" is not a product difference.**
  - In matched 2023-25 windows scored/WHO = 0.995, with sd(log) 0.026, because the two are the same WHO
    dashboard data.
  - The 1.12 is observed CFR over the in-sample centre, which D7's a_j absorbs (t = 0.9).
  - The sd_product = 0.3 label ("product mismatch sd 0.261") is therefore wrong. It describes the spread of
    the centre's error, 0.21-0.32.
- **In-progress WHO year (the 2026 snapshot rows) must be excluded from the fit.**
  - Partial-year deaths lag their cases, and the same weeks are scored in the likelihood.
  - The fs trend end is not shielded by s(obs): NGA's 2026 row alone moves its centre from 2.78% to 2.15%.
  - Excluding all 19 in-progress rows moves NGA +33% and NAM +69%.
- **Carry-forward must key on each country's own last year.**
  - The m=2 trend extrapolates linearly past a country's data: SOM (EMRO, so no AFRO data after 2022) -18%,
    GNB -34%.
  - The patch is est_CFR_hierarchical_r2r3.patch in that dir. It passes the existing tests.
- **tau is unidentified.**
  - s(iso, bs="re") duplicates the fs null-space intercept: predictions are unchanged without it and REML
    differs by 0.3.
  - Dropping the 2026 rows flips tau from 0.62 to 0.003.
  - It is harmless while every location is seen, but it would make an unseen country's prior overconfident.
- **The endemic-PPV ratio (mu * chi_end/chi_epi) is empirically small.** Realized 2023+ reported CFR is
  0.98-1.03x mu in medoid free runs.
- **The production 0.55.12 national medoids do NOT replay under the v0.96 engine.** ETH, KEN, MOZ, SSD and MWI
  go extinct before 2023 even at mu_jt = 0, so medoid replays are not a valid v2.1 baseline.

**Why:** these are the facts a rebuild or a calibration test decision turns on. Each was measured, not
assumed.

**How to apply:**
- Do not "fix" the median centre to the mean.
- Do not apply a global 1.12 factor.
- Before any rebuild of config_default mu_jt or priors mu_jt, apply the in-progress and own-last-year patch
  and bump config to v5.1 and priors to v16.1.

Related: [[reference_cfr_mu_j0_identity]], [[project_mu_j_epidemic_factor_prior]], [[project_cfr_v21_decisions]].
