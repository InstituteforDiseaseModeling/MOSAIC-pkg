---
name: v0103-2018-start-redteam
description: v0.103.0 (v1.0 candidate, config v7.0 / priors v18.0, 2018 start, OCV nu split, D8 E/I tier rule) red team 2026-10-03 - repro recipe, what verified, the open traps (two-pass priors, stale MOSAIC-docs spec, MOZ repo sibling, UGA k)
metadata:
  type: project
---

Branch feat/v0103-2018-start (HEAD f6416434d, base d50d46f59 = main b3a5d5a98). Verdict APPROVE-WITH-NITS.

**Repro recipe that worked (21 min wall):** `git archive HEAD` into scratch, copy `git show
d50d46f59:` v6.2/v17.1 rda+json into data/ and inst/extdata (the pass-1 start state), own rlib copy,
then install + (priors via the DOCS-redirect wrapper, install, config, install) x3 with
MOSAIC_BUILD_DATE_START=2018-01-01. Passes 2 and 3 byte-identical to the shipped md5s; model/input
and MOSAIC-docs untouched. Pass 1 differs from the builder's pass 1 only because the builder ran
afd71b1b8's description text. Scripts: claude/v0103_redteam/ (run_all.sh, compare_*.R).
The vaccination chain (process_GTFCC -> combine -> est_vaccination_rate, date_stop 2030-12-31,
NOT the registry's Sys.Date()+540) also regenerates byte-identical.

**Open traps after v0.103.0:**
- A window move needs TWO priors passes (est_initial_V1_V2 divides by the INSTALLED config N), but
  the make_config_default.R header recipe and the run-mosaic skill describe one pass, and no guard
  fires. NEWS tells users "rebuild with MOSAIC_BUILD_DATE_START=2023-01-01". Fix: priors builder
  passes N from param_N_population_size.csv at date_start (what the config builder uses).
- (RESOLVED in MOSAIC-docs 6784c23..2c99ff0, local and unpushed as of 2026-10-03) The 04 spec said
  2023 start, all-first-dose nu, 16 quiet starts and 100k/day. See [[v1-docs-compliance-pass]].
- MOSAIC-Mozambique make_config_MOZ.R (2017-08-01 start) and subnational builder read the all-dose
  nu file and set nu_2 = 0: the old convention, now on delivery-dated timing.
- UGA cases k 0.87 -> 0.13 (own fit 0.117, 17% above the 0.1 bound, so the clamp->trend rule
  misses it); COD 29 -> 6.4, KEN/ZMB/ZWE x2.2. Not in NEWS.

**Verified:** 2023+ overlap identical on every time field (nu_1 only integer->double, engine-neutral:
.sim_f32 + round), every v7.0/v18.0 changelog and NEWS number, panel trend to 2e-15, 12/12 mutants
killed, full suite 14,319/0/39 with the builder's exact skip list.
Related: [[reviewer-checklist]], [[rcmdcheck-baseline-v048]], [[v0102-psi-collapse-review]].
