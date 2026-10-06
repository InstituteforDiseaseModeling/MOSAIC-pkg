---
name: v1-docs-compliance-pass
description: MOSAIC-docs v1.0 pre-release compliance + accuracy pass (2026-10-03, range 9eb5f32..13b4f02, fixes e1592df/ab3ff78/2c99ff0) - recurring docs-agent failure modes, verification recipe, open maintainer decisions
metadata:
  type: project
---

Reviewed the 13 unpushed MOSAIC-docs commits against MOSAIC-pkg e6beba91d (v0.103.0, config v7.0 /
priors v18.0). About 60 numbers checked; most were right. The errors were stale rules and
terminology, not arithmetic. Scripts: MOSAIC-pkg/claude/v1_docs_review/ (q*.R, check_rmd.R = purl +
parse + chunk-header parse, check_dollars.py). Private lib: claude/v1_docs_review/rlib.

**Recurring docs-agent failure modes (check these first on any docs review):**
- Agents edit STYLE-GUIDE.md to self-register notation (532e89e, 88232c0: C_jw, y-hat_jw,
  w-bar_jw extension). Run `git log origin/main..main -- STYLE-GUIDE.md` every time and list the
  result as a maintainer decision.
- Terminology collision: in the docs "reconstructed" means tier 2 (a WHO multi-week report
  spread over its weeks). The package and NEWS call AI Fourier tier-3 rows "reconstructions".
  Text copied from NEWS brings the clash with it.
- A rule changes in the package after the docs paragraph was written. v0.103.0 added
  near-bound censoring of the dispersion (k*exp(-1.96 se/k) <= 0.1). The 05 text still
  described clamped fits only.
- Window-dependent numbers go stale when the window moves (05 "~1,400 daily cells" was the
  2023-window day count).
- Captions name panel letters ("(A)/(B)") that the figure does not have.
- Unlinked citations ("statement of 5 July 2023", "situation report #4", "Abubakar et al. 2018").

**Open maintainer decisions:** the self-registered symbols; plain w_i in eq:bayes-2; the
STYLE-GUIDE psi* row still says psi_jt is "raw output". "Abubakar et al. 2018" has no URL and its
source is unverified: the nearest match, Massing/Aboubakar 2018 PLoS NTD, reports 67.2%/81.9%, not
65%. The 03 links to open-meteo-pipeline and enso-data 404 publicly; jhu_cholera_data is private
too. ai-cholera-data-mining is public.

Related: [[v0103-2018-start-redteam]], [[reviewer-checklist]], [[ggplot-date-breaks-pixel-trap]].
