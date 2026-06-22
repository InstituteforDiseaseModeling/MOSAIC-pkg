---
name: reviewer-checklist
description: Institutional review checklist for MOSAIC data-object/config bumps and engine-semantic changes
metadata:
  type: feedback
---

Durable review checks for MOSAIC, built from caught regressions.

**Data-object bumps (config_default / priors_default):**
- Rebuild the changed array from its committed source and assert bit-exact match
  to the shipped .rda/.json (e.g. psi_jt from pred_psi_suitability_day.csv). See
  [[config-default-psi-provenance]].
- Structural-diff the .json (python json walk) to confirm ONLY the intended keys
  changed -- catches accidental window/prior drift.
- Verify version string bumped in BOTH the data-raw maker AND the metadata$version
  inside the object, and that the drift-guard test (test-cfr-pipeline-consistency.R,
  the Lesson-#12 guard) passes.
- Confirm the regeneration recipe is reproducible from the committed package +
  canonical pipeline (model/LAUNCH.R). If produced by an ad-hoc/compute-VM run with
  overrides, that is a provenance finding -- the artifact is hand-curated, document it.

**Always check for the compute-env parity trap:**
- `git log origin/main..HEAD` -- unpushed config/data commits mean hedgehog/Coiled
  (which install from remote) silently run STALE defaults on large calibrations.
  This is a SHOULD-FIX every time a default data object changes.

**Engine-semantic / field changes:** grep ALL siblings (grep -l old R/), incl.
temporarily-unused fns (Lesson #11). Use reported_deaths/reported_cases not
disease_* (Lesson #12).

**New fns / fixes ship WITHOUT a test = the #1 recurring smell** (Lessons #1/#6).
On any bug-fix commit, grep tests/testthat for the fixed function name; if zero
references, flag missing regression test (e.g. 979f066e CV fix had none).

**Build/runtime separation:** model/ is .Rbuildignore'd -> large CSVs there are
repo-bloat, not build breakage. Don't conflate.
