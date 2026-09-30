---
name: merge-duplicate-internal-defs
description: Merging parallel review branches can leave two definitions of one internal helper in different R/ files; with no Collate field the alphabetically-last file silently wins
metadata:
  type: reference
---

integrate/deep-review (2026-09-30) ended up with `.mosaic_best_subset_weights` defined in BOTH
`R/grid_search_best_subset.R` (2 args) and `R/run_MOSAIC_helpers.R` (3 args, `verbose`). DESCRIPTION
has no `Collate:`, so files load alphabetically and `run_MOSAIC_helpers.R` wins; the other copy is
dead and a caller written against it would break the moment either file is renamed.

**How to apply:** after any multi-branch merge, run
`grep -hoE '^[.A-Za-z_][.A-Za-z0-9_]* <- function' R/*.R | sort | uniq -d` before trusting tests —
tests pass either way because only one copy is live. Also: jsonlite drops names of atomic
vectors (see [[project_rundir_control_json_wrapper_trap]]); persist per-channel settings as named lists.
