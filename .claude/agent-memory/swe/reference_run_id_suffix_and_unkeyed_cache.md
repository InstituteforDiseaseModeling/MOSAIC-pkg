---
name: run-id-suffix-and-unkeyed-cache
description: Two recurring artifact-routing failure modes in MOSAIC (both hit the mobility code) — a run-id/suffix parameter that routes reads but not writes, and a derived-artifact cache whose filename omits inputs that change the contents
metadata:
  type: reference
---

Two failure modes that keep recurring wherever a function can be run in more than one
"mode" and writes to a shared output directory. Both are silent: the wrong file is
produced or consumed, nothing errors, and you only find out when a downstream consumer
happens to need the clobbered artifact.

## A. A `suffix` / run-id argument that routes only half the I/O

The pattern is a local helper (`.out()`, `.in()`, `.fig()`) that inserts the run id into a
filename. It fails the moment ONE call site bypasses the helper.

Verified instances in the mobility work:
- `est_mobility()`'s `.out()` originally matched `[a-z_]+` before `.csv`, so the
  uppercase stems `mobility_M.csv` / `mobility_D.csv` / `mobility_N.csv` were written
  **unsuffixed** and a fused run overwrote the production air-derived files.
- `plot_mobility_fused()` takes a `suffix` argument that routes every **read** but none of
  its four `ggsave()` calls — the PNG names are string literals — so two different fused
  runs silently overwrite each other's publication figures and the figure carries no
  record of which run made it. Its sibling `plot_mobility()` does suffix its figures, so
  the two are inconsistent.
- A suffix defaulted from a mode argument (`od_source`) but still user-overridable gives
  **no guarantee at all**: `est_mobility(od_source = "fused", suffix = "")` clobbers
  production exactly as before. The default is not the contract.

Checks that actually catch these:
1. `grep -n 'write\.csv\|ggsave\|png(\|saveRDS' R/<file>.R` and confirm EVERY hit goes
   through the helper — count them, don't eyeball.
2. Route the *directory* or make the helper the only thing that can construct a path;
   a regex over filenames will always miss a case.
3. If the run id can be empty, assert it is non-empty whenever the mode is non-default.

Also: because the artifacts have no run manifest, a partial re-run leaves a
mixed-timestamp set (`mobility_M.csv` from run A next to `mobility_pi.csv` from run B) that
nothing detects. `ls -la model/input/` mtimes are the only forensic tool.

## B. Derived-artifact caches keyed on too little

Every input that changes the contents must appear in the cache key, or a `cache = TRUE`
hit returns the wrong object *and prints a reassuring message*.

Verified instances:
- `get_travel_time_matrix()` writes `processed/mobility/D_traveltime_hours.csv` with **no
  key at all**. The only guard is `setequal(rownames(D), iso_codes)`, which does not see
  `aggregate_factor`, `aggregate_fun`, or `dataset_id`. Changing `aggregate_fun` from
  `"mean"` to `"min"` — a ~7x change in friction — silently returns the old matrix.
- Its friction raster key `<dataset>_agg<fact>_<fun>.tif` omits the **bounding box**, which
  is derived from the requested `iso_codes`. A regional subset run caches a small raster
  under the same name; the next continental run reuses it and every out-of-extent pair
  comes back `Inf`.
- `process_mobility_od_data(iso_codes = <subset>)` overwrites the 40-country
  `M_structure_*.csv` at a fixed path. Same for `rake_mobility_od_to_tau()` ->
  `M_fused_raked.csv`.

Rule: hash the full argument set into the filename (or write a sidecar `.json` of the
args and compare it), and make the cache-hit message name the key it matched on.
Related: [[r-filesystem-and-regex-traps]], [[psock-export-and-dead-guard-traps]].
