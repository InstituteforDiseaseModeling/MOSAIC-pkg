---
name: calrep-v151-traps
description: 1.5.1 calibration-report traps — hand NAMESPACE needs S3method(grid::makeContent,...); jsonlite "_row" from named-vector data.frames; testing draw-time gTrees via grid.force+grid.grab; byte-compare upgraded JSON with key-sorted lists
metadata:
  type: reference
---

Learned building MOSAIC 1.5.1 (report reads only calibration_diagnostics.json + metric_glossary.yml).

- **NAMESPACE is hand-maintained** (`devtools::document()` skips it) with `exportPattern("^[[:alpha:]]+")`,
  so a method named `makeContent.cls` is EXPORTED (R CMD check: undocumented object; review: leaks into the
  API). Name it `.something` and register with the 3-argument form
  `S3method(grid::makeContent, mosaic_calrep_left, .mosaic_calrep_left_content)` — not exported, no Rd.
- **jsonlite writes `"_row"`** for a data.frame whose rownames are not the default integers — e.g. built
  from `unlist(named_list)`. `unname()` / `rownames(df) <- NULL` before `write_json(dataframe = "rows")`,
  else an upgraded file differs from the in-run one.
- **Testing a draw-time gTree:** calling `makeContent()` standalone uses the whole-page viewport, so
  layout-dependent output (reason overflow) differs. Draw the real page on `pdf(NULL, 11, 8.5,
  encoding = "WinAnsi.enc")`, then `grid.force()` and walk `grid.grab(wrap.grobs = FALSE)` for text labels.
  Pre-measuring overflow = draw page 1 on a null device with a state env the makeContent writes into.
- **Determinism check for an upgrader:** compare JSON parsed back from disk with recursively key-sorted
  lists (key order differs between writer and upgrader); compare `as_json(x)` round-trips when one side is
  data frames. A 1.5.0 file's keys were exactly the 1.5.1 keys minus the additions (checked against a real
  1.5.0 stub run from a `git worktree` of main) — so "strip the additions" is a faithful 1.5.0 emulator.
- **Weekly trajectory sums need the arrays in memory**: in-run they always exist (even with
  `persist_ensemble_arrays = FALSE`); a re-render needs persisted arrays, else falls back to daily sums
  (marked `weekly_meta.source = "daily_sums"`).
- Base `pdf()` glyph rules: see [[cairo-pdf-glyph-trap]].
