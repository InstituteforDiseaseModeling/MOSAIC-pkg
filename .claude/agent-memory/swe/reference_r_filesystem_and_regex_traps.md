---
name: r-filesystem-and-regex-traps
description: Eight verified R-level traps found auditing pipeline + mobility code — path-used-as-regex in sub(), which.max on all-NA, list.files hidden-file default, as.Date(NULL) length-0 Date, a filter comparing against a value the data never takes, diag() silently dropping names, raster::raster() stripping values, and ggsave writing a figure from a 0-row data.frame
metadata:
  type: reference
---

Each verified by experiment in a sandbox, not by reading. Reusable anywhere in the
package that touches the filesystem or builds a selection filter.

**1. A filesystem path used as a regex pattern.** `sub(paste0("^", dir, "/?"), "", d)`
to make a path relative is the common idiom and it is wrong. `dir` is user-supplied and
may hold regex metacharacters:
- unbalanced `[` (dir named `MOSAIC[x`) -> **hard error** `invalid regular expression ... Missing ']'`
- balanced `[v2]`, `(backup)`, `+` -> **no error, silently fails to strip**, so the
  "relative" column holds the full absolute path; any downstream `substr(x, 1, 46)`
  then truncates every row to the same path prefix and the report is unreadable.
`Dropbox (Personal)` / `OneDrive - Foo` are realistic macOS roots, so this is live risk.
Use `substring(d, nchar(dir) + 2L)` or `sub(..., fixed = TRUE)` (no `^` anchor then).

**2. `which.max()` on an all-NA vector returns `integer(0)`.** `file.info(hits)$mtime`
is NA for a dangling symlink or an unreadable file, so
`new <- hits[which.max(mt)]` yields `character(0)` and the very next
`data.frame(path = new, ...)` dies with **"arguments imply differing number of rows: 1, 0"**.
The *partial*-NA case is nastier and more likely: `max(mt)` without `na.rm` is NA, so a
derived `stale` column becomes NA and any `if (r$stale)` downstream throws
**"missing value where TRUE/FALSE needed"**. One dangling symlink in a raw/ directory can
therefore abort a whole pipeline in its preflight. Always `mt <- mt[!is.na(mt)]` before
`max`/`which.max`.

**3. `list.files()` already excludes dotfiles** (`all.files = FALSE` is the default), so a
follow-up `grepl("(^|/)\\.", basename(f))` filter is dead code. Also: applying a regex to
`basename(f)` makes the `(^|/)` alternation unreachable, and an *unanchored* literal like
`"README"` drops legitimate artifacts such as `README_country_counts.csv`. Anchor
filename filters (`"^README"`, `"\\.md$"`).

**4. `as.Date(NULL)` does NOT error** — it returns a zero-length `Date`. A `NULL` date
argument therefore sails through validation and detonates far downstream as
`argument is of length zero` inside whatever does `if (date_stop < date_max)`.

**5. A filter comparing against a value the data never takes.** `Filter(function(s) s$group != "4", reg)`
where every group is `"4A"`/`"4B"` is always TRUE, so the filter never removes anything —
here it meant the documented-default `include_suitability = FALSE` still queued a
multi-hour TensorFlow step. Same family as the always-FALSE guards in
[[psock-export-and-dead-guard-traps]] and CLAUDE.md lessons #13/#14: **assert the filter's
comparison value actually appears in the data** (`stopifnot(any(groups == "4"))`), or
compare on the prefix the data really has. Test a selection filter by its *cardinality
change*, not by whether it runs.

**6. `diag(m)` silently returns an UNNAMED vector unless `rownames(m)` and `colnames(m)`
are elementwise identical.** Verified: `colnames(m) <- gsub("^X", "", colnames(m))` — the
reflexive "undo read.csv's X-prefix" idiom — turns `XKX` into `KX` on the columns only,
`identical(rn, cn)` goes FALSE, and `diag(m)` loses its names. The usual next line
`names(y) <- names(diag(m))` then assigns NULL, and `y[match(names(N), names(y))]` returns
**all NA** with no error, which a JAGS/likelihood call downstream consumes as garbage.
Two rules: (a) after `read.csv(..., check.names = FALSE)` the `gsub("^X","")` is dead code
that can only ever corrupt — delete it, don't keep it "just in case"; (b) never rely on
`diag()` for names, use `rownames(m)`.

**7. `raster::raster(x)` strips the values when `x` is a `RasterLayer`, but keeps them
when `x` is a terra `SpatRaster`.** Verified both ways (raster 3.6.32). So
`fr <- raster::raster(malariaAtlas::getRaster(...))` works today only because
malariaAtlas >= 1.5 returns terra objects; if that ever reverts, `fr` becomes an
all-NA geometry-only raster and the whole downstream chain (aggregate -> gdistance
transition -> costDistance) returns `Inf` everywhere with no error. Guard any
`raster::raster(<other raster object>)` with `stopifnot(raster::hasValues(fr))`.

**8. `ggsave()` on a 0-row data.frame writes a real PNG and returns cleanly.** Verified:
an inner-`merge()` that matches nothing gives `nrow == 0`; `median(numeric(0))` is NA, so a
`paste0("Median ", round(median(x)), "%")` subtitle renders literally as "Median NA%",
`scale_x_log10()` on empty data does not complain, and a blank publication figure lands on
disk. Any figure whose caption/subtitle is computed from the data needs
`stopifnot(nrow(d) > 0)` before the plot — the plot itself will never tell you.

Corollary for step/DAG drivers: `unmet <- setdiff(intersect(deps, ids_in_plan), done_ok)`
intentionally ignores deps outside the plan, which is right for resume but means a
targeted re-run silently consumes stale upstream artifacts with no message. And a driver
that executes in registry order needs a topological assertion, not a convention — the
order being correct today is not a guarantee.
