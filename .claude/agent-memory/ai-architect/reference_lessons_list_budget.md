---
name: reference_lessons_list_budget
description: CLAUDE.md's Lessons-Learned list is 56% of the always-loaded package file and its per-item length is growing monotonically; cap new entries at 30-40 words and archive the narrative
metadata:
  type: reference
---

Measured at v0.84.0: `MOSAIC-pkg/CLAUDE.md` = 4,820 words; § Lessons Learned alone = **2,721 words
(56%)**, and 46% of the combined always-loaded pair with `MOSAIC/CLAUDE.md` (1,056 w).

Per-item length is **monotonically growing**, which is the number to watch:

| items | era | mean words |
|---|---|---:|
| #1-#10 | v0.22 | 31 |
| #11-#16 | v0.29-v0.70 | 219 |
| #17-#18 | v0.72-v0.73 | 489 |

#17 + #18 alone are 978 words — 36% of the list, 17% of the whole file. Two incident write-ups.

**Rule: a new entry is 30-40 words — the reusable rule plus one clause of mechanism.** The incident
narrative (what was measured, which commit, which harness) goes in the commit message, NEWS, or a
`claude/lessons/` archive. The list's job is to change behaviour on the next task, not to be the
record.

**What actually earns an always-loaded slot** (judged by whether the rule's *shape* recurs, not by
how bad the incident was):
- **#13** — a guard of the form `is.null(<thing the defaults always fill>)` is dead on arrival.
  Cited by live code (`R/removed_api.R:6-10`, `sample_parameters.R`'s version-skew guard). Recurred
  as #14(b).
- **#11** — a rename applied to N-1 of N siblings. Cited in a later commit message as the reason the
  author grepped for the *identifier* rather than the pattern.
- **#14(a)** — deleting a file on the strength of its docstring instead of its call sites. Keeps
  recurring as *documentation* staleness, not just deletion: the root CLAUDE.md's live
  `reticulate::import("laser_cholera")` snippet and `migrate-laser-r.md` §614's "`run_LASER()` … the
  only engine entry point" are both this shape, inside the very documents that record the lesson.

**What does not:** #1/#3/#6/#7/#8/#9/#10 are seven statements of one rule — *a function created with
no call site, or a call site silently dropped* — and three of them name code that no longer exists
(`est_transmission_spatial_structure()`, `get_ENSO_forecast_from_json()`, the four
`plot_model_fit*` files). Merge to one item with the tags as a bare list.

**Recurring defects in the list itself:** numbering has been transposed (order emitted is
`1..14, 16, 15, 17, 18`); items outlive their subjects; and #15 points at
`claude/oracle/verify_draw_sites.py`, which does not exist — a lesson that says "keep the derivation
runnable" while citing a deleted script teaches the opposite. Check item subjects with
`grep -rln` against `R/`/`tests/` on every audit, and remember `claude/` is **gitignored**, so
anything a lesson cites there is unrecoverable once lost.
