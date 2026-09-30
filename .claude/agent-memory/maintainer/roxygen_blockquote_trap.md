---
name: roxygen-blockquote-trap
description: a roxygen line starting with ">" (e.g. ">= 1") is a markdown block quote; document() prints a failure and SKIPS that Rd, leaving it stale while other Rds regenerate
metadata:
  type: feedback
---

When a wrapped roxygen line begins with `>` / `>=`, roxygen2 markdown fails ("block quotes are not
currently supported") and silently keeps the old .Rd for that topic. Seen v0.100.0 in
est_initial_R.R (fit_beta_with_variance_inflation_R) — the fixer committed code + a stale Rd.

**Why:** document() still exits 0, so "ran document()" is not evidence the Rd is current.
**How to apply:** after document(), grep its output for "✖" lines; reword wrapped lines so none start with `>`.
