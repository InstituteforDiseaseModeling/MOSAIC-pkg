---
name: scratch-build-traps
description: Tooling traps hit on 2026-10-03. Roxygen `\%` wipes @details; a shared claude/ rlib reinstalled mid-run loads the old MOSAIC; the priors JSON twin rounds to 16 digits, so exact CRN parity needs the rda; placeholder checks must skip verbatim fenced output.
metadata:
  type: reference
---

## roxygen percent signs

MOSAIC sets `Roxygen: list(markdown = TRUE)`. In that mode, write `95%` in roxygen text.
- `95\%` becomes `95\\%` in the Rd, and the `%` then comments out the rest of the line.
- In @details that throws "mismatched braces or quotes", and roxygen drops the whole section.
- `devtools::document(quiet = TRUE)` hides the warning.
- Check `git diff --stat man/`: an Rd losing about 40 lines for a small doc edit is this trap.
- Run `tools::checkRd()` on each changed Rd.

## Shared rlibs under claude/

Other agents reinstall rlibs such as `claude/v0103_rebuild/rlib` (MOSAIC 0.103.0) while you work.
- During a reinstall, `library(MOSAIC)` silently falls back to `~/Library/R/.../MOSAIC` (0.91.14).
- That fallback is what produced "not an exported object" errors.
- Always add `stopifnot(packageVersion("MOSAIC") == ...)`.
- For before/after comparisons, do not depend on a shared rlib:
  1. Run `git archive HEAD | tar -x` into scratch, then `git init` there.
  2. Add a second worktree of that scratch repo for the pristine side.
  3. `pkgload::load_all()` each side in a separate Rscript, saving results to RDS.
- Used for the F4 patch in `claude/v0103_uga_k_ruling/` (pkg/ and pristine/).
- A patch generated there with `git diff --cached` (new files included) can be checked against the live
  worktree with `git -C <worktree> apply --check`, which is read-only.

## Priors JSON twin vs rda

The `inst/extdata/priors_default.json` twin rounds every value to 16 significant digits, about 1e-15
relative error (checked on 17.1, beta_j0_tot and p_beta).
- Drawing from the JSON instead of the rda breaks byte-exact reproduction of a CRN simulation.
- When exact parity matters, read `data/priors_default.rda`, e.g. via `git show <sha>:data/priors_default.rda`.

## Placeholder checks vs verbatim outputs

A "no unfilled @@X@@" check fails if a document embeds verbatim tool output that quotes a placeholder (the
mutant self-test prints `@@VERSION@@`). Scan only outside fenced code blocks.

See [[nb-dispersion-estimator-traps]].
