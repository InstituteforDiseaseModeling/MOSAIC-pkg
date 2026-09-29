---
name: roxygen-escaped-percent-trap
description: A hand-written \% inside an Rd macro (\strong{}, \code{}) silently DELETES the whole roxygen section; bare % is escaped correctly by roxygen. Reproduced + bisected.
metadata:
  type: reference
---

**Never hand-write `\%` in a roxygen block in this package. Write a bare `%`.**

MOSAIC has `Roxygen: list(markdown = TRUE)`. roxygen2 (tested 8.1.0) **already escapes a bare
`%`** to `\%` on output. A hand-written `\%` is therefore double-escaped to `\\%`, which Rd reads
as "literal backslash" + "start of comment". Everything after it on that line — including a
closing `}` — is commented out.

**Two distinct outcomes, measured by bisection in a one-function throwaway package:**
- `\%` in **plain text** (`Exactly 95\% of rows.`) — section survives, but the Rd on disk carries
  `95\\%` and the rendered help drops the rest of the line. ~26 MOSAIC files are in this state
  already (pre-existing; committed Rd all show `\\%`). Cosmetic, widespread, not urgent.
- `\%` **inside an Rd macro** (`\strong{83.7\% were identical}`) — the comment swallows the macro's
  closing brace, the section is unbalanced, and **roxygen emits an empty section**. `R CMD check`
  then reports `checking Rd files ... NOTE / prepare_Rd: <file>.Rd:NN: Dropping empty section
  \details`. The entire prose is gone from the man page with no error at document() time.

Caught live at `R/download_WB_data.R:49` (`\strong{83.7\% were identical} ... (8.3\%) ... >1\%`),
which silently deleted the whole 30-line `@details` block (credentials note, newest-wins contract,
the measured API-vs-portal comparison). Only emptied section package-wide.

**Detection:** `grep -rn "^#'.*\\\\%" R/*.R`, then check which hits sit inside `\strong{}`/`\code{}`/
`\emph{}`. Or just read the `Dropping empty section` NOTE — it names the file and line.

Related: [[rcmdcheck-baseline-v084]], [[reviewer-checklist]].
