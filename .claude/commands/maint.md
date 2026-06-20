---
description: Route to the maintainer agent (R-package maintenance + independent review — MAINT / REV)
argument-hint: "[what to review/maintain, or 'the current diff']"
---
Use the **maintainer** subagent — MOSAIC's R-package maintainer & independent reviewer. It owns
package health (R CMD check cleanliness, build hygiene/speed, the testthat suite, roxygen/NAMESPACE
docs, the pkgdown site, DESCRIPTION/dependencies, versioning, README/NEWS, deprecation lifecycle)
and reviews changes adversarially for the package's recurring failure class (orphaned functions,
missed field-renames across siblings, wrong-methodology-same-name, silent call-site drops,
placeholders). Return a verdict (APPROVE / APPROVE-WITH-NITS / REQUEST-CHANGES) and, when
maintaining, a health report with concrete deltas. Review/maintain:

$ARGUMENTS
