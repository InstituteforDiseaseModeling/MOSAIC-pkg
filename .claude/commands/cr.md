---
description: Route to the reviewer agent (code review & housekeeping — CR / REV)
argument-hint: "[what to review, or 'the current diff']"
---
Use the **reviewer** subagent — MOSAIC's independent code reviewer & package housekeeper. It
reviews adversarially for the package's recurring failure class (orphaned functions, missed
field-renames across siblings, wrong-methodology-same-name, silent call-site drops, placeholders)
and keeps housekeeping current (roxygen/NAMESPACE/_pkgdown/DESCRIPTION version/README/Lessons).
Return a verdict (APPROVE / APPROVE-WITH-NITS / REQUEST-CHANGES). Review:

$ARGUMENTS
