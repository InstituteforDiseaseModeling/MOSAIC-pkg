---
name: mosaic-docs-style-guide
description: MOSAIC-docs has its own binding CLAUDE.md + STYLE-GUIDE.md that are NOT auto-loaded in MOSAIC-pkg-rooted sessions; no new math symbols, "we" voice, italic "Note that" caveats, fixed hat/tilde meanings
metadata:
  type: reference
---

Before editing any `MOSAIC-docs/*.Rmd`, read `MOSAIC-docs/CLAUDE.md` and `MOSAIC-docs/STYLE-GUIDE.md` in full. Sessions rooted in MOSAIC-pkg load only the root and MOSAIC-pkg CLAUDE.md files, so these two never show up in context unless you open them.

Rules that bit me on the v1.0 psi section (2026-10-03, MOSAIC-docs 13b4f02):
- **No new math symbols** without maintainer approval. That includes LSTM internals: bold vectors, target or rate symbols, slope/intercept letters, a lambda for a penalty. Describe these in words, and keep existing equations and symbols verbatim. `eq:psi` keeps `h_t`, `w_h`, `b_h`.
- Several letters are taken: `y` (counts), `w` (weights and reporting week), `lambda_j` (initial conditions), `z_{psi*}` (smoothing), `g_j`/`B_yr` (CFR GAM).
- Hat means best-fit/estimated, bar means mean, tilde means a truncated or normalized weight. Do not use tilde for a smoothed series.
- Methodology goes in first-person plural. Caveats are italic standalone "*Note that ...*" paragraphs, not bold labels. The model description has no prose bullet lists.
- Captions are 1-3 sentences. Citations are inline links; prefer the DOI (FiLM is https://doi.org/10.1609/aaai.v32i1.11671).

**How to apply:** draft in a scratch file, then scan every `$...$` in the edited section against the guide's symbol table before committing. Static checks need no render: parse the chunk options with base R `parse()`, and check fences and `$` balance.
