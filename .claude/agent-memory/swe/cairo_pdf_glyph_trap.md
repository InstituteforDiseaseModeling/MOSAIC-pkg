---
name: cairo-pdf-glyph-trap
description: capabilities("cairo") lies on this macOS box (probe a real cairo_pdf); plus base pdf() plotmath glyph coverage (no ℓ), WinAnsi.enc for dashes, pdftoppm cannot show bold/italic (use qlmanage)
metadata:
  type: feedback
---

When a `plot_*` function needs proper Unicode glyphs (R², →, Δ) in a **PDF**, the default
`pdf()` device cannot encode them and `ggsave(device = grDevices::cairo_pdf)` is the fix —
but **`capabilities("cairo")` is necessary, not sufficient.**

**Why:** On John's macOS dev box `capabilities("cairo")` returns TRUE, yet the cairo DLL
fails to `dlopen` at *draw time* with `Library not loaded: /opt/X11/lib/libXrender.1.dylib`
(XQuartz not fully installed). So a `capabilities()`-only guard passes the check then
explodes when ggsave actually renders, killing the plot.

**How to apply:** Probe the real device, not just the capability. Open a throwaway
`grDevices::cairo_pdf(tempfile())` + `dev.off()` inside `tryCatch(..., error/warning = FALSE)`;
only use cairo if that round-trips. Then *also* wrap the real cairo `ggsave` in `tryCatch`
and fall back to the default PDF device with ASCII-downgraded labels (R2, ->, delta) on
failure. PNG (raster) handles Unicode regardless of cairo, so the PNG can always carry the
proper glyphs. Reference implementation: `.mosaic_cairo_pdf_works()` +
`.build_plot(glyphs=)` in `R/plot_model_subset_optimization.R`.

**Base `pdf()` without cairo (2026-10-22, report page-1 prototype):**
- Greek, Σ, ∈, √, ′ (`B*minute`), ⌈⌉ (`group(lceil, x, rceil)`), `widehat(k)`, ∝, ≥/≤
  (`phantom() >= x`) all render via plotmath (Symbol font). There is **no ℓ** (not in plotmath
  or Adobe Symbol); `italic(l)` reads as 1/I, so write L_i / L_max or "ll".
- Em/en dash: default ISOLatin1 substitutes "-"; `pdf(encoding = "WinAnsi.enc")` renders them
  (name is `WinAnsi.enc`; "CP1252" fails to load). ≥ is in neither — use plotmath.
- **pdftoppm cannot verify bold/italic** here: base-14 fonts are not embedded and poppler
  draws Helvetica-Bold/Oblique as regular (no gs to embed). For a true-font check use Quartz:
  `qlmanage -t -s 1600 -o DIR file.pdf` (page 1 only). Width measurements in R use the real
  AFM metrics, so trust them over the pdftoppm look.
