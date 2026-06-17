---
name: cairo-pdf-glyph-trap
description: capabilities("cairo") lies on this macOS box; must probe an actual cairo_pdf device before relying on it for Unicode glyphs in plot PDFs
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
