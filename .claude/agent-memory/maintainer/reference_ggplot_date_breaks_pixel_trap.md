---
name: ggplot-date-breaks-pixel-trap
description: Reading a date off a ggplot PNG - scale_x_date(date_breaks = "12 months") does NOT put year breaks on 1 January; the anchor month depends on the range, so assume nothing and reproduce the breaks
metadata:
  type: reference
---

`scale_x_date(limits = c(start, stop), date_breaks = "12 months", date_labels = "%Y")` in
ggplot2 4.0.3 anchors its breaks on a month that depends on the range. With limits from 2015-01-01
the anchor was 1 June for a stop of 2026-09, 1 May for a stop of late December 2026, and 1 March
for a stop of 2030-12. A tick labelled "2026" can therefore be 1 May 2026.

This nearly caused a wrong correction on 2026-10-03. The `plot_climate_data()` "today" line on
MOSAIC-docs `climate_data_MOZ_weekly.png` measured as "~16 Dec 2025" under a 1-January
assumption. With the real 1 May anchor it is about 15 April 2026, so the caption "as drawn in April
2026" was correct.

**How to apply:** before dating anything from figure pixels:
1. Find the label centres (dark-pixel clusters under the axis) and the major gridlines (the
   wider grey runs).
2. Reproduce the breaks with `ggplot_build(p)$layout$panel_params[[1]]$x$breaks` on a stub with
   the same limits.
3. Validate the method on a figure whose line carries a printed date. The ENSO figure's
   "2026-04-14" label measured as 2026-04-13.

Data before a `limits` lower bound are censored (dropped), so the first data pixel marks the
first in-range date.
Related: [[v1-docs-compliance-pass]].
