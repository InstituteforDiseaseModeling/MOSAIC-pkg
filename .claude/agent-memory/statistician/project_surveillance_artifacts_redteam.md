---
name: surveillance-artifacts-redteam
description: Red-team of fix/surveillance-artifacts (pkg 871802f3d, MOSAIC-data 7954843, 2026-10-01) - reproducible and conservative, but SOM 2026/COG 2023 duplicates, SSD 2024 R3 carve-out residual, ZAF/NAM window spans; data reaches calibration only on a config_default rebuild (done: v6.1, v0.101.0)
metadata:
  type: project
---

Verdict (2026-10-01): no merge blocker. Outputs are byte-identical on independent regeneration,
every FIX cell is logged, the serial test suite and R CMD check are clean, and 15/18 mutants are
killed (survivors: the 2x drop ratio, ISO-vs-calendar year in R3, and one equivalent mutant).

**Why it matters:** config_default v6.0 still carries the OLD fit target (ZAF daily max 199). The
new surveillance, peaks and weights reach calibration only when config_default is rebuilt.
**Outcome:** config_default v6.1 (v0.101.0) is that rebuild. NEWS 0.101.0 records fixes for most items
below: sub-half-case rows emptied, aggregate AI weeks dropped (incl. COG 2023 w29), imputed rows only fill
the gap to the WHO account, documented absences remove SSD 2023-05..2024-09 imputed rows, ZAF window
shaped and WHO weeks dated Monday-Sunday, update_mosaic_data WHO-annual edge added; CIV now takes the
panel-trend k (0.98). SOM 2026 is not mentioned; re-check before relying on it.

**How to apply (gate that rebuild on):**
- CIV flips to a Poisson cases likelihood because R3 leaves sub-0.5 residual rows; empty them.
- Reconstructed cells inflate NB k; see [[surveillance-reconstruction-traps]].
- Missed duplicates: SOM 2026 fourier 212 re-spreads the WHO weekly 233; the COG 2023 AI
  "observed" 63 duplicates WHO's 69.
- R3's min(O, .) carve-out keeps 1,004 SSD 2024 imputed cases where the AI documents zero. A gap
  rule would also remove 89k pre-2023 (ZWE 2009, GNB 2006 kept 21k against a WHO total of 37).
- Window span: ZAF W1-W35 vs the AAR's Feb-Jul (about 23% misplaced); NAM spread back to W1
  although its first case was on 2 Mar 2025.
- update_mosaic_data lacks the process_WHO_annual_data -> combiner dependency edge.
- The commit bumps neither DESCRIPTION nor NEWS.
