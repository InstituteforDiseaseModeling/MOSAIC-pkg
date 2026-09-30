---
name: idmc-role-triangulation
description: IDMC IDU HDX CSVs mix role="Recommended figure" and role="Triangulation" rows; process_IDMC_data sums both (~25% person inflation) — filter to Recommended
metadata:
  type: reference
---

IDU per-country HDX CSVs carry a `role` column: "Recommended figure" vs "Triangulation".
IDMC guidance: use the Recommended figure when available; triangulation rows are corroborating
estimates, NOT additive. Measured on raw/IDMC/hdx_2026-09-18 (38 files): 6,206 Recommended rows
(17.83M persons) + 2,766 Triangulation rows (4.38M, 31% of rows). process_IDMC_data (v0.75.0) keeps
all rows and sums `figure`, so idmc_*_displaced is inflated ~25% and some *_active weeks may be
triangulation-only. Latent while no consumer reads DATA_IDMC; must be fixed before the panels are wired
into compile_suitability_data. Also: `qualifier` ("more than or equal to", "approximately") is ignored.

**How to apply:** any IDU aggregation → filter `role == "Recommended figure"` first. Related:
[[ai-source-integration-provenance]] (same "trust discriminator column" lesson).
