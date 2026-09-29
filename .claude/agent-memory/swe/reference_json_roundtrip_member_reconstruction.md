---
name: json-roundtrip-member-reconstruction
description: 1_inputs/*.json are written at 15 significant digits (jsonlite digits=NA), so re-sampling a posterior member from them is NOT bit-identical in multi-location configs; digits=I(17) fixes it
metadata:
  type: reference
---

`.mosaic_write_json()` (R/run_MOSAIC_helpers.R) uses `jsonlite::write_json(..., digits = NA)`,
which is 15 significant digits, not an exact double round-trip. Re-sampling a member from
`1_inputs/config.json` + `priors.json` gives parameters differing by ~1e-15 relative.
Measured 2026-09-28 at v0.92.1 (prior draws, config_default subsets, seed p*1000+s):
- single location (MOZ): 0/80 members differ.
- 4 locations (ETH,KEN,SOM,UGA): 31/80 not bit-identical, 7/80 differ by >5% in total
  cases, p95 rel err 0.10. Chaotic amplification of ULP noise via integer rounding/draws.
- Re-writing the same objects with `digits = I(17)`: 0/80 differ.

**Why it matters:** any post-hoc path that "reconstructs members from seeds" from the JSON
inputs (the R_eff `recompute_ci` faithfulness gate, tol 5% at p95) will fail on genuine
current-version multi-location runs. Tests that mock `sample_parameters` never see it.

**How to apply:** when reviewing/writing seed-based reconstruction, test the JSON
round-trip, not in-memory objects. Pre-v0.68 artifacts fail such gates anyway (Python
engine). Related: [[pure-R engine cost model (PR #122)]], [[sim engine identity tests]].
