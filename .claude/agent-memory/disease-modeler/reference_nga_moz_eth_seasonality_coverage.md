---
name: nga-moz-eth-seasonality-coverage
description: Cholera seasonal phase + observed-surveillance coverage cliffs for NGA/MOZ/ETH (and MWI/COD) from config_default 2023-anchored build; used to judge forecast-CV cutoff fairness/scoreability
metadata:
  type: reference
---

From `MOSAIC::config_default` (daily grid 2018-01-01 -> 2027-02-04, 3322 days; reported_cases/deaths matrices are [40 loc x 3322 day], rows ordered by `location_name`, no dimnames).

**Last OBSERVED (non-NA) surveillance date per country** (= scoreability cliff for held-out horizons):
- NGA cases/deaths end 2026-05-10 (cases 3052/3052 obs, 100% present -> fully-imputed multi-source target; shape skill against it is a DATA ARTIFACT, not model skill -> keep NGA exploratory/never-pooled)
- MOZ end 2026-06-07 (cases 3080 obs, 1360 nz -> genuine sparse surveillance, clean)
- ETH end **2026-03-01** (EARLIEST cliff; the binding constraint for any recent cutoff)
- MWI end 2026-05-03; COD end 2026-05-10

**Normalized monthly case climatology (2018-2025), peak month = 1.00 — countries are ANTI-PHASED:**
- NGA: peak Jul-Sep (Aug 1.00), trough Dec-Mar (boreal-summer/rainy-peaking)
- MOZ: peak Feb-Apr (Mar 1.00), trough Jul-Sep (austral rainy season)
- ETH: broad mid-year, near-bimodal Apr 1.00 / Aug 0.97, trough Dec-Jan

**How to apply (forecast-CV cutoff design):**
- ic_t0 = 2023-02-01 for the shipped 2023 build (see [[data-driven-ic-t0-generalization]]); any cutoff >= 2024-06 clears the IC-leak window by 16+ mo.
- Anti-phasing means a fixed cutoff triple hits a DIFFERENT seasonal phase per country -> use 6mo-spaced origins so each anti-phased country is tested both into-peak and into-trough (phase-balanced + zero OOS-window overlap for the moving-block bootstrap).
- ETH's 2026-03-01 cliff caps the common scoreable cutoff at ~2025-06-01 for a full 6mo horizon; 2025-10-01+5mo lands ON the ETH cliff (terminal horizon unscoreable).
- Deaths embargo: ETH long gamma_1 dwell ([[eth-deaths-cfr-dwell-mismatch]]) leaks pre-T infections into early deaths cells -> >=6wk deaths embargo (8wk ETH), not the 4wk cases default.
- Coupling for independent CV: NGA(Lake-Chad/west) weakest, MOZ(southern, net source) and ETH(horn) moderate -> independent CV defensible for this set; real coupling concern is COD corridor only ([[west-central-southern-regional-defs]], [[diprete-2025-transmission-units]]).
