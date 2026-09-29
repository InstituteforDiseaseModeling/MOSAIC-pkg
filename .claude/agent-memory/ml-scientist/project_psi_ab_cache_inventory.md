---
name: psi-ab-cache-inventory
description: Dugong psi_evolve caches vs production defaults — NO cache matches production (all pinned country_static=off/balance=FALSE except N9); manifests are shard-raced to 1-of-10 cutoffs; sharded refit is ~1.6 h wall not 15 h serial
metadata:
  type: project
---

Inventory of `~/psi_evolve/psi_cache_*` on **dugong** (26 caches, 4.8 GB) as the two arms of a
production-psi vs ND(DLinear)-psi full-pipeline A/B (2026-09-28).

**Fact 1 — no cache is production-configured.** `claude/psi_evolve/run_arm.R:155-156` *pins*
`country_static="off"`, `country_balance=FALSE` on every arm, overriding the promoted production
defaults (`"auto"`/`TRUE`, `R/run_rolling_cv_suitability.R:169-172`, commit c657ee4ab). Only
`psi_cache_N9` turned them on, and it used `"frozen"` not `"auto"`. **`auto` and `frozen` are the
same model** when the bundle has static covariates — `lstm_film_suitability.R:454`
`.cs_frozen <- .cs_mode %in% c("auto","frozen")`; `auto` differs only in degrading instead of
erroring when they are absent, and the logs confirm `country_static: 12 covariates x 40 countries`
is present. So N9 == production LSTM apart from `n_seeds=10` (prod 5) and `epoch_select_seeds=2`
(prod: absent). No dlinear+auto+balance cache exists.

**Fact 2 — every manifest records 1 of 10 cutoffs (shard race).** `run_arm.R` shards cutoffs over
10 processes; each calls `prefit_rolling_cv_psi()`, which writes the *whole* manifest at
`dir_cache/psi_manifest.json` containing only its own shard's cutoffs. Last writer wins. The CSVs
are fine (cutoff-keyed, disjoint); the manifest is not. Consequence: **`run_rolling_cv(psi_cache=)`
hard-errors on 9 of 10 cutoffs** (`.rcv_psi_cache_lookup`, `R/run_rolling_cv.R:588`). Reading the
`psi_<cutoff>.csv` directly (the make_config_default psi-refit route) is unaffected. Fix if
reusing: rebuild a merged manifest from a *local copy*, or shard by giving each process the full
cutoff list and letting the spec_hash cache-hit skip the others (serialises the manifest write).

**Fact 3 — `spec_hash` includes `n_seeds` and `epoch_select_seeds`** (`.rcv_psi_spec_hash` drops
only `parallel_seeds`). So a production spec (`n_seeds=5`, no HA-02) can never hash-match a
10-seed/HA-02 cache even if the architecture is identical.

**Fact 4 — refit cost is ~1.6 h wall, not 15 h.** `prefit_rolling_cv_psi()` iterates cutoffs
serially; concurrency comes from N sharded processes (`launch_arm.sh ARM NSHARD NSEEDS THREADS`).
Measured per cutoff at n_seeds=10 + HA-02(k=2), 9 inner folds, 10 concurrent shards @ 9 threads:
DLinear 14.7-19.1 min (fold-fit 0.37 min), LSTM 32.4-38.0 min (fold-fit ~1.1 min), LSTM+D9b+N8
38.1 min. Production (n_seeds=5, **no** HA-02) is 45 fold-fits + 5 refits vs 18+10, so it is
*more* expensive per cutoff: ~68 min LSTM, ~24 min DLinear. Ten shards in parallel => ~1.1 h +
~0.4 h for both arms. RAM is not the constraint on dugong (~5 GB/process, 93 GB for 18 processes
of 1511 GB) — the 32 GB OOM rule in [[project_lstm_v2_v034_plan]] is about small boxes.
**Thread gotcha:** each shard must get `MOSAIC_PSI_CORE_BUDGET` / `MOSAIC_PSI_TF_INTRAOP` /
`OMP_NUM_THREADS` = THREADS, else every TF process sizes its pools to all 176 cores and thrashes.

**Fact 5 — the seasonal baseline is unavailable before ~2025-04-01.** `config_default` starts
2023-01-01 and `.rcv_baseline(type="seasonal")` returns NA when the finite-observed IS span is
< 730 days (`R/evaluate_rolling_cv.R:338-339`). Blocks 1-4 (2024-01-01 … 2024-10-01) therefore have
no seasonal baseline for any country. From 2025-04-01 on, 25-28 of 40 countries qualify, including
all 16 of the psi_evolve burden pool.

**Fact 6 — replicate divergence is large for BOTH families at the psi level** (disjoint seed block,
same spec, 132,880 cells/cutoff): LSTM P000/P000R mean |Δψ| 0.013-0.017, p95 ~0.07-0.09, max 0.83;
DLinear NDe/NDeR mean 0.007-0.019, p95 0.04-0.09, max 0.60. DLinear's trunk never reads
`rec_dropout` but still uses `layer_dropout` — it is not deterministic, just differently noisy.
The 0.10% "replicate floor" in the registry is an *aggregate MAE* figure and hides this; see
[[psi-replicate-floor-is-family-specific]].

Related: [[nd-dlinear-evidence-audit]], [[psi-evolve-redteam]], [[psi-artefact-provenance-v077]].
