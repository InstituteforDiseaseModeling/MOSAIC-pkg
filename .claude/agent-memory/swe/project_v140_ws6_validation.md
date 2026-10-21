---
name: v140-ws6-validation
description: MOSAIC 1.4.0 WS6.3-6.5 dugong validation (2026-10-20) - real adaptive stop points match the WS2 replay, measured ensemble memory model (config 65 B/cell, master ~90 B/cell/member-run), HSIC sensitivity 67 GB at p=916, harness in claude/plan_convergence/ws6/
metadata:
  type: project
---

Pre-red-team-fix snapshot (single green check) installed in ~/R/lib_v140 on dugong; runs in
~/v140_validation/runs, report claude/plan_convergence/ws6/WS6_REPORT.md.

- **Real runs re-draw the production draws** (same config/priors/control, seed = sim id), so the
  adaptive stop reproduces the offline replay exactly where the replay is not capped: MOZ 21,161 in 7
  checks (identical batch list). COD stopped at 75,262 vs replay 69,586 only because
  max_batch_predictive = 10k steps overshoot. UGA's "stalled" flag is transient (one new best draw at
  ~20k collapses ESS_all; slope recovers to >1 after 70k), so it is NOT set at the cap.
- **Ensemble memory (measured, calc_model_ensemble 1.4.0):** one sampled config ~65 B per loc x day
  (continental 2018 = 8.9 MB, not the 21 MB older notes assumed); master peak ~0.8 GB + ~90 B x n_loc x
  T x |B| x n_iter; each PSOCK worker = base (1.0 national/regional, 1.65 continental GB) + |B| x
  config (the clusterExport of param_configs). Continental 168 workers: 108 -> ~0.45 TB, 300 -> ~0.76
  TB, 500 -> ~1.1 TB. National 1000x5 on 16 workers: master 2.3 GB.
- **calc_model_parameter_sensitivity (HSIC, n = 2000) holds one n x n kernel per parameter**: 67 GB
  master and 16 min at p = 916 (Kenya counties). Dominates multi-location master RSS, not the ensemble.

Related: [[window-and-ensemble-cost-scaling]], [[v140-ws2-pipeline-traps]], [[remote-process-mgmt-traps]].
