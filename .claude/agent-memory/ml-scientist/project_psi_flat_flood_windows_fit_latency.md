---
name: psi-flat-flood-windows-fit-latency
description: NGA-2024-H2 / MOZ-2025-H1 flat frozen-psi in flood outbreaks is a fit-window/label-inconsistency effect, NOT absent flood input; flood covariate IS populated and is the strongest screening class
metadata:
  type: project
---

Two flood/cyclone cholera outbreaks that OCV-4 forecast-CV MISSED, investigated from the psi model side (frozen psi cache dugong:~/MOSAIC/MOSAIC-pkg/claude/forecast_cv_ocv4_q2yr/psi_cache/psi_<cutoff>.csv).

**Finding: fit-window/data-latency + training-label inconsistency, NOT absent flood input.**

Raw psi (pred_raw) confirmed flat in FROZEN fit (<=2024-01) for both windows, but climbs MONOTONICALLY as the fit date advances past the event (same calendar window):
- NGA 2024-H2: pred_raw max 0.013 (fit24-01) → 0.027 (24-04) → 0.113 (24-07) → 0.30 (24-10) → 0.42 (25-01).
- MOZ 2025-H1: pred_raw max ~0.05 flat through fit25-01, then jumps to 0.28 at fit25-04.
Controls (LSTM CAN produce high psi): NGA 2025-H1 raw max 0.46; MOZ 2024-H1 raw max 0.63.

**Flood covariate IS populated** (MOSAIC-data/processed/cholera/weekly/cholera_country_weekly_suitability_data.csv, coverage to 2026-10-29): NGA emdat_flood_prob peaks 0.74 (2024-08-29), anom 0.56; MOZ 0.21 (2025-02), anom 0.10. So input is present and strong — hand-off to data-engineer is NOT warranted for these two windows (rule out absent-input hypothesis).

**Root mechanism = training-period flood↔cholera decoupling.** In the <=2024-01 training window the flood→target_D coupling FLIPS year-to-year per country: NGA 2021 flood_prob 0.5-0.58 → target_D=1; NGA 2022 flood_prob 0.5-0.85 → target_D=0 (cases 0/NA). MOZ 2019-2022 repeated flood_prob 0.3-0.56 → target_D=0, only 2022-12/2023 couples. Gradient descent correctly learns flood is a near-zero-net / sign-flipping predictor → predicts flat psi. Adding a post-event year that DOES couple tips the mapping → psi rises. This is an honest weak-signal / label-noise limit, consistent with arch_bench: NO NN beats seasonal_naive/persistence (best NN lstm3 MAE 0.150 vs seasonal_naive 0.060), all NNs negative-bias (under-predict).

**Flood features DO survive screening and ARE main-input (not FiLM):** v7.3 = strongest signal class, all sc>=0.76 (MOSAIC-Mozambique/claude/minfeat_v7_iterations.md). All 38 v7.3 features feed the LSTM trunk; FiLM conditions ONLY on region_id/country_id embeddings (lstm_film_suitability.R). psi* attenuation is MOOT here (raw already ~0). Scaler is train-only (build_suitability_sequences.R ~L292) — correct for leakage, and part of why later fits differ.

**Verdict: model/data-signal-side, but a genuine weak-signal limit, not a fixable bug.** The flood→psi link is learnable-but-not-robustly-learned because the training labels don't support it consistently. Candidate mitigations (all speculative, none validated): (a) higher/asymmetric sample weight on flood-coincident positive weeks; (b) target reformulation that credits flood-onset lead (target_D floors episodic spikes toward 0 in off-years); (c) subnational suitability so national flood_prob isn't diluted by no-cholera provinces (see [[project_psi_saturation_country_regimes]]). Do NOT oversell any of these — the honest read is psi is not forecast-grade for episodic flood shocks.

Note: OCV-4 psi cache ran parallel_seeds=10 on dugong (1.5TiB) — fine there; violates the 32GB OOM constraint in [[project_lstm_v2_v034_plan]] only on small boxes.
