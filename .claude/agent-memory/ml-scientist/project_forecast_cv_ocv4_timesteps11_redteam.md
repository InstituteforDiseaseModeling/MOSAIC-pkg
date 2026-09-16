---
name: forecast-cv-ocv4-timesteps11-redteam
description: OCV-4 forecast-CV psi red-team — timesteps=11 forced by rw_test_months=3 (ts=13 → 0 CV steps → hard error); true window floor is 88d not 89-92d; re-validation gate baseline is ambiguous
metadata:
  type: project
---

Red-team of `claude/forecast_cv_ocv4_q2yr_run.R` es_spec (`rw_test_months=3, timesteps=11, rw_step_months=1, n_seeds=10`), verified against committed code 2026-07-06. VERDICT: GO with a mandatory re-validation gate.

**timesteps=11 is FORCED, not chosen.** `.psi_make_rw_cv_steps` (rolling_cv_suitability.R:33-56): `min_test_days = timesteps*7+7`. Recomputed all 9 cutoffs:
- ts=13 → min_test=98 → **0 RW steps every cutoff** → `.psi_build_data` hard-errors ("0 RW steps generated", build_suitability_sequences.R:385). The run cannot execute at 13/3.
- ts=12 → min_test=91 → viable (30-42 steps) but drops ~40% of folds (only exact-91d windows survive; 88/90d dropped).
- ts=11 → min_test=84 → 51-63 steps, all windows clear.

**The real window floor is 88 days, NOT the SPEC-comment's "89-92d".** `test_end = min(raw_end, cutoff-1L)` (line 54) off-by-one + calendar-month arith yields spans 88-91 (Feb-containing windows hit 88). ts=11 (84d) clears with 4d margin. SPEC comment is imprecise but conclusion holds.

**timesteps propagates as a REAL architecture change** (not just a CV-window check): `.psi_build_data(timesteps=ac$timesteps)` → `.psi_build_sequences` builds `array(dim=c(n,timesteps,feats))` (build_suitability_sequences.R:61) → FiLM trunk `timesteps <- dim(X_train)[2]` (lstm_film_suitability.R:100). Final LSTM uses `return_sequences=FALSE` (last hidden state, no pooling), so 11 vs 13 weeks genuinely changes the recurrent memory. rec_dropout=0.10 active → per-seed non-determinism at all timesteps. **Re-validation is warranted.**

**Re-validation gate baseline is AMBIGUOUS (MAJOR).** PLAN §2.5 says "re-validate vs psi_arch_bench baseline (timesteps=13)". But `claude/psi_arch_bench/run_benchmark.R:189` ran at **timesteps=5L**, and the B4 *fixture* is timesteps=13. Three different numbers. Correct gate: fit psi at ts=11/test=3 AND ts=13/test=5 on ≥1 cutoff, compare pooled (10-seed logit-median) psi vs held-out obs on shape (cross-country Pearson) + WIS, NOT MAE alone (MAE rewards flat psi — the exact failure lstm_v2 fixes). Fallback ts=13/rw_test=5 is self-consistent (min_test=98 « ~150d 5-mo window).

**Things that PASSED:**
- Spec-hash: `.rcv_psi_spec_hash` serializes arch_control (timesteps/rw_test_months/rw_step_months IN hash), strips only parallel_seeds; `.rcv_strip_date_keys` strips only the 4 date keys. prefit↔calibrate agree.
- Flat-fill retired for THIS spec: covariates+ENSO cover to 2026-10-29 for all 4 ISOs; latest cutoff 2026-01 + 3mo → scored window ends ~2026-04, inside coverage → NO na.locf flat-fill (unlike 6-mo runs). `.drop_filled_prediction_tail` IS wired into lstm_v2 path (est_suitability.R:348, commit dc53617c) — supersedes the stale "lstm_v2-unfixed" memory for the fill-drop mechanism.
- parallel_seeds=10 on dugong (1.5 TiB): 10×6GB=60GB « 85% ceiling. OOM constraint was 32GB-laptop-specific; safe here.
- Pooling logit-MEDIAN (ensemble_suitability.R:343). Matches D2.

**Global-fit caveat (MINOR):** psi fit pools ALL ISOs ≤T (country_pool="all_mosaic"), so per-country "independence" is only at the CALIBRATE stage — psi itself shares cross-country info. Fine for conditional model-skill framing (D4 hindcast), but the 4-country comparison is not psi-independent; note it.

**PREFLIGHT ACCURACY AUDIT (2026-07-07): recommend CHANGE ts=11→13 via rw_test=5.**
- ts is the ONLY lever forced by rw_test=3. Decouple: set `rw_test_months=5L, timesteps=13L` (fixture/production/B4 values). min_test=98d « ~150d 5-mo IS-CV window → 30-42 steps every cutoff, viable. This restores production 13-week recurrent memory AND the exact CV window the fixture was validated on (~0.224 median Pearson). The 5-mo IS-CV test window is INTERNAL to the psi fit (train_end→test within IS≤T); it does NOT change what OOS window MOSAIC forecasts (that's SPEC$horizons_months=3, unaffected). Earlier analysis called ts=13/rw_test=5 "conservative, not leaky". Real skill at risk from ts=11 is small but non-zero (rec memory truncation on a weak signal) and ENTIRELY avoidable — no reason to eat it.
- **n_seeds=10:** production default is 5 (fixture=3→prod 5). 10 is ABOVE production = a real (cheap, legit) win: halves pooled logit-median noise vs 5. On dugong 1.5TiB, parallel_seeds=10 fits. KEEP 10. Could go 15-20 for marginal further denoise but diminishing; 10 is fine.
- **All other arch_control = production/B4 fixture values** (units 128/64/32, dropout 0.3, rec_dropout 0.10, l2 5e-4, lr 1e-3, epochs 200 early-stopped @patience 10, batch 128, region_map snf_k5). NONE are "fast" settings. Resolve in .psi_load_arch_control (run_rolling_cv_suitability.R:161) = fixture + {n_seeds, region_map} overrides. es_spec sets NO feature_set → falls to est_suitability default `feature_set="v7.3"` (38 screening-informed features), response_var=transmission_intensity, bias_correct=TRUE. All correct/best.
- **feature_set="v7.3" (38 feats) vs "default" (~85 candidates):** v7.3 IS the recommended richest-LEGITIMATE set (multi-discipline screening selected it; "default" is the unscreened kitchen-sink that v7.3 was chosen OVER). KEEP v7.3. Not a lever.
- **Realized covariates (D4):** correct choice to MAXIMIZE demonstrated MODEL skill; honestly labeled HINDCAST throughout. NOT leakage — covariates are climate/ENSO only; no post-T CASE info enters psi features (psi is trained on cases≤T binary, predicts from covariates). Perfect-foresight covariates are the intended, disclosed design.
- **Global all-ISO fit:** HELPS focal-country psi (more training signal for the shared trunk); does not dilute — FiLM country/region embeddings specialize per-country. Legit accuracy win. KEEP all_mosaic.
- VERDICT: CHANGE-FIRST — flip to rw_test=5/ts=13, keep everything else. Then the PLAN 2.5 "gate" is moot (we ARE the production config, no shortened-memory risk to validate).
