---
name: ai-suitability-build-provenance
description: AI-enhanced default psi build — what the AI data IS (fourier=synthetic, not climate-derived so no climate leakage), confidence-weight mitigation IS wired (B4 fixture), but build is non-canonical/gitignored sandbox
metadata:
  type: reference
---

Red-team review of the AI-enhanced default suitability build (verified 2026-06-19, local laptop). Supersedes wrong claims in earlier review.

**What the AI data IS (no climate circularity).** AI repo `build_weekly_timeseries.py` builds a per-country **Fourier seasonal template purely from OBSERVED cholera weekly fractions** (case-weighted mean of year-normalised weekly fractions, BIC-selected K) — NO climate input. So fourier rows are synthetic temporal reconstructions of annual/quarterly totals, but training climate->cholera on them is NOT fitting climate to itself. Circularity risk = LOW. `source=JHU/WHO` inside the AI file = the ORIGIN db of the disaggregated total, not "observed weekly".

**Composition (sandbox combined, in-window matters).** Of 73,439 AI rows only **1,163 are `observed`** (real mined weekly); rest = 44,382 fourier (synthetic) + 26,951 documented_zero (assumed_zero already dropped by process_AI_cholera_data keep-filter). In the ACTUAL LSTM fit window (>=2015-05-06): AI=9,240 vs direct=11,827; AI is 5,724 documented_zero + 2,707 fourier + 809 observed. AI cases_binary==1 only 10/9,240 -> AI in-window acts almost entirely as NEGATIVE (non-outbreak) climate examples. Binary target comes from est_epidemic_peaks (WHO/JHU/SUPP daily), NOT AI cases -> no AI->label leakage.

**Confidence-weight: median 0.5 not 0.6.** AI cw: min 0.3 / median 0.5 / max 1.0. fourier cw quantiles 0.33/0.475/0.50/0.50/0.90. Direct WHO/JHU/SUPP all cw=1.0. AI is ~100% of pre-2010 rows; direct floor ~2010 (JHU), WHO 2023+. cw alone CANNOT separate synthetic from observed (fourier reaches 0.9).

**Mitigation IS wired (corrects prior doubt).** `use_confidence_weight: true` is PINNED in `inst/fixtures/B4_rolling_cv_spec.yml:49`. est_suitability(arch_control=NULL) loads fixture+prod overrides, so production `ac$use_confidence_weight`=TRUE. `.psi_configure_loss` apply_cw_overlay multiplies sample weight by cw (total-weight-preserving). It's a docstring-only listed knob but genuinely flows: build_suitability_sequences -> data_bundle$confidence_weight_train -> lstm loss.

**BLOCKERS for production (severity HIGH).**
1. [RESOLVED 2026-06-19 in LAUNCH.R, not yet committed by parent] NON-CANONICAL gap: default psi trained on gitignored sandbox; LAUNCH.R called process_cholera_surveillance_data WITHOUT include_ai and never called process_AI_cholera_data. FIX wired into model/LAUNCH.R + LAUNCH_sanitized.R: added `process_AI_cholera_data(PATHS)` before the surveillance combine and set `process_cholera_surveillance_data(PATHS, include_ai = TRUE)`; recipe doc at model/README_psi_provenance.md. AI repo data present (40 cholera_weekly_<ISO>.csv, nested under ai-cholera-data-mining/data/<ISO>/, found via recursive=TRUE), so chain is now fully reproducible. STILL OPEN: est_suitability arch_control in LAUNCH.R is NULL (=B4-fixture "E"-ish default, fit 2015) and does NOT reproduce the shipped "G" psi (fit 2010, rw_subsample=5, multi-seed) — left as TODO(parent) to set final arch_control. use_confidence_weight=TRUE confirmed on default path.
2. Canonical processed data NOT clobbered (verified: canonical mtime Jun 15, sandbox build Jun 18 wrote only to sandbox; canonical combined 2MB no-AI vs sandbox 10.8MB with-AI). SAFE on that axis.
3. compile params MATCH LAUNCH.R exactly (cutoff=NULL,use_epidemic_peaks=TRUE,date_start=2000-01-01,forecast_mode=TRUE,horizon=9,include_lags=TRUE). Parity OK.

See [[ai_source_integration_provenance]], [[reference_who_surveillance_pipeline]].
