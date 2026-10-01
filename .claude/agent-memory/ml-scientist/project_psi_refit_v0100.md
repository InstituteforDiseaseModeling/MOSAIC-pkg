---
name: psi-refit-v0100
description: v0.100.0 production psi refit on dugong (2026-09-30): 5- and 10-seed runs, disjoint-seed replicate shows the 10-seed ensemble is stable (r 0.96) so the change vs shipped (r 0.24) is systematic; clamp, guard-edge and manifest caveats
metadata:
  type: project
---

The v0.100.0 production psi was refit on dugong on 2026-09-30. Outputs were staged at `MOSAIC-pkg/claude/rebuild_stage2/psi/` on the laptop.

**Settings:** v7.3 features, target_D_rate_per_country_floored, bias_correct=TRUE, snf_k5, n_seeds=5 (seeds 11-55), fit window 2015-01-01 to 2026-08-20 (auto-detected), prediction window 2018-01-01 to 2027-04-29 (end auto-detected). The run used parallel_seeds=5 with `MOSAIC_PSI_CORE_BUDGET=170`, which gave 34 TF threads per worker and a loadavg of about 48 on 176 cores. It took **15.6 min**. The shipped psi (v0.77.0) used n_seeds=3 and an explicit fit_date_stop=2025-06-01.

**Running the fit from a separate library needs `R_LIBS=<lib>` in the launch env.** PSOCK seed workers call `library(MOSAIC)` from the default libPaths. Without `R_LIBS`, the version check errors and the fit silently falls back to serial.

**How it compares with the shipped psi:**
- Median per-country r is 0.39 over 2018+ but only **0.19 over the 2023+ config window**. The median |diff| is about 2x the within-run seed IQR.
- Some big level shifts have no matching input change: LBR r=-0.46, UGA and KEN psi* roughly halved, NAM/LBR/SSD psi* roughly doubled. **CORRECTED by the 10-seed replicate below: these shifts are NOT refit noise.**
- Big target changes in the panel: BEN cases x5.9, ZAF target mean /3, CIV /2.6, UGA x2.
- In-sample r(psi, target) is higher for the new psi on the common 2018-2025-06 window (0.81 vs 0.44). This is in-sample and was not compared against a climatology baseline, so it is not evidence of skill.

**Caveats:**
- 37% of cells sit at the 0.01 clamp. GMB is 100% clamped, and BFA, SEN, GHA and GNB are each ~90% clamped.
- The bias correction fell back to identity for 9 countries and was guarded for 7. The amplitude check warned for BFA and ZAF (amp_ratio 2.01).
- The manifest is now **11.9 MB**, because `rw_diagnostics$fold_predictions` is embedded. The v0.77 manifest was 4 KB, so committing it to model/input would bloat git.

**No fill tail:** "Dropped 0" was genuine here. All terminal pred_raw runs longer than 7 days are the 0.01 clamp, and the day and week files both end 2027-04-29.

**10-seed follow-up (same day).** Run A used seeds 11-110 (23.2 min at parallel_seeds=10, budget 170). Replicate B used disjoint seeds 121-220 (`seed_base=121L`, budget 80, run concurrently).
- **Replicate agreement over 2023+ (median per country):** A vs B r=0.964, mean|diff| 0.025. A vs the 5-seed run: r=0.956, mean|diff| 0.032. A vs shipped: r=0.24, mean|diff| 0.128. Replicate noise is about 20% of the change vs shipped.
- **So the shifts are systematic.** LBR, KEN and UGA reproduce at A-vs-B r≥0.97 and are real consequences of the new panel, fit window and code. Do not dismiss big psi changes as run-to-run noise without a disjoint-seed replicate. The ensemble is far more stable than single-seed keras.
- **Still level-unstable at 10 seeds:** CIV (mean ratio A/B 1.36), SLE (0.72), and GMB (fully clamped).
- **The bias-correction amplitude guard sits right at its threshold.** amp_ratio≈2.00-2.02 flipped BDI, LBR and ZAF between replicates.
- **The 5-seed run is not an independent check on the 10-seed run:** the 5 seeds (11-55) are a prefix of the 10 (11-110). Use seed_base for independence.
- **Slim manifest:** dropping only fold_predictions takes it from 22.7 MB to 17 KB.
