---
name: psi-refit-v0100
description: v0.100.0 production psi refit on dugong (2026-09-30): settings, 15.6 min at parallel_seeds=5, new-vs-shipped psi agree only r~0.19 over the 2023+ engine window, clamp/manifest-bloat caveats
metadata:
  type: project
---

The v0.100.0 production psi was refit on dugong on 2026-09-30. Outputs were staged at `MOSAIC-pkg/claude/rebuild_stage2/psi/` on the laptop.

**Settings:** v7.3 features, target_D_rate_per_country_floored, bias_correct=TRUE, snf_k5, n_seeds=5 (seeds 11-55), fit window 2015-01-01 to 2026-08-20 (auto-detected), prediction window 2018-01-01 to 2027-04-29 (end auto-detected). The run used parallel_seeds=5 with `MOSAIC_PSI_CORE_BUDGET=170`, which gave 34 TF threads per worker and a loadavg of about 48 on 176 cores. It took **15.6 min**. The shipped psi (v0.77.0) used n_seeds=3 and an explicit fit_date_stop=2025-06-01.

**Running the fit from a separate library needs `R_LIBS=<lib>` in the launch env.** PSOCK seed workers call `library(MOSAIC)` from the default libPaths. Without `R_LIBS`, the version check errors and the fit silently falls back to serial.

**How it compares with the shipped psi:**
- Median per-country r is 0.39 over 2018+ but only **0.19 over the 2023+ config window**. The median |diff| is about 2x the within-run seed IQR.
- Some big level shifts have no matching input change: LBR r=-0.46, UGA and KEN psi* roughly halved, NAM/LBR/SSD psi* roughly doubled. Treat these as refit variability plus the extra 14 months of training data, not as input effects (see [[psi-artefact-provenance-v077]]).
- Big target changes in the panel: BEN cases x5.9, ZAF target mean /3, CIV /2.6, UGA x2.
- In-sample r(psi, target) is higher for the new psi on the common 2018-2025-06 window (0.81 vs 0.44). This is in-sample and was not compared against a climatology baseline, so it is not evidence of skill.

**Caveats:**
- 37% of cells sit at the 0.01 clamp. GMB is 100% clamped, and BFA, SEN, GHA and GNB are each ~90% clamped.
- The bias correction fell back to identity for 9 countries and was guarded for 7. The amplitude check warned for BFA and ZAF (amp_ratio 2.01).
- The manifest is now **11.9 MB**, because `rw_diagnostics$fold_predictions` is embedded. The v0.77 manifest was 4 KB, so committing it to model/input would bloat git.

**No fill tail:** "Dropped 0" was genuine here. All terminal pred_raw runs longer than 7 days are the 0.01 clamp, and the day and week files both end 2027-04-29.
