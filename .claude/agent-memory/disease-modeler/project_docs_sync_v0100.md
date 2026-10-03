---
name: docs-sync-v0100
description: MOSAIC-docs synced to MOSAIC-pkg v0.100.0 then v0.100.1 (priors v17.0 / config v6.0), 2026-09-30; notation choices, package plot defects that bite every docs regen, and spec facts verified on the way
metadata:
  type: project
---

Synced twice on 2026-09-30: v0.100.0 (260981e..d43e022, render 73d99ac) and v0.100.1 (11f6f06 text, b7b0a0b tables/figures, 9193e21 render; live on docs.idmod.org/MOSAIC-docs at "11:36 PM PDT").

- Registered notation: beta_{j0}^{tot}, p_beta, omega^{mob}/gamma^{mob}, t_0 (now = date_start), lambda_j. Avoid d_1/d_2 for doses (d_jt = mortality); describe dose pairing in words.
- R0_env formula unchanged under per-capita dose W/N (slope 1/(kappa N) x S~N = 1/kappa).
- Package plot defects (handed off; fixed in pkg v0.101.0, see below): plot_seasonal_transmission(+_example) hardcode legend years "1994-2024"/"2023-2024", plot_seasonal_clustering hardcodes "2014-2024" AND reads DOCS_TABLES/pred_seasonal_dynamics.csv that nothing writes any more (docs copy is 2024-11; its clusters differ from the current daily ones). Workaround used: relabel display strings at render time + 7-day block means of model/input/pred_seasonal_dynamics_day.csv (Ward clusters verified identical). Script: MOSAIC-pkg/claude/docs_refresh_v01001/regen_docs_figures.R (gitignored scratch).
- plot_CFR_by_country beta panel: 1000-point grid hides the AFRO spike (sd ~1e-4); colours come from the palette, not red/blue.
- plot_vaccine_effectiveness panels B/E/C/F show the DATA fits (param_vaccine_effectiveness.csv), not the shipped priors (5%-widened CI; phi ~2-5x wider).
- **Fixed in pkg v0.101.0 (release/v1.0 commits, merged):** legend years derived from the data, plot_seasonal_clustering reads the daily fits (weekly means; DOCS_TABLES only as legacy fallback), CFR beta adaptive grid + sqrt y, vaccine panels subtitled as data fits. The relabel workaround is no longer needed; plot_vaccine_effectiveness's "Density" (grid-normalised mass) is still open.
- Docs vaccination section + vaccination_*.png describe WHO-ICG-only doses (data_vaccinations_WHO_*, unchanged since 2025-08) but config nu_1_jt uses deduplicated GTFCC+WHO; open follow-up.
- Facts verified: phi_2 < phi_1 in ~48% of independent draws and phi_2 is inert when nu_2_jt = 0 (default); p_beta Beta(5.48,10.10) realized 95% 0.14-0.60 (fit_beta_from_ci keeps the mode, cannot hit the requested 0.1-0.5); 24/40 countries keep template V1/V2 priors (means 1%/0.5%) which exceed their prop_R means (median 0.002).

**How to apply:** the seasonal/CFR plot functions are fixed (v0.101.0), so regenerate docs figures from the package directly. Reuse the symbols above. See [[v0100-rebuild-priors-v17]].
