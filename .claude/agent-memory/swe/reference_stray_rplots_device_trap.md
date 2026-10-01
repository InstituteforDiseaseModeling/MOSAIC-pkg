---
name: stray-rplots-device-trap
description: How to find what leaves Rplots.pdf (trapping options(device=) recipe) and why plot_* funcs cause it; render_MOSAIC_figures holds pdf(NULL) since the v0.100 rebuild
metadata:
  type: reference
---
Rplots.pdf in a cwd = something drew with no device open (Rscript default device = pdf("Rplots.pdf")).

**Find it:** `options(device = function(...) { print(sys.calls()); grDevices::pdf(NULL) })` then run the
pipeline on a copy of a real run dir in a temp wd. Each hit prints the exact stack. Note the hit may show
under `ggsave -> get_plot_background` because ggsave forces the lazy `plot=` promise (the plot_* call
itself prints).

**Offenders (print/grid.arrange unconditionally):** plot_spatial_hazard, plot_diffusion_pi,
plot_departure_tau, plot_mobility_flux_matrix, plot_mobility_flux_network (grid.arrange), plus
plot_model_likelihood when verbose. Many legacy data plot_* also print (not on run path).
Fix (commit c238e95c5, rebuild branch): render_MOSAIC_figures + both PSOCK render workers hold pdf(NULL).
add_reproductive_numbers opens no device.

**Test trap:** an earlier test's open device absorbs the draws and false-passes -> `graphics.off()` first,
then local device option counter + temp wd (test-render_spatial_group.R).
