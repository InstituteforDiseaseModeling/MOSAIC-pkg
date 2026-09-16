---
name: reff-resim-ci-review
description: Cori R_eff re-sim posterior CI review (v0.57→0.58) — hot-path grid equivalence proof, resim-recipe parity sites, missing thread-pin landmine for the post-hoc batch
metadata:
  type: project
---

Reviewed the Cori R_eff re-simulation posterior CI addition (calc_Reff.R, add_reproductive_numbers.R, calc_model_ensemble.R grid, plot_Reff.R). APPROVE/GO with thread-pin fix before the 120-core batch.

**Hot-path grid equivalence (PROVEN, reusable fact):** `.mosaic_build_trajectories` time grid changed `which(seq_len(n) %% line_stride == 1L)` -> `seq.int(1L, n, by = max(1L, line_stride))`. Verified in R full-vector across many n: the two are IDENTICAL for ALL strides >= 2 (incl production default 7L -> {1,8,15,22,29,...}). They differ ONLY at stride 1 (old=EMPTY bug, new=full daily). Production caller run_MOSAIC.R:2743 does NOT pass line_stride (uses default 7) => every existing trajectory artifact schema is UNCHANGED. Safe.

**Resim recipe parity sites (must stay in lockstep — Lesson #2 class):** `.mosaic_reff_resim_ci` reconstructs members identically to the real worker `run_param_stoch_simulation` in calc_model_ensemble.R:
- config: `.mosaic_clamp_transmission_params(sample_parameters(PATHS, priors, config, seed=parameter_seeds[p], sample_args=sampling_args, verbose=FALSE))` (worker L462-465 == resim L468-471)
- LASER seed: `seed <- param_idx*1000L + stoch_idx` (worker L648 == resim L518)
- engine: `lc$run_model(paramfile=.mosaic_prepare_config_for_python(cfg), quiet=TRUE)`, import "laser.cholera.metapop.model"
If any of these three drift in the worker, the resim faithfulness gate will start failing — update both.

**Member-weight convention:** ensemble uses `rep(parameter_weights, times=n_stoch)/n_stoch` (param-fastest); member m=(s-1)*nP+p, w[m]=pw[p]/nS. Resim matches. CLAUDE.md note warns `each=` mis-pairs — do NOT switch to each.

**LANDMINE (B1): post-hoc resim path does NOT pin BLAS/Numba threads.** add_reproductive_numbers/.mosaic_reff_resim_ci drive lc$run_model OUTSIDE run_MOSAIC, which is the only place that calls `.mosaic_set_blas_threads(1L)` (run_MOSAIC.R:981, with a comment explaining the in-process LASER path oversubscribes numba/MKL/OpenBLAS otherwise). Running ~27 concurrent post-hoc processes on a 120-core VM with unpinned numba => 27x120 threads thrashing. Fix = pin 6 thread env vars (or call .mosaic_set_blas_threads(1L)) per worker before engine import. Same failure class as project_est_suitability_tf_thread_oversubscription. File I/O IS safe (per-output_dir tempfile+atomic rename, no shared scratch).

**Gate math untested LASER-free (N1):** re_vec/agg_rel_err/cor_median + gate_pass only exercised by engine smoke test; unit suite covers .mosaic_reff_to_mat, weighted-quantile reduction, burn-in masking (logic-mirror not integration), grid equivalence. A synthetic-matrix gate test would close it.
