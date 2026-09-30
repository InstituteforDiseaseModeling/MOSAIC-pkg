---
name: reference-keras3-r-api-traps
description: keras3-R API traps verified on this laptop (keras3 1.4.0) — layer_concatenate(name=) is broken, axis is 1-based, and new hyperparameters need the arch_hp whitelist
metadata:
  type: reference
---

Verified on the laptop stack: keras3 R **1.4.0**, TF available in
`~/.virtualenvs/r-mosaic` (so keras/TF-gated tests DO run locally with
`NOT_CRAN=true`; they skip only on CI).

**`keras3::layer_concatenate(list(a, b), name = "x")` raises `KeyError: 0`.** Mechanism:
`split_dots_named_unnamed(list(...))$unnamed` is a *named* empty list when every dot is
named, so `inputs <- c(inputs, dots$unnamed)` attaches `names = c("","")` to the inputs
list, reticulate marshals it as a Python dict, and `Concatenate.build()` does
`input_shape[0]`. Happens for every call form (`list(a,b)`, bare `a, b`, with or without
`axis`). Workaround: build then apply —
`layer_concatenate(axis = 2L, name = "x")(list(a, b))`. This is the sole reason
`claude/psi_evolve/tft/tft_model.R` does not build here; with that one change the smoke
test reproduces the committed 74,831 params exactly.

**`axis` is 1-BASED in keras3 R.** `layer_concatenate(axis = 2L)` on `(B, T, F)`
concatenates over T (verified: (52,32)+(12,32) -> (64,32)); `axis = 1L` errors.
`op_sum(..., axis = -2L)` counts from the end. `keras.ops.slice`'s `start_indices` stay
0-based (the R wrapper does not shift them) and `shape = -1` for "all remaining" works on
the TF backend only.

**`dim()` returns zero-length on a KerasTensor** — `if (dim(x) == 3)` silently becomes
`if (logical(0))`. Use `length(x$shape)`.

**`layer_dense` on `(B, T, n_vars, 1)` shares one weight matrix across `n_vars`.** So a
"per-variable" transform written that way is a *shared* scalar embedding. Prove it with
`l$count_params()`: 1 -> 32 with bias is 64 params total, not 64 x n_vars.

**A new model hyperparameter needs FOUR sites, not one.** `.psi_fit_predict_lstm()` reads
`hp$<knob>`, but `arch_hp` in `R/run_rolling_cv_suitability.R:297-324` is an explicit
`list(...)` **whitelist** — a knob absent from it never reaches the model however it is
passed through `arch_control`. The other two sites are `int_fields` in
`.psi_load_arch_control()` (integer coercion) and the `provenance = list(...)` block
(~line 493). `dlinear_kernel` currently exists only at the read site, so it is
unreachable and the DLinear trunk is permanently k=5.

See [[psi-evolve-redteam-review]].
