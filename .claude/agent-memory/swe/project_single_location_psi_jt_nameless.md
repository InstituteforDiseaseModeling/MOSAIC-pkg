---
name: single-location-psi-jt-nameless
description: get_location_config() psi_jt has NO column names + date_start/stop are strings — align psi swaps positionally on seq.Date(), not on colnames
metadata:
  type: project
---

When injecting a replacement `psi_jt` into a SINGLE-LOCATION config from
`get_location_config(iso=...)`, you CANNOT align on `colnames(cfg$psi_jt)` — the
single-location `psi_jt` is a `[1 x n_days]` matrix with **NULL column names**.
(`make_config_default.R:364-372` aligns via `acast(iso ~ date)` rownames/colnames
because it builds the multi-iso matrix fresh; that pattern does NOT transfer to a
single-location config that has already dropped the names.)

**Why:** a pre-flight psi-swap on hedgehog (psi_percapita validation, 2026-06-18)
produced `swapped psi_jt = 1 x 0` / `mean = NaN` because `match(colnames(cfg$psi_jt), ...)`
matched against NULL. Caught before any calibration.

**How to apply:** reconstruct the config's daily date axis positionally —
`seq.Date(as.Date(cfg$date_start), as.Date(cfg$date_stop), by="day")` — its length
equals `ncol(cfg$psi_jt)` by construction. Then
`sel <- match(as.character(target_dates), colnames(new_psi))` against the acast'd
variant. Two extra traps: (1) `cfg$date_start`/`cfg$date_stop` are **character
strings**, not Date objects — wrap in `as.Date()` or `seq.Date` errors with
"'from' must be a finite number". (2) LSTM psi forecast files can end a few days
short of the config `date_stop` (e.g. variant horizon 2026-10-29 vs config
2026-11-01 = 3-day tail gap). Verify the gap is TAIL-ONLY (no interior NAs) then
LOCF-fill a bounded tail (<=7 days); STOP on any interior NA or larger tail gap.

Harness: `MOSAIC-pkg/claude/psi_percapita_sandbox/psi_validation_harness.R`.
Related: [[hedgehog-run-infra]].
