---
name: state-rows-subassign-refcount
description: In the R engine, `state$rows[[k]]$ch <- v` copies the whole rows pointer vector or not depending on refcount CONTEXT — free at top level, copies inside any function taking `state`, or after any .sim_gather(); this is how CLAUDE.md lesson #18 recurs
metadata:
  type: reference
---

# `state$rows[[k]]$ch <- v` copies — but only in some contexts

The v0.73.0 state representation is `state$rows`, a list of per-tick
environments. The ten per-tick phases write through a **hoisted row binding**
(`rh <- state$rows[[here]]; rh$S <- v`), which is a pure environment write and
never copies. `sim_state.R`'s own documentation, however, gives the house idiom as
`state$rows[[row]]$S <- v` — and **that form is a nested subassignment on the
list**, which duplicates the whole `nticks+1` pointer vector whenever the
reference count allows.

Whether it copies is **context-dependent**, which is why it is easy to miss.
Measured with `tracemem(state$rows)`:

| context | copies? |
|---|---|
| write at top level, fresh state | **no** |
| write after any `.sim_gather(state, …)` call | **yes** |
| write inside a function that received `state` as an argument | **yes** |
| the real `sim_phase_derived_values()` (both of the above) | **yes**, once per tick |

So a `tracemem` check in the REPL says "clean" while the production path copies.
Always reproduce the real calling context.

## Consequences

`sim_derived.R` writes `spatial_hazard` in a `for (i in seq_len(nticks))` loop
using the copying form, giving **O(nticks²)** allocation: `nticks × (nticks+1) × 8`
bytes = 15.6 MB at the default 1,398-tick window, 129 MB at a 2015-start window.
Measured per-tick cost of that phase rises linearly in `nticks`
(5.7 / 8.6 / 10.0 / 14.3 µs at 175 / 350 / 700 / 1398 ticks) where the hoisted
form is flat at 5.7-6.4 µs. 11 ms/run at 1,398 ticks, ~105 ms/run extrapolated at
4,018. Reported as ENG-A-01 in the v0.84.0 deep review; not fixed as of v0.84.0.

## Rules

1. Never write `state$rows[[k]]$channel <- v`. Bind the row first:
   `e <- state$rows[[k]]; e$channel <- v`.
2. Never take a second reference to `state$rows` (`rws <- state$rows`) — it makes
   every later subassignment copy, permanently, and R's refcount is not decremented
   when the alias leaves scope.
3. When benchmarking this structure, the **container-inflation experiment**
   (pad the list to 4x its length, run the same ticks) is contaminated if you build
   the padded list via a local: `rows <- st$rows; length(rows) <- n; st$rows <- rows`
   leaves refcount 2 and the arm then copies on every write. My first pass
   over-reported the effect exactly this way. `tracemem` the padded arm before
   quoting its number (CLAUDE.md lessons #17 and #18).

Related: [[reference_rengine_cost_model]] for the rest of the engine cost model,
[[reference_engine_oracle_version_trap]] for what the replay gate does and does
not certify.
