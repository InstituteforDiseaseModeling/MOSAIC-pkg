# Allocate simulation state for the R transmission engine

State mirrors the Python engine's layout exactly: every per-patch series
is conceptually a `(nticks + 1) x npatches` array with **time in rows
and patch in columns**, and the transpose to the `[patch, time]` result
orientation happens once at the end in `sim_results.R`. Flipping to
`[patch, time]` early would mean re-deriving every off-by-one in the
port, so it is deliberately not done.

## Usage

``` r
sim_alloc_state(nticks, npatches)
```

## Arguments

- nticks:

  Integer number of simulation ticks.

- npatches:

  Integer number of patches.

## Value

An environment holding `rows` (the per-tick environments), `coupling`,
and the shape/prototype metadata the results assembler and the invariant
checker read.

## Details

Storage mode is per field, not uniform: anything counting people or
events is `integer` (inheriting `np.int32`'s rounding discipline), while
rates, hazards and continuous quantities are `double`.

## Why one environment per tick

State is `state$rows`, a list of `nticks + 1` **environments**, one per
tick, each holding every channel for that tick. Reading is
`state$rows[[row]]$S`; writing is `state$rows[[row]]$S <- v`. The phases
bind the two rows they need once (`rh <- state$rows[[here]]`,
`rn <- state$rows[[nxt]]`) and then address channels by name.

This is a measured decision, not a preference, and it is the second such
change to this structure – the history is worth keeping because both
moves were driven by the same mechanism.

Held as one `(nticks + 1) x npatches` matrix per channel, a row write
cost about **15 microseconds** against 0.65 for a row read, because the
subassignment copied the entire matrix. Moving to a per-channel *list*
of per-tick vectors took a full 1398-tick run from **2.91 s to 1.18 s**
and allocation from 4.9 GB to 987 MB, on the reasoning that "a list
element write is a pointer store, so it does not copy".

That reasoning was wrong, and measurably so. The channel lists were
reached through an environment, so `state$S[[i]] <- v` is a
*subassignment* into `state$S`: the `*tmp*` fetch raises the list's
reference count, and `[[<-` therefore duplicated the whole `nticks + 1`
pointer vector on **every write**.
[`tracemem()`](https://rdrr.io/r/base/tracemem.html) confirms a copy per
write, and the cost was linear in `nticks` (1.5 / 3.3 / 5.9 / 11.6
microseconds per write at 200 / 700 / 1399 / 2800 rows) – so it was
invisible at fixture scale and worst in production. Padding the channel
lists to 4x their length without touching the dynamics moved a 1.02 s
run to 2.08 s, pricing the copies at **0.354 s, about 35 percent of the
run**.

An environment binding is a pointer store with no such copy, and there
is no longer any long vector to duplicate: `state$rows` is written once,
here, and never again. Per-write cost falls from **6.15 to 0.20
microseconds** and stops depending on `nticks`; reads go from 0.13 to
0.18, which is why the phases hoist the row lookup out of the channel
accesses rather than repeating `state$rows[[here]]`.

The profiler had attributed this cost to the phase bodies (52.65 percent
self time) and to `<GC>` (11.16 percent), which is where allocation
churn always lands – not to anything that looks like a state write. See
lesson 17 in `CLAUDE.md`.

## Reference semantics

Both `state` and each element of `state$rows` are environments, so the
phase functions mutate them in place rather than returning a modified
copy. The `state <- phase(state, ...)` idiom in the tick loop is a
convention rather than a copy: the value returned is the same
environment that went in. Do not rely on a pre-call snapshot of the
state remaining unchanged.
