# Replay fixture provenance

The `replay_*.rds` fixtures in this directory are frozen recordings of the
**Python `laser-cholera` transmission engine**, used to validate the pure-R
port (Tier B parity — see `migrate-laser-r.md` §7).

Each fixture holds every PRNG call the oracle made, in order — tick, phase,
draw site, kind, trial count, probability/rate, and result — plus the result
channels it produced. Replaying them removes the PRNG from the comparison, so
the R engine can be checked draw-for-draw rather than distributionally.

**These tests require no Python.** `laser-cholera` is not a dependency of this
package and is never imported by the test suite.

## Oracle build

| | |
|---|---|
| `laser-cholera` version | `0.16.1` |
| commit | `06938c1513b1c4d38fc902b62d1954ba4449ebe3` |
| source tree SHA-256 | `af85c17a8b8226254c60e8b56f5195b2d69977a955a713e26ee7574cb34bcb1d` |
| numpy | `2.5.3` |
| Python | `3.13.15` |
| generated | 2026-09-10, local laptop, `mo` conda env at `/software/conda/envs/mo` |

The commit SHA and source SHA-256 are recorded rather than just the tag: a tag
can move, and a local editable checkout can drift from the wheel pinned in
`inst/python/environment.yml`. The entire correctness argument for the port
rests on these fixtures, so "which build produced this" must stay answerable.
Each fixture also carries its own copy of this metadata under `$meta$oracle`.

## Fixture inventory

| File | Components | Ticks | Patches | Draws | Size | Role |
|---|---|---|---|---|---|---|
| `replay_susceptible_census.rds` | Susceptible, Census | 60 | 40 | 120 | 50 KB | A-0 spike; keeps the two-component subset path exercised |
| `replay_full_pipeline.rds` | all 9 ported | 60 | 40 | 1,315 | 250 KB | the workhorse: every one of the 22 draw sites fires |
| `replay_single_location.rds` | all 9 ported | 60 | 1 | 1,315 | 10 KB | `npatches = 1`, which takes different reshaping paths |
| `replay_full_length.rds` | all 9 ported | 1,398 | 40 | 30,751 | 5.35 MB | the regression anchor |
| `tier_a_default.rds`, `tier_a_single_location.rds` | — | — | 40 / 1 | — | 44 KB | Tier A deterministic precomputation |

`DerivedValues` is absent from all of them; it is the one component still
unported, so `spatial_hazard` and `coupling` are the two missing channels.

**The full-length fixture earns its 5.35 MB.** Two real port bugs survived the
60-tick fixtures and were caught only at full length: a float32 rounding
difference in the `Vaccinated` dose-one pro-rata split that first moved one
person between `S` and `V1` at **tick 99**, and the environmental reservoir
drift that first breached a naive relative tolerance at **tick 37** of this
config. A short fixture covers every draw *site* but nothing that *accumulates*.

## Regenerating

The generator lives in `claude/oracle/` and needs the `mo` conda env. It is a
historical artefact, not a build step:

```bash
cd claude/oracle
python truncate_config.py --in ../../inst/extdata/config_default.json \
    --nticks 60 --out cfg/default60.json
python verify_shim.py --config cfg/default60.json          # shim transparency gate
python dump_fixture.py --config cfg/default60.json \
    --components Susceptible,Census --out out/a0 \
    --label replay_susceptible_census
Rscript -e 'MOSAIC:::laser_convert_oracle_dump("claude/oracle/out/a0", \
    "tests/testthat/fixtures/replay_susceptible_census.rds")'
```

The three A-2 fixtures, which cover the nine ported components:

```bash
cd claude/oracle
PORTED=Susceptible,Exposed,Recovered,Infectious,Vaccinated,Census,HumanToHuman,EnvToHuman,Environmental
python dump_fixture.py --config cfg/default60.json --components $PORTED \
    --out out/full60 --label replay_full_pipeline
python dump_fixture.py --config cfg/moz60.json --components $PORTED \
    --out out/moz60full --label replay_single_location
python dump_fixture.py --config ../../inst/extdata/config_default.json \
    --components $PORTED --out out/full1398 --label replay_full_length
```

then convert each with `MOSAIC:::laser_convert_oracle_dump(<dump>, <rds>)`.

Before trusting a regenerated fixture, re-run the draw-site check:

```bash
python claude/oracle/verify_draw_sites.py
```

It re-derives the list of active `model.prng` call sites from the oracle source
and diffs it per site against both R tables. This exists because the first
version of those tables was built by pairing an ordered list of conceptual steps
against an ordered list of line numbers: five `infectious.py` labels ended up on
the wrong line, one real site was missing and one phantom site was invented, and
because the two errors cancelled, the **count still came to 22** and a
count-based check passed. See CLAUDE.md lesson #15.

Draw sites are recorded as Python `file:line` and mapped to the engine's own
site labels by `.LASER_ORACLE_SITE_MAP` in `R/laser_fixture.R`. Those line
numbers are specific to the build above — regenerating from a different version
moves them, and the map then raises an unmapped-site error rather than silently
mispairing sites.

## The oracle is single-precision, and where that is observable

`params.py` casts most parameters to `np.float32`, and the state arrays `W`,
`Lambda` and `Psi` are float32 too. The R port computes in double. Two
consequences, handled differently and deliberately:

**Where float32 reaches an integer, the port reproduces it.** `round(local_frac
* S_next)`, `round(sigma * progressing)`, `round(phi_1 * comp_doses)`,
`round(nu_*_jt[tick])`, `round(drawn / chi_eff)` and the two `epidemic_threshold`
comparisons all turn a float32 quantity into an integer. There, reproducing the
stored precision is not optional: an integer that differs by one decorrelates
the whole draw sequence. `.laser_f32()` in `R/laser_params.R` does the
round-trip, and the list of affected fields is documented at its definition.

**Where it reaches only a float, the port stays in double** and is the more
accurate of the two. `pi_ij` is the worked example: the oracle's haversine runs
entirely in float32 (`params.py` stores `latitude`/`longitude` as float32 and
`np.radians` of a float32 stays float32), which is why it needs a looser
tolerance than the other precomputed matrices.

The largest such difference is the environmental reservoir `W`, and it is the
only one that **feeds back**: the decay draw's rate is `delta_jt * W`, so W's
representation error re-enters as a draw parameter and accumulates. Over the
1,398-tick default config the drift reaches 1.07e8 absolute against a reservoir
of order 1e14 — about 1e-6 of the channel's own scale, exactly float32
accumulation size — which is why `environmental/decay` carries its own replay
tolerance in `.LASER_SITE_TOL`. Nothing integer depends on it: all 19 integer
channels are bit-identical over the full run.

## Known oracle quirk

A config whose `epidemic_peaks` list is **empty** crashes the oracle at
`params.py:584` (`dict_to_propertysetex` builds a column-less DataFrame and
then reads `.iso_code`). This bites when truncating the default config to a
window containing no peak. `laser-cholera` is read-only, so fixture windows are
chosen to contain at least one peak. The R engine must handle zero peaks
gracefully regardless — nothing stops a user configuring a short window.
