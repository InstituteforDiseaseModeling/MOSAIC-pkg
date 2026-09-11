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

Draw sites are recorded as Python `file:line` and mapped to the engine's own
site labels by `.LASER_ORACLE_SITE_MAP` in `R/laser_fixture.R`. Those line
numbers are specific to the build above — regenerating from a different version
moves them, and the map then raises an unmapped-site error rather than silently
mispairing sites.

## Known oracle quirk

A config whose `epidemic_peaks` list is **empty** crashes the oracle at
`params.py:584` (`dict_to_propertysetex` builds a column-less DataFrame and
then reads `.iso_code`). This bites when truncating the default config to a
window containing no peak. `laser-cholera` is read-only, so fixture windows are
chosen to contain at least one peak. The R engine must handle zero peaks
gracefully regardless — nothing stops a user configuring a short window.
