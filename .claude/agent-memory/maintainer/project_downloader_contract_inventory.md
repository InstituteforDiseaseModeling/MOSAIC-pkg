---
name: downloader-contract-inventory
description: The five MOSAIC download_* functions and how they differ (return shape, failure signal, atomicity, snapshot naming); the non-atomic-write + newest-wins-resolver corruption trap
metadata:
  type: project
---

Inventory built during the v0.91.5/0.91.6 data-acquisition review (2026-09-18, uncommitted batch).

**Why:** MOSAIC now has five downloaders hitting five external services plus two newest-wins
resolvers. They were written independently and do not share a contract, so "the download step
reported ok" means five different things.

**How to apply:** treat this table as the contract any new `download_*()` must match, and re-check
it whenever one of the five changes.

| function | returns | total-outage signal | artifact safety | snapshot naming |
|---|---|---|---|---|
| `download_IDMC_data` | `invisible(data.frame)` per ISO | `message()` only — **never errors/warns** | partial `dest` left in place | dated **dir** `hdx_<date>/` |
| `download_WB_data` | `invisible(data.frame)` per indicator | `message()` only — **never errors/warns** | writes straight to final path | dated **file**, retrieval date |
| `download_UN_WPP_data` | `invisible(data.frame)` per measure | `stop()` (uncaught `download.file`) | tmp for the gz, direct for the 3 CSVs | dated **file**, retrieval date |
| `download_mobility_od_sources` | `invisible(data.frame)` per source | `message()` only — **never errors/warns** | tmp-free but `unlink()`s short responses | dated **dir** `snapshot_<date>/` |
| `download_EMDAT_data` | `invisible(<character path>)` | `stop()` on all three routes | tempfile + validate + copy (**the good pattern**) | dated file; date means *retrieval* (api), *URL stamp* (portal), or *data cutoff* (dataverse) |

**The corruption trap (demonstrated, not theoretical).** Non-atomic write + a newest-wins resolver
promotes a truncated file to canonical:
`.wb_write_bulk_csv()` writes into the final path; if the write is interrupted the partial file
carries **today's** date, so `.wb_newest_raw()` / `.rank_raw_candidates()` rank it above the
complete portal export, and `process_WB_*_data()` reads a silently-short panel (measured: 3 of 40
countries, no error, no warning). `overwrite = FALSE` then reports `ok=TRUE, note="existing"`
forever. Same shape in `download_UN_WPP_data` (the `all(file.exists(dests))` guard passes on a
truncated file) and `download_IDMC_data`.

**Green-that-means-nothing helpers.** `.idmc_snapshot_summary()` hardcodes `ok = TRUE` for any file
that exists — a 0-byte file and an HTML 502 page both report `ok=TRUE, n_events=0`, indistinguishable
from a country that genuinely has no events. `.wpp_summarise()` returns `ok=TRUE, n_rows=0,
year_min=Inf, year_max=-Inf` for a 0-row CSV. Both verified.

**No `options(timeout=)` raise anywhere.** `?download.file` is explicit: default 60 s "is often
insufficient for downloads of large files (50MB or more) and so should be increased when
download.file is used in packages", idiom `options(timeout = max(300, getOption("timeout")))`.
Abel-Cohen is ~23 MB, the EM-DAT Dataverse xlsx ~10 MB. No retry/backoff in any of the five.

**Write-safety precedent (checked, answer is narrower than it looks).** No function in `R/`
currently writes under `PATHS$DATA_RAW`. `process_WHO_annual_data()` writes to
`PATHS$DATA_WHO_ANNUAL`, whose value *happens to be* under `raw/WHO/annual/...`. So the precedent
exists in effect but was never generalised, and both CLAUDE.md files state the READ-ONLY rule
without exception. Seven new writers + a `PROVENANCE.md` **appender** need an explicit CLAUDE.md
amendment (owner: `ai-architect`), or the next agent will "fix" the downloaders.

Related: [[update-mosaic-data-registry-review]], [[rcmdcheck-baseline-v084]], [[reviewer-checklist]].
