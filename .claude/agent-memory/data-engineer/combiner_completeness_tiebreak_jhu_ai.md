---
name: combiner-completeness-tiebreak-jhu-ai
description: Combiner source selection + JHU PHANTOM rows — 4,144 OSF phantom zero-fills (phantom<=>no collection id) masqueraded as JHU observations; dropped at source v0.100.0; observed>imputed>priority rule; peaks observed-only
metadata:
  type: project
---

Fixed 2026-09-30 (MOSAIC-pkg 610e93557 + corrections 7ad56960e/42be07a36; MOSAIC-data 64b69ab).

**JHU phantom rows (the real bug, caught by red-team after my first fix):** raw
`Public_surveillance_dataset.rds` has `phantom == TRUE` on 4,144 country-level rows — weeks the OSF
archive zero-fills without a report: sCh=0, cCh/deaths NA, observation_collection_id NA (phantom holds
exactly when the id is missing). process_JHU_weekly_data now drops them (7,596 rows remain; 4,851
non-phantom sCh==0 rows are real reported zeros, kept). My first write-up's "2,235 weeks returned to
JHU / 4.9x inflation" was 2,187 phantom weeks — I verified numbers but never checked whether the
winning JHU rows were real. Lesson: when a fix makes source X win, audit what X's rows ARE (flags,
collection ids), not just the counts.

**Combiner rule:** per (iso, date_start) one whole row: non-empty > observed (method NA /
observed / documented_zero) > imputed > has-cases > WHO>JHU>AI>SUPP. Deaths completion only from
another observed row with floor(x+0.5)-equal cases; completed week gets min confidence_weight;
`source_deaths` column. Field-wise coalescing rejected (74/231 candidate weeks were different reports).

**Peaks:** est_epidemic_peaks blanks non-observed weeks + drops peaks >50% imputed window; csv and
data/epidemic_peaks.rda regenerated together (159 peaks). Before, 221/372 peaks were Fourier artifacts.

**How to apply:** diagnose combiner changes with source-transition tables
([[surveillance-revision-is-source-precedence]]). Dropping phantoms means AI fourier now gap-fills
those weeks (2,257 weeks, 214k cases at cw ~0.5) — by design, but a fit-target change. MOSAIC-docs
03-data.Rmd JHU paragraph does not yet mention the phantom drop. Daily combined CSV is gitignored.
