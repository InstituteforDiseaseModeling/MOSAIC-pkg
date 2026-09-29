---
name: reference_roster_readme_drifts_on_scope
description: The roster README's four tables stay correct on counts/membership but drift on scope wording; the agent frontmatter description is the source of truth, so diff the README against it, not against itself
metadata:
  type: reference
---

`.claude/agents/README.md` has four places that must agree with the agent files — quick-reference
table, roster table, routing cheat-sheet, verification counts. Audits keep checking the wrong axis.

**Counts and membership do not drift.** Verified clean at v0.84.0: "eight agents", "six development
& maintenance specialists", the 7-skill list, the 11 slash commands, unique `name:` values — all
matched disk exactly, through the whole engine migration.

**Scope wording drifts, and it drifts *independently of the agent file*.** `swe.md`'s own
`description` was correctly rewritten to "the pure-R transmission engine (sim_engine.R and its
sim_params/state/results siblings), PSOCK parallel execution" — while the README's routing
cheat-sheet still read "`run_MOSAIC()` loop, **Dask/PSOCK**, **reticulate bridge**", the
quick-reference and roster one-liners still said "**Python bridge**", and the verification matrix
still listed "'A Dask worker deadlocks' → swe" as a positive routing test.

**How to apply:** diff the README's prose cells against each agent's **frontmatter `description`**,
which is what the orchestrator actually matches on — never against the README's own other tables,
which is what a self-consistency check does and why this drift survives. Same for the README's skill
blurbs: they restate each skill's scope and rot separately from the `SKILL.md` frontmatter
(README's hedgehog-run blurb was still advertising "the control + `dask_spec` recipe" after both VM
skills documented the backend's removal). Also check the roster table's tool cell against
`tools:` — `swe` had `WebFetch, WebSearch` granted and missing from the table.

**Related gap worth remembering:** no agent description covers the engine's *bit-identity* contract
(the replay fixtures, `tests/testthat/fixtures/ORACLE.md`, the PRNG draw-site registry, the
`rng`-vs-`replay` mode split, integer-vs-double channel typing). `swe` owns `sim_engine.R`,
`statistician` owns the likelihood, `disease-modeler` owns parameter meaning — the seam between them
is where lessons #15-#18 all landed. This is the rare case where *adding* a line beats pruning,
because there is nothing to prune that covers it. See [[feedback_coiled_steer_drifts_across_skills]]
for the multi-file grep discipline this needs.
