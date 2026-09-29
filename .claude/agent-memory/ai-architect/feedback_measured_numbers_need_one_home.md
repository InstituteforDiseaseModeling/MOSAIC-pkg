---
name: feedback_measured_numbers_need_one_home
description: A measured performance number must live in exactly one doc and be cited elsewhere; replicated measurements rot together and the newest measurement is the one that never propagates
metadata:
  type: feedback
---

A **measured** number (RAM/worker, runtime, speedup, worker count) belongs in exactly ONE document.
Every other surface cites that document. Never transcribe it, not even "for convenience."

**Why:** the "~1.0 GB per worker" figure was replicated to four surfaces — `MOSAIC-pkg/CLAUDE.md`
§ Troubleshooting, `dugong-run/SKILL.md` §2 (where it justifies `n_cores = 170L`),
`run-mosaic/SKILL.md` §5, and `migrate-laser-r.md` §617 where it originated (926 MB, laptop,
engine-only loop). When the 100k/40-location dugong run later measured 1.13-1.46 GB/worker for the
simulation phase and **19.3-23.6 GB/worker for the ensemble phase** (`pipeline-performance-plan.md`
§1a), the new number landed in the plan doc and propagated to **none** of the four. The direction of
failure is systematic: a fresh measurement gets written where it was taken, and the always-loaded
copy keeps the old value. So the *stalest* copy is the one loaded into every session.

Second failure the replication caused: the always-loaded copy described only the cheap phase, so it
read as "memory is no longer what caps worker count" while the expensive phase was ~20x worse per
worker and set the real ceiling. A number that omits its phase is worse than no number.

**How to apply:** when auditing, grep the *number* (`grep -rn "1.0 GB\|992 MB\|GB/worker"`) not the
topic — copies use different wording around the same digits. When pruning, delete the number from
the always-loaded file and leave a citation; measured numbers are the ❌-exclude
"frequently-changing facts" class, and they fail the litmus test outright (nothing an agent does
changes between 1.0 and 1.4). Same rule for the parameter count in `sample_parameters()` ("301",
actually ~948 at 40 locations, stale since v0.14.1) — see
[[feedback_skill_constants_rot_despite_hedge]], which is the transcription-of-constants case;
this is the transcription-of-*measurements* case, and it rots faster because measurements get
re-taken.
