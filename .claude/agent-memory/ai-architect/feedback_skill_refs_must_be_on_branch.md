---
name: feedback-skill-refs-must-be-on-branch
description: When auditing a skill that cites a "canonical runbook"/sibling doc, verify the target exists on the CURRENT branch (HEAD), not just somewhere in git
metadata:
  type: feedback
---

A skill/README reference to a sibling doc is only valid if that doc is reachable from HEAD. Grep on
disk is necessary but not sufficient — check `git merge-base --is-ancestor <commit> HEAD`.

**Why:** During the docker-image-update re-review, the skill, the README entry, and both new helper
scripts all cited `azure/DOCKER_IMAGE_UPDATE.md` as the "canonical runbook (7 steps + rollback)".
The file existed in git (commit b8203e11, a parallel "v0.43.0" by another author) but on an UNMERGED
branch `tt/docker_image_rebuild_061626` — NOT an ancestor of HEAD (whose real v0.43.0 is 35ddba73).
So on the working branch the runbook simply did not exist; every "see the runbook" pointer dangled,
and the "automates steps 1-5 & 7 / step 6" mapping referenced step numbers defined only in the
absent file. The skill authored against another contributor's branch is the drift source.

**How to apply:** (1) For any cited path, run `git cat-file -e HEAD:<path>` or `ls`, AND if it
appears in git history but not HEAD, run `git branch -a --contains <commit>` to see it's on an
unmerged branch. (2) Fix by making the skill self-sufficient — if it already carries the full
procedure inline (this one had all 7 steps in its own numbered list), convert external "see runbook"
pointers into internal section refs (§3/§4) rather than inventing a doc. (3) Code-comment refs inside
the scripts are out of always-loaded context scope but will mislead a debugging agent — flag them for
the human (best fix is merging/restoring the runbook, a judgment call), don't silently edit code I
don't own.

Related: [[reference-agent-roster-sharing]] (the README/roster tables are the usual co-drift cluster).
