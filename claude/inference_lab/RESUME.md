# START HERE — how to resume the inference lab

There is no scheduler. **The ledger is the state.** Any session, human or agent, resumes the program
by following this file. It is designed to survive context compaction and month-long gaps.

## Resume in five steps

1. **Read the state.** `ledger/STATE.md` — the queue, the promoted stack, open questions, and any
   outstanding harness bugs. If the "Harness bugs" section is non-empty, **fix those first**; no
   result is admissible while it has entries.
2. **Verify the substrate.**
   ```bash
   git -C <worktree> rev-parse --abbrev-ref HEAD      # must be inference-lab
   git -C <worktree> log --oneline -1                 # record this as base_sha
   ssh dugong 'ps -eo comm --no-headers | grep -c "^R$"'   # must be 0 before launching
   ```
   Never work in `/Users/johngiles/MOSAIC/MOSAIC-pkg` — that shared checkout is on
   `feature/psi-torch-port` and carries a colleague's uncommitted EMDAT/IDMC work.
3. **Take the next queue item** from `STATE.md`, respecting the dependency order in `PROTOCOL.md` §5.
4. **Run the lifecycle** (`PROTOCOL.md` §4): propose -> implement -> run -> verdict -> reason -> requeue.
   Roles must stay separated (§1): the agent that implements an arm never scores it.
5. **Append to the ledger and rewrite `STATE.md`.** A run that is not in the ledger did not happen.

## Standing decisions (set 2026-09-17, do not re-litigate without the user)

- **Autonomy:** the lab runs, scores and promotes freely onto `inference-lab` **without asking**.
  Promotion to `main` requires explicit human approval. Report a digest of verdicts, not each run.
- **Persistence:** resumable protocol only. No cron, no standing agent team, nothing runs unattended.
- **Scope:** **inference machinery only** — weighting, subset selection, ensemble combination,
  proposal, diagnostics, and the likelihood's *scoring mechanics*. See "Out of scope" below.

## Out of scope for the lab (measure freely; never ship; escalate with evidence)

These are epidemiological or structural commitments, not tuning knobs:
- priors and parameter pinning — `zeta_ratio` left-truncation, pinning `rho_deaths` / `phi_2` /
  `prop_S_initial`, revisiting `mu_j_epidemic_factor`
- model structure — deaths conditional on cases, any change to compartments or flows
- any new R/Python dependency

The lab MAY run an out-of-scope arm as a *measurement* to quantify what it would buy, and should
record that number. It may not promote one, even onto `inference-lab`.

## The one-paragraph brief for a fresh agent

> MOSAIC's calibration reports five green convergence metrics on a posterior that is statistically
> indistinguishable from a random subset of draws. Four independent causes were measured
> (`../review_inference/PLAN.md` §1), each alone sufficient to produce exact-IS ESS = 1.00. This lab
> tests fixes experimentally, on held-out data, with negative controls that must fail and a paired
> noise gate, because the thing that broke MOSAIC's inference was a metric that could not fail.
> Read `PROTOCOL.md` before proposing anything.

## Fast sanity check that the harness still works

Run the three negative controls alone (`PROTOCOL.md` §3.4) at T0. All three must FAIL their gates.
If any passes, stop: the benchmark is broken and nothing measured against it counts.
