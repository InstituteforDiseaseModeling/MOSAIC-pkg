# MOSAIC inference lab — experimental protocol

A persistent, resumable program for upgrading MOSAIC's inference. Any session (human or agent)
picks this up by reading `ledger/REGISTRY.jsonl` and `ledger/STATE.md`. **The ledger is the state.**

Source of hypotheses: `../review_inference/PLAN.md` (seven-agent review, 2026-09-17) plus anything
that arises. New ideas are welcome and expected — but they enter through the same gate as the
pre-registered ones (§4).

---

## 0. Why this protocol looks paranoid

The review that motivated this lab found that MOSAIC's calibration reported five green convergence
metrics on a posterior that was statistically indistinguishable from a random subset of draws. The
metrics were not wrong by accident — they were **structurally unable to fail**. An autonomous
experiment loop is prone to exactly the same failure: it will happily optimise a number that cannot
move, or promote a win that is seed luck.

Every rule below exists because we measured that specific failure today:

| Rule | The failure it prevents | Measured today |
|---|---|---|
| Negative controls in every batch (§3.4) | optimising a metric that cannot fail | `ess_best` identical to 15 s.f. across two different datasets |
| Paired replicates + noise gate (§3.3) | promoting seed luck | rank-1 draw carried **4.5 SD** of winner's curse |
| Held-out scoring only (§3.1) | in-sample self-deception | subset optimiser selects from 1e5 candidates on **zero** held-out data |
| Pre-registration (§4) | post-hoc rationalisation | a fixed `Delta*=4` constant became the de facto estimator with no record of the decision |
| Worktree isolation (§2) | corrupting shared state | an agent mutated the shared checkout mid-session; a sibling reported it as a real defect |
| Commit SHA in every record (§2) | auditing the wrong code | a whole review round audited a branch 3 releases stale |
| Bounded waits (§6) | wedged watchers | 9 unsatisfiable `until` loops, one alive 2 days |

---

## 1. Roles — implementer and evaluator are NEVER the same agent

- **Coordinator** — owns the queue, the ledger, and the reasoning. Decides what to try next from
  results. Does not implement or score.
- **Implementer** (`swe`) — writes the arm. Never scores its own arm.
- **Evaluator** (`statistician`) — runs the benchmark and writes the verdict. Never sees the
  implementation diff before scoring; scores from the arm spec and the outputs only.
- **Red team** (`maintainer`) — for any arm proposed for promotion, tries to break the result:
  looks for the metric that cannot fail, the leak, the noise-sized effect.
- **Domain check** (`disease-modeler`) — for any arm touching priors, the likelihood's biological
  content, or parameter pinning.

The separation is the point. Today's review found an agent whose own test *asserted the broken state
as the requirement*. An agent that both builds and grades will do this.

---

## 2. Substrate — isolation is mandatory

- Long-lived branch **`inference-lab`**, forked from `main`, never merged back wholesale.
- One **git worktree per arm**, under the session scratchpad. Never work in
  `/Users/johngiles/MOSAIC/MOSAIC-pkg` — that shared checkout sits on `feature/psi-torch-port`
  and currently carries a colleague's uncommitted EMDAT/IDMC work.
- Every ledger record carries `base_sha` and `arm_sha`. An arm whose recorded SHA does not match
  the tree it ran in is **void**, not "probably fine".
- Promotion to `main` is a separate, human-approved step (§5). The lab never pushes to `main`.

---

## 3. The benchmark — the heart of the thing

### 3.1 Scoring is out-of-sample, always
Primary metric: **WIS on held-out temporal blocks** via the existing `run_rolling_cv()` /
`evaluate_rolling_cv()` machinery. Never score an arm on the window its weights were fitted to.
Rationale: `optimize_ensemble_subset()` is Caruana et al. (2004) sorted-ensemble initialisation
**minus the held-out set**, selecting from 25,000-100,000 candidates. In-sample improvement here is
not evidence.

### 3.2 The ladder — cheap first, expensive only for survivors

| tier | config | n | cost | purpose |
|---|---|---|---|---|
| **T0 smoke** | ETH | 1,000 | ~2 min | does it run, does it write outputs |
| **T1 primary** | ETH x 3 seeds | 10,000 | ~30 min | the workhorse; all promote/reject decisions start here |
| **T2 generalise** | ETH, MOZ, COD, NGA x 2 seeds | 10,000 | ~2 h | does it hold across regimes (COD is psi-saturated; southern belt is not) |
| **T3 confirm** | 40 locations | 25,000 | ~3 h | only for arms that passed T2; the publishable number |

`n = 10,000` is the measured saturation point (SCALE). Going above it in T1/T2 buys nothing and
costs everything. **Single-location runs are also more informative for parameter recovery** —
7/10 ETH location parameters identified at 25k single-country vs 1/10 at 100k joint.

### 3.3 Noise discipline — paired, replicated, pre-specified threshold
- Arms and baseline run on the **same seed block** (common random numbers).
- >= 3 replicates at T1, >= 2 at T2.
- Report the **paired** difference and its SD across replicates.
- **Promotion requires the paired mean improvement to exceed 2 x the paired SD.** An unpaired or
  single-replicate comparison is never sufficient.
- Report the replicate SD of the score itself (LIKE's A4). We measured `sd(log L-hat)` = 70-148 nats
  at fixed theta; a difference smaller than that is not a result.

### 3.4 Negative controls — every batch, no exceptions
Each benchmark batch includes control arms that **must fail**:
- `ctrl_shuffle` — shuffle the likelihood column. Any metric that does not degrade is disqualified
  as a decision metric for that batch.
- `ctrl_random_subset` — replace the selected subset with a random |B|-subset. The primary metric
  must be significantly better than this null (SCALE's random-subset band).
- `ctrl_noop` — a no-change arm run through the full harness. Its measured "improvement" is the
  harness's own noise floor and is subtracted from expectations.

If a control passes, **the batch is void and the metric is the bug**, not the arm.

### 3.5 The metric panel (report all; decide on the primary)
- **Primary:** held-out WIS (cases, deaths, reported separately AND combined).
- **Calibration:** empirical coverage of the 50% and 95% intervals; PIT histogram.
- **Honest inference diagnostics:** `ess_is`, `khat`, `eps_B = Delta_(B)`.
- **Bias:** cases and deaths bias ratios on held-out windows.
- **Parameter recovery:** count of parameters moving > 0.25 prior SD *and* beyond the random-subset
  null; CI width ratio vs prior.
- **Cost:** wall-clock, core-hours, simulations used.

**Banned as decision metrics** (proven information-free): `ESS_B`, `A`, `CVw`, `param_ess`. They may
be logged for continuity, never used to promote.

---

## 4. The experiment lifecycle

1. **Propose.** Coordinator writes an arm spec to `arms/<id>.md`: hypothesis in one sentence, the
   mechanism, the predicted direction and rough size, the primary metric, the decision rule, and the
   tier to start at. **Predictions are recorded before the run.** A hypothesis that cannot be wrong
   is not an experiment.
2. **Implement.** `swe` builds it in a fresh worktree. Records `arm_sha`.
3. **Run.** Evaluator runs the tier ladder. Controls included.
4. **Verdict.** Evaluator writes `reports/<id>.md`: measured vs predicted, paired stats, controls,
   verdict in {PROMOTE, ITERATE, REJECT, VOID}.
5. **Reason.** Coordinator records *why* in the ledger — particularly when the prediction was wrong,
   since that is where the information is. Wrong predictions are not failures; unrecorded ones are.
6. **Queue update.** Results change the queue. Dependencies (§5) are re-checked.

Every step appends one JSON line to `ledger/REGISTRY.jsonl`. `ledger/STATE.md` holds the current
queue, the promoted stack, and open questions, and is rewritten after each verdict.

---

## 5. Dependency order (from PLAN.md §2) — not negotiable

```
A2 (process noise in the dispersion)  ──┐
A1 (kill -y*log(1e6))  ─────────────────┼──> re-calibrate any temperature AFTER these
A5 (weekly scoring)  ───────────────────┘
        │
        └──> B1 (fixed-zeta fractional posterior)  ──> B2 (resample members) ──> B4 (held-out selection)
        │
        └──> C1 (pre-sim screen) ──> C2/C4 (budget, refit) ──> C5 (SMC)
```

Rationale: sharper weighting applied before the score noise is fixed amplifies seed luck. SAMP's
explicit instruction is *"do not just switch to exact IS weights"* — ranking survives the noise
(92% subset overlap), exponential weights do not.

**Standing decisions (set 2026-09-17 by the user; see `RESUME.md`):**
- **Autonomy.** The lab runs, scores and promotes onto `inference-lab` *without asking*. Promotion to
  `main` requires explicit human approval. Report a digest of verdicts, not each run.
- **Persistence.** Resumable protocol only — no scheduler, nothing unattended.
- **Scope.** Inference machinery only: weighting, subset selection, ensemble combination, proposal,
  diagnostics, and the likelihood's *scoring mechanics* (A1/A2/A3/A4/A5/A6/A7 all qualify).

**Out of scope — measure freely, never ship, escalate with the number:**
- priors and parameter pinning (`zeta_ratio` truncation, pinning `rho_deaths`/`phi_2`/`prop_S_initial`,
  `mu_j_epidemic_factor`)
- model structure (deaths-conditional-on-cases, compartment or flow changes)
- any new R/Python dependency
- any promotion to `main`

---

## 6. Operational hygiene

### 6.0 VERIFY AFTER LAUNCH — the check that costs seconds and saves hours

**After launching any batch, confirm all three before walking away:**

```bash
pgrep -fc "[i]nflab_arm.R"                                        # == expected arm count
ps -eo etime,args --no-headers | grep "[i]nflab_arm" \
  | awk '{print $1}' | sort -u                                    # must be ONE distinct value
uptime                                                            # load ~= arms x cores_each
```

**More than one distinct elapsed time means more than one set is running.** That is the signature of
a kill-then-relaunch where the `pkill` had not taken effect before the new launch. Concurrent
calibrations write shards, `samples.parquet` and `summary.json` to the SAME paths and silently
corrupt each other.

This happened on 2026-09-21 (HB-03): four orphaned processes (reparented to systemd) ran alongside
four real ones for ~90 minutes, load average 232 on 176 cores, all eight writing to four
directories. A whole country's wave-3 output had to be discarded. **99% CPU is not evidence of
health — a load average well above the core count is evidence of the opposite.**

Killing also leaves PSOCK workers orphaned at PPID 1 (222 of them that day, still burning CPU after
their masters died). Always re-check `ps -eo comm | grep -c '^R$'` after a kill, and confirm the
survivors belong to nobody else before clearing them.

Prefer `--done-marker` idempotence (`.done_<tag>` files) so a relaunch skips completed work rather
than redoing or duplicating it.

- **Never** pipe a long-running `Rscript` into `head` — R ignores SIGPIPE and the process wedges.
  Redirect to a file, read the file.
- Every wait loop is bounded AND checks that the thing it waits for is still alive. Nine
  unsatisfiable watchers accumulated in one day without this; one had been polling for two days.
- Use the bracket idiom (`[r]un_x`) for every `pgrep`/`grep` over process lists; plain patterns
  self-match and have produced three wrong answers in one session.
- Long runs: `setsid nohup` on dugong, PID recorded in the ledger; check liveness by PID, not pattern.
- Check `dugong` is idle before launching (`ps -eo comm | grep -c '^R$'`).

---

## 7. Stopping rules

- An arm that is VOID twice for the same reason: stop, fix the harness, record the harness bug.
- An arm line with three consecutive REJECTs: stop, write what was learned, move to the next queue item.
- A promoted change that later fails T3: demote, and record the T1/T2-vs-T3 discrepancy — that gap
  is itself a finding about the benchmark.
- **The lab stops entirely** if a negative control passes twice in a row: the benchmark is broken and
  no result from it is admissible until fixed.
