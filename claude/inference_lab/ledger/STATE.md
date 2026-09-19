# Inference lab — current state

**Base:** `main` @ v0.90.5 (`c947820a3`). Lab branch **`inference-lab`** cut from it.
**Status:** queue pre-registered, nothing run yet.
**Standing decisions:** auto-promote within `inference-lab`, human gates `main`; resumable protocol
(no scheduler); **inference machinery only**. See `../RESUME.md`.

## Promoted stack (validated, in dependency order)
_(empty)_

## Queue — pre-registered, predictions recorded BEFORE running

Each row's prediction is a commitment. A wrong prediction is a result; an unrecorded one is waste.

| id | hypothesis | predicted effect (primary = held-out WIS unless stated) | tier | depends on |
|---|---|---|---|---|
| **A1** | Replacing `-y*log(1e6)` with an eps-floored mean makes the LL a density again | deaths bias 3.27 -> ~1.3, cases 1.98 -> ~1.25 at identical subset; WIS improves; LL spread /8.4 | T1 | — |
| **A5** | Weekly block scoring removes reporting artefacts and most of the VIF | signal-to-noise 6.11 -> ~43; WIS improves or holds; <=11% loss of discrimination | T1 | — |
| **A4** | Reporting per-draw replicate SD exposes the winner's curse | no WIS change (diagnostic only); expect gap(rank1, rank2) < 2*sqrt(2)*SD on most runs | T0 | — |
| **A3** | Common random numbers across draws differences out seed noise | `sd(log L-hat)` at fixed theta falls >=2x; between-draw ranking stabilises | T1 | — |
| **A2** | Dispersion representing process + observation variance fixes the score noise | **115 replicates of one theta give ESS >> 10** (currently 3-13). This is the acceptance test | T1 | A1, A5 |
| **A7** | `LL/tau-hat` composite adjustment calibrates the independence assumption | ESS 1.00 -> ~60/4,000 with A1+A5 in place | T1 | A1, A5 |
| **C1** | A linear pre-simulation screen on theta predicts bad draws | AUC ~0.86; keeping 50% retains ~100% of the true top-0.1%; ~2x effective budget | T1 | — |
| **B1** | Fixed-zeta fractional posterior over all retained draws | **ESS becomes Theta(n)**; CI width ratio 0.962 -> ~0.94; WIS holds or improves | T1 | A1, A2, A5 |
| **B2** | Systematic resampling of ensemble members beats top-K | held-out WIS improves; member logL span widens toward the target distribution | T1 | B1 |
| **B4** | Block-held-out subset selection removes the Caruana overfit | T1 gain smaller than in-sample gain (that gap IS the overfit); coverage improves | T1 | B1 |
| **C3** | Per-location calibration identifies more parameters at lower cost | 1/10 -> ~7/10 location parameters identified; cost NEGATIVE | T2 | — |
| **C4** | One weight-corrected proposal refit round | hit rate 0.5% -> ~3.5%; **ESS_IS still ~1.00** (predicted NOT to fix degeneracy) | T1 | A2 |

## Explicitly predicted NULL results (recording these matters as much)
- **C4 will not fix ESS.** Already measured once; if the lab sees ESS improve, the harness is wrong.
- **B1 alone will not fix the dimension problem.** Improvement is confined to ~5 identified
  directions of 58. Expect marginal summaries to move little even when B1 works.
- **Tempered-as-shipped is not a candidate.** It is 86x sharper than documented (96% of mass on one
  draw). Superseded by B1.

## Deferred — OUT OF LAB SCOPE (measure freely, escalate with the number, never ship)

These came out of the review and are real, but they are epidemiological or structural commitments:

| item | evidence | why deferred |
|---|---|---|
| Left-truncate `zeta_ratio` at 1 | 16.3% of draws have asymptomatics shedding more than symptomatics; the code comment claims 1e-6 from a prior that does not ship | prior change |
| Pin `rho_deaths`, `phi_2`/`omega_2`, drop `prop_S_initial` | all three provably inert (algebraic cancellation / `nu_2_jt` sums to zero / S is the simplex residual) | prior + parameter pinning |
| Revisit `mu_j_epidemic_factor` | the v15.18 reshape was justified by "statistically UNIDENTIFIED"; that is now false and the mode moved the wrong way | prior change |
| Deaths ~ Binomial(cases, CFR) | removes the +0.36 weekly residual double-count; makes CFR directly fitted | model structure |
| SMC / `hmer` / IMIS | the structural fix for the proposal | new dependency (`IMIS`, `synlik` are CRAN-archived) |

## Open questions the lab should answer
1. **Does a larger |B| predict better?** Unreadable today past 115. Rebuild at |B| in {60,115,500,2000},
   score out-of-sample. ~30 min on dugong. **Highest-value single experiment; run it early.**
2. Do T1 (ETH) verdicts transfer to T3 (40 locations)? Measure the transfer rate; if it is poor, the
   ladder is wrong and the lab is optimising the wrong benchmark.
3. What is `zeta` by held-out WIS, and how far is it from the implied `alpha ~ 2.65` observations?

## Harness bugs found (must be empty before results are admissible)
_(none yet)_
