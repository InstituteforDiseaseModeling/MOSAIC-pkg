---
name: v140-theory-redteam
description: MOSAIC 1.4.0 convergence red-team (2026-10-20) - eta*-flat is N-dependent (BFA green 7k, yellow 20k+), tempered k-hat pre-asymptotic (falls with N), MCE p90 = 1.28x noise inflation, alpha total over locations
metadata:
  type: project
---

Theory red-team of feat/v1.4.0-convergence (scripts on laptop at MOSAIC-pkg/claude/redteam/theory/0*.R).

- **The flat flag (eta* >= 0.999) is N-dependent.** It needs ESS(eta = 1) >= the absolute target of 1,000, and that
  ESS grows with N. BFA is GREEN at 7-15k draws and YELLOW ("posterior close to prior") at 20k and above, with the
  same PIT-KL. BFA is a top PLATEAU, not a flat likelihood: 808 draws tied at the max, a log-likelihood range of
  1.9e7, 7 cases.
  **Why:** an absolute ESS target turns an informativeness check into a run-length check.
  **How to apply:** any informativeness metric must be N-invariant (ESS/N, or the median PIT-KL).
- **The tempered weights are bounded** (NB pmf <= 1), so the true k <= 0 and k-hat is pre-asymptotic: it falls with N
  (ZAF 2.78 at 5k to 0.57 at 50k; SOM 1.52 to 0.44). The ESS still grows linearly (ZAF slope 0.96). A red
  "use a sharper sampler" reason is wrong when k-hat is falling. CIV drops about 0.03 per doubling (needs > 500k for
  0.5). The 0.5-0.7 "usable" band is for PSIS; plain IS needs < 0.5 (Vehtari 2024 section 3).
- **The per-parameter MCSE is homogeneous** and equals 1/sqrt(ESS_best). The batch-means p90 over about 39
  parameters is about 1.28x that, which is sqrt(qchisq(.9, 9) / 9): noise, not heterogeneity between parameters. The
  estimator is calibrated against disjoint halves (ratio 0.90-0.97) despite the whole-pool selection.
- **alpha is a total:** eta = alpha / n_obs, with n_obs summed over locations, gives about alpha/J per location in
  multi-location runs (undocumented).
- **Optional stopping on ESS is mild:** +3-8 % at the stop, and P(green) of an independent replicate 0.43-1.0.
- **The truncated best-fit posterior:** median CI ratio 0.99, but 9 % of parameters lose more than 20 % of their
  width, and the mean shifts reach 0.40 SD (more than the MCE target).

**Round-1 fixes (statistician, 2026-10-20; scripts claude/redteam/theory/r1/12-15):** f1-based flat (see
[[untempered-ess-share]]); k-hat trend from sim-order prefixes N/4..N (falling iff <= -0.02/doubling; UGA is the only
"not falling" one at 30-50k); projection gated on slope only; MCE batch floor 1% of member weight; two greens:
fresh-rerun green 0.891 -> 0.999 (bootstrap, 16 models x 60), +25% draws (one geometric step); 50k status unchanged
15/6/6 (+BFA yellow at every N); cost 2.22M (1.58x) two greens, 2.05M single, 1.904M = red-team P6 without BFA-at-cap.
Replay trap: subsampling WITHOUT replacement from the 50k pool at N ~ 25k halves the variance (FPC) and biases
P(green); use bootstrap (with replacement) streams for fresh-rerun probabilities.

Related: [[project_v140_ws1_statistical_core]], [[project_coarsened_posterior_v130]].
