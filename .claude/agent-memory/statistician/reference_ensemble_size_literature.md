---
name: ensemble-size-literature
description: Verified literature rates for how a best-subset/accepted-set size should scale with N (ABC kNN, Gibbs eta, GLUE/HM, Occam, CRPS finite-M, IS collapse) — no framework fixes a count; review at claude/ensemble_size_review/REVIEW.md (2026-10-09)
metadata:
  type: reference
---

Full review with DOIs and extracted texts: `MOSAIC-pkg/claude/ensemble_size_review/REVIEW.md` (+ `src/`).

**Invariant: every framework fixes a TARGET (quantile rate, eta, discrepancy threshold) and
lets the count grow with N; none fixes the count.** MOSAIC's n ≈ 1.08·ESS_best is the outlier.

Verified rates (do not re-derive):
- Biau-Cérou-Guyader 2015 Cor 4.1: k_N ∝ N^{(p+4)/(m+p+4)} (m>4); consistency needs k→∞, k/N→0.
- Blum 2010 Remark 1: bandwidth n^{-1/(d+5)}, MSE n^{-4/(d+5)} → accepted ∝ n^{5/(d+5)}.
- Fearnhead-Prangle 2012: h = O(N^{-1/(4+d)}) → N_acc ∝ N^{4/(4+d)} (x20 N → x1.31 at d=40, x3.8 at d=5).
- Snyder et al 2008 eq.19: E(1/w_max)−1 ≈ sqrt(2 log N)/τ, τ² = var of log-lik across draws;
  collapse unless N ≫ exp(τ²/2). Chatterjee-Diaconis 2018: N ≈ exp(KL). PSIS: S > 10^{1/(1−k̂)}.
- Ferro 2008: E[CRPS_M] = CRPS_∞ + E|X−X'|/(2M) → ×(1+1/M) if reliable; M=108 costs ~0.9%.
- Wu & Martin 2023: GPC (Syring-Martin coverage) best of 4 eta-selection rules; Lyddon picks eta too big.
- Occam C=20 ⇔ ΔAIC ≤ 6; B&A Δ rules need ĉ (QAIC) under overdispersion → eta = 1/ĉ.

**Why:** the user asked (2026-10-09) whether ~100 is robust at 250k-1M sims. Answer: stable but
not principled; the target sharpens toward an over-confident in-sample mode and the MC error does
not fall. The proposed test is "eta by held-out WIS, n_eff as output, floor ~400 / p".

**How to apply:** cite these instead of re-searching. Pair with [[best-subset-selection-is-data-free]],
[[subset-ranking-beats-size]] and [[inference-scaling-measured-v0903]].
