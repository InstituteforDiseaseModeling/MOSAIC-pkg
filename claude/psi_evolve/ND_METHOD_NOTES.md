# ND / DLinear — the method, and which refinements are worth our compute

Written 2026-09-23, after ND came out ahead of every LSTM arm on pooled MAE
while being the flattest arm in the programme.

---

## 1. What ND actually is, and how it differs from the paper

DLinear comes from Zeng et al., *Are Transformers Effective for Time Series
Forecasting?* (AAAI 2023, arXiv:2205.13504). The paper's model is:

```
x  ->  moving-average TREND  +  REMAINDER        (series decomposition)
       each mapped L -> H by ONE linear layer, summed
```

That is the entire model. No nonlinearity, no attention, no recurrence. Its
point was polemical: on the standard LTSF benchmarks it beat the transformers
of the day, which meant those transformers were not earning their complexity.

**Ours is not that model.** `arch_control$trunk = "dlinear"` replaces only the
*encoder*, mapping `(B, T=13, F=38) -> (B, 32)`, and then feeds the unchanged
hierarchical-FiLM head. So ND is a DLinear **trunk** inside a nonlinear
country/region-conditioned model. Three consequences:

- the paper's headline claim (linear beats transformers end-to-end) is not what
  we tested; we tested "linear encoder beats recurrent encoder, head held fixed";
- our map is `T -> units_3` (13 -> 32), not `L -> H`. It is not a forecaster on
  its own and cannot be read as one;
- results from the LTSF literature transfer only as far as the encoder.

**The context-length gap is the big caveat on everything below.** This
literature works at L = 96-720. We are at **T = 13**. Mechanisms that need room
to operate (patching, frequency interpolation, multi-scale mixing) have almost
none here.

## 2. The three defects this wave fixes

Found by reading our implementation against the reference:

| defect | why it matters | arm |
|---|---|---|
| **zero-padding** the moving average | features are z-scored, so zeros drag the trend toward the global feature mean at *both* window edges — and the worst-hit position is the **last timestep, the forecast anchor**. Zeng et al. replicate the end values. | `NDe` |
| **no L2** on the trunk kernels | lstm/gru/tcn all carry `hp$l2`; dlinear alone was unpenalised, i.e. the only trunk whose capacity was unconstrained | `NDr` |
| **`dlinear_kernel` unreachable** | read by `.psi_fit_predict_lstm` but never passed by `run_rolling_cv_suitability`, so the decomposition window was permanently pinned at 5 and a sweep was impossible | `NDk9` |

Plus `NDi` = DLinear-I, per-feature linear maps instead of one shared map — the
paper's "individual" variant, ~38x the trunk weights.

## 3. Refinements worth running next, ranked for OUR problem

### A. Closed-form ridge, as the honest ceiling — *highest value, near-zero cost*

Toner & Darlow (arXiv:2403.14587) prove DLinear, NLinear and RLinear are
**functionally equivalent to unconstrained linear regression on an augmented
feature set**, so they admit closed-form solutions — and closed-form OLS
**beat SGD-trained versions in 72% of their test settings**.

Our trunk is linear but our head is not (FiLM is multiplicative), so ND as a
whole has no closed form. But that is an argument for running the *pure*
model: a direct per-country ridge from the 13x38 window to the target, solved
in closed form, takes **seconds**, has no seeds, no epochs and no
reproducibility problem — and it is the cleanest possible answer to "is any of
this neural machinery earning its keep?". Given that a constant already matches
the blend's headline, this should have been the first arm in the programme.

### B. Degree of cross-country sharing — *directly targets our known failure*

The 2026 survey (arXiv:2606.27282) finds that **series within a dataset
disagree about hyperparameters, and the optimal degree of cross-series sharing
ranges from fully shared to fully per-series**. That is precisely our central
unresolved failure: cross-country correlation 0.19-0.27 against an observed
0.014, and D9b/N8/N9 only nudged it. Every arm to date is one globally shared
trunk. A sharing sweep (global / region / per-country) is cheap on a linear
trunk in a way it never was on the LSTM.

### C. Mixture-of-Linear-Experts — *strongest new build*

MoLE (arXiv:2312.06786) trains several linear experts plus a router that mixes
them per input. Reported: error reduced in **78%** of dataset-settings, and
linear models reaching SOTA in **68%** of experiments versus 25% for a single
linear model. Its stated motivation — linear models cannot adapt their
prediction rule as patterns change — is our problem said in other words, and
**our country embedding is a ready-made router input**. Super-Linear
(arXiv:2509.15105) is the pretrained descendant.

### D. Lookback, per-country rather than global

Same survey, finding 1: **optimal lookback is strongly series-specific and
often non-monotonic in the horizon** (fitted exponents from +0.46 to -0.19
across datasets). We pin `timesteps = 13` for all 40 countries. The existing
W1/W2 arms sweep it *globally*, which the literature suggests is the wrong
granularity.

### E. Trailing-fraction normalisation

Finding 2: **normalising over a learned trailing fraction of the context beats
normalising over all of it**, almost universally. Cheap to add and it speaks to
our level problem (every arm predicts 43-53% of the observed level).

### F. What to SKIP, and why

- **RevIN** (Kim et al., ICLR 2022). Two reasons against: Toner & Darlow find
  that for DLinear specifically the detrending already does RevIN's job, and
  arXiv:2510.04667 reports RevIN failing catastrophically under extreme
  outliers (MSE +683%) — which describes this panel, bounded [0,1] with many
  near-zero countries and COD saturated.
- **FITS** (arXiv:2307.03756) and **SparseTSF**. Both are frequency/periodicity
  machines built for long context. An rFFT over 13 steps has ~7 usable bins;
  there is nothing to interpolate.
- **PatchTST / TimeMixer++**. Already gated out in `PLAN_ARCHITECTURES.md` for
  the same reason, and ND strengthens that call rather than weakening it.

## 4. Reading list

**Start here — the method itself**
1. Zeng, Chen, Zhang, Xu (2023). *Are Transformers Effective for Time Series
   Forecasting?* AAAI. https://arxiv.org/abs/2205.13504 — DLinear/NLinear, the
   decomposition, and the benchmark critique. Read §3 for the model, §4 for why
   the transformer baselines lost.

**The theory that should change how we run ND**
2. Toner & Darlow (2024). *An Analysis of Linear Time Series Forecasting
   Models.* https://arxiv.org/abs/2403.14587 — the equivalence proof and the
   closed-form-beats-SGD result. The single most actionable paper for us.
3. *How Good Can Linear Models Be for Time-Series Forecasting?* (2026).
   https://arxiv.org/abs/2606.27282 — preprocessing, not architecture, is the
   lever; lookback and cross-series sharing are series-specific.

**Extensions that fit our heterogeneity problem**
4. Ni et al. (2023). *Mixture-of-Linear-Experts for Long-term Time Series
   Forecasting.* https://arxiv.org/abs/2312.06786
5. *Super-Linear: A Lightweight Pretrained Mixture of Linear Experts.* (2025)
   https://arxiv.org/abs/2509.15105

**Normalisation — read together, they disagree productively**
6. Kim et al. (2022). *Reversible Instance Normalization for Accurate
   Time-Series Forecasting against Distribution Shift.* ICLR.
   https://iclr.cc/virtual/2022/poster/6034
7. *Noise or Signal? Deconstructing Contradictions and an Adaptive Remedy for
   Reversible Normalization.* (2025) https://arxiv.org/abs/2510.04667

**Lightweight successors — context, not candidates for T = 13**
8. Xu et al. (2024). *FITS: Modeling Time Series with 10k Parameters.*
   https://arxiv.org/abs/2307.03756 (code: https://github.com/VEWOXIC/FITS)
9. *MixLinear: Extreme Low Resource MTS Forecasting with 0.1K Parameters.*
   https://arxiv.org/abs/2410.02081
10. *CATS-Linear: Classification Auxiliary Linear Model.*
    https://arxiv.org/abs/2510.08661
11. *RS-GLinear: Residual-Stacked Gaussian.* https://arxiv.org/abs/2510.03788

**Already in our plan, for comparison**
12. Das et al. (2023). *TiDE: Long-term Forecasting with Dense MLP Encoders* —
    the MLP encoder-decoder sibling, `NE` in `PLAN_ARCHITECTURES.md`.
13. Lim et al. (2019). *Temporal Fusion Transformers.*
    https://arxiv.org/abs/1912.09363 — `tft/README.md`.
