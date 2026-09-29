---
name: beta-from-mean-cv-jshape-cliff
description: Method-of-moments Beta from (mean, CV) is J-shaped iff CV >= 1; the usual mu(1-mu) variance clamp guards a boundary ~53x further out and is dead code for small mu
metadata:
  type: reference
---

For a method-of-moments Beta with `k = mu(1-mu)/v - 1`, `shape1 = mu*k`, and
`v = (mu*CV)^2`:

```
shape1 = (1 - mu) / CV^2        (exact; ~ 1/CV^2 for small mu)
```

**So the density is J-shaped (mode at 0, unbounded density) iff CV >= 1** — the mean
is irrelevant. Checked: CV 0.45 -> shape1 4.936; 0.75 -> 1.777; 0.99 -> 1.020;
1.00 -> 0.999.

**The usual guard does not guard this.** Clamping `v <- pmin(v, mu*(1-mu)*0.999)`
fires only when `CV^2 > 0.999(1-mu)/mu`, i.e. **CV > 52.9** at mu = 3.6e-4 — 53x
past the cliff that actually matters. And when it *does* fire it produces exactly
the pathology it was written to prevent: `k -> 1/0.999 - 1 = 0.001`, both shapes
~1e-3, a two-point mass at 0 and 1. Silently. The correct guard is on
`shape1 >= 1` (or warn), not on the variance bound.

**Lognormal-derived CVs do not transfer their intervals to a Beta.** The
"factor-F 95% interval -> sdlog = log(F)/3.92 -> CV = sqrt(exp(sdlog^2)-1)" recipe
matches a *moment*, not the interval. At small mu the Beta is effectively a Gamma,
whose lower tail is heavier:

| CV   | lognormal 95% factor | realised Beta 95% hi/lo |
|------|----------------------|-------------------------|
| 0.45 | 5.4                  | **6.4**                 |
| 0.55 | 7.0                  | **10.2**                |
| 0.75 | 13.7                 | **29.3**                |

A "factor of 10" interval needs CV ~ 0.546, not 0.75. Always compute
`qbeta(c(.025,.975), shape1, shape2)` and report the realised ratio — never quote
the lognormal factor you started from.

Related: [[gravity-likelihood-invariants]].
