---
name: gravity-likelihood-invariants
description: mobility::mobility departure-diffusion Poisson likelihood factorises into a multinomial in (gamma, omega) — three consequences that look like bugs or like safety but are neither
metadata:
  type: reference
---

`mobility` 0.6.5 `fit_departure_diffusion(type="power")` JAGS model:

```
M[i,j] ~ dpois(lambda[i,j])
lambda[i,i] = theta*N_i*(1-tau)
lambda[i,j] = theta*N_i*tau * x_ij/sum_j x_ij      x_ij = N_j^omega * (D_ij+0.001)^(-gamma)
```

**The factorisation.** Because `sum_{j!=i} p_ij = 1` by construction,
`sum_ij lambda_ij = theta*sum_i N_i` (free of gamma, omega), so

```
LL(gamma,omega | theta,tau) = sum_{i!=j} M_ij * log p_ij(gamma,omega) + const
```

a pure **multinomial** objective. Verified numerically: holding `theta*tau` fixed
and moving tau 0.3 -> 0.999 changes the off-diagonal LL by 9e-13.

Three consequences, all counter-intuitive:

1. **A zero diagonal forces tau -> 1.** With `M_ii = 0` the diagonal term is
   `-theta*(1-tau)*sum(N)`; substituting the row-total constraint `theta*tau = c`
   gives `-c*(1-tau)/tau*sum(N)`, strictly increasing on (0,1). Any row-normalised
   or contiguity-style OD matrix (zero diagonal by construction) will report
   `tau = 1.000` and a meaningless theta. This is **structural, not a fit failure** —
   and it does **not** contaminate gamma/omega (profile-orthogonal, see above).
   Their MCMC diagnostics are still garbage; do not chase them.

2. **Scaling M is a pure precision dial.** The objective is *linear* in M, so
   multiplying counts by `s` leaves the argmax untouched but multiplies the Hessian
   by `s`: `se ∝ 1/sqrt(s)`, measured exactly (se(gamma) 0.00725 -> 0.00229 ->
   0.00072 -> 0.00023 for s = 1e3..1e6). "The scale doesn't move gamma/omega" is
   true for the point estimate and **false for the uncertainty**. Feeding a
   unit-free structure in as `round(M * 1e5)` fabricates a CI as if 4M trips were
   observed.

3. **Row totals silently re-weight the estimand.** Origin `i` enters the objective
   with weight = its row total. A row-normalised matrix gives every origin *equal*
   weight; a flow/raked matrix weights by volume. Measured on the MOSAIC fused
   matrix this alone moved gamma 1.7619 -> 1.8325 (+4.0%) with no change to D.
   Never compare gammas across differently-weighted M.

**gamma scale-invariance in D is real but proves too much.** `D -> k*D` leaves the
row-normalised kernel identical, so gamma is *exactly* invariant (measured 1.0e-7
with the offset removed; the `+0.001` offset leaks ~1e-4 in gamma, negligible for
D in km or hours). Therefore invariance can never *excuse* an observed gamma gap —
it predicts zero gap from that source. It also means absolute distance units are
unidentified, so agreement of gamma is **not** validation of a travel-time surface.

See [[beta-from-mean-cv-jshape-cliff]].
