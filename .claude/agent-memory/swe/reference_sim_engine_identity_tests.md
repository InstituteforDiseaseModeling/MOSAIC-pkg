---
name: sim-engine-identity-tests
description: Oracle-free way to verify the R engine — set rho=rho_deaths=chi=1 to make the reporting binomials deterministic, then assert exact channel identities; includes the confirmed reported_cases/reported_deaths lag alignment (cases carry an extra +1 day)
metadata:
  type: reference
---

# Verifying the engine without the Python oracle

The replay fixtures only cover one config family (see
[[reference_engine_oracle_version_trap]]). For anything they miss, these
oracle-free identities are the gate. The trick: **set `rho = rho_deaths = 1` and
`chi_endemic = chi_epidemic = 1`, which makes every reporting binomial
deterministic** (`Binom(n, 1) == n`), turning a stochastic channel into an exact
equation.

## Confirmed identities (v0.84.0, 40 patches, verified at several lags)

With result matrices `[npatches, nticks]` and 1-based column `c`:

```
reported_deaths[, c] == disease_deaths[, c - delta_reporting_deaths]
reported_cases [, c] == new_symptomatic[, c - 1 - delta_reporting_cases]
```

both exact (0 for out-of-range source columns), verified at
`(delta_cases, delta_deaths)` = (0,0), (0,5), (5,0), (5,5), (1,1), (14,14). A ±1
shift of either fails, so the alignment is uniquely pinned.

**The extra `- 1` on cases is real and is inherited from the oracle**, not a port
bug: `new_symptomatic` is written at `[tick+1]` while `reported_cases` is written
at `[tick]`. Consequences worth knowing before touching likelihood date alignment:
- cases lag incidence by `delta_reporting_cases + 1` days; deaths lag
  `disease_deaths` by exactly `delta_reporting_deaths`. The two series are
  mutually offset by a day.
- **`reported_cases[, 1]` is structurally zero for every configuration** (it reads
  the never-written seed row). `reported_deaths[, 1]` is not.

## Other identities that hold exactly (max relative error 0)

- **Population conservation:** `N[, t] == N[, t-1] + births[, t] - non_disease_deaths[, t] - disease_deaths[, t]`, with `N[, 0]` := the sum of the `*_j_initial` config fields.
- **`Psi`:** `Psi[, t] == beta_jt_env[, t] * (1 - theta_j) * W[, t-1] / (kappa + W[, t-1])`, with `W[, 0] := 0`. So `Psi[, 1]` is always 0.
- **`Lambda`** (for `t >= 2`; `t = 1` reads the seed row, which is not in the output):
  `Lambda[, t] == pmax(beta_jt_human[, t] * eff^alpha_1 / N[, t-1]^alpha_2, 0)` where
  `eff = local_frac * I[, t-1] + colSums((tau_i * I[, t-1]) * pi_ij)` and `I = Isym + Iasym`.
- **`incidence == incidence_env + incidence_human`**, bit-identical.
- **Dose schedule alignment:** `dose_one_doses == matrix(as.integer(round(MOSAIC:::.sim_f32(nu_1_jt))), nrow = npatches)`, bit-exact over 1,398 ticks whenever the clamp does not bind — and ±1-day shifts both fail, so this pins the `nu_*_jt[tick+1, ]` indexing.

## Column-to-Python-index cheat sheet

`sim_results.R` applies three different trims, so the mapping from result column
to Python tick is channel-dependent and this is the easiest thing to get wrong:
- `SIM_CHANNELS_TRIM_FIRST` (compartments, incidence, `N`, `W`, `Lambda`, `Psi`, `new_symptomatic`, `spatial_hazard`): column `c` ↔ Python index `c`, i.e. state **after** tick `c-1`.
- `SIM_CHANNELS_TRIM_LAST` (`births`, `disease_deaths`, `non_disease_deaths`, `reported_cases`, `reported_deaths`): column `c` ↔ Python index `c-1`, written **at** tick `c-1`.
- `dose_one_doses` / `dose_two_doses`: already `nticks`-shaped, column `c` ↔ tick `c-1`.

The initial conditions are **not in the output at all** — column 1 is already one
tick of dynamics past `date_start`.
