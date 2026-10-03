---
name: tau-i-departure-semantics
description: tau_i is a per-DAY away-fraction in the engine (not weekly, not a trip-incidence); since priors v17.x the production prior is the overland lognormal (the 1000x-deflated air Beta is only a fallback); the E3 "~1:90 StatsSA" and "~100x overland" multipliers are transcription/unit errors
metadata:
  type: reference
---

Established 2026-09-18 while red-teaming `model/input/param_tau_departure_overland.csv`
(built by `est_overland_tau_prior()` in `R/rake_mobility_od_to_tau.R`) against the
MOSAIC-OCV E3 provenance.

## 1. What tau_i MEANS in the engine (authoritative)
`laser/cholera/metapop/params.py:172` sets `nticks = (date_stop - date_start).days + 1`
=> **one tick = one day**. `humantohuman.py` applies, every tick:
`local_i = (1 - tau_i) * I_i` and `immigrating_i = (tau_i * I_i) * pi_ij`.
There is **no traveller compartment and no return process** — tau is re-applied fresh
each day from a static scalar.

=> the algebra implements a **stationary away-fraction** (Sattenspiel-Dietz / Keeling-Rohani
"visiting" coupling), i.e.
`tau_i = short-term departures per person-day x mean days away per trip`.
It is NOT a departure *incidence* unless you assume mean trip length = 1 day.
Long-term migrants must be EXCLUDED (their infections and their N already live in the
destination patch), so the quantity is circular/short-term presence only.

**Units:** the air fit reads `oag_africa_2017_mean_daily.csv` (`est_mobility.R`, "Getting mean
DAILY OAG flight data"), so `param_tau_departure.csv` tau is **daily**. `04-model-description.Rmd`
L753 correctly says daily; **L650 and the L724 figure caption say "weekly" and are STALE** —
they contradict the engine and have caused at least one 7x error downstream (see §3).

## 2. HISTORY (true at priors v15.18, SUPERSEDED): the air tau prior was diffuse and zero-spiked
**Current state (verified 2026-10-02, priors v17.1 / config v6.1):** all 40 `tau_i` priors are
lognormal from `param_tau_departure_overland.csv` (sdlog 0.587 for 26 E3 countries, 0.868 for 14
default countries at the 0.0025/wk median); config `tau_i` == prior median exactly. The Beta route
below only runs if that file is missing. See [[mobility-overland-vs-blend]].
`data-raw/make_priors_default.R:848` sets `tau_uncertainty_factor <- 0.001`, multiplying the
fitted Beta concentration by 1e-3. In `priors_default` v15.18 that gives:
- median CV **1.40** (not the raw file's ~0.04)
- **31 of 40** countries have `shape1 < 1` => density monotone decreasing, **mode at 0**
- median 95% span factor ~4,800x; ERI/SWZ/GNB prior medians are ~1e-9 to 1e-20
- on average **32%** of each country's prior mass sits below 10% of its own prior mean

**Implication:** a large share of the "MOSAIC cross-border coupling is weak / spillover ~ 0"
story is NOT the air data — it is this deflation creating a spike at zero in a
prior-as-proposal sampler (see [[required-n-prior-proposal]]). Any claim that the air tau prior
is "immovable at shape2 ~2e8" is describing the CSV, not production.

## 3. The E3 amplitude multipliers are compounded errors — do not cite them
- `E3-mobility-overland-adjustment.md:81` (the real evidence): StatsSA Tourism 2024
  (Report 03-51-02) "68-91% of SA's ~8.9M annual cross-border arrivals are OVERLAND by road"
  => air is 9-32% of flow => **road:air ~ 2:1 to 10:1**.
- `E3-mobility-production-design.md:38` mutated "91%" into "**~1/90 air:road**", then into
  "the empirical ~100x air-undercount". `R/rake_mobility_od_to_tau.R:6,80` inherited "~1:90".
  **There is no 90:1 anchor.**
- The "45x-561x, median 84x" lift in `E3-mobility-west-refit.md` §5 compares a **weekly**
  overland tau to a **daily** air tau => inflated by exactly 7x. Engine-relevant lift is
  median **10-12x** (range 0.8x-148x).
The CSV's own numbers (weekly / 7) are unaffected by both errors; only the justification text is.

## 4. MOSAIC's 40 patches exclude major land neighbours
`iso_codes_mosaic` has **no DJI, SDN, LSO** (also no MDG/COM/EGY/LBY/DZA/MAR/TUN).
Because `pi_ij` row-normalises over in-set patches only, a tau counting ALL outbound crossings
mis-routes out-of-set flow onto in-set neighbours. Binding for **ETH** (Galafi/Dewele->DJI and
Metema/Humera->SDN are its two biggest corridors), **SSD** (Renk/Joda->SDN), **ZAF**
(Maseru->LSO), TCD/NER/ERI/MRT. tau should arguably carry an explicit in-set fraction.

## 5. Engine cannot consume an overland pi
`metapop/utils.py::get_pi_from_lat_long` rebuilds pi at runtime from lat/lon great-circle +
sampled gamma/omega + initial N. Adopting an overland tau without an overland-consistent
gamma/omega routes amplified departures through the AIR kernel — the metric-mismatch trap.
**Resolved in production:** the shipped kernel is the blend fit (gamma 1.90), which is 93% overland
by flow and within dgamma 0.054 of the overland-only fit — not the air kernel (1.36).

See also [[param-identifiability-eth-v0903]] (tau_i is absent from the OAT classification
because a single-patch ETH run makes it inert).
