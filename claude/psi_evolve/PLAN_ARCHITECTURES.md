# Phase 3 — new architectures: what is required, and the experiments

Scope: TFT, TiDE, PatchTST, TimeMixer++ (plus DLinear as the mandated cheap
control). Written after auditing what the codebase already supports.

---

## 1. What is already in place

| requirement | status |
|---|---|
| attention / norm / conv / einsum / permute primitives | **present** — keras3 1.5.1 has `layer_multi_head_attention`, `layer_layer_normalization`, `layer_conv_1d`, `layer_einsum_dense`, `layer_permute`, `layer_reshape` |
| **new package dependency** | **NOT needed for any of the four.** All buildable in keras3-in-R |
| a pluggable encoder slot | **present** — `arch_control$trunk` registry (`lstm|gru|tcn`), contract `(B, T, F) -> (B, units_3)` |
| static country covariates | **present** — `.PSI_STATIC_COUNTRY_COVARIATES`, 12 fields, already z-scored per country (built for D9b) |
| context length as a knob | **present** — `arch_control$timesteps` (default 13) |
| fold availability at long context | **not a constraint**: fold counts are identical at ts = 13/26/52/104 (21 for an F4-like geometry, 33 for F6); sequence loss is 3% / 5% / 11% / 22% |

## 2. What is missing — the one real gating dependency

**The known-future vs observed-past feature classification does not exist as a
reusable artifact.** AR03 derived something equivalent (it excluded "the 20
features not observable at a 12-week origin", 38 -> 18) but only the arm's score
was recorded; the list itself was not preserved.

TFT and TiDE both require the 38 covariates split into three streams:

- **static** — have it (the 12 country covariates)
- **known-future** — climate/seasonal fields genuinely available past the cutoff
- **observed-past** — everything knowable only up to the origin

So **re-deriving that split is a prerequisite for Tier 2 and should be done as a
standalone, reviewable artifact** (`.PSI_KNOWN_FUTURE_COVARIATES`), not inline
inside an architecture. It is also independently valuable: it is the honest
statement of what the model is allowed to know at forecast time.

## 3. The context-length problem — read before costing PatchTST/TimeMixer++

**PatchTST and TimeMixer++ are built for context lengths in the hundreds.** Our
window is **13**. At T = 13, PatchTST with patch_len 4 / stride 2 produces ~5
patches; the mechanism that makes it work has no room to operate. TimeMixer++'s
multi-scale mixing can downsample 13 -> 6 -> 3 and little more.

Running either at T = 13 would be a test of the wrong thing, and a null would be
uninformative. Hence Tier 0 below is a **prerequisite, not an optional extra**.

---

## Experiments

### Tier 0 — does context length matter at all? (one-line config, no new code)

| arm | change | vs |
|---|---|---|
| **W1** | `timesteps = 26` (6 months) | P000 |
| **W2** | `timesteps = 52` (1 year) | P000 |

Cheap, and it also directly probes the timing failure: if the model cannot
locate phase because it only ever sees 13 weeks, more context should move
`dir_acc`. Note sequences per country fall only 3% -> 11%.

**GATE: if neither W1 nor W2 moves `dir_acc`/`dcor`, drop PatchTST and
TimeMixer++ entirely** — they exist to exploit long context, and we would have
just shown there is nothing to exploit.

### Tier 1 — drop-in trunks (no data changes, no scoring changes)

These satisfy the existing contract `(B, T, F) -> (B, units_3)` and slot into
the trunk registry. The FiLM head, sequence builder, CV, and scoring are all
untouched, so each is **one function plus a registry entry**.

| arm | trunk | build required | runs at T=13? |
|---|---|---|---|
| **ND** | **DLinear** — series decomposition (moving-average trend + remainder), one linear map each, summed | trivial; ~20 lines | yes |
| **NT** | **TCN** | **already written**, never run | yes |
| **NM** | **TimeMixer++** — multi-scale downsample, time-mixing + feature-mixing MLPs per scale, cross-scale aggregation | moderate: `layer_permute` + `layer_dense` blocks, no attention needed | only at T >= 26 |
| **NP** | **PatchTST** — patchify, channel-independent transformer encoder, flatten head | moderate: patching via `layer_reshape`, then `layer_multi_head_attention` + norm | only at T >= 52 |

**ND is the highest-value cheap test in the whole plan.** DLinear matching our
LSTM would say the architecture programme is misdirected — and we already have a
version of that signal, since week-of-year climatology beats every arm on trend.

### Tier 2 — whole-model replacements (subsume the FiLM head)

These are **not trunks**. They replace the scaffold, because their whole point is
routing static / known-future / observed-past streams differently.

| arm | build required |
|---|---|
| **NF — TFT** | (a) the feature split above; (b) 3 new model inputs; (c) variable-selection networks; (d) static context gating (4 static encoders feeding LSTM state + enrichment); (e) interpretable multi-head attention; (f) **native quantile head** — which would *replace*, not supplement, the current interval path |
| **NE — TiDE** | (a) the feature split; (b) MLP encoder-decoder with residual blocks and covariate projection. Considerably lighter than TFT |

**TFT is the best structural match to this problem** — it is the only candidate
that treats "covariates that extend past the cutoff" as a first-class input type
rather than an accident, which is precisely the confusion at the root of the
`lead = 0` finding. It is also the largest build.

### Ordering and gates

```
T2 (lead=12)  ──► must land first; every architecture below would otherwise be
                  trained on the same concurrent target and re-test the wrong thing
   │
   ├─► ND (DLinear)   ── cheap control, run regardless
   ├─► NT (TCN)       ── already built, run regardless
   │
   └─► W1/W2 (context) ──► gate ──► NM (TimeMixer++), NP (PatchTST)
                                │
                                └─► feature split ──► NE (TiDE) ──► NF (TFT)
```

## 4. Risks to state up front

1. **Reproducibility.** keras psi diverges up to 0.98 run-to-run at production
   scale, where the parked torch port was bitwise reproducible. Attention-based
   trunks add stochastic depth/dropout on top of that. If cross-arm differences
   land near the 0.1% MAE replicate floor, they will be unresolvable. Reviving
   the torch path is the mitigation, and is itself a sizeable piece of work.
2. **Data volume.** ~20k sequences total. TFT and PatchTST are usually reported
   on far more. The DLinear critique exists precisely because complex models
   underperform in this regime — hence ND first.
3. **Scope discipline.** Four architectures is a lot of new surface. The gates
   above exist so that a null at Tier 0 or from ND stops the spend rather than
   inviting "try the next one".
