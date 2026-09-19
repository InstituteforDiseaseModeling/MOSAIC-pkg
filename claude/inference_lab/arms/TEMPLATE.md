# Arm <id> — <one-line hypothesis>

**Mechanism.** What in the code changes, and why that should move the metric.
**Prediction (recorded before running).** Direction and rough size, on the named primary metric.
**Falsifier.** What result would make this wrong. If none exists, this is not an experiment.
**Primary metric.** Held-out WIS unless stated, with the tier.
**Decision rule.** Promote if paired mean improvement > 2 x paired SD AND no guard regresses AND all
negative controls failed as required.
**Guards that must not regress.** coverage(50/95), `khat`, `ess_is`, cost.
**Escalation.** Does this touch the likelihood's form, priors, or dependencies? If yes -> human decision
before promotion, regardless of result.
