---
name: feedback-skill-constants-rot-despite-hedge
description: When auditing skills, the "reference don't transcribe" hedge in a skill header does NOT excuse a wrong enumerated constant in the body — verify enum/preset/path values even when the skill cites a ?fn source of truth
metadata:
  type: feedback
---

A skill can open with "Reference, don't transcribe — cite `?fn` not a value that will rot" and STILL
ship a wrong constant in its body. The hedge mitigates *drifting numbers* (prior centers, versions);
it does NOT cover **enumerated argument values** a reader will copy verbatim.

Why: the run-mosaic skill correctly cites `?mosaic_io_presets` yet listed the presets as
"fast / balanced / thorough" — actual accepted values are `default/debug/fast/archive`
(`R/presets.R:29`). "balanced" is only the prose description of `default`; "thorough" doesn't exist.
A reader copies the enum, not the man-page pointer.

How to apply: when auditing any skill, grep the actual `function(arg = c(...))` signature for every
enumerated value the skill names (presets, architecture strings, method choices, column names,
dir-tree paths). The reference-not-transcribe discipline is necessary but not sufficient — verify
enums and paths regardless of the hedge.
