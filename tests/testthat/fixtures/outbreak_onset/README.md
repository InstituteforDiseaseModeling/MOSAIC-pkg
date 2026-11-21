# Outbreak onset parity fixture

Pins `calc_outbreak_onset()` to the onset-detection handoff of 18 November 2026: a Python
implementation with its report, kept outside the package. The growth rule is that
implementation's "tuned2" settings. The Zheng rule is its port of OutbreakExtractR
(HopkinsIDD, main @ 4fcdc21), which it had verified week for week against the R package.

| File | Content |
|---|---|
| `weekly.csv` | Weekly observed suspected cases for BEN, RWA, ZWE, UGA, GIN (the handoff's `weekly_observed.csv`, which equals `config_default$reported_cases` summed to Monday weeks with imputed days kept), and the population on each Monday (`param_N_population_size.csv`). |
| `expected.csv` | The handoff's own outbreak tables for those countries, not output of this package. Four tables: growth full series, growth real time, Zheng full series, Zheng real time. |
| `smoother_BEN.csv` | The handoff smoother's full-series fit for BEN: smoothed mean `mu`, growth rate `r` and its lower 95% bound `lo` per week, and the selected `lam`. |

The countries were chosen for their edge cases:
- BEN: two separate 2024 waves.
- RWA: a growth outbreak open at the end; a Zheng outbreak closing inside the padding.
- ZWE: a merged 2018 double peak and a 28-week reporting gap.
- UGA: a 24-week gap.
- GIN: cases but no growth outbreak.

The handoff's tables are mapped to the package's conventions:
- An open outbreak has an empty `end_week`.
- A Zheng outbreak that closes only inside the trailing padding is open.
- A retrospective outbreak has an empty `detection_week`.

The handoff spread 60 segments over each series. The package's segments have a fixed width
(`segment_weeks`, default 7.5), so the tests pass `segment_weeks = n / 60`, where `n` is the
number of analysed weeks, to reproduce its knots.

The full 40-country parity run, and the scripts that built these files, are in the
maintainer's working directory (`claude/outbreak_onset/scripts/04_parity.R` and
`05_make_fixture.R`). That directory is not part of the package.
