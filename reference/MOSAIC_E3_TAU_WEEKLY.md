# Per-country overland departure rates from the E3 border-throughput evidence

Weekly outbound departure probability per person, assembled in the
MOSAIC-OCV E3 investigation from border-throughput counts (Goma
~50k/day, Busia ~23k/day, Ressano Garcia ~7.1k/day), IOM DTM
flow-monitoring, and the StatsSA P0351 overland share. Values are the
four regional `params_<region>.json` margins.

## Usage

``` r
MOSAIC_E3_TAU_WEEKLY
```

## Format

Named numeric vector, weekly departure probability per person.

## Source

MOSAIC-OCV `output/E3_mobility_route2b_*/params_*.json`; method in
`notes/E3-mobility-west-refit.md` section 3.

## Details

**These are evidence-anchored approximations, not measurements.** Every
underlying count is a floor (recorded crossings only, movements not
persons), so the band is deliberately wide and is carried into the prior
as such – see
[`est_overland_tau_prior`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/est_overland_tau_prior.md).

Where two regions disagreed the **larger** value is kept (UGA: central
0.003 vs eastern 0.002 -\> 0.003), on the grounds that each regional
estimate counted only that region's corridors and so understates total
outbound flow.

**Ethiopia is revised up from E3's 3e-4 to 1e-3/week.** E3's value
implies ~5,400 outbound crossings/day for 127M people – fewer than
Rwanda (13.8M) and fewer than a single Mozambican border post – and the
Horn had the thinnest border-throughput evidence of the four regions.
E3's own border-district decomposition
(`tau_national ~ f_border x tau_border`, calibrated off MOZ) gives
~6.7e-4. 1e-3 keeps Ethiopia comfortably the lowest evidenced value,
preserving the ordering, without asserting something arithmetically
implausible. **Any conclusion about Ethiopian cross-border spillover
must be re-run across this band**: the earlier “no resolvable spillover”
finding was an arithmetic consequence of the least-evidenced number in
the panel.
