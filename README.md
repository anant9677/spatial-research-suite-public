# GISFORUS Spatial Research Suite — Geospatial & Remote-Sensing Analysis Pipeline

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.22075123.svg)](https://doi.org/10.5281/zenodo.22075123)
[![License: MIT](https://img.shields.io/badge/License-MIT-green.svg)](LICENSE)

A **GISFORUS™** project by Anant Kumar Pathak. An open, general-purpose **R / Shiny + Google Earth Engine**
pipeline for reproducible, publishable remote-sensing research — land-cover & change analysis, spectral-index
libraries, trend/correlation statistics, and a marine/coral module. Designed as an extensible suite, not a
single-study tool.

> **Companion paper.** This suite produced the analysis in:
> Pathak, A. K. (2026). *The optical ambiguity of coral and algae: replacing standalone impact evaluations with
> a nested BACI design to prevent policy misattribution in coastal tourism.* SSRN preprint.
> `‹add SSRN link/DOI once posted›`
>
> The study uses the 2018–2022 tourism closure of **Maya Bay (Ko Phi Phi Leh, Thailand)** as a natural
> experiment in a **three-tier nested before–after/control–impact (BACI)** design — Maya Bay (impact),
> Ko Phi Phi Don (internal control) and Ko Tarutao (external climate control) — to show that the reef's
> apparent post-closure "greening" is a region-wide, likely macroalgal signal rather than a closure effect,
> and that a blue–green optical index cannot on its own separate coral from algae.

---

## What it does

A **sequential, mask-aware pipeline** where each step hands its region/mask to the next, over a
user-drawn boundary and date range, computed on Google Earth Engine via `rgee`:

**Marine / coral**
- **Coral Health** — blue-green bottom index (`ln(green) − ln(blue)`) on optically-shallow reef
  (Sentinel-2, 10 m) and a **Landsat long-record variant** (1984+, 30 m) for pre-2018 baselines.
- **Coral-vs-Algae (heuristic)** — temporal-variability screening that flags stable (coral-likely)
  vs fluctuating (algae-likely) bottom, to guard against reading macroalgae as coral recovery.
- **Marine Heat Stress** — SST anomaly and **Degree Heating Weeks** (NOAA OISST, bleaching driver).
- **Turbidity** — calibrated (Nechad et al.) and NDTI; **Chlorophyll-a** (NDCI).
- **Allen Coral Atlas** benthic-habitat masking (coral / rubble / sand / seagrass …).

**Pressure / context**
- **Nighttime Lights** (VIIRS DNB, NASA Black Marble), **Built-up** (NDBI / IBI), and a full
  index library (NDVI, NDWI/MNDWI, LST/UHI, CHIRPS, Sentinel-5P air quality, Sentinel-1 SAR, …).

**Analysis & reporting**
- **Trend** (OLS + 95% CI, Mann-Kendall + Theil-Sen), **Cross-indicator correlation**
  (Pearson & Spearman) with built-in interpretation caveats (same-sensor inflation, spurious
  shared trends, coral/algae ambiguity).
- **LULC engine** — Random-Forest land-cover classification with Olofsson (2014) error-adjusted
  area and Pontius & Millones (2011) quantity/allocation disagreement, plus FRAGSTATS-style
  landscape metrics.
- **Publishable HTML report** for every run: executive summary, maps, a **Sensors & datasets**
  table, a **reproducibility/provenance** table (CRS, software versions, parameters, bounding
  box), auto-written methods with DOIs, and honest limitations.

The suite can also combine its indicators into higher-level **management frameworks** (e.g. a
condition-versus-pressure decision space using a DPSIR + BACI / interrupted-time-series design;
see `docs/methodology-framework.md`). The companion paper deliberately keeps its focus narrower — a
nested-BACI test that separates tourism impact from climate impact — and treats such compound indices
as a framework for future work rather than a validated deliverable.

---

## Architecture

- **R / Shiny** modular app; each analysis is a self-contained module in `R/`.
- **Google Earth Engine** via `rgee` (Python `earthengine-api` through `reticulate`).
- No database or account system — analysis runs entirely from a boundary + date range.

### Modules (`R/`)
| File | Role |
|---|---|
| `mod_gee.R` | Core GEE Cloud-Analytics + the sequential marine/coral pipeline & report |
| `marine_engine.R` | Benthic stack, deglint, depth-invariant/bottom indices |
| `gee_indices.R` | Spectral-index library (local rgee compute) |
| `mod_lulc.R` | LULC classification, accuracy, change & landscape metrics |
| `mod_carto.R` | Publication-quality map rendering |
| `mod_extractor.R` | Boundary / shapefile handling, incl. bounding-box ROI entry (exact-coordinate cropping) |
| `mod_insights.R`, `export_engine.R`, `processing_router.R`, `multivariate_engine.R` | Insights, exports, routing, multivariate helpers |
| `global.R` | GEE session init + all shared/pure analysis functions |
| `app.R` | Entry point — builds the UI, wires the modules, runs the Export Manager |

---

## Running it

Requires **R (≥ 4.2)**, the R packages in the app headers (`shiny`, `rgee`, `sf`, `terra`,
`ggplot2`, …), and a Python env with `earthengine-api` (via `reticulate` / `rgee`).

**Earth Engine authentication is *not* included and is per-user.** Provide your own credentials:

```r
# one-time, per machine:
#   gcloud auth application-default login --scopes=...earthengine,...cloud-platform
# then rgee/earthengine-api uses your own Google Earth Engine project.
```

Then launch from the repository root (the folder holding `app.R`, `global.R`, and `R/`):

```r
shiny::runApp()          # or open app.R in RStudio and click "Run App"
```

Shiny auto-loads every module in `R/`; `global.R` initialises the Earth Engine session.
This is an **open build** — no accounts, no login, no payments: every module and every
export is available immediately from a boundary + date range.

No API keys, service-account keys, or credentials of any kind are committed to this repository.

---

## Reproducibility & honesty

Every run records the exact parameters to reproduce it, and the report states its own limits:
these are **satellite screening tools, not field-validated**. In particular the blue-green bottom
index cannot separate **live coral from macroalgae** — read it as "dark benthic cover", confirm
with in-situ / hyperspectral data, and see the interpretation notes attached to every correlation.
This optical ambiguity is the central methodological point of the companion paper.

---

## Reproducing the nested-BACI case study

The paper's natural experiment uses the 2018–2022 tourism closure of **Maya Bay** in a **three-tier
nested design**: an impact site, an internal control that shares Maya Bay's climate and tourism market,
and a low-tourism external control that anchors the region-wide climate signal. To reproduce it
end-to-end, run the same pipeline over each of the three ROIs and the three time windows, then compare.

**1. Study-area ROIs** (WGS84 / EPSG:4326 — enter directly in the *Shapefile Extractor* bounding-box fields,
or draw on the map):

| Tier | Role | Longitude (E) | Latitude (N) |
|---|---|---|---|
| **T1 Maya Bay** | impact — closed 2018–2022 | 98.761 – 98.769 | 7.673 – 7.681 |
| **T2 Ko Phi Phi Don** | internal control — open throughout | 98.752 – 98.800 | 7.708 – 7.798 |
| **T3 Ko Tarutao** | external control — low tourism | 99.585 – 99.714 | 6.493 – 6.742 |

**2. Three time windows** (drive the same pipeline for each tier, then compare):

| Phase | Date range | Sensor to use |
|---|---|---|
| Pre-closure baseline (PRE) | `2015-01-01 → 2018-05-31` | Landsat long-record coral (Sentinel-2 is sparse pre-2019) |
| Closure (DURING) | `2018-06-01 → 2021-12-31` | Sentinel-2 (2019+) + Landsat |
| Post-reopening (POST) | `2022-01-01 → 2025-12-31` | Sentinel-2 |

**3. Pipeline order** (GEE Cloud Analytics → sequential pipeline): Land Mask → Allen Coral Atlas
coral mask → **Coral Health** (blue-green bottom index) → **Coral-vs-Algae** variability screen →
**Marine Heat Stress** (SST anomaly + Degree Heating Weeks) → **Turbidity** (Nechad). Each step hands its
region/mask to the next; export the HTML report at the end. Use the report's absolute period-mean bottom
index (class-midpoint × class-share) so values are comparable across the per-run percentile stretches.

**4. Reading the result — the nested comparison.** The bottom index *rises* from PRE to POST at **all three
tiers** — Maya Bay +0.183, Ko Phi Phi Don +0.149, and, decisively, the low-tourism external control
Ko Tarutao **+0.251 (the most)**. The rise therefore does not track tourism status: the local
difference-in-differences (Maya − open Phi Phi) is only +0.034, and a reef that was never closed and
carries little tourism rose furthest. Peak Degree Heating Weeks escalated to 11.6 °C-weeks (severe-bleaching
range) and the Coral-vs-Algae screen grew more algae-like at every site. The apparent "recovery" is thus a
region-wide, likely macroalgal darkening — **not** a Maya Bay closure effect — and the blue-green index
cannot by itself confirm coral. Confirm with in-situ / hyperspectral data before any ecological claim.

The provenance table in every report records the exact CRS, date range, bounding box, parameters,
and software versions used, so a reviewer can re-run the identical analysis.

---

## How to cite

If you use this **software**, please cite it (see `CITATION.cff`, which GitHub renders as a
"Cite this repository" button):

> Pathak, A. K. (2026). *GISFORUS Spatial Research Suite — Geospatial & Remote-Sensing Analysis Pipeline*
> (Version 1.1.0) [Computer software]. GISFORUS. Zenodo. https://doi.org/10.5281/zenodo.22075123

If you refer to the **study / findings**, please also cite the paper:

> Pathak, A. K. (2026). *The optical ambiguity of coral and algae: replacing standalone impact evaluations
> with a nested BACI design to prevent policy misattribution in coastal tourism.* SSRN preprint.
> `‹add SSRN link/DOI once posted›`

**Software DOI:** [10.5281/zenodo.22075123](https://doi.org/10.5281/zenodo.22075123) — this is the *concept DOI*
and always resolves to the latest release. Zenodo also mints a version-specific DOI for each release.

Please also cite the underlying datasets and methods listed in each report's
**Data sources & references** section (Sentinel-2/Landsat, NOAA OISST, Allen Coral Atlas,
Hedley 2005, Lyzenga 1981, Mann-Kendall, Theil-Sen, Reynolds 2007, Olofsson 2014, Pontius & Millones 2011, …).

## License & trademark

The **source code** is released under the MIT License — see `LICENSE`.

**GISFORUS™** and the GISFORUS logo are trademarks of Anant Kumar Pathak. The MIT license covers
the code only; it does **not** grant any right to use the GISFORUS name or logo. You are welcome to
use, modify, and redistribute the code under MIT, but please do so under your own name/branding, not
under "GISFORUS". Full terms are in [`TRADEMARK.md`](TRADEMARK.md).

## About GISFORUS

**GISFORUS** is a geospatial software brand by Anant Kumar Pathak, building reproducible remote-sensing
and Earth-observation tools for research and decision-making. The Spatial Research Suite is its flagship
open research pipeline.

## Contact

Anant Kumar Pathak · anant4infinity@gmail.com · GISFORUS
