# GISFORUS Spatial Research Suite — Geospatial & Remote-Sensing Analysis Pipeline

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.22075124.svg)](https://doi.org/10.5281/zenodo.22075124)
[![License: MIT](https://img.shields.io/badge/License-MIT-green.svg)](LICENSE)

A **GISFORUS™** project. An open, general-purpose **R / Shiny + Google Earth Engine** pipeline for
reproducible, publishable remote-sensing research — land-cover & change analysis, spectral-index
libraries, trend/correlation statistics, and a marine/coral module. Designed as an extensible
suite, not a single-study tool.

> **Case study:** Maya Bay (Ko Phi Phi Leh, Thailand) — using the 2018–2022 tourism
> closure as a natural experiment to separate tourism impact from climate impact on the reef.

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

A companion **research framework** (`docs/methodology-framework.md`) turns these indicators into a
**compound reef–tourism management index** (Reef Condition × Tourism Pressure → traffic-light
decision classes with precautionary triggers), using a DPSIR + BACI / interrupted-time-series design.

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
| `mod_extractor.R` | Boundary / shapefile handling |
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
export is available immediately from a boundary + date range. (The internal build's separate
*Statistical Analysis* tab is not part of this release; trend, correlation and confidence-interval
statistics are still produced inside the GEE pipeline and its reports.)

No API keys, service-account keys, or credentials of any kind are committed to this repository.

---

## Reproducibility & honesty

Every run records the exact parameters to reproduce it, and the report states its own limits:
these are **satellite screening tools, not field-validated**. In particular the blue-green bottom
index cannot separate **live coral from macroalgae** — read it as "dark benthic cover", confirm
with in-situ / hyperspectral data, and see the interpretation notes attached to every correlation.

---

## Reproducing the Maya Bay case study

The paper's natural experiment uses the 2018–2022 tourism closure of **Maya Bay
(Ko Phi Phi Leh, Thailand)** to separate tourism pressure from climate pressure on the reef.
To reproduce it end-to-end:

**1. Study area.** In the *Shapefile Extractor* (or by drawing a ROI on the map), use the bay and
its fringing reef. Approximate bounding box (WGS84 / EPSG:4326 — adjust to your exact ROI):

| | Longitude (E) | Latitude (N) |
|---|---|---|
| min | 98.758 | 7.672 |
| max | 98.772 | 7.686 |

**2. Three time windows** (drive the same pipeline for each, then compare):

| Phase | Date range | Sensor to use |
|---|---|---|
| Pre-closure baseline | `2016-01-01 → 2018-05-31` | Landsat long-record coral (Sentinel-2 is sparse pre-2019) |
| Closure | `2018-06-01 → 2021-12-31` | Sentinel-2 (2019+) + Landsat |
| Post-reopening | `2022-01-01 → 2025-12-31` | Sentinel-2 |

**3. Pipeline order** (GEE Cloud Analytics → sequential pipeline): Land Mask → Allen Coral Atlas
coral mask → **Coral Health** (blue-green bottom index) → **Coral-vs-Algae** variability screen →
**Marine Heat Stress** (SST anomaly + Degree Heating Weeks) → **Turbidity** (Nechad) → **Trend**
(OLS + 95% CI, Mann-Kendall/Theil-Sen) → **Cross-indicator correlation**. Each step hands its
region/mask to the next; export the HTML report at the end.

**4. Reading the result — the key caveat.** The bottom index *rises* after reopening (peaking in the
2024 heatwave year). This is **not** proof of coral recovery: the blue-green index cannot tell live
coral from macroalgae, and the **Coral-vs-Algae** variability map flags that benthic cover as
fluctuating (algae-likely), not stable (coral-likely). Read the rise as "more dark benthic cover,"
consistent with macroalgae — and confirm with in-situ / hyperspectral data before any ecological claim.

The provenance table in every report records the exact CRS, date range, bounding box, parameters,
and software versions used, so a reviewer can re-run the identical analysis.

---

## How to cite

If you use this software, please cite it (see `CITATION.cff`, which GitHub renders as a
"Cite this repository" button):

> Pathak, A. K. (2026). *GISFORUS Spatial Research Suite — Geospatial & Remote-Sensing Analysis Pipeline* (Version 1.0.0) [Computer software].
> GISFORUS. Zenodo. https://doi.org/10.5281/zenodo.22075124

**DOI:** [10.5281/zenodo.22075124](https://doi.org/10.5281/zenodo.22075124) — this is the *concept DOI* and always resolves to the latest release. Zenodo also mints a version-specific DOI for each release.

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
