# Spatial Research Suite — Reef & Land Remote-Sensing Pipeline

An open **R / Shiny + Google Earth Engine** analysis pipeline for satellite monitoring of
coral reefs under tourism pressure, and general land-cover / change analysis. Built for
reproducible, publishable remote-sensing research.

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

No API keys, service-account keys, or credentials of any kind are committed to this repository.

---

## Reproducibility & honesty

Every run records the exact parameters to reproduce it, and the report states its own limits:
these are **satellite screening tools, not field-validated**. In particular the blue-green bottom
index cannot separate **live coral from macroalgae** — read it as "dark benthic cover", confirm
with in-situ / hyperspectral data, and see the interpretation notes attached to every correlation.

---

## How to cite

> Pathak, A. K. (2026). *Spatial Research Suite — Reef & Land Remote-Sensing Pipeline.*
> https://github.com/anant9677/spatial-research-suite

Please also cite the underlying datasets and methods listed in each report's
**Data sources & references** section (Sentinel-2/Landsat, NOAA OISST, Allen Coral Atlas,
Hedley 2005, Lyzenga 1981, Mann-Kendall, Theil-Sen, Reynolds 2007, Olofsson 2014, Pontius & Millones 2011, …).

## License

MIT — see `LICENSE`. (Switch to CC-BY-4.0 or another license if you prefer stronger attribution terms.)

## Contact

Anant K. Pathak · anant4infinity@gmail.com
