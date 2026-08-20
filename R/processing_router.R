# =========================================================================
# PROCESSING_ROUTER.R  —  Dynamic local-R vs Google-Earth-Engine dispatcher
# =========================================================================
# WHAT THIS IS (and, honestly, what it is NOT yet)
# ------------------------------------------------------------------------
# GOAL: for a SMALL Area of Interest, run the analysis locally in R (sf/terra)
# instead of on Google Earth Engine; for a large AOI, keep using GEE.
#
# THE HONEST ARCHITECTURAL REALITY — read before wiring this in:
#   Earth Engine is NOT just a compute backend for this app. It is the DATA
#   SOURCE. Every LULC classification, cloud-analytics chart, and temporal
#   series is built on imagery (Landsat / Sentinel) that lives on Google's
#   servers and is pulled through GEE. To run a step "100% locally" you must
#   FIRST have the pixels on the machine. So "0% GEE for a small AOI" is only
#   possible if the imagery itself comes from a NON-GEE source (a public STAC
#   catalog of Cloud-Optimized GeoTIFFs — e.g. Microsoft Planetary Computer,
#   Element84 Earth Search, or USGS) that terra reads directly.
#
#   That imagery-ingest pipeline + a local re-implementation of each analysis
#   (cloud masking, index math, RF classify, zonal stats, temporal series) is
#   a genuine sub-project — NOT a one-file refinement. This router is the clean
#   seam that makes that migration incremental and SAFE: modules call
#   route_engine(aoi); today most branches still return "gee", and we flip them
#   to "local" one analysis at a time as each local path is built and verified.
#
# WHAT IS READY NOW:
#   * route_engine() / aoi_area_sqkm() — the decision layer (tested).
#   * local_zonal_stats() — a real, GEE-free zonal-statistics implementation on
#     a terra raster (the first analysis wired to the local path).
#   * ROUTE_TABLE — an explicit, honest map of which analyses can run local
#     TODAY vs which still need the imagery-ingest pipeline.
# =========================================================================

# --- 1. THRESHOLD (senior-architect default; tune to your Cloud Run memory) ---
# Rationale: a local terra/ranger classification loads the AOI's pixels into RAM.
# At Landsat 30 m, 2,500 sq km ≈ 2.8M pixels/band — comfortable on a 4 GiB
# instance with several bands + indices. Beyond this, GEE's server-side compute
# is both faster and safer against OOM, so we hand large AOIs back to GEE.
LOCAL_AOI_MAX_SQKM <- 2500

# --- 2. DECISION LAYER (pure, testable) --------------------------------------
aoi_area_sqkm <- function(sf_obj) {
  if (is.null(sf_obj)) return(NA_real_)
  tryCatch(as.numeric(sum(sf::st_area(sf_obj))) / 1e6, error = function(e) NA_real_)
}

# Returns "local" or "gee". NA / unknown area -> "gee" (safe default: never
# silently attempt a local run we can't size).
route_engine <- function(sf_obj, max_sqkm = LOCAL_AOI_MAX_SQKM) {
  a <- aoi_area_sqkm(sf_obj)
  if (is.na(a) || a <= 0) return("gee")
  if (a <= max_sqkm) "local" else "gee"
}

use_local_engine <- function(sf_obj, max_sqkm = LOCAL_AOI_MAX_SQKM) {
  identical(route_engine(sf_obj, max_sqkm), "local")
}

# --- 3. HONEST COVERAGE MAP --------------------------------------------------
# TRUE  = a local (sf/terra) path exists and can be routed today.
# FALSE = still GEE-only; needs the imagery-ingest pipeline first. This is the
#         backlog for "100% coverage", made explicit instead of pretended.
ROUTE_TABLE <- list(
  boundary_area_and_geometry = TRUE,   # pure sf — already local everywhere
  zonal_statistics           = TRUE,   # local_zonal_stats() below (needs a local raster)
  raster_area_by_class       = TRUE,   # terra::freq on a local classified raster
  imagery_fetch              = FALSE,  # NEEDS non-GEE STAC/COG ingest (the linchpin)
  cloud_masking              = FALSE,  # needs local imagery first
  spectral_indices           = FALSE,  # trivial in terra ONCE imagery is local
  rf_classification          = FALSE,  # ranger runs local; needs local training pixels
  temporal_series            = FALSE,  # needs a local multi-date imagery stack
  chart_generation           = TRUE,   # already local (ggplot2) — consumes whatever stats it's given
  exports_pdf_zip            = TRUE    # already local (export_engine.R)
)

# --- 4. FIRST CONCRETE LOCAL CAPABILITY: zonal statistics --------------------
# GEE-free zonal stats: given a terra SpatRaster (a value layer already on disk/
# in memory) and an sf/terra vector of zones, compute per-zone summaries. This
# is what the "local" branch of the Statistics module calls when route_engine()
# says "local" AND a local raster is available.
local_zonal_stats <- function(value_raster, zones_sf,
                               stats = c("mean", "median", "min", "max", "sd"),
                               zone_id_col = NULL) {
  if (!requireNamespace("terra", quietly = TRUE)) stop("terra is required for local_zonal_stats().")
  zones_v <- if (inherits(zones_sf, "SpatVector")) zones_sf else terra::vect(zones_sf)
  if (!is.null(zone_id_col) && zone_id_col %in% names(zones_v)) {
    ids <- as.data.frame(zones_v)[[zone_id_col]]
  } else {
    ids <- seq_len(nrow(zones_v))
  }
  fun <- function(v) {
    v <- v[!is.na(v)]
    if (!length(v)) return(stats::setNames(rep(NA_real_, length(stats)), stats))
    vals <- vapply(stats, function(s) switch(s,
      mean = mean(v), median = stats::median(v), min = min(v),
      max = max(v), sd = stats::sd(v), NA_real_), numeric(1))
    stats::setNames(vals, stats)
  }
  ex <- terra::extract(value_raster, zones_v)          # data.frame: ID + value column
  vcol <- names(ex)[2]
  agg <- lapply(split(ex[[vcol]], ex$ID), fun)
  out <- do.call(rbind, agg)
  data.frame(zone = ids, out, row.names = NULL, check.names = FALSE)
}
