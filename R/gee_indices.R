# =========================================================================
# GEE_INDICES.R  —  Self-contained rgee index engine (spectral + terrain)
# =========================================================================
# Powers the expanded analysis catalogue the landing page advertises. Every
# index here is computed IN THE SHINY SERVER via rgee (the same way the LULC
# map, temporal grid and publication maps already call Earth Engine), so no
# external compute-service change is needed for these features.
#
# Design:
#   * build_optical_composite() collapses sensor differences ONCE into a
#     standardized reflectance image with named bands (BLUE, GREEN, RED,
#     REDEDGE, NIR, SWIR1, SWIR2). Sentinel-2 is used when available (>=2015),
#     otherwise harmonized Landsat. Every spectral index is then a plain
#     expression on those named, physically-scaled (0..1 reflectance) bands —
#     which eliminates per-sensor band-number bugs.
#   * SPECTRAL_INDEX_RECIPES is a data table: one row per index (expression,
#     palette, nominal range, red-edge requirement). Adding an index = adding a
#     row, not writing a function.
#   * build_index_image() returns the SAME contract as get_feature_img()
#     (list(img, pal, min, max, sensor)) so it slots straight into the existing
#     map/temporal/carto pipeline.
#   * compute_rgee_analytics() returns the SAME shape as the compute service's
#     `result` (tile_url, palette, min, max, sensor, stats, histogram) so the
#     existing render_result() in mod_gee.R consumes it unchanged — the Insights
#     Dashboard then works automatically.
#
# Correctness note: band math here is standard published formulae, but Earth
# Engine cannot run in the CI sandbox, so these must be validated on the TEST
# deployment (each index rendered once) before promoting to live.
# =========================================================================

# ---- The NEW analyses handled locally by rgee (existing compute-service
#      features like LST/VIIRS/NO2 are deliberately NOT in this list so their
#      proven path is untouched). ----
RGEE_NATIVE_FEATURES <- c(
  # Vegetation
  "Enhanced Vegetation (EVI)", "Soil-Adjusted Veg (SAVI)", "Modified SAVI (MSAVI2)",
  "Green NDVI (GNDVI)", "Atmospheric-Resistant Veg (ARVI)", "Visible ARVI (VARI)",
  "Red-Edge NDVI (NDRE)",
  # Water & Moisture
  "Modified NDWI (MNDWI)", "Moisture Index (NDMI)", "Land Surface Water (LSWI)",
  "Auto Water Extraction (AWEI)", "Turbidity (NDTI)", "Chlorophyll-a (NDCI)",
  # Urban & Soil
  "Urban Index (UI)", "Bare Soil Index (BSI)", "Built-up Index (IBI)",
  # Fire & Snow
  "Normalized Burn Ratio (NBR)", "Burn Area Index (BAI)", "Snow Index (NDSI)",
  # Geology / Minerals
  "Clay Minerals Index", "Ferrous Minerals Index", "Iron Oxide Index",
  # Terrain (DEM)
  "Terrain Aspect", "Hillshade", "Topographic Position (TPI)", "Terrain Ruggedness (TRI)"
)

is_rgee_native_feature <- function(f_name) {
  isTRUE(f_name %in% RGEE_NATIVE_FEATURES)
}

# ---- Standardized band palettes (kept short, brand-neutral, colour-blind-safe-ish) ----
.PAL_VEG   <- c('#d73027', '#fdae61', '#ffffbf', '#a6d96a', '#1a9850')
.PAL_WATER <- c('#8c510a', '#d8b365', '#f6e8c3', '#c7eae5', '#5ab4ac', '#01665e')
.PAL_BUILT <- c('#1a9850', '#ffffbf', '#fdae61', '#d73027', '#7f0000')
.PAL_MOIST <- c('#a6611a', '#dfc27d', '#f5f5f5', '#80cdc1', '#018571')
.PAL_BURN  <- c('#1a9850', '#a6d96a', '#ffffbf', '#fdae61', '#d73027')
.PAL_SNOW  <- c('#08306b', '#4292c6', '#9ecae1', '#deebf7', '#ffffff')
.PAL_GEO   <- c('#440154', '#3b528b', '#21918c', '#5ec962', '#fde725')
.PAL_TERR  <- c('#004529', '#78c679', '#ffffcc', '#d95f0e', '#993404')
.PAL_GREY  <- c('#000000', '#666666', '#bbbbbb', '#ffffff')

# ---- Spectral index recipes (expression on named reflectance bands) ----
# rng = nominal display range (auto-refined by get_feature_img's 2/98 percentile);
# s2_only = TRUE for indices needing the red-edge band (Sentinel-2 only).
SPECTRAL_INDEX_RECIPES <- list(
  "Enhanced Vegetation (EVI)"        = list(rename = "EVI",   expr = "2.5 * ((NIR - RED) / (NIR + 6 * RED - 7.5 * BLUE + 1))", pal = .PAL_VEG,   rng = c(-1, 1)),
  "Soil-Adjusted Veg (SAVI)"         = list(rename = "SAVI",  expr = "1.5 * ((NIR - RED) / (NIR + RED + 0.5))",                 pal = .PAL_VEG,   rng = c(-1, 1)),
  "Modified SAVI (MSAVI2)"           = list(rename = "MSAVI2",expr = "(2 * NIR + 1 - sqrt((2 * NIR + 1) ** 2 - 8 * (NIR - RED))) / 2", pal = .PAL_VEG, rng = c(-1, 1)),
  "Green NDVI (GNDVI)"               = list(rename = "GNDVI", expr = "(NIR - GREEN) / (NIR + GREEN)",                           pal = .PAL_VEG,   rng = c(-1, 1)),
  "Atmospheric-Resistant Veg (ARVI)" = list(rename = "ARVI",  expr = "(NIR - (2 * RED - BLUE)) / (NIR + (2 * RED - BLUE))",     pal = .PAL_VEG,   rng = c(-1, 1)),
  "Visible ARVI (VARI)"              = list(rename = "VARI",  expr = "(GREEN - RED) / (GREEN + RED - BLUE)",                    pal = .PAL_VEG,   rng = c(-1, 1)),
  "Red-Edge NDVI (NDRE)"             = list(rename = "NDRE",  expr = "(NIR - REDEDGE) / (NIR + REDEDGE)",                       pal = .PAL_VEG,   rng = c(-1, 1), s2_only = TRUE),

  "Modified NDWI (MNDWI)"            = list(rename = "MNDWI", expr = "(GREEN - SWIR1) / (GREEN + SWIR1)",                       pal = .PAL_WATER, rng = c(-1, 1)),
  # Water-quality indices (turbidity & chlorophyll). NDTI needs no red-edge; Chl-a (NDCI)
  # uses the red-edge band, so it is Sentinel-2 only (gated below like NDRE).
  "Turbidity (NDTI)"                = list(rename = "NDTI",  expr = "(RED - GREEN) / (RED + GREEN)",                           pal = c("#08306b", "#4292c6", "#d9c8a5", "#8c6d31"), rng = c(-1, 1)),
  "Chlorophyll-a (NDCI)"            = list(rename = "CHLA",  expr = "(REDEDGE - RED) / (REDEDGE + RED)",                       pal = c("#0d3b66", "#3aa17e", "#c7e34a", "#e3b505", "#d7263d"), rng = c(-1, 1), s2_only = TRUE),
  "Moisture Index (NDMI)"           = list(rename = "NDMI",  expr = "(NIR - SWIR1) / (NIR + SWIR1)",                           pal = .PAL_MOIST, rng = c(-1, 1)),
  "Land Surface Water (LSWI)"       = list(rename = "LSWI",  expr = "(NIR - SWIR1) / (NIR + SWIR1)",                           pal = .PAL_MOIST, rng = c(-1, 1)),
  "Auto Water Extraction (AWEI)"    = list(rename = "AWEI",  expr = "4 * (GREEN - SWIR1) - (0.25 * NIR + 2.75 * SWIR2)",       pal = .PAL_WATER, rng = c(-1, 1)),

  "Urban Index (UI)"                = list(rename = "UI",    expr = "(SWIR2 - NIR) / (SWIR2 + NIR)",                           pal = .PAL_BUILT, rng = c(-1, 1)),
  "Bare Soil Index (BSI)"           = list(rename = "BSI",   expr = "((SWIR1 + RED) - (NIR + BLUE)) / ((SWIR1 + RED) + (NIR + BLUE))", pal = .PAL_BUILT, rng = c(-1, 1)),
  "Built-up Index (IBI)"            = list(rename = "IBI",   expr = paste0(
      "( (SWIR1 - NIR)/(SWIR1 + NIR) - ( (1.5*((NIR-RED)/(NIR+RED+0.5))) + ((GREEN-SWIR1)/(GREEN+SWIR1)) )/2 ) / ",
      "( (SWIR1 - NIR)/(SWIR1 + NIR) + ( (1.5*((NIR-RED)/(NIR+RED+0.5))) + ((GREEN-SWIR1)/(GREEN+SWIR1)) )/2 )"),
      pal = .PAL_BUILT, rng = c(-1, 1)),

  "Normalized Burn Ratio (NBR)"     = list(rename = "NBR",   expr = "(NIR - SWIR2) / (NIR + SWIR2)",                           pal = .PAL_BURN,  rng = c(-1, 1)),
  "Burn Area Index (BAI)"           = list(rename = "BAI",   expr = "1.0 / ((0.1 - RED) ** 2 + (0.06 - NIR) ** 2)",           pal = rev(.PAL_BURN), rng = c(0, 100)),
  "Snow Index (NDSI)"               = list(rename = "NDSI",  expr = "(GREEN - SWIR1) / (GREEN + SWIR1)",                       pal = .PAL_SNOW,  rng = c(-1, 1)),

  "Clay Minerals Index"             = list(rename = "CLAY",  expr = "SWIR1 / SWIR2",                                           pal = .PAL_GEO,   rng = c(0.5, 2.5)),
  "Ferrous Minerals Index"          = list(rename = "FERROUS", expr = "SWIR1 / NIR",                                          pal = .PAL_GEO,   rng = c(0.5, 2)),
  "Iron Oxide Index"                = list(rename = "IRONOX",expr = "RED / BLUE",                                             pal = .PAL_GEO,   rng = c(0.5, 3))
)

# ---- DEM / terrain recipes (SRTM 30m) ----
DEM_TERRAIN_FEATURES <- c("Terrain Aspect", "Hillshade", "Topographic Position (TPI)", "Terrain Ruggedness (TRI)")

# -------------------------------------------------------------------------
# Build a standardized reflectance composite with named bands.
# -------------------------------------------------------------------------
build_optical_composite <- function(start_date, end_date, agg, ee_roi) {
  agg_fun <- function(col) switch(agg, "Mean" = col$mean(), "Max" = col$max(), "Min" = col$min(), col$median())

  s2 <- ee$ImageCollection('COPERNICUS/S2_SR_HARMONIZED')$filterBounds(ee_roi)$filterDate(start_date, end_date)$map(mask_s2_clouds)
  if (s2$size()$getInfo() > 0) {
    comp <- agg_fun(s2)
    img <- comp$select(c('B2', 'B3', 'B4', 'B5', 'B8', 'B11', 'B12'))$
      rename(c('BLUE', 'GREEN', 'RED', 'REDEDGE', 'NIR', 'SWIR1', 'SWIR2'))$
      divide(10000)$clip(ee_roi)
    return(list(img = img, sensor = "Sentinel-2", has_rededge = TRUE))
  }

  l_col <- get_harmonized_landsat_collection(start_date, end_date, ee_roi)
  if (is.null(l_col) || l_col$size()$getInfo() == 0) {
    stop("No Sentinel-2 or Landsat imagery found for this area and date range. Try a wider date range.")
  }
  comp <- agg_fun(l_col)
  # Landsat C2 L2 surface reflectance scaling: DN * 0.0000275 - 0.2
  refl <- comp$select(c('SR_B2', 'SR_B3', 'SR_B4', 'SR_B5', 'SR_B6', 'SR_B7'))$
    multiply(0.0000275)$add(-0.2)$
    rename(c('BLUE', 'GREEN', 'RED', 'NIR', 'SWIR1', 'SWIR2'))
  # Landsat has no red-edge band; add a placeholder copy of NIR so expressions that
  # reference REDEDGE don't fail to compile (red-edge indices are gated as s2_only upstream).
  img <- refl$addBands(refl$select('NIR')$rename('REDEDGE'))$clip(ee_roi)
  list(img = img, sensor = describe_landsat_sensors(start_date, end_date), has_rededge = FALSE)
}

.index_band_map <- function(comp) {
  list(
    BLUE = comp$select('BLUE'), GREEN = comp$select('GREEN'), RED = comp$select('RED'),
    REDEDGE = comp$select('REDEDGE'), NIR = comp$select('NIR'),
    SWIR1 = comp$select('SWIR1'), SWIR2 = comp$select('SWIR2')
  )
}

# -------------------------------------------------------------------------
# build_index_image(): returns the get_feature_img() contract, or NULL if the
# feature isn't one of the rgee-native analyses.
# -------------------------------------------------------------------------
build_index_image <- function(f_name, start_date, end_date, agg, ee_roi, dyn_scale) {
  # ---- terrain / DEM ----
  if (f_name %in% DEM_TERRAIN_FEATURES) {
    dem <- ee$Image('USGS/SRTMGL1_003')
    if (f_name == "Terrain Aspect") {
      img <- ee$Terrain$aspect(dem)$rename('Aspect')$clip(ee_roi)
      return(list(img = img, pal = c('#ff0000', '#ffff00', '#00ff00', '#00ffff', '#0000ff', '#ff00ff', '#ff0000'), min = 0, max = 360, sensor = "SRTM GL1"))
    } else if (f_name == "Hillshade") {
      img <- ee$Terrain$hillshade(dem)$rename('Hillshade')$clip(ee_roi)
      return(list(img = img, pal = .PAL_GREY, min = 0, max = 255, sensor = "SRTM GL1"))
    } else if (f_name == "Topographic Position (TPI)") {
      # TPI = elevation - mean(elevation in a neighbourhood)
      neigh <- dem$focal_mean(radius = 5, units = 'pixels', kernelType = 'square')
      img <- dem$subtract(neigh)$rename('TPI')$clip(ee_roi)
      return(list(img = img, pal = .PAL_TERR, min = -30, max = 30, sensor = "SRTM GL1"))
    } else if (f_name == "Terrain Ruggedness (TRI)") {
      # TRI proxy: local standard deviation of elevation (3x3 neighbourhood)
      img <- dem$reduceNeighborhood(reducer = ee$Reducer$stdDev(), kernel = ee$Kernel$square(1))$rename('TRI')$clip(ee_roi)
      return(list(img = img, pal = .PAL_TERR, min = 0, max = 50, sensor = "SRTM GL1"))
    }
  }

  # ---- spectral indices ----
  recipe <- SPECTRAL_INDEX_RECIPES[[f_name]]
  if (is.null(recipe)) return(NULL)

  comp <- build_optical_composite(start_date, end_date, agg, ee_roi)
  if (isTRUE(recipe$s2_only) && !isTRUE(comp$has_rededge)) {
    stop(paste0(f_name, " needs the red-edge band, which only Sentinel-2 provides (from 2015 onward). ",
                "Pick a date range in 2015 or later for this index."))
  }
  img <- comp$img$expression(recipe$expr, .index_band_map(comp$img))$rename(recipe$rename)$clip(ee_roi)
  list(img = img, pal = recipe$pal, min = recipe$rng[1], max = recipe$rng[2], sensor = comp$sensor)
}

# -------------------------------------------------------------------------
# compute_rgee_analytics(): the local-engine equivalent of a compute-service
# "run-analytics" response. Returns list(tile_url, palette, min, max, sensor,
# stats, histogram) — the exact shape render_result() already consumes.
# All EE calls are synchronous getInfo() (same as the temporal grid).
# -------------------------------------------------------------------------
compute_rgee_analytics <- function(f_name, start_date, end_date, agg, ee_roi, dyn_scale) {
  fd <- get_feature_img(f_name, start_date, end_date, agg, ee_roi, dyn_scale)
  if (is.null(fd) || is.null(fd$img)) stop("This analysis is not available on the local GEE engine.")
  img <- fd$img
  band <- tryCatch(img$bandNames()$getInfo()[[1]], error = function(e) NULL)
  if (is.null(band)) stop("Could not resolve the computed image band.")

  # --- zonal statistics (one combined reduceRegion) ---
  reducer <- ee$Reducer$mean()$
    combine(ee$Reducer$median(), sharedInputs = TRUE)$
    combine(ee$Reducer$minMax(), sharedInputs = TRUE)$
    combine(ee$Reducer$stdDev(), sharedInputs = TRUE)$
    combine(ee$Reducer$sum(), sharedInputs = TRUE)$
    combine(ee$Reducer$count(), sharedInputs = TRUE)
  r <- img$reduceRegion(reducer = reducer, geometry = ee_roi, scale = dyn_scale,
                        maxPixels = 1e13, bestEffort = TRUE, tileScale = 16)$getInfo()
  gv <- function(suffix) {
    v <- r[[paste0(band, "_", suffix)]]
    if (is.null(v)) NA_real_ else as.numeric(v)
  }
  stats <- list(mean = gv("mean"), median = gv("median"), minimum = gv("min"),
                maximum = gv("max"), std_dev = gv("stdDev"), sum = gv("sum"),
                valid_pixels = gv("count"))

  # --- histogram (best-effort; a failure here must not break the analysis) ---
  histogram <- tryCatch({
    hmin <- if (!is.na(stats$minimum)) stats$minimum else fd$min
    hmax <- if (!is.na(stats$maximum)) stats$maximum else fd$max
    if (is.na(hmin) || is.na(hmax) || hmax <= hmin) NULL else {
      fh <- img$reduceRegion(reducer = ee$Reducer$fixedHistogram(hmin, hmax, 30L),
                             geometry = ee_roi, scale = dyn_scale, maxPixels = 1e13,
                             bestEffort = TRUE, tileScale = 16)$getInfo()
      arr <- fh[[band]]
      if (is.null(arr) || length(arr) == 0) NULL else {
        bins   <- lapply(arr, function(pair) pair[[1]])
        counts <- lapply(arr, function(pair) pair[[2]])
        list(bins = bins, counts = counts)
      }
    }
  }, error = function(e) NULL)

  # --- map tile (proven getMapId pattern, as used by mod_lulc.R) ---
  map_info <- img$getMapId(list(min = fd$min, max = fd$max, palette = fd$pal))
  tile_url <- map_info$tile_fetcher$url_format

  list(tile_url = tile_url, palette = as.list(fd$pal), min = fd$min, max = fd$max,
       sensor = fd$sensor, stats = stats, histogram = histogram)
}
