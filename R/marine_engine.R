# =========================================================================
# marine_engine.R  —  BENTHIC / COASTAL rgee BACKEND
#
# Adds the shallow-water pre-processing chain the terrestrial engine lacks:
#   * Sentinel-2 SR Harmonized collection + SCL cloud mask
#   * NDWI land/water mask (strip ocean for terrestrial de-pollution, OR
#     isolate water for benthic mapping)
#   * Sun-glint correction (Hedley et al. 2005) — image-derived slope from the
#     NIR band, no manual glint polygon needed
#   * Depth-Invariant Index (Lyzenga 1978/1981) — ln(Blue),ln(Green) with the
#     attenuation ratio estimated from the water-pixel covariance
#   * A benthic band stack ready for the existing Random-Forest / CART classifier
#
# These are plain function DEFINITIONS: nothing here calls Earth Engine at
# source time, so the file boots inertly (the app + CI are unaffected). The
# functions only touch EE when a benthic analysis is actually run.
#
# Sentinel-2 SR band convention used here: B2=Blue, B3=Green, B4=Red, B8=NIR
# (10 m). Reflectance is scaled to 0–1 (raw SR / 10000).
# =========================================================================

# ---- 1. Sentinel-2 SR collection, SCL-cloud-masked, scaled to reflectance ----
get_s2_sr_collection <- function(start_date, end_date, ee_roi, max_cloud = 20) {
  mask_s2 <- function(img) {
    scl  <- img$select("SCL")
    # Drop: 0 no-data, 1 saturated, 3 cloud-shadow, 8/9 med/high cloud, 10 cirrus, 11 snow
    good <- scl$neq(0)$And(scl$neq(1))$And(scl$neq(3))$And(scl$neq(8))$
                And(scl$neq(9))$And(scl$neq(10))$And(scl$neq(11))
    img$updateMask(good)$divide(10000)$copyProperties(img, list("system:time_start"))
  }
  ee$ImageCollection("COPERNICUS/S2_SR_HARMONIZED")$
    filterBounds(ee_roi)$
    filterDate(start_date, end_date)$
    filter(ee$Filter$lt("CLOUDY_PIXEL_PERCENTAGE", max_cloud))$
    map(rgee::ee_utils_pyfunc(mask_s2))
}

# ---- 2. NDWI land/water mask ----
# keep = "water": retain water pixels (NDWI > thresh) — for benthic mapping.
# keep = "land" : retain land  pixels (NDWI <= thresh) — strips ocean so a
#                 terrestrial index/LULC isn't polluted by surrounding sea.
apply_water_mask <- function(img, keep = c("water", "land"), ndwi_thresh = 0.0,
                             green_band = "B3", nir_band = "B8") {
  keep <- match.arg(keep)
  ndwi <- img$normalizedDifference(c(green_band, nir_band))$rename("NDWI")
  mask <- if (identical(keep, "water")) ndwi$gt(ndwi_thresh) else ndwi$lte(ndwi_thresh)
  img$updateMask(mask)
}

# ---- 3. Sun-glint correction (Hedley et al. 2005), image-derived ----
# For each visible band Vi:  Vi' = Vi - b_i * (NIR - min_NIR)
# where b_i = slope of the Vi-on-NIR regression over the (masked) region, and
# min_NIR is the darkest NIR value there. Removes the additive surface-reflection
# component so what's left is (mostly) light that reached the seabed.
deglint_hedley <- function(img, ee_roi, vis_bands = c("B2", "B3", "B4"),
                           nir_band = "B8", scale = 10) {
  nir     <- img$select(nir_band)
  min_nir <- ee$Number(nir$reduceRegion(
    reducer = ee$Reducer$min(), geometry = ee_roi, scale = scale,
    maxPixels = 1e13, bestEffort = TRUE)$get(nir_band))
  out <- img
  for (b in vis_bands) {
    # linearFit on a 2-band image [x = NIR, y = Vi] -> $get('scale') is the slope b_i.
    fit   <- img$select(list(nir_band, b))$reduceRegion(
      reducer = ee$Reducer$linearFit(), geometry = ee_roi, scale = scale,
      maxPixels = 1e13, bestEffort = TRUE)
    slope <- ee$Number(fit$get("scale"))
    corr  <- img$select(b)$subtract(
      nir$subtract(ee$Image$constant(min_nir))$multiply(ee$Image$constant(slope))
    )$rename(b)
    out <- out$addBands(corr, NULL, TRUE)  # overwrite the raw band with the deglinted one
  }
  out
}

# ---- 4. Depth-Invariant Index (Lyzenga), one band pair ----
# Xi = ln(Ri), Xj = ln(Rj). Attenuation ratio ki/kj = a + sqrt(a^2 + 1),
# a = (var_i - var_j) / (2 * cov_ij), estimated from the water-pixel covariance.
# DII = Xi - (ki/kj) * Xj  →  a value that depends on bottom type, not depth.
depth_invariant_index <- function(img, ee_roi, band_i = "B2", band_j = "B3", scale = 10) {
  xi <- img$select(band_i)$log()$rename("Xi")
  xj <- img$select(band_j)$log()$rename("Xj")
  arr <- xi$addBands(xj)$toArray()$reduceRegion(
    reducer = ee$Reducer$covariance(), geometry = ee_roi, scale = scale,
    maxPixels = 1e13, bestEffort = TRUE)$get("array")
  covm  <- ee$Array(arr)
  var_i <- ee$Number(covm$get(list(0L, 0L)))
  var_j <- ee$Number(covm$get(list(1L, 1L)))
  covij <- ee$Number(covm$get(list(0L, 1L)))
  a     <- var_i$subtract(var_j)$divide(covij$multiply(2))
  ratio <- a$add(a$pow(2)$add(1)$sqrt())               # ki/kj
  xi$subtract(xj$multiply(ee$Image$constant(ratio)))$rename(paste0("DII_", band_i, "_", band_j))
}

# ---- 5. Full benthic stack, ready for the classifier ----
# Returns a multi-band image (deglinted visible + DII bands) over WATER only,
# plus the band-name vector to hand to img$select(bands)$classify(rf).
# Classes to train against these bands: "Live Coral", "Sand/Rubble", "Deep Water".
build_benthic_stack <- function(start_date, end_date, ee_roi, max_cloud = 20, agg = "Median") {
  col <- get_s2_sr_collection(start_date, end_date, ee_roi, max_cloud)
  img <- switch(agg, "Mean" = col$mean(), "Max" = col$max(), "Min" = col$min(), col$median())$clip(ee_roi)

  water     <- apply_water_mask(img, keep = "water")
  deglinted <- deglint_hedley(water, ee_roi = ee_roi, vis_bands = c("B2", "B3", "B4"), nir_band = "B8")
  dii_bg    <- depth_invariant_index(deglinted, ee_roi, "B2", "B3")   # Blue–Green (classic bottom index)
  dii_br    <- depth_invariant_index(deglinted, ee_roi, "B2", "B4")   # Blue–Red (extra separability)

  stack <- deglinted$select(c("B2", "B3", "B4"))$addBands(dii_bg)$addBands(dii_br)
  list(img = stack, band_names = c("B2", "B3", "B4", "DII_B2_B3", "DII_B2_B4"))
}

# ---- 6. Per-class AREA (hectares) for any classified image ----
# Wraps ee.Image.pixelArea() grouped by class value into a clean data.frame:
#   Class_ID, Area_m2, Area_Hectares. Feeds straight into the correlation study
#   (Built-up ha vs Live-Coral ha). class_band = the integer class band.
classified_area_hectares <- function(classified_img, ee_roi, class_band = NULL, scale = 10) {
  cb <- if (is.null(class_band)) classified_img$bandNames()$get(0) else class_band
  area_img <- ee$Image$pixelArea()$addBands(classified_img$select(list(cb))$rename("class"))
  groups <- area_img$reduceRegion(
    reducer  = ee$Reducer$sum()$group(groupField = 1L, groupName = "class"),
    geometry = ee_roi, scale = scale, maxPixels = 1e13, bestEffort = TRUE, tileScale = 16
  )$getInfo()$groups
  if (is.null(groups) || length(groups) == 0) return(NULL)
  do.call(rbind, lapply(groups, function(g) {
    m2 <- as.numeric(g$sum %||% NA)
    data.frame(Class_ID = as.integer(g$class), Area_m2 = round(m2, 1),
               Area_Hectares = round(m2 / 1e4, 3), stringsAsFactors = FALSE)
  }))
}

# ---- 7. Coral / benthic-composition layer (DEEP WATER EXCLUDED) ----
# The reliable coral step. It:
#   1. Builds the deglinted, water-masked benthic stack.
#   2. Keeps only OPTICALLY SHALLOW water — pixels where light actually reached
#      the seabed and came back. Deep water has a near-zero deglinted green
#      signal, so a small threshold on B3 removes the deep surrounding ocean.
#   3. Returns the Lyzenga depth-invariant bottom index (variation = bottom TYPE,
#      not depth) together with a DATA-DRIVEN 2/98 percentile stretch, so the map
#      shows real spatial contrast instead of clamping to a single colour.
# Returns list(img = DII over shallow reef, mask = shallow-reef mask,
#              min, max = stretch bounds). NOTE: this is a reflectance-based
# benthic index, a screening tool — class identities need field validation.
coral_health_layer <- function(start_date, end_date, ee_roi, upstream_mask = NULL,
                               shallow_thresh = 0.015, scale = 10, max_cloud = 20) {
  bs    <- build_benthic_stack(start_date, end_date, ee_roi, max_cloud = max_cloud, agg = "Median")
  green <- bs$img$select("B3")
  shallow <- green$gt(shallow_thresh)                      # deep clear water -> ~0
  reef  <- if (!is.null(upstream_mask)) shallow$And(upstream_mask) else shallow
  dii   <- bs$img$select("DII_B2_B3")$updateMask(reef)
  pct   <- tryCatch(dii$reduceRegion(reducer = ee$Reducer$percentile(list(2, 98)),
             geometry = ee_roi, scale = max(10, scale), maxPixels = 1e12,
             bestEffort = TRUE, tileScale = 16)$getInfo(), error = function(e) NULL)
  vals  <- tryCatch(sort(as.numeric(unlist(pct))), error = function(e) numeric(0))
  if (length(vals) >= 2 && is.finite(vals[1]) && is.finite(vals[length(vals)]) && vals[1] < vals[length(vals)]) {
    mn <- vals[1]; mx <- vals[length(vals)]
  } else { mn <- -1.5; mx <- 0.5 }                          # safe fallback
  list(img = dii, mask = reef, min = mn, max = mx)
}
