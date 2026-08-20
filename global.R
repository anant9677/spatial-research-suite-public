# =========================================================================
# GLOBAL.R - SPATIAL RESEARCH SUITE (ORIGINAL AUTH & DOCKER SAFE)
# =========================================================================

# --- 1. DYNAMIC PYTHON ENVIRONMENT SETUP ---
if (Sys.getenv("IS_DOCKER") == "TRUE") {
  py_path <- "/opt/venv/bin/python"
  Sys.setenv(RETICULATE_PYTHON = py_path)
  key_path <- "/app/data/gee_key.json"
} else {
  py_path <- "/Users/anant/.virtualenvs/rgee/bin/python"
  Sys.setenv(RETICULATE_PYTHON = py_path)
  # 🔑 Portable service-account key resolution. The old hard-coded LULC_Docker
  # path breaks once the repo moves (→ "run earthengine authenticate"). Prefer
  # the app-relative data/ dir (works when launched from apps/test or
  # apps/personal, whose data/ is symlinked to test/data), then fall back to
  # known locations, and only last of all the legacy absolute path.
  key_path <- local({
    cands <- c("data/gee_key.json",
               "apps/test/data/gee_key.json",
               "/Users/anant/Documents/gisforus-platform/apps/test/data/gee_key.json",
               "/Users/anant/Documents/R/LULC_Docker/data/gee_key.json")
    hit <- cands[file.exists(cands)]
    if (length(hit)) hit[1] else cands[1]
  })
}

library(shiny)
library(shinyjs)
library(terra)
library(sf)
library(ranger)
library(ggplot2)
library(ggspatial) 
library(dplyr)
library(tidyr)
library(leaflet)
library(leaflet.extras) 
library(htmlwidgets) 
library(DT)
library(zip)          
library(httr)      
library(rgee)
library(reticulate)
library(bslib)
library(geojsonio)
library(jsonlite) 
library(shinycssloaders)
library(rnaturalearth)
library(httr2)
library(writexl)
library(promises)
library(future)
library(jose)
library(openssl)

# 🚀 EXPANDED CATALOGUE ENGINE: R/gee_indices.R defines the rgee spectral/terrain index recipes,
# build_index_image() and compute_rgee_analytics(). It lives under R/ so it's unit-tested and
# autoloaded for the MODULE servers — but get_feature_img() below runs in the environment where
# global.R is sourced, and Shiny autoloads R/ into a SEPARATE "support" environment that this file
# cannot see. So we ALSO source it here (local = TRUE => into global.R's own environment) so
# get_feature_img() can find build_index_image(). No source-time side effects, so the double-load
# under tests/autoload is harmless.
if (file.exists("R/gee_indices.R")) source("R/gee_indices.R", local = TRUE)
# 🐠 BENTHIC/COASTAL ENGINE: R/marine_engine.R adds Sentinel-2 shallow-water
# pre-processing (NDWI land/water mask, Hedley sun-glint correction, Lyzenga
# Depth-Invariant Index) + a classifier-ready benthic band stack and per-class
# hectare extraction. Function definitions only — inert at source time, so this
# never affects app boot or CI; the EE calls fire only when a benthic run happens.
if (file.exists("R/marine_engine.R")) source("R/marine_engine.R", local = TRUE)

# 🚀 Used ONLY for pure-R/sf work (e.g. Shapefile Extractor's boundary loading) — safe here
# because no Python/reticulate object ever needs to cross the process boundary, unlike the
# earlier in-process GEE async attempt which hit unrecoverable cross-process Python errors.
# GEE-heavy work instead goes through the separate GEE Compute Service (see GEE_COMPUTE_API_URL).
plan(multisession, workers = 2)

# 🚀 GEE COMPUTE SERVICE INTEGRATION: the "Run Analytics" button now calls this standalone
# microservice (see /plumber_service in the repo) instead of running GEE computation in-process.
# This is what lets it scale horizontally on Cloud Run independent of this Shiny app — the
# earlier in-process future/reticulate attempt hit unrecoverable cross-process Python object
# errors; an HTTP call has no such problem, since no Python object ever crosses a process boundary.
# Configurable via env var so the same code works against a local test server or the deployed one.
GEE_COMPUTE_API_URL <- Sys.getenv("GEE_COMPUTE_API_URL", "https://gee-compute-service-173824452148.us-central1.run.app")

# 🚀 SECURITY FIX: gee-compute-service no longer allows unauthenticated access — anyone who
# guessed its URL could otherwise call it directly, bypassing this app's rate limits entirely
# and running up GEE usage/cost with no restriction. This app now proves its own identity via a
# Google-issued identity token, fetched from Cloud Run's local metadata server (fast — no real
# network round-trip) and cached for ~50 minutes (tokens are valid for about an hour).
# Locally (no metadata server available, e.g. RStudio dev), this fails harmlessly and simply
# sends no auth header — fine since local testing points GEE_COMPUTE_API_URL at an unauthenticated
# local Plumber server anyway.
.identity_token_cache <- new.env()

# 🚀 LOCAL DEV: mint an audience-scoped Google identity token from a service-account key, so the
# app can authenticate to the DEPLOYED gee-compute-service from a LOCAL machine — where Cloud Run's
# metadata server (the normal token source) doesn't exist, which is exactly why local calls get a
# 403. Standard SA JWT-bearer flow: sign a JWT carrying `target_audience`, exchange it at Google's
# token endpoint for an id_token. The SA used here needs roles/run.invoker on the compute service.
mint_identity_token_from_sa <- function(sa_path, audience) {
  sa <- jsonlite::fromJSON(sa_path)
  now <- as.numeric(Sys.time())
  claim <- jose::jwt_claim(
    iss = sa$client_email, sub = sa$client_email,
    aud = "https://oauth2.googleapis.com/token",
    iat = now, exp = now + 3600, target_audience = audience
  )
  signed_jwt <- jose::jwt_encode_sig(claim, openssl::read_key(sa$private_key))
  resp <- httr2::request("https://oauth2.googleapis.com/token") |>
    httr2::req_body_form(grant_type = "urn:ietf:params:oauth:grant-type:jwt-bearer", assertion = signed_jwt) |>
    httr2::req_perform()
  httr2::resp_body_json(resp)$id_token
}

get_gee_service_identity_token <- function() {
  cached <- .identity_token_cache$token
  cached_time <- .identity_token_cache$time
  if (!is.null(cached) && !is.null(cached_time) && as.numeric(difftime(Sys.time(), cached_time, units = "mins")) < 50) {
    return(cached)
  }
  
  token <- tryCatch({
    httr2::request("http://metadata.google.internal/computeMetadata/v1/instance/service-accounts/default/identity") |>
      httr2::req_url_query(audience = GEE_COMPUTE_API_URL) |>
      httr2::req_headers(`Metadata-Flavor` = "Google") |>
      httr2::req_timeout(5) |>
      httr2::req_perform() |>
      httr2::resp_body_string()
  }, error = function(e) NULL)
  
  # LOCAL DEV FALLBACK: off Cloud Run there's no metadata server, so mint an audience-scoped token
  # from a service-account key instead (set GEE_COMPUTE_SA_KEY in .Renviron to that key's path).
  # Lets `apps/test` call the DEPLOYED compute service locally without a 403. On Cloud Run this
  # never runs, because the metadata fetch above already succeeded.
  if (is.null(token)) {
    sa_key <- Sys.getenv("GEE_COMPUTE_SA_KEY", "")
    if (nzchar(sa_key) && file.exists(sa_key)) {
      token <- tryCatch(mint_identity_token_from_sa(sa_key, GEE_COMPUTE_API_URL), error = function(e) NULL)
    }
  }

  if (!is.null(token)) {
    .identity_token_cache$token <- token
    .identity_token_cache$time <- Sys.time()
  }
  token
}

# Force Reticulate to lock onto the correct environment without crashing
try(reticulate::use_python(py_path, required = TRUE), silent = TRUE)
Sys.setenv(GOOGLE_APPLICATION_CREDENTIALS = key_path)

# --- 2. GEE INITIALIZATION (now a reusable function — needed again inside each async worker) ---
# 🚀 LONG-TERM ARCHITECTURE FIX: saved_images used to cache LIVE EE image objects (Python
# references) for reuse across modules (e.g. Publication Map). This works fine synchronously, but
# breaks the moment any computation moves to a background async process (a future different R/Python
# session) — the cached object becomes invalid the instant that process ends. Instead, we now cache
# a lightweight "recipe" (feature name, dates, scale, ROI as WKT — all plain, transferable R data) and
# recompute the actual EE image on demand via this resolver. This is what makes the app safely
# convertible to async processing without silently breaking cross-module image reuse.
resolve_saved_image <- function(recipe) {
  roi_sf <- sf::st_as_sfc(recipe$roi_wkt, crs = 4326)
  ee_roi <- sf_as_ee(roi_sf)
  
  if (recipe$type == "simple") {
    f_data <- get_feature_img(recipe$feature, recipe$start_d, recipe$end_d, recipe$agg, ee_roi, recipe$scale)
    if (is.null(f_data) || is.null(f_data$img)) stop("Could not recompute this saved image — the underlying data may no longer be available for this date range.")
    return(f_data$img)
    
  } else if (recipe$type == "trend") {
    # 🚀 FIX (trend "not enough year data"): coerce to integers so seq() yields the full year
    # sequence (a stored character/factor year would make seq() error or degenerate to length 1).
    years <- seq(as.integer(round(recipe$start_year)), as.integer(round(recipe$end_year)))
    rec_month <- as.integer(round(recipe$month))
    img_list <- list()
    for (yr in years) {
      start_d <- as.Date(sprintf("%d-%02d-01", as.integer(yr), rec_month))
      end_d <- seq(start_d, by = "1 month", length.out = 2)[2]
      f_data <- tryCatch(get_feature_img(recipe$feature, as.character(start_d), as.character(end_d), recipe$agg, ee_roi, recipe$scale), error = function(e) NULL)
      if (!is.null(f_data) && !is.null(f_data$img)) {
        band_name <- f_data$img$bandNames()$get(0)
        tagged <- f_data$img$select(list(band_name))$rename('value')$toFloat()$addBands(ee$Image$constant(yr)$rename('year')$toFloat())
        img_list[[length(img_list) + 1]] <- tagged
      }
    }
    if (length(img_list) < 2) stop("Not enough valid years found to recompute this trend.")
    
    coll <- ee$ImageCollection$fromImages(img_list)
    trend_img <- coll$select(list('year', 'value'))$reduce(ee$Reducer$linearFit())
    slope_img <- trend_img$select('scale')$clip(ee_roi)
    
    if (isTRUE(recipe$sig_only)) {
      n_years <- length(img_list)
      kendall_img <- coll$select(list('year', 'value'))$reduce(ee$Reducer$kendallsCorrelation())
      tau_band <- kendall_img$bandNames()$get(0)
      tau_img <- kendall_img$select(list(tau_band))
      s_stat <- tau_img$multiply(n_years * (n_years - 1) / 2)
      var_s <- n_years * (n_years - 1) * (2 * n_years + 5) / 18
      z_img <- s_stat$divide(sqrt(var_s))
      sig_mask <- z_img$abs()$gte(1.96)$clip(ee_roi)
      slope_img <- slope_img$updateMask(sig_mask)
    }
    return(slope_img)
  }
  
  stop("Unknown saved image recipe type: ", recipe$type)
}

initialize_gee_session <- function(quiet = TRUE) {
  tryCatch({
    if (!exists("ee", envir = .GlobalEnv)) assign("ee", reticulate::import("ee"), envir = .GlobalEnv)
    ee <- get("ee", envir = .GlobalEnv)
    
    # 🔐 RESILIENT AUTH: try the service-account key first, but if it is
    # missing OR unusable (e.g. the SA was deleted → invalid_grant), fall back to
    # local ADC (`gcloud auth application-default login`) instead of hard-failing.
    sa_ok <- FALSE
    if (file.exists(key_path)) {
      sa_ok <- tryCatch({
        google_auth <- reticulate::import("google.oauth2.service_account")
        gee_scopes <- c("https://www.googleapis.com/auth/earthengine", "https://www.googleapis.com/auth/cloud-platform")
        creds <- google_auth$Credentials$from_service_account_file(key_path)$with_scopes(gee_scopes)
        ee$Initialize(credentials = creds, project = "ee-anant4infinityy")
        message("✅ GEE Authenticated via Service Account JSON")
        TRUE
      }, error = function(e) {
        message("⚠️ Service-account key found but unusable (", conditionMessage(e), "). Falling back to ADC.")
        FALSE
      })
    }
    if (!sa_ok) {
      if (Sys.getenv("IS_DOCKER") == "TRUE") {
        stop("CRITICAL ERROR: no usable GEE credentials in Docker (key at ", key_path, " missing or invalid)")
      } else {
        # ⚠️ The unconditional Sys.setenv(GOOGLE_APPLICATION_CREDENTIALS = key_path) near the
        # top of this file points google-auth at a missing/dead key file, which BLOCKS gcloud
        # ADC discovery (ee$Initialize then asks you to "run earthengine authenticate"). Clear
        # it in the embedded Python so EE uses ~/.config/gcloud ADC instead.
        Sys.unsetenv("GOOGLE_APPLICATION_CREDENTIALS")
        try(reticulate::py_run_string("import os; os.environ.pop('GOOGLE_APPLICATION_CREDENTIALS', None)"), silent = TRUE)
        ee$Initialize(project = "ee-anant4infinityy")
        message("✅ GEE Authenticated via Local ADC Token")
      }
    }
    TRUE
  }, error = function(e) { 
    message("❌ GEE Python/Init Error: ", as.character(e)) 
    if (!quiet) stop(e) # 🚀 DIAGNOSTIC FIX: async workers need the REAL error to surface (not silently swallowed as FALSE), so we can see what actually broke instead of a confusing generic downstream error.
    FALSE
  })
}

# 🚀 TEST-SAFETY: testthat automatically sets the TESTTHAT=true env var during test runs (this is
# a standard testthat convention, not something we set ourselves) — skipping GEE session init and
# the live network fetch below during tests means global.R's pure/logic functions can be unit-
# tested without needing GEE credentials, network access, or multi-second startup latency. This
# has ZERO effect on normal app startup (Docker/production never sets TESTTHAT) — the app behaves
# identically to before outside of a test run.
if (Sys.getenv("TESTTHAT") != "true") {
  initialize_gee_session()
}

# NOTE: async processing (future/promises) was tried and reverted for stability — see
# conversation history / commit notes. If revisiting this, look at a separate microservice
# architecture instead of in-process future+reticulate, which had persistent cross-process issues.

# --- 3. GLOBAL SYSTEM OPTIONS ---
Sys.setenv(GDAL_HTTP_TIMEOUT="120", GDAL_HTTP_MAX_RETRIES="3", VSI_CACHE="TRUE", VSI_CACHE_SIZE="100000000", CPL_CURL_IGNORE_ERROR="YES")
options(shiny.maxRequestSize = 1000 * 1024^2) 

# Prevent RAM overflow in Cloud Run (Crucial for Docker stability)
terraOptions(memfrac = 0.5, tempdir = tempdir()) 

# --- 3b. LIVE USD -> INR EXCHANGE RATE (fetched once at app startup, with safe fallback) ---
USD_TO_INR <- if (Sys.getenv("TESTTHAT") == "true") {
  83.5 # test-safety: skip the live network fetch during tests, use the same fallback value
} else {
  tryCatch({
    res <- httr::GET("https://api.exchangerate-api.com/v4/latest/USD", httr::timeout(5))
    rate <- httr::content(res, "parsed")$rates$INR
    if (is.null(rate) || !is.numeric(rate) || rate <= 0) stop("Invalid rate received")
    message("✅ Live USD->INR rate loaded: ", rate)
    as.numeric(rate)
  }, error = function(e) {
    message("⚠️ Could not fetch live FX rate, using fallback: ", as.character(e))
    83.5 # fallback rate — update this occasionally if it drifts far from market rate
  })
}

# Formats a USD amount as "₹X.XX ($Y.YY)" for display in the UI
format_price_display <- function(usd_amount) {
  inr_amount <- usd_amount * USD_TO_INR
  sprintf("₹%.2f ($%.2f)", inr_amount, usd_amount)
}

# --- 4. HELPER FUNCTIONS ---
# Null-coalescing helper: returns b if a is NULL/NA, otherwise a
`%||%` <- function(a, b) if (is.null(a) || (length(a) == 1 && is.na(a))) b else a

info_tooltip <- function(html_content) { tags$span(class = "info-icon", "\u24d8", tags$div(class = "tooltip-text", HTML(html_content))) }

inject_map_elements <- function(map_obj, map_title) {
  map_obj %>%
    addScaleBar(position = "bottomleft", options = scaleBarOptions(metric = TRUE, imperial = FALSE)) %>%
    addControl(html = sprintf("<div class='map-title'>%s</div>", map_title), position = "topleft", className = "") %>%
    addControl(html = "<svg class='north-arrow' viewBox='0 0 100 100' xmlns='http://www.w3.org/2000/svg'><circle cx='50' cy='50' r='46' fill='rgba(255,255,255,0.9)' stroke='#ccc' stroke-width='2'/><polygon points='50,12 62,52 50,44 38,52' fill='#26333e'/><polygon points='50,88 62,52 50,60 38,52' fill='#d8d4c8'/><text x='50' y='30' font-size='16' font-weight='bold' text-anchor='middle' fill='#26333e' font-family='sans-serif'>N</text></svg>", position = "bottomright", className = "")
}

# 🚀 SECURITY/COST FIX: caps the maximum boundary area any single analysis can run on.
# Without this, a user (malicious or accidental) could select an entire continent and trigger
# a massive, slow, expensive GEE computation that could stall the shared container for everyone
# and blow through GEE quota. Returns TRUE if within limits, otherwise shows an error and returns FALSE.
MAX_ROI_AREA_SQKM <- 2000000  # ~ India's total land area; generous for legitimate research use

# 🚀 SECURITY/COST FIX: simple per-session cooldown for heavy GEE operations.
# Without this, a user (or a stray double-click) can fire the same expensive analysis repeatedly
# in quick succession, burning GEE quota and Cloud Run CPU for no benefit. Returns TRUE if the
# action is allowed to proceed, FALSE (with a notification) if it's too soon since the last one.
# 🚀 UX FIX: shows/hides the "processing" banner via a direct JS custom message instead of a
# reactive value. A reactive value set right before a long synchronous/blocking computation does
# NOT actually reach the browser until Shiny's reactive flush cycle runs — which only happens
# AFTER the blocking call finishes, causing the banner to "flash" at the end instead of showing
# throughout. Sending a custom message updates the DOM immediately, before the blocking work starts.
set_busy <- function(session, text) {
  session$sendCustomMessage("showBusyBanner", list(text = text))
}
clear_busy <- function(session) {
  session$sendCustomMessage("hideBusyBanner", list())
}

# 🚀 UX FIX: units lookup so numeric results are self-explanatory (e.g. "35.1" alone means
# nothing; "35.1 °C" does). Used by Batch Processing's results table.
# 🚀 SCIENTIFIC FIX: short inline citation shown directly under each map/chart (not just bundled
# into the end-of-export methodology file) — the standard convention in published figures.
get_feature_citation <- function(feature_name) {
  switch(feature_name,
         "Surface Temperature (LST)" = "USGS Landsat Collection 2 Level 2 Science Products",
         "Urban Heat Island (UHI)" = "USGS Landsat Collection 2 Level 2 Science Products",
         "Vegetation Health (NDVI)" = "Copernicus Sentinel-2 (ESA) / USGS Landsat Collection 2",
         "Water Body Mapping (NDWI)" = "Copernicus Sentinel-2 (ESA) / USGS Landsat Collection 2",
         "Urban Sprawl (NDBI)" = "USGS Landsat Collection 2 Level 2 Science Products",
         "Blue-Green Infrastructure (BGI)" = "Copernicus Sentinel-2 (ESA) / USGS Landsat Collection 2",
         "Precipitation (CHIRPS)" = "Funk et al. (2015), CHIRPS Rainfall Estimates, UCSB Climate Hazards Group",
         "Elevation (DEM)" = "Farr et al. (2007), SRTM Digital Elevation Model v3, USGS/NASA",
         "Terrain Slope" = "Derived from Farr et al. (2007), SRTM Digital Elevation Model v3",
         "Population Density (WorldPop)" = "WorldPop (www.worldpop.org), School of Geography and Environmental Science, University of Southampton",
         "Nighttime Lights (VIIRS)" = "NOAA/NCEI, VIIRS Day/Night Band Nighttime Lights",
         "Air Quality: NO2 (Sentinel-5P)" = "Copernicus Sentinel-5P (ESA), TROPOMI instrument",
         "Air Quality: CO (Sentinel-5P)" = "Copernicus Sentinel-5P (ESA), TROPOMI instrument",
         "Flood Vulnerability (JRC Water)" = "Pekel et al. (2016), JRC Global Surface Water, European Commission",
         "Flood/Water Extent (Sentinel-1 SAR)" = "Copernicus Sentinel-1 (ESA), C-band SAR",
         "Google Earth Engine data catalog" # default
  )
}

# =========================================================================
# SENSOR / DATASET REGISTRY  — study-relevant metadata for every satellite
# sensor the pipeline can draw on. Used to render the "Sensors & datasets"
# table in the pipeline report and the Insights tab, so every run documents
# exactly which sensor produced each result (platform, EE dataset ID, temporal
# coverage, native resolution, bands used, and its role in the study).
# `match` = lowercase substrings looked for in each step's citation string.
# =========================================================================
gf_sensor_registry <- function() {
  list(
    list(sensor = "Sentinel-2 MSI (surface reflectance)",
         dataset = "COPERNICUS/S2_SR_HARMONIZED",
         coverage = "2017-03 → present",
         resolution = "10 m (blue/green/red/NIR); 20 m (red-edge/SWIR)",
         bands = "B2 blue, B3 green, B4 red, B5 red-edge, B8 NIR, B11 SWIR + SCL cloud mask",
         purpose = "Coral Health bottom index, Turbidity (NDTI), Chlorophyll-a (NDCI), water indices",
         match = c("s2_sr", "sentinel-2", "sentinel 2")),
    list(sensor = "Landsat 5/7/8/9 (Collection 2, Level-2 SR)",
         dataset = "LANDSAT/LC09|LC08|LE07|LT05/C02/T1_L2",
         coverage = "1984 → present (multi-mission)",
         resolution = "30 m (optical); 100 m thermal, resampled 30 m",
         bands = "SR blue/green/red/NIR/SWIR1/SWIR2 + ST thermal",
         purpose = "Urban Sprawl (NDBI), Urban Heat / LST, long-record indices",
         match = c("landsat", "lc08", "lc09", "le07", "lt05")),
    list(sensor = "Suomi-NPP VIIRS Day/Night Band",
         dataset = "NOAA/VIIRS/DNB/MONTHLY_V1/VCMSLCFG",
         coverage = "2012-04 → present (monthly)",
         resolution = "~500 m",
         bands = "avg_rad (nighttime radiance)",
         purpose = "Nighttime Lights — human-activity / tourism-pressure proxy",
         match = c("day/night band", "vcmslcfg", "avg_rad")),
    list(sensor = "NASA Black Marble (VIIRS VNP46A2)",
         dataset = "NASA/VIIRS/002/VNP46A2",
         coverage = "2012-01 → present (daily, gap-filled)",
         resolution = "~500 m",
         bands = "Gap_Filled_DNB_BRDF_Corrected_NTL (nW/cm2/sr)",
         purpose = "Nighttime Lights — BRDF/stray-light-corrected tourism/activity proxy",
         match = c("vnp46", "black marble")),
    list(sensor = "NASA SRTM digital elevation",
         dataset = "USGS/SRTMGL1_003",
         coverage = "Static (acquired Feb 2000)",
         resolution = "30 m",
         bands = "elevation",
         purpose = "Land Mask (land/ocean footprint), terrain layers",
         match = c("srtm")),
    list(sensor = "Allen Coral Atlas — benthic habitat",
         dataset = "ACA/reef_habitat/v2_0",
         coverage = "Static ~2018–2020 baseline",
         resolution = "~5 m mapped classes",
         bands = "benthic class (Coral/Algae, Sand, Rubble, Rock, Seagrass, Microalgal mats)",
         purpose = "Coral-habitat mask — restricts marine steps to mapped coral",
         match = c("allen coral", "reef_habitat", "aca/")),
    list(sensor = "NOAA OISST v2.1 — sea-surface temperature",
         dataset = "NOAA/CDR/OISST/V2_1",
         coverage = "1981-09 → present (daily)",
         resolution = "0.25° (~25 km)",
         bands = "sst, anom (SST anomaly vs 1971–2000)",
         purpose = "Marine Heat Stress — coral-bleaching driver (regional)",
         match = c("oisst", "optimum interpolation")),
    list(sensor = "Sentinel-5P TROPOMI",
         dataset = "COPERNICUS/S5P/OFFL/L3_NO2 | L3_CO",
         coverage = "2018-07 → present",
         resolution = "~1.1 km (regridded)",
         bands = "NO2 / CO column density",
         purpose = "Air-quality indicators",
         match = c("sentinel-5p", "tropomi", "s5p")),
    list(sensor = "Sentinel-1 C-band SAR",
         dataset = "COPERNICUS/S1_GRD",
         coverage = "2014-10 → present",
         resolution = "10 m",
         bands = "VV / VH backscatter",
         purpose = "Flood / water extent (cloud-penetrating)",
         match = c("sentinel-1", "s1_grd", "c-band sar")),
    list(sensor = "CHIRPS rainfall",
         dataset = "UCSB-CHG/CHIRPS/DAILY",
         coverage = "1981 → present (daily)",
         resolution = "0.05° (~5.5 km)",
         bands = "precipitation",
         purpose = "Precipitation context",
         match = c("chirps")),
    list(sensor = "WorldPop population",
         dataset = "WorldPop/GP/100m/pop",
         coverage = "2000 → 2020 (annual)",
         resolution = "100 m",
         bands = "population count",
         purpose = "Population-density context",
         match = c("worldpop")),
    list(sensor = "JRC Global Surface Water",
         dataset = "JRC/GSW1_4/GlobalSurfaceWater",
         coverage = "1984 → 2021 baseline",
         resolution = "30 m",
         bands = "water occurrence",
         purpose = "Flood-vulnerability / permanent-water context",
         match = c("global surface water", "jrc/gsw", "pekel"))
  )
}

# Given the citation strings of the steps that actually ran, return a data.frame
# of just the sensors used (registry order), for the report/insights table.
gf_sensors_used_df <- function(citations) {
  citations <- citations[!is.null(citations)]
  if (!length(citations)) return(NULL)
  hay <- tolower(paste(unlist(citations), collapse = "  ||  "))
  reg <- gf_sensor_registry()
  rows <- list()
  for (sdef in reg) {
    if (any(vapply(sdef$match, function(m) grepl(m, hay, fixed = TRUE), logical(1)))) {
      rows[[length(rows) + 1]] <- data.frame(
        `Sensor / platform`         = sdef$sensor,
        `Dataset (Earth Engine ID)` = sdef$dataset,
        `Temporal coverage`         = sdef$coverage,
        `Native resolution`         = sdef$resolution,
        `Bands used`                = sdef$bands,
        `Role in this study`        = sdef$purpose,
        check.names = FALSE, stringsAsFactors = FALSE)
    }
  }
  if (!length(rows)) return(NULL)
  do.call(rbind, rows)
}

# Standard "Interpretation notes" box appended under any Cross-Indicator Correlation
# output (report + Insights). Inline-styled so it renders identically in the self-contained
# HTML report AND inside the Shiny Insights tab (which does not load the report CSS).
gf_correlation_caveats_html <- function() {
  paste0(
    "<div style='margin:12px 0 4px;background:#fbf7ec;border:1px solid #e6d9b8;border-left:4px solid #c99a2e;border-radius:6px;padding:11px 16px;'>",
    "<div style='font-size:11.5px;font-weight:700;text-transform:uppercase;letter-spacing:.07em;color:#9a6f18;margin-bottom:6px;'>Interpretation notes</div>",
    "<ul style='margin:0 0 0 18px;padding:0;font-size:12px;color:#5c5433;line-height:1.55;'>",
    "<li style='margin-bottom:6px;'><b>Same-sensor pairs can be inflated.</b> Two indicators from the <i>same</i> sensor / scenes (e.g. Coral Health and Urban Sprawl, both Landsat) share year-to-year atmospheric &amp; calibration variation, which can inflate their correlation. Cross-sensor pairs (e.g. an optical index vs OISST sea-surface temperature) are more trustworthy.</li>",
    "<li style='margin-bottom:6px;'><b>Shared time trends are spurious.</b> Two series that both drift the same way over the years correlate strongly even with no real link (spurious regression). Where a relationship matters, compare detrended year-to-year deviations, not the raw series.</li>",
    "<li style='margin-bottom:6px;'><b>The bottom index is not &lsquo;live coral&rsquo;.</b> The blue-green bottom index darkens for coral <i>or</i> macroalgae / seagrass, so a rising index under warming may reflect algal overgrowth after coral loss, not recovery &mdash; read it as &lsquo;dark benthic cover&rsquo; and confirm class identity in the field.</li>",
    "<li><b>Short series, low power.</b> With only a few years, wide confidence intervals mean even a sizeable r may be non-significant; treat these as screening associations, not proof of causation.</li>",
    "</ul></div>")
}

# Runtime software versions for the reproducibility table (best-effort; each piece guarded).
gf_runtime_versions <- function() {
  parts <- paste0("R ", as.character(getRversion()))
  rgee_v <- tryCatch(as.character(utils::packageVersion("rgee")), error = function(e) NA_character_)
  if (!is.na(rgee_v)) parts <- c(parts, paste0("rgee ", rgee_v))
  ee_v <- tryCatch(as.character(reticulate::py_get_attr(get("ee", envir = .GlobalEnv), "__version__")), error = function(e) NA_character_)
  if (!is.na(ee_v)) parts <- c(parts, paste0("earthengine-api ", ee_v))
  sha <- tryCatch({ x <- suppressWarnings(system("git rev-parse --short HEAD", intern = TRUE, ignore.stderr = TRUE)); if (length(x) && nzchar(x[[1]])) x[[1]] else NA_character_ }, error = function(e) NA_character_)
  if (!is.na(sha)) parts <- c(parts, paste0("app ", sha))
  paste(parts, collapse = "; ")
}

# Peer-reviewed method references (with DOIs where confidently known) appended to the report's
# reference list, selected by which methods the run actually used. Data-source citations come
# separately from each step; these are the METHOD citations a publishable report needs.
gf_method_references <- function(marine = FALSE, trend = FALSE, correl = FALSE, heat = FALSE, aca = FALSE, land = TRUE) {
  refs <- character(0)
  if (isTRUE(marine)) refs <- c(refs,
    "Hedley, J.D., Harborne, A.R. & Mumby, P.J. (2005). Simple and robust removal of sun glint for mapping shallow-water benthos. International Journal of Remote Sensing, 26(10), 2107-2112.",
    "Lyzenga, D.R. (1981). Remote sensing of bottom reflectance and water attenuation parameters in shallow water. International Journal of Remote Sensing, 2(1), 71-82.")
  if (isTRUE(trend)) refs <- c(refs,
    "Mann, H.B. (1945). Nonparametric tests against trend. Econometrica, 13(3), 245-259. https://doi.org/10.2307/1907187",
    "Sen, P.K. (1968). Estimates of the regression coefficient based on Kendall's tau. Journal of the American Statistical Association, 63(324), 1379-1389. https://doi.org/10.1080/01621459.1968.10480934")
  if (isTRUE(heat)) refs <- c(refs,
    "Reynolds, R.W. et al. (2007). Daily high-resolution-blended analyses for sea surface temperature. Journal of Climate, 20(22), 5473-5496. https://doi.org/10.1175/2007JCLI1824.1")
  if (isTRUE(aca)) refs <- c(refs,
    "Lyons, M.B. et al. (2020). Mapping the world's coral reefs using a global multiscale earth observation framework. Remote Sensing in Ecology and Conservation, 6(4), 557-568. https://doi.org/10.1002/rse2.157")
  if (isTRUE(land)) refs <- c(refs,
    "Farr, T.G. et al. (2007). The Shuttle Radar Topography Mission. Reviews of Geophysics, 45, RG2004. https://doi.org/10.1029/2005RG000183")
  refs
}

# Shared across modules (GEE Cloud Analytics + Statistical Analysis) so the feature list never drifts out of sync.
GEE_FEATURE_CHOICES <- list(
  "Vegetation" = c("Vegetation Health (NDVI)", "Enhanced Vegetation (EVI)", "Soil-Adjusted Veg (SAVI)",
                   "Modified SAVI (MSAVI2)", "Green NDVI (GNDVI)", "Atmospheric-Resistant Veg (ARVI)",
                   "Visible ARVI (VARI)", "Red-Edge NDVI (NDRE)"),
  "Water & Moisture" = c("Water Body Mapping (NDWI)", "Modified NDWI (MNDWI)", "Moisture Index (NDMI)",
                         "Land Surface Water (LSWI)", "Auto Water Extraction (AWEI)",
                         "Turbidity (NDTI)", "Chlorophyll-a (NDCI)"),
  "Urban & Soil" = c("Urban Sprawl (NDBI)", "Urban Index (UI)", "Bare Soil Index (BSI)", "Built-up Index (IBI)"),
  "Fire & Snow" = c("Normalized Burn Ratio (NBR)", "Burn Area Index (BAI)", "Snow Index (NDSI)"),
  "Geology & Minerals" = c("Clay Minerals Index", "Ferrous Minerals Index", "Iron Oxide Index"),
  "Terrain & Topography" = c("Elevation (DEM)", "Terrain Slope", "Terrain Aspect", "Hillshade",
                             "Topographic Position (TPI)", "Terrain Ruggedness (TRI)"),
  "Climate & Environment" = c("Surface Temperature (LST)", "Urban Heat Island (UHI)", "Precipitation (CHIRPS)",
                              "Blue-Green Infrastructure (BGI)", "Air Quality: NO2 (Sentinel-5P)",
                              "Air Quality: CO (Sentinel-5P)", "Flood Vulnerability (JRC Water)",
                              "Flood/Water Extent (Sentinel-1 SAR)"),
  "Economy & Population" = c("Nighttime Lights (VIIRS)", "Population Density (WorldPop)")
)

get_feature_units <- function(feature_name) {
  switch(feature_name,
         "Surface Temperature (LST)" = "\u00b0C",
         "Urban Heat Island (UHI)" = "\u00b0C",
         "Vegetation Health (NDVI)" = "index (-1 to 1)",
         "Water Body Mapping (NDWI)" = "index (-1 to 1)",
         "Urban Sprawl (NDBI)" = "index (-0.5 to 0.5)",
         "Blue-Green Infrastructure (BGI)" = "index (-0.2 to 0.6)",
         "Precipitation (CHIRPS)" = "mm",
         "Elevation (DEM)" = "meters",
         "Terrain Slope" = "degrees",
         "Population Density (WorldPop)" = "people/pixel",
         "Nighttime Lights (VIIRS)" = "radiance (nW/cm\u00b2/sr)",
         "Air Quality: NO2 (Sentinel-5P)" = "mol/m\u00b2",
         "Air Quality: CO (Sentinel-5P)" = "mol/m\u00b2",
         "Flood Vulnerability (JRC Water)" = "% occurrence",
         "Flood/Water Extent (Sentinel-1 SAR)" = "dB (VV backscatter)",
         # --- expanded catalogue (rgee-native) ---
         "Enhanced Vegetation (EVI)" = "index (-1 to 1)",
         "Soil-Adjusted Veg (SAVI)" = "index (-1 to 1)",
         "Modified SAVI (MSAVI2)" = "index (-1 to 1)",
         "Green NDVI (GNDVI)" = "index (-1 to 1)",
         "Atmospheric-Resistant Veg (ARVI)" = "index (-1 to 1)",
         "Visible ARVI (VARI)" = "index (-1 to 1)",
         "Red-Edge NDVI (NDRE)" = "index (-1 to 1)",
         "Modified NDWI (MNDWI)" = "index (-1 to 1)",
         "Moisture Index (NDMI)" = "index (-1 to 1)",
         "Land Surface Water (LSWI)" = "index (-1 to 1)",
         "Auto Water Extraction (AWEI)" = "index",
         "Turbidity (NDTI)" = "index (-1 to 1)",
         "Chlorophyll-a (NDCI)" = "index (-1 to 1)",
         "Urban Index (UI)" = "index (-1 to 1)",
         "Bare Soil Index (BSI)" = "index (-1 to 1)",
         "Built-up Index (IBI)" = "index (-1 to 1)",
         "Normalized Burn Ratio (NBR)" = "index (-1 to 1)",
         "Burn Area Index (BAI)" = "index",
         "Snow Index (NDSI)" = "index (-1 to 1)",
         "Clay Minerals Index" = "band ratio",
         "Ferrous Minerals Index" = "band ratio",
         "Iron Oxide Index" = "band ratio",
         "Terrain Aspect" = "degrees (0–360)",
         "Hillshade" = "0–255",
         "Topographic Position (TPI)" = "meters",
         "Terrain Ruggedness (TRI)" = "meters (σ)",
         "" # default: no known unit
  )
}

check_rate_limit <- function(rv, action_key, cooldown_seconds = 8) {
  if (is.null(rv$last_action_times)) rv$last_action_times <- list()
  last_time <- rv$last_action_times[[action_key]]
  now <- Sys.time()
  if (!is.null(last_time) && as.numeric(difftime(now, last_time, units = "secs")) < cooldown_seconds) {
    showNotification("Please wait a few seconds before running this again.", type = "warning", duration = 4)
    return(FALSE)
  }
  rv$last_action_times[[action_key]] <- now
  TRUE
}

# 🚀 UX FIX: whenever a boundary is set anywhere in the app, the relevant map should immediately
# pan/zoom to it and show its outline — instead of leaving the map at whatever view it was on,
# requiring the user to manually search for where their boundary actually is.
zoom_to_boundary <- function(map_id, mask_vect) {
  tryCatch({
    mask_4326 <- sf::st_transform(mask_vect, 4326)
    bbox <- sf::st_bbox(mask_4326)
    leaflet::leafletProxy(map_id) %>%
      leaflet::clearGroup("Boundary") %>%
      leaflet::addPolygons(data = mask_4326, fill = FALSE, color = "#8b3a2b", weight = 3, group = "Boundary", options = leaflet::pathOptions(clickable = FALSE)) %>%
      leaflet::fitBounds(as.numeric(bbox["xmin"]), as.numeric(bbox["ymin"]), as.numeric(bbox["xmax"]), as.numeric(bbox["ymax"]))
  }, error = function(e) NULL)
}

validate_roi_size <- function(sf_obj) {
  area_sqkm <- tryCatch(as.numeric(sum(sf::st_area(sf_obj))) / 1e6, error = function(e) NA_real_)
  if (is.na(area_sqkm)) return(TRUE) # fail-open on measurement error — don't block legitimate use over an edge case
  if (area_sqkm > MAX_ROI_AREA_SQKM) {
    showNotification(
      sprintf("Selected area (%.0f sq.km) exceeds the maximum supported size (%.0f sq.km). Please select a smaller region.", area_sqkm, MAX_ROI_AREA_SQKM),
      type = "error", duration = 10
    )
    return(FALSE)
  }
  TRUE
}

# 🚀 COST FIX: originally tiered 512/768/1024px by boundary size — these feed mod_gee.R's
# Temporal Grid, which renders each year as a SMALL TILE inside a multi-panel gallery grid, not
# a single full-page map. Print/document embedding of a gallery tile needs far less than
# 1024px — reduced tiers below cut total pixels (and therefore GEE fetch + local render compute)
# substantially for the common case, with no visible quality loss at gallery-tile display size.
get_thumb_dimensions <- function(sf_obj, base_max = 600) {
  area_sqkm <- tryCatch(as.numeric(sum(sf::st_area(sf_obj))) / 1e6, error = function(e) NA_real_)
  if (is.na(area_sqkm)) return(base_max)
  if (area_sqkm <= 100) return(400)
  if (area_sqkm <= 5000) return(500)
  return(base_max)
}

get_dynamic_scale <- function(sf_obj) {
  if(is.null(sf_obj)) return(30)
  area_sqkm <- as.numeric(sum(st_area(sf_obj))) / 1e6
  if (area_sqkm <= 5000) { return(30) }          
  else if (area_sqkm <= 25000) { return(100) }  
  else if (area_sqkm <= 100000) { return(250) }  
  else if (area_sqkm <= 500000) { return(500) }  
  else { return(1000) }                          
}

# 🚀 MEMORY FIX: render/thumbnail COMPUTATION scale for Publication Maps. GEE's getThumbURL can
# throw "User memory limit exceeded" when a per-pixel-heavy op (e.g. classify()) is evaluated at
# native 30m over a large ROI. This picks a coarser scale for the on-screen preview as area grows,
# so the thumbnail renders within GEE's memory budget. ROIs <=100 sq.km keep native 30m detail;
# larger ones step up progressively. Only affects the DISPLAY thumbnail — the downloadable GeoTIFF
# is still fetched at full native scale (fetch_geotiff_safe independently retries at coarser scales).
get_render_scale <- function(sf_obj, base_scale = 30) {
  if (is.null(sf_obj)) return(base_scale)
  area_sqkm <- tryCatch(as.numeric(sum(sf::st_area(sf_obj))) / 1e6, error = function(e) NA_real_)
  if (is.na(area_sqkm)) return(base_scale)
  if (area_sqkm <= 100)    return(base_scale)          # <=100 sq.km: native detail
  if (area_sqkm <= 1000)   return(max(base_scale, 100))
  if (area_sqkm <= 10000)  return(max(base_scale, 250))
  if (area_sqkm <= 50000)  return(max(base_scale, 500))
  return(max(base_scale, 1000))
}

# Reprojects a to-be-thumbnailed EE image to the render scale so GEE computes it at that (coarser,
# for large ROIs) resolution BEFORE getThumbURL — the actual memory-crash guard. No-op at native
# scale (small ROIs) so existing small-area output is unchanged. EE's default nearest-neighbour
# resampling preserves discrete class colours for categorical (LULC) maps.
reproject_for_thumb <- function(vis_image, render_scale) {
  if (is.null(render_scale) || render_scale <= 30) return(vis_image)
  vis_image$reproject(crs = "EPSG:4326", scale = render_scale)
}

js_coords <- "
  function(el, x) {
    var map = this;
    var coordDiv = document.createElement('div');
    coordDiv.className = 'gf-coord-box';
    coordDiv.innerHTML = 'Move cursor over map';
    el.appendChild(coordDiv);
    map.on('mousemove', function(e) {
      coordDiv.innerHTML = 'Lat ' + e.latlng.lat.toFixed(4) + '   Lon ' + e.latlng.lng.toFixed(4);
    });
  }
"

# 🚀 REFACTOR: methodology/citations text generator, extracted into one reusable function so
# both the .md export AND the PDF summary report use the exact same auto-populated content.
generate_methodology_text <- function(cart_rv, rv, gee_rv, flows, classification = NULL, study_area_sf = NULL) {
  cite_text <- "# Spatial Research Suite: Methodology & Data Report\n"
  cite_text <- paste0(cite_text, "Generated on: ", Sys.Date(), "\n\nIf you use these results in a research paper, please cite the following sources:\n\n## 1. Software & Platform\n- Pathak, A. K. (2026). Spatial Research Suite [Web Application].\n- Gorelick, N., et al. (2017). Google Earth Engine: Planetary-scale geospatial analysis for everyone. *Remote Sensing of Environment*.\n\n## 2. Analysis-Specific Citations\n")
  has_lulc <- any(grepl("lulc|report", cart_rv$items$ID))
  if(has_lulc) { cite_text <- paste0(cite_text, "### Machine Learning (LULC)\n- Breiman, L. (2001). Random Forests. *Machine Learning*, 45(1), 5-32.\n- Landsat 8/9: USGS Landsat Collection 2 Tier 1 Level 2 Science Products.\n\n") }
  has_gee <- any(grepl("gee", cart_rv$items$ID))
  if(has_gee) { cite_text <- paste0(cite_text, "### Cloud Analytics Datasets\n- Climate/Precipitation: CHIRPS Daily (Funk et al., 2015).\n- Elevation/Topography: SRTM Digital Elevation Data Version 3 (Farr et al., 2007).\n- Population: WorldPop Global High Resolution Population Denominators.\n\n") }
  
  cite_text <- paste0(cite_text, "---\n\n## Analysis Parameters (auto-generated from this session)\n\n")
  
  sa <- if (!is.null(study_area_sf)) study_area_sf else rv$mask_vect
  if (!is.null(sa)) {
    area_sqkm <- tryCatch(round(as.numeric(sum(sf::st_area(sa))) / 1e6, 2), error = function(e) NA)
    region_tag <- tryCatch(get_region_label(sa), error = function(e) "Unknown")
    cite_text <- paste0(cite_text, sprintf("**Study Area:** %s\n**Area:** %s sq.km\n\n", gsub("_", " ", region_tag), if(is.na(area_sqkm)) "Unknown" else format(area_sqkm, big.mark=",")))
  }
  
  if (has_gee && length(gee_rv$saved_images) > 0) {
    cite_text <- paste0(cite_text, "### Cloud Analytics — Exact Parameters Used\n\n")
    for (map_name in names(gee_rv$saved_images)) {
      recipe <- gee_rv$saved_images[[map_name]]
      sensor_note <- gee_rv$sensor_used[[map_name]] %||% NA
      if (is.list(recipe) && !is.null(recipe$type)) {
        if (recipe$type == "simple") {
          cite_text <- paste0(cite_text, sprintf(
            "- **%s**: Feature = `%s`, Date Range = %s to %s, Aggregation = %s, Spatial Resolution = %sm%s\n",
            map_name, recipe$feature, recipe$start_d, recipe$end_d, recipe$agg, recipe$scale,
            if (!is.na(sensor_note)) sprintf(", Data Source = %s", sensor_note) else ""
          ))
        } else if (recipe$type == "trend") {
          cite_text <- paste0(cite_text, sprintf(
            "- **%s**: Feature = `%s`, Years = %d-%d, Month = %s, Test = Mann-Kendall (%s), Spatial Resolution = %sm\n",
            map_name, recipe$feature, recipe$start_year, recipe$end_year, month.name[recipe$month],
            if(isTRUE(recipe$sig_only)) "significant pixels only, p<0.05" else "all pixels shown", recipe$scale
          ))
        }
      }
    }
    cite_text <- paste0(cite_text, "\n> **Note on cross-sensor consistency:** Where a result's Data Source lists more than one sensor (e.g. \"Landsat 5 + 7 + 8 (harmonized)\"), band names were aligned across sensors but their underlying spectral response and calibration still differ slightly. Small value differences (a few percent) between sensors are normal and expected — treat multi-sensor results as broadly comparable trends rather than exact like-for-like values.\n\n")
  }
  
  if (has_lulc && !is.null(classification$model)) {
    classifier_label <- switch(classification$classifier_type %||% "rf", "cart" = "CART (Decision Tree)", "svm" = "Support Vector Machine", "Random Forest")
    cite_text <- paste0(cite_text, "### LULC Classification — Exact Parameters Used\n\n")
    cite_text <- paste0(cite_text, sprintf("- **Classifier:** %s%s\n", classifier_label, if(!is.na(classification$trees_used %||% NA)) sprintf(" (%d trees)", classification$trees_used) else ""))
    if (!is.null(classification$indices_used) && length(classification$indices_used) > 0) {
      cite_text <- paste0(cite_text, sprintf("- **Spectral Indices Used:** %s\n", paste(classification$indices_used, collapse=", ")))
    }
    if (!is.null(rv$training_points_count)) {
      cite_text <- paste0(cite_text, sprintf("- **Training Samples:** %d points/polygons across %d classes\n", rv$training_points_count, nrow(rv$class_labels)))
    }
    if (!is.null(rv$acc_text)) {
      cite_text <- paste0(cite_text, sprintf("- **Validation Metrics:**\n```text\n%s\n```\n", rv$acc_text))
    }
    cite_text <- paste0(cite_text, "\n")
  }
  cite_text <- paste0(cite_text, "---\n\n## 3. Methodological Workflows (Flowcharts)\n\n")
  if(has_lulc) { cite_text <- paste0(cite_text, "### LULC Machine Learning Workflow\n```text\n", flows$lulc, "\n```\n\n") }
  if(has_gee) { cite_text <- paste0(cite_text, "### Surface Temperature (LST) Workflow\n```text\n", flows$lst, "\n```\n\n### Precipitation (CHIRPS) Workflow\n```text\n", flows$chirps, "\n```\n\n### Optical Indices (NDVI/NDWI) Workflow\n```text\n", flows$optical, "\n```\n\n") }
  
  cite_text
}

get_flowcharts <- function() {
  list(
    lulc = "\n[Landsat 8/9 Level-2 Collection]\n        |\n        v\n[Cloud Masking & Spatial Filtering (ROI)]\n        |\n        v\n[Feature Extraction: Surface Reflectance + Spectral Indices (NDVI, NDWI, EVI, NDBI)]\n        |\n        v\n[Training Data Generation] ---> [Random Forest Classifier (Configurable Trees)]\n        |                                     |\n        v                                     v\n[OOB Model Validation & Accuracy] <--- [Pixel-based Classification]\n                                              |\n                                              v\n                           [Spatiotemporal Grid & Zonal Area Statistics]\n",
    lst = "\n[Landsat 8/9 Collection 2 Tier 1 (Thermal Band: ST_B10)]\n        |\n        v\n[Temporal & Spatial Filtering]\n        |\n        v\n[Radiometric Scaling: (Band * 0.00341802) + 149.0]\n        |\n        v\n[Kelvin to Celsius Conversion: Temp - 273.15]\n        |\n        v\n[Temporal Aggregation (Mean/Median/Max/Min)]\n        |\n        v\n[Zonal Reducers & Time-Series Extraction]\n",
    chirps = "\n[CHIRPS Daily Climate Hazards Group InfraRed Precipitation]\n        |\n        v\n[Temporal & Spatial Filtering]\n        |\n        v\n[Temporal Aggregation (Sum / Total Rainfall)]\n        |\n        v\n[Zonal Reducers & Area Distribution Histogram]\n",
    optical = "\n[Copernicus Sentinel-2 / Landsat Collection]\n        |\n        v\n[Cloud Masking & Quality Assessment]\n        |\n        v\n[Spectral Band Math (e.g., NDVI, NDWI)]\n        |\n        v\n[Temporal Aggregation (Median/Mean)]\n        |\n        v\n[Quantitative Zonal Reducers & Time-Series]\n"
  )
}

get_shape_name <- function(sf_obj) {
  if ("shapeName" %in% names(sf_obj)) return(as.character(sf_obj$shapeName))
  possible_cols <- names(sf_obj)[grep("name", tolower(names(sf_obj)))]
  if (length(possible_cols) > 0) return(as.character(sf_obj[[possible_cols[1]]]))
  return(paste("Feature", 1:nrow(sf_obj)))
}

# --- 5. CLOUD ANALYTICS FEATURE MATRICES ---

# Masks clouds/cirrus in Sentinel-2 imagery using the QA60 band (bits 10 & 11).
# Applying this BEFORE compositing (mean/median/max/min) removes cloud-contaminated
# pixels from the calculation instead of averaging them into the result.
mask_s2_clouds <- function(image) {
  qa <- image$select('QA60')
  cloud_bit <- bitwShiftL(1L, 10)
  cirrus_bit <- bitwShiftL(1L, 11)
  mask <- qa$bitwiseAnd(cloud_bit)$eq(0)$And(qa$bitwiseAnd(cirrus_bit)$eq(0))
  image$updateMask(mask)
}

# Masks clouds & cloud-shadow in Landsat C2 L2 imagery using the QA_PIXEL band (bits 3 & 4).
mask_l8_clouds <- function(image) {
  qa <- image$select('QA_PIXEL')
  cloud_bit <- bitwShiftL(1L, 3)
  shadow_bit <- bitwShiftL(1L, 4)
  mask <- qa$bitwiseAnd(cloud_bit)$eq(0)$And(qa$bitwiseAnd(shadow_bit)$eq(0))
  image$updateMask(mask)
}

# Computes scientifically accurate Land Surface Temperature using NDVI-based emissivity
# correction (Sobrino et al. method) instead of raw brightness temperature.
compute_true_lst <- function(img_agg) {
  bt <- img_agg$select('ST_B10')$multiply(0.00341802)$add(149.0) # Brightness Temp (Kelvin)
  ndvi <- img_agg$normalizedDifference(c('SR_B5', 'SR_B4'))
  pv <- ndvi$subtract(0.2)$divide(0.3)$pow(2)$clamp(0, 1) # Vegetation proportion, NDVI 0.2-0.5 range
  emissivity <- pv$multiply(0.004)$add(0.986)
  lambda <- 10.895e-6; rho <- 1.438e-2 # Landsat 8 Band 10 central wavelength; radiation constant
  bt$divide(ee$Image(1)$add(bt$multiply(lambda / rho)$multiply(emissivity$log())))$subtract(273.15)
}

# Reverse-geocodes any boundary (admin shapefile OR a freehand-drawn polygon) to a
# "State_Country" style label by spatially matching its centroid against rnaturalearth's
# country + state polygons. Used to give downloaded files meaningful names instead of generic ones.
get_region_label <- function(sf_obj) {
  tryCatch({
    centroid <- sf::st_centroid(sf::st_union(sf::st_make_valid(sf::st_as_sf(sf_obj))))
    centroid <- sf::st_transform(centroid, 4326)
    
    countries <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
    hit <- suppressWarnings(sf::st_join(sf::st_as_sf(centroid), countries[, "name"], join = sf::st_intersects))
    country_name <- hit$name[1]
    if (is.na(country_name) || is.null(country_name)) return("UnknownRegion")
    
    state_name <- NA
    states <- tryCatch(rnaturalearth::ne_states(country = country_name, returnclass = "sf"), error = function(e) NULL)
    if (!is.null(states) && "name" %in% names(states)) {
      hit2 <- suppressWarnings(sf::st_join(sf::st_as_sf(centroid), states[, "name"], join = sf::st_intersects))
      state_name <- hit2$name[1]
    }
    
    label <- if (!is.na(state_name) && !is.null(state_name)) paste0(state_name, "_", country_name) else country_name
    gsub("[^A-Za-z0-9]+", "_", label)
  }, error = function(e) "UnknownRegion")
}

# Renames Landsat 5/7 (TM/ETM+) bands to match Landsat 8/9 (OLI) band naming convention,
# so the SAME index formulas (NDVI, NDBI, LST, etc.) work unchanged regardless of which
# sensor actually captured the imagery.
harmonize_landsat_bands <- function(image, sensor) {
  if (sensor %in% c("L5", "L7")) {
    image$select(
      c('SR_B1','SR_B2','SR_B3','SR_B4','SR_B5','SR_B7','ST_B6'),
      c('SR_B2','SR_B3','SR_B4','SR_B5','SR_B6','SR_B7','ST_B10')
    )
  } else {
    # 🚀 FIX: L8/L9 images already use the target band names, but still carry MANY extra
    # bands (QA_PIXEL, ST_ATRAN, ST_CDIST, SR_QA_AEROSOL, etc.) that L5/L7 don't have after
    # renaming. ee.ImageCollection.merge() requires identical band sets across all images,
    # so L8/L9 must be subset down to the SAME 7 bands too, or merging with harmonized L5/L7
    # throws "Expected a homogeneous image collection" errors.
    image$select(c('SR_B2','SR_B3','SR_B4','SR_B5','SR_B6','SR_B7','ST_B10'))
  }
}

# 🚀 SCIENTIFIC FIX: Returns a cloud-masked, band-harmonized Landsat ImageCollection spanning
# whichever sensor(s) were actually operational during the requested date range. Landsat 8 alone
# only exists from Feb 2013 onward — this extends genuine satellite coverage back to 1984 (Landsat 5)
# instead of years before 2013 being silently skipped/empty across the app.
get_harmonized_landsat_collection <- function(start_date, end_date, ee_roi) {
  sd <- as.Date(start_date); ed <- as.Date(end_date)
  collections <- list()
  
  if (sd <= as.Date("2012-05-05")) { # Landsat 5 operational window (1984-2012)
    l5 <- ee$ImageCollection('LANDSAT/LT05/C02/T1_L2')$filterBounds(ee_roi)$filterDate(start_date, end_date)$map(mask_l8_clouds)$map(function(img) harmonize_landsat_bands(img, "L5"))
    collections[[length(collections) + 1]] <- l5
  }
  if (ed >= as.Date("1999-04-15") && sd <= Sys.Date()) { # Landsat 7 (1999-present; SLC-off gaps after 2003, still usable)
    l7 <- ee$ImageCollection('LANDSAT/LE07/C02/T1_L2')$filterBounds(ee_roi)$filterDate(start_date, end_date)$map(mask_l8_clouds)$map(function(img) harmonize_landsat_bands(img, "L7"))
    collections[[length(collections) + 1]] <- l7
  }
  if (ed >= as.Date("2013-02-11")) { # Landsat 8
    l8 <- ee$ImageCollection('LANDSAT/LC08/C02/T1_L2')$filterBounds(ee_roi)$filterDate(start_date, end_date)$map(mask_l8_clouds)$map(function(img) harmonize_landsat_bands(img, "L8"))
    collections[[length(collections) + 1]] <- l8
  }
  if (ed >= as.Date("2021-10-31")) { # Landsat 9
    l9 <- ee$ImageCollection('LANDSAT/LC09/C02/T1_L2')$filterBounds(ee_roi)$filterDate(start_date, end_date)$map(mask_l8_clouds)$map(function(img) harmonize_landsat_bands(img, "L9"))
    collections[[length(collections) + 1]] <- l9
  }
  
  if (length(collections) == 0) return(NULL)
  merged <- collections[[1]]
  if (length(collections) > 1) for (i in 2:length(collections)) merged <- merged$merge(collections[[i]])
  merged
}

# 🚀 SCIENTIFIC FIX: for ML classification specifically (NOT simple ratio indices like NDVI/LST),
# matching band NAMES across sensors is not enough — Landsat 5/7 (TM/ETM+) and Landsat 8/9 (OLI) have
# different spectral response functions, so a classifier trained on one sensor's reflectance values
# systematically misclassifies imagery from a different sensor even with identical band names/ranges.
# This restricts classification-related fetches to Landsat 8+9 only, the sensor family a classifier
# can actually be expected to generalize across. Simple index features keep the full 1984+ range.
get_landsat89_collection <- function(start_date, end_date, ee_roi) {
  sd <- as.Date(start_date); ed <- as.Date(end_date)
  collections <- list()
  if (ed >= as.Date("2013-02-11")) {
    l8 <- ee$ImageCollection('LANDSAT/LC08/C02/T1_L2')$filterBounds(ee_roi)$filterDate(start_date, end_date)$map(mask_l8_clouds)
    collections[[length(collections) + 1]] <- l8
  }
  if (ed >= as.Date("2021-10-31")) {
    l9 <- ee$ImageCollection('LANDSAT/LC09/C02/T1_L2')$filterBounds(ee_roi)$filterDate(start_date, end_date)$map(mask_l8_clouds)
    collections[[length(collections) + 1]] <- l9
  }
  if (length(collections) == 0) return(NULL)
  merged <- collections[[1]]
  if (length(collections) > 1) merged <- merged$merge(collections[[2]])
  merged
}

# 🚀 SCIENTIFIC FIX (cross-sensor transparency): describes which Landsat sensor(s) actually
# cover a date range, matching get_harmonized_landsat_collection()'s own era logic. Surfaced to
# users so they know if a result mixes multiple sensors (which can introduce small value
# differences even after band-name harmonization — worth knowing when comparing results).
describe_landsat_sensors <- function(start_date, end_date) {
  sd <- as.Date(start_date); ed <- as.Date(end_date)
  sensors <- c()
  if (sd <= as.Date("2012-05-05")) sensors <- c(sensors, "Landsat 5")
  if (ed >= as.Date("1999-04-15") && sd <= Sys.Date()) sensors <- c(sensors, "Landsat 7")
  if (ed >= as.Date("2013-02-11")) sensors <- c(sensors, "Landsat 8")
  if (ed >= as.Date("2021-10-31")) sensors <- c(sensors, "Landsat 9")
  if (length(sensors) == 0) return("Unknown")
  if (length(sensors) > 1) paste0(paste(sensors, collapse=" + "), " (harmonized)") else sensors
}

get_feature_img <- function(f_name, start_date, end_date, agg, ee_roi, dyn_scale, stretch = TRUE) {
  # stretch=FALSE skips the display-only 2/98 percentile computation (an extra, sometimes slow
  # reduceRegion getInfo). Per-year rebuilds for the Trend & Correlation steps only need the raw
  # band, so they pass FALSE — this removes ~2 blocking EE round-trips PER build call (a big deal
  # when a coarse product like VIIRS is rebuilt once per year across several indicators).
  base_col <- NULL; img <- NULL; pal <- NULL; min_v <- 0; max_v <- 1; sensor_used <- "Unknown"
  
  check_col <- function(col, dataset_name) {
    if(is.null(col) || col$size()$getInfo() == 0) {
      stop(paste("No", dataset_name, "imagery found for this specific Date Range and Area. Please select a wider date range."))
    }
    return(col)
  }
  
  # 🚀 EXPANDED CATALOGUE: rgee-native indices (spectral + terrain) are built by the
  # data-driven recipe engine in R/gee_indices.R. If this feature is one of them, delegate
  # (returns NULL for the legacy features below, so their proven code is untouched). The
  # shared 2/98 percentile stretch at the end still refines the display range.
  .recipe_img <- build_index_image(f_name, start_date, end_date, agg, ee_roi, dyn_scale)
  if (!is.null(.recipe_img)) {
    img <- .recipe_img$img; pal <- .recipe_img$pal
    min_v <- .recipe_img$min; max_v <- .recipe_img$max; sensor_used <- .recipe_img$sensor
  } else if(f_name == "Vegetation Health (NDVI)") {
    s2_col <- ee$ImageCollection('COPERNICUS/S2_SR_HARMONIZED')$filterBounds(ee_roi)$filterDate(start_date, end_date)$map(mask_s2_clouds)
    if (s2_col$size()$getInfo() > 0) {
      img <- switch(agg, "Mean"=s2_col$mean(), "Max"=s2_col$max(), "Min"=s2_col$min(), s2_col$median())$normalizedDifference(c('B8', 'B4'))$rename('NDVI')$clip(ee_roi)
      sensor_used <- "Sentinel-2"
    } else {
      # 🚀 SCIENTIFIC FIX: Sentinel-2 only exists from 2015 onward — fall back to harmonized
      # Landsat (1984+) so pre-2015 NDVI requests get real data instead of being skipped.
      l_col <- check_col(get_harmonized_landsat_collection(start_date, end_date, ee_roi), "Sentinel-2 or Landsat")
      img_agg <- switch(agg, "Mean"=l_col$mean(), "Max"=l_col$max(), "Min"=l_col$min(), l_col$median())
      img <- img_agg$normalizedDifference(c('SR_B5', 'SR_B4'))$rename('NDVI')$clip(ee_roi)
      sensor_used <- describe_landsat_sensors(start_date, end_date)
    }
    pal <- c('#d73027', '#fdae61', '#1a9850'); min_v <- -1; max_v <- 1
  } else if(f_name == "Surface Temperature (LST)") {
    base_col <- check_col(get_harmonized_landsat_collection(start_date, end_date, ee_roi), "Landsat")
    img_agg <- switch(agg, "Mean"=base_col$mean(), "Max"=base_col$max(), "Min"=base_col$min(), base_col$median())
    img <- compute_true_lst(img_agg)$rename('LST')$clip(ee_roi); pal <- c('blue', 'yellow', 'red'); min_v <- 15; max_v <- 50
    sensor_used <- describe_landsat_sensors(start_date, end_date)
  } else if(f_name == "Precipitation (CHIRPS)") {
    base_col <- check_col(ee$ImageCollection('UCSB-CHG/CHIRPS/DAILY')$filterBounds(ee_roi)$filterDate(start_date, end_date), "CHIRPS")
    img <- switch(agg, "Mean"=base_col$mean(), "Median"=base_col$median(), "Max"=base_col$max(), "Min"=base_col$min(), base_col$sum())$rename('Rainfall')$clip(ee_roi); pal <- c('white', 'blue', 'darkblue'); min_v <- 0; max_v <- 2000
    sensor_used <- "CHIRPS Daily"
  } else if(f_name == "Elevation (DEM)") {
    img <- ee$Image('USGS/SRTMGL1_003')$rename('Elevation')$clip(ee_roi); pal <- c('green', 'yellow', 'brown', 'white'); min_v <- 0; max_v <- 5000
    sensor_used <- "SRTM GL1"
  } else if(f_name == "Terrain Slope") {
    img <- ee$Terrain$slope(ee$Image('USGS/SRTMGL1_003'))$rename('Slope')$clip(ee_roi); pal <- c('white', 'gray', 'black'); min_v <- 0; max_v <- 90
    sensor_used <- "SRTM GL1"
  } else if(f_name == "Population Density (WorldPop)") {
    base_col <- check_col(ee$ImageCollection("WorldPop/GP/100m/pop")$filterBounds(ee_roi), "WorldPop")
    img <- switch(agg, "Mean"=base_col$mean(), "Max"=base_col$max(), "Min"=base_col$min(), base_col$median())$rename('Population')$clip(ee_roi); pal <- c('white', 'yellow', 'red', 'darkred'); min_v <- 0; max_v <- 500
    sensor_used <- "WorldPop"
  } else if(f_name == "Nighttime Lights (VIIRS)") {
    base_col <- check_col(ee$ImageCollection('NOAA/VIIRS/DNB/MONTHLY_V1/VCMSLCFG')$filterBounds(ee_roi)$filterDate(start_date, end_date), "VIIRS")
    img <- switch(agg, "Mean"=base_col$mean(), "Max"=base_col$max(), "Min"=base_col$min(), base_col$median())$select('avg_rad')$clip(ee_roi); pal <- c('black', 'yellow', 'white'); min_v <- 0; max_v <- 100
    sensor_used <- "VIIRS DNB"
  } else if(f_name == "Water Body Mapping (NDWI)") {
    s2_col <- ee$ImageCollection('COPERNICUS/S2_SR_HARMONIZED')$filterBounds(ee_roi)$filterDate(start_date, end_date)$map(mask_s2_clouds)
    if (s2_col$size()$getInfo() > 0) {
      img <- switch(agg, "Mean"=s2_col$mean(), "Max"=s2_col$max(), "Min"=s2_col$min(), s2_col$median())$normalizedDifference(c('B3', 'B8'))$rename('NDWI')$clip(ee_roi)
      sensor_used <- "Sentinel-2"
    } else {
      l_col <- check_col(get_harmonized_landsat_collection(start_date, end_date, ee_roi), "Sentinel-2 or Landsat")
      img_agg <- switch(agg, "Mean"=l_col$mean(), "Max"=l_col$max(), "Min"=l_col$min(), l_col$median())
      img <- img_agg$normalizedDifference(c('SR_B3', 'SR_B5'))$rename('NDWI')$clip(ee_roi)
      sensor_used <- describe_landsat_sensors(start_date, end_date)
    }
    pal <- c('brown', 'white', 'blue'); min_v <- -1; max_v <- 1
  } else if(f_name == "Air Quality: NO2 (Sentinel-5P)") {
    base_col <- check_col(ee$ImageCollection('COPERNICUS/S5P/OFFL/L3_NO2')$filterBounds(ee_roi)$filterDate(start_date, end_date), "Sentinel-5P NO2")
    img <- switch(agg, "Mean"=base_col$mean(), "Max"=base_col$max(), "Min"=base_col$min(), base_col$median())$select('NO2_column_number_density')$clip(ee_roi); pal <- c('black', 'blue', 'purple', 'cyan', 'green', 'yellow', 'red'); min_v <- 0; max_v <- 0.0002
    sensor_used <- "Sentinel-5P"
  } else if(f_name == "Air Quality: CO (Sentinel-5P)") {
    base_col <- check_col(ee$ImageCollection('COPERNICUS/S5P/OFFL/L3_CO')$filterBounds(ee_roi)$filterDate(start_date, end_date), "Sentinel-5P CO")
    img <- switch(agg, "Mean"=base_col$mean(), "Max"=base_col$max(), "Min"=base_col$min(), base_col$median())$select('CO_column_number_density')$clip(ee_roi); pal <- c('black', 'blue', 'purple', 'cyan', 'green', 'yellow', 'red'); min_v <- 0; max_v <- 0.05
    sensor_used <- "Sentinel-5P"
  } else if(f_name == "Urban Sprawl (NDBI)") {
    base_col <- check_col(get_harmonized_landsat_collection(start_date, end_date, ee_roi), "Landsat")
    img_agg <- switch(agg, "Mean"=base_col$mean(), "Max"=base_col$max(), "Min"=base_col$min(), base_col$median())
    img <- img_agg$normalizedDifference(c('SR_B6', 'SR_B5'))$rename('NDBI')$clip(ee_roi); pal <- c('white', 'lightgray', 'gray', 'red', 'darkred'); min_v <- -0.5; max_v <- 0.5
    sensor_used <- describe_landsat_sensors(start_date, end_date)
  } else if(f_name == "Urban Heat Island (UHI)") {
    base_col <- check_col(get_harmonized_landsat_collection(start_date, end_date, ee_roi), "Landsat")
    img_agg <- switch(agg, "Mean"=base_col$mean(), "Max"=base_col$max(), "Min"=base_col$min(), base_col$median())
    img <- compute_true_lst(img_agg)$rename('UHI_LST')$clip(ee_roi); pal <- c('#313695', '#4575b4', '#74add1', '#abd9e9', '#e0f3f8', '#ffffbf', '#fee090', '#fdae61', '#f46d43', '#d73027', '#a50026'); min_v <- 25; max_v <- 45
    sensor_used <- describe_landsat_sensors(start_date, end_date)
  } else if(f_name == "Blue-Green Infrastructure (BGI)") {
    s2_col <- ee$ImageCollection('COPERNICUS/S2_SR_HARMONIZED')$filterBounds(ee_roi)$filterDate(start_date, end_date)$map(mask_s2_clouds)
    if (s2_col$size()$getInfo() > 0) {
      img_agg <- switch(agg, "Mean"=s2_col$mean(), "Max"=s2_col$max(), "Min"=s2_col$min(), s2_col$median())
      img <- img_agg$normalizedDifference(c('B8', 'B4'))$add(img_agg$normalizedDifference(c('B3', 'B8')))$divide(2)$rename('BGI')$clip(ee_roi)
      sensor_used <- "Sentinel-2"
    } else {
      l_col <- check_col(get_harmonized_landsat_collection(start_date, end_date, ee_roi), "Sentinel-2 or Landsat")
      img_agg <- switch(agg, "Mean"=l_col$mean(), "Max"=l_col$max(), "Min"=l_col$min(), l_col$median())
      img <- img_agg$normalizedDifference(c('SR_B5', 'SR_B4'))$add(img_agg$normalizedDifference(c('SR_B3', 'SR_B5')))$divide(2)$rename('BGI')$clip(ee_roi)
      sensor_used <- describe_landsat_sensors(start_date, end_date)
    }
    pal <- c('white', 'lightblue', 'blue', 'lightgreen', 'darkgreen'); min_v <- -0.2; max_v <- 0.6
  } else if(f_name == "Flood Vulnerability (JRC Water)") {
    img <- ee$Image("JRC/GSW1_4/GlobalSurfaceWater")$select('occurrence')$clip(ee_roi); pal <- c('white', 'lightblue', 'blue', 'darkblue'); min_v <- 0; max_v <- 100
    sensor_used <- "JRC Global Surface Water"
  } else if(f_name == "Flood/Water Extent (Sentinel-1 SAR)") {
    # SAR sees through clouds & works day/night — ideal for flood mapping during monsoon when optical sensors are cloud-blocked.
    # Water surfaces are smooth and reflect radar away from the sensor, so they appear as very low VV backscatter.
    base_col <- check_col(ee$ImageCollection('COPERNICUS/S1_GRD')$filterBounds(ee_roi)$filterDate(start_date, end_date)$filter(ee$Filter$eq('instrumentMode', 'IW'))$filter(ee$Filter$listContains('transmitterReceiverPolarisation', 'VV'))$select('VV'), "Sentinel-1 SAR")
    img_agg <- switch(agg, "Mean"=base_col$mean(), "Max"=base_col$max(), "Min"=base_col$min(), base_col$median())
    smoothed <- img_agg$focal_median(radius = 50, units = 'meters') # speckle reduction
    img <- smoothed$rename('SAR_VV_dB')$clip(ee_roi); pal <- c('#08306b', '#4292c6', '#c6dbef', '#f7fbff'); min_v <- -25; max_v <- 0
    sensor_used <- "Sentinel-1 SAR"
  } else { return(NULL) }
  
  if (isTRUE(stretch)) {
    bands_info <- tryCatch(img$bandNames()$getInfo(), error=function(e) list())
    if(length(bands_info) > 0) {
      mm <- tryCatch(img$reduceRegion(reducer=ee$Reducer$percentile(list(2, 98)), geometry=ee_roi, scale=dyn_scale * 2, maxPixels=1e10, bestEffort=TRUE, tileScale=16)$getInfo(), error=function(e) NULL)
      if(!is.null(mm)) {
        c_mi <- as.numeric(mm[[paste0(bands_info[1], "_p2")]])
        c_ma <- as.numeric(mm[[paste0(bands_info[1], "_p98")]])
        if(!is.na(c_mi)) min_v <- c_mi
        if(!is.na(c_ma) && c_ma > min_v) max_v <- c_ma
      }
    }
  }
  return(list(img = img, pal = pal, min = min_v, max = max_v, sensor = sensor_used))
}

# 🚀 STABILITY FIX: DT (DataTables) resolves its JS/CSS htmlwidget dependencies lazily, the
# first time any datatable() widget is actually rendered — which meant the FIRST user session
# in a freshly-booted container to open a Batch Processing or Zonal Statistics result could hit
# a brief window where dt-core-bootstrap5's JS files weren't yet fully registered, causing
# intermittent 404s and a results table that never appeared (even though the underlying analysis
# had completed correctly). Rendering one throwaway table here, at container startup, forces this
# resolution to happen once before any real user request arrives, instead of on a real user's session.
# --- 5h. STRUCTURED LOGGING (Cloud Run searchable) ------------------------------------------

# Emits a single-line, greppable log entry via message() — Cloud Run captures stderr/stdout
# automatically (visible in `gcloud logging read` / Cloud Console Logs), but PLAIN message() text
# ("Processing Error: ...") is hard to filter or correlate across a busy log stream. This wraps
# every log line in a consistent `key=value` format so operators can filter directly, e.g.:
#   gcloud logging read '... AND textPayload:"level=ERROR"'
#   gcloud logging read '... AND textPayload:"module=mod_carto"'
#   gcloud logging read '... AND textPayload:"event=publication_map_render"'
# `...` accepts any named detail values (numbers/strings/logicals) and are included as-is;
# NULL/NA detail values are simply omitted rather than printed as "NULL"/"NA" noise.
# session_id is optional but strongly recommended for anything inside a moduleServer — pass
# `session$token` (a short, stable per-session identifier) so all log lines from one user's
# session can be grepped together, e.g. `textPayload:"session=abc123"`.
log_event <- function(level = c("INFO", "WARN", "ERROR"), module, event, ..., session_id = NULL) {
  level <- match.arg(level)
  details <- list(...)
  details <- details[!vapply(details, function(x) is.null(x) || (length(x) == 1 && is.na(x)), logical(1))]
  detail_str <- if (length(details) > 0) {
    paste(sprintf("%s=%s", names(details), vapply(details, function(x) paste(as.character(x), collapse = ","), character(1))), collapse = " ")
  } else ""
  ts <- format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z", tz = "UTC")
  line <- trimws(sprintf("[%s] level=%s module=%s event=%s session=%s %s", ts, level, module, event, session_id %||% "-", detail_str))
  message(line)
  invisible(line)
}

tryCatch({ invisible(htmltools::renderTags(DT::datatable(data.frame(x = 1)))) }, error = function(e) NULL)

# =========================================================================
# 🚀 SHARED FUNCTION LIBRARY
# =========================================================================
# The functions below were previously duplicated (sometimes with subtle differences) across
# mod_lulc.R, mod_carto.R, and mod_extractor.R. Consolidating them here means every module calls
# the SAME, single, tested implementation — a fix or improvement made once applies everywhere,
# instead of needing to be found and re-applied in every copy (which is exactly how several bugs
# in this app's history slipped through: a fix applied in one copy, forgotten in another).

# --- 5a. SPECTRAL INDICES & CLASSIFICATION-IMAGE PREP ---------------------

# Adds whichever spectral indices the user selected (NDVI/NDWI/EVI/NDBI) as extra bands on a
# Landsat-derived image. Returns both the updated image AND the full list of band names now
# present (base reflectance bands + whichever indices were added), since callers need to know
# exactly which bands exist before selecting a subset for training/classification.
add_selected_indices <- function(img, indices) {
  bands <- c('SR_B2', 'SR_B3', 'SR_B4', 'SR_B5', 'SR_B6', 'SR_B7')
  if ("NDVI" %in% indices) {
    img <- img$addBands(img$normalizedDifference(c('SR_B5', 'SR_B4'))$rename('NDVI'))
    bands <- c(bands, 'NDVI')
  }
  if ("NDWI" %in% indices) {
    img <- img$addBands(img$normalizedDifference(c('SR_B3', 'SR_B5'))$rename('NDWI'))
    bands <- c(bands, 'NDWI')
  }
  if ("EVI" %in% indices) {
    img <- img$addBands(img$expression('2.5 * ((NIR - RED) / (NIR + 6 * RED - 7.5 * BLUE + 10000))',
                                       list(NIR = img$select('SR_B5'), RED = img$select('SR_B4'), BLUE = img$select('SR_B2')))$rename('EVI'))
    bands <- c(bands, 'EVI')
  }
  if ("NDBI" %in% indices) {
    img <- img$addBands(img$normalizedDifference(c('SR_B6', 'SR_B5'))$rename('NDBI'))
    bands <- c(bands, 'NDBI')
  }
  return(list(img = img, bands = bands))
}

# Applies the SAME per-band min-max normalization used at classifier-training time. A classifier
# trained on normalized (0-1 scaled) bands will misclassify almost everything if later fed raw,
# unnormalized reflectance values — this is what caused the "single dominant colour" bug earlier
# in this app's history, in three different places that each re-implemented this by hand.
# If norm_mins/norm_maxs are NULL (classifier trained before normalization was tracked, or a
# genuinely un-normalized use case), returns the image with just the requested bands selected —
# no normalization applied, but still a safe, valid image.
apply_saved_normalization <- function(img, bands, norm_mins = NULL, norm_maxs = NULL) {
  if (is.null(norm_mins) || is.null(norm_maxs)) return(img$select(bands))
  mins_img <- ee$Image$constant(as.list(unname(norm_mins)))$rename(bands)
  maxs_img <- ee$Image$constant(as.list(unname(norm_maxs)))$rename(bands)
  img$select(bands)$subtract(mins_img)$divide(maxs_img$subtract(mins_img))
}

# Fetches ONE year's median Landsat 8/9 composite, ready to classify: collection fetch + cloud
# mask (via get_landsat89_collection) + median + clip + spectral indices + saved normalization,
# all in one call. Used everywhere a "re-render this year fresh" step is needed (Temporal Grid,
# Publication Map's year-specific LULC maps, Change Detection's two comparison years) — previously
# each of these three call-sites re-implemented this same multi-step sequence by hand.
fetch_year_landsat_image <- function(year, month, ee_roi, indices, training_bands, norm_mins = NULL, norm_maxs = NULL) {
  s_d <- sprintf("%04d-%02d-01", year, month)
  e_d <- as.character(seq(as.Date(s_d), length = 2, by = "4 months")[2] - 1)
  col <- get_landsat89_collection(s_d, e_d, ee_roi)
  if (is.null(col) || col$size()$getInfo() == 0) stop(sprintf("No Landsat imagery available for %d.", year))
  img <- col$median()$clip(ee_roi)
  res <- add_selected_indices(img, indices)
  apply_saved_normalization(res$img, training_bands, norm_mins, norm_maxs)
}

# --- 5b. CLASSIFICATION DISPLAY & CHANGE DETECTION ------------------------

# Remaps a classified image's raw Class_ID values to sequential 0..N-1 (sorted) IDs for DISPLAY
# only, with a matching palette — this is what guarantees each class gets its own correct,
# discrete color instead of GEE's getMapId/visualize() linearly interpolating (and scrambling)
# colors when class IDs aren't contiguous or don't match palette-array order. The ORIGINAL
# (un-remapped) classified image is left untouched by this function — callers needing it for
# GeoTIFF export (where downloaded pixel values must match the user's own class scheme) should
# keep their own reference to it. Returns everything a caller typically needs next: the ready-to-
# fetch visualize() image, the remapped-but-not-yet-visualized image, the palette, the diagnostic
# "unclassified" bucket index, and the ID-sorted class-label rows.
get_classification_vis <- function(classified, class_labels) {
  if (nrow(class_labels) < 1) stop("No class labels are defined — cannot render classification colors.")
  # 🚀 FIX: guard against "Image.visualize: Cannot provide a palette when visualizing more than
  # one band" — classify() can occasionally yield more than one band depending on classifier
  # configuration, so always select band 0 explicitly before remap/visualize rather than assuming
  # single-band input. mod_carto.R's LULC branches already did this defensively; this makes the
  # guarantee apply uniformly everywhere get_classification_vis() is called (mod_lulc.R's
  # run_model, Publication Maps, class-wise Statistics), fixing it once instead of per call-site.
  classified <- classified$select(0)
  sorted_labels <- class_labels[order(class_labels$Class_ID), ]
  from_ids <- as.list(sorted_labels$Class_ID)
  to_ids <- as.list(seq_along(from_ids) - 1)
  unclassified_idx <- length(from_ids) # diagnostic bucket for unmatched pixel values (shows grey instead of vanishing)
  classified_vis <- classified$remap(from_ids, to_ids, unclassified_idx)
  pal <- c(unname(trimws(sorted_labels$Class_Color)), "#999999")
  vis_img <- classified_vis$visualize(min = 0, max = unclassified_idx, palette = pal)
  list(vis_img = vis_img, classified_vis = classified_vis, pal = pal, unclassified_idx = unclassified_idx, sorted_labels = sorted_labels)
}

# Computes a minimum-mapping-unit-filtered change mask between two already-classified images:
# pixels that changed class, EXCLUDING isolated "salt-and-pepper" single/small-patch pixels that
# are more likely classification noise than genuine land-cover change (standard remote-sensing
# practice — a connected patch of at least min_patch_pixels, default ~0.8 ha at 30m, is required).
compute_class_change_mask <- function(img1, img2, min_patch_pixels = 9) {
  change_mask_raw <- img1$eq(img2)$Not()$selfMask()
  patch_size <- change_mask_raw$connectedPixelCount(maxSize = 100, eightConnected = TRUE)
  change_mask_raw$updateMask(patch_size$gte(min_patch_pixels))
}

# Computes per-class area (in a flat Class_ID/Area_sqm data.frame) for a classified image over a
# boundary. Deliberately returns just the raw grouped areas, not a fully-formatted table — the
# two real callers (single-year LULC stats vs. multi-year Temporal Grid stats) each need slightly
# different final shaping (one adds a Year column, one doesn't), so shaping is left to the caller.
# Returns NULL if no pixels/groups were found (caller should treat this as "nothing to report",
# not necessarily an error — e.g. a boundary with genuinely no valid classified pixels).
get_area_by_class_groups <- function(classified_img, ee_roi, scale) {
  areaImage <- ee$Image$pixelArea()$addBands(classified_img)
  areas <- areaImage$reduceRegion(
    reducer = ee$Reducer$sum()$group(groupField = 1, groupName = "class_id"),
    geometry = ee_roi, scale = scale, maxPixels = 1e13, bestEffort = TRUE, tileScale = 16
  )$getInfo()
  if (length(areas) == 0 || length(areas$groups) == 0) return(NULL)
  do.call(rbind, lapply(areas$groups, function(g) data.frame(Class_ID = g$class_id, Area_sqm = g$sum)))
}

# --- Landscape COMPOSITION & DIVERSITY (pure base R, no packages) ---------------------------------
# Standard landscape-ecology diversity indices computed from per-class area shares (any positive
# unit — km2, hectares, pixel counts or percentages; normalised internally). These describe how
# many cover types there are and how evenly the area is split among them:
#   * richness  : number of classes actually present (area > 0)
#   * shdi      : Shannon's Diversity Index  = -SUM p_i * ln(p_i)   (0 = one class; higher = more
#                 classes / more even)
#   * shei      : Shannon's Evenness Index   = SHDI / ln(richness)  in [0,1] (1 = perfectly even)
#   * simpson   : Simpson's Diversity        = 1 - SUM p_i^2        (prob. two random pixels differ)
#   * dominance : area share of the single largest class            in [0,1]
# Returns NULL if no positive class shares are supplied.
landscape_diversity <- function(shares) {
  p <- suppressWarnings(as.numeric(shares))
  p <- p[is.finite(p) & p > 0]
  if (!length(p)) return(NULL)
  p <- p / sum(p)
  richness <- length(p)
  shdi <- -sum(p * log(p))
  shei <- if (richness > 1) shdi / log(richness) else 0
  list(richness = richness, shdi = shdi, shei = shei,
       simpson = 1 - sum(p^2), dominance = max(p))
}

# Shared HTML renderer for a landscape_diversity() result (used by both the GEE and
# LULC Insights tabs). Returns "" when there is nothing to show. `scope` names what the
# classes are (e.g. "value classes", "land-cover classes") for the plain-language note.
gf_diversity_html <- function(d, scope = "classes") {
  if (is.null(d)) return("")
  paste0(
    "<div style='margin-top:8px;background:#f2f5f7;border-radius:5px;padding:8px 10px;'>",
    "<div style='font-size:12px;font-weight:600;color:#26333e;margin-bottom:3px;'>Landscape diversity</div>",
    "<table class='table table-sm' style='font-size:12px;margin-bottom:2px;'><tbody>",
    sprintf("<tr><td>Richness (classes present)</td><td><b>%d</b></td><td>Shannon diversity (SHDI)</td><td><b>%.3f</b></td></tr>", d$richness, d$shdi),
    sprintf("<tr><td>Shannon evenness (SHEI)</td><td><b>%.3f</b></td><td>Simpson diversity</td><td><b>%.3f</b></td></tr>", d$shei, d$simpson),
    sprintf("<tr><td>Dominant class share</td><td><b>%.1f%%</b></td><td></td><td></td></tr>", d$dominance * 100),
    "</tbody></table>",
    sprintf("<div style='font-size:10px;color:#8a97a0;'>Across these %s: <b>SHDI</b> rises with more and more-even classes (0 = a single class dominates). <b>SHEI</b> is 0-1 (1 = perfectly even split). <b>Simpson</b> = the chance two random pixels belong to different classes.</div>", scope),
    "</div>")
}

# --- Landscape CONFIGURATION (patch structure) metrics -------------------------------------------
# Derives the standard FRAGSTATS-style configuration metrics from raw quantities extracted from a
# classified image (patch = a contiguous block of one class). Pure arithmetic (the Earth Engine
# extraction happens in the caller); NA-safe so a partially-failed extraction still yields what it
# can. Inputs in m^2 / counts / metres; outputs in the units reviewers expect:
#   * np      : number of patches
#   * pd      : patch density (patches per 100 ha)
#   * mps_ha  : mean patch size (ha)            = total area / NP
#   * awmps_ha: area-weighted mean patch size (ha) — the patch a random pixel sits in, on average
#   * lpi     : largest patch index (%)         = largest patch / total area
#   * ed      : edge density (m per ha)
landscape_patch_metrics <- function(total_area_m2, np = NA, largest_patch_m2 = NA,
                                    aw_mean_patch_m2 = NA, edge_len_m = NA) {
  ta <- suppressWarnings(as.numeric(total_area_m2))
  if (!isTRUE(is.finite(ta)) || ta <= 0) return(NULL)
  fin <- function(x) { x <- suppressWarnings(as.numeric(x)); isTRUE(is.finite(x)) }
  np <- suppressWarnings(as.numeric(np)); ha <- ta / 1e4
  list(
    total_ha = ha,
    np       = if (fin(np) && np > 0) round(np) else NA_real_,
    pd       = if (fin(np) && np > 0) np / ha * 100 else NA_real_,
    mps_ha   = if (fin(np) && np > 0) (ta / np) / 1e4 else NA_real_,
    awmps_ha = if (fin(aw_mean_patch_m2)) as.numeric(aw_mean_patch_m2) / 1e4 else NA_real_,
    lpi      = if (fin(largest_patch_m2) && as.numeric(largest_patch_m2) > 0) 100 * as.numeric(largest_patch_m2) / ta else NA_real_,
    ed       = if (fin(edge_len_m) && as.numeric(edge_len_m) >= 0) as.numeric(edge_len_m) / ha else NA_real_
  )
}

# --- CROSS-INDICATOR CORRELATION (pure base R) ---------------------------------------------------
# Links driver time-series (tourism proxy: nighttime lights / built-up; water quality: turbidity /
# chlorophyll) to a response series (coral condition), year by year. Reports both Pearson (linear)
# and Spearman (rank/monotonic) correlation with p-values, and an optional lag search (does the
# driver LEAD the response by 1-2 years?). All correlations are computed on the OVERLAPPING finite
# years of each pair, so a short/patchy indicator doesn't void the whole matrix.
#
# `series` = named list of numeric vectors, each aligned to the SAME years vector (NAs allowed for
# missing years). Returns NULL if fewer than two indicators or fewer than three shared years.
gf_correlation_analysis <- function(series, method = c("pearson", "spearman")) {
  method <- match.arg(method)
  nm <- names(series); k <- length(nm)
  if (is.null(nm) || k < 2) return(NULL)
  lens <- vapply(series, length, integer(1))
  if (length(unique(lens)) != 1L || lens[1] < 3) return(NULL)
  R <- matrix(NA_real_, k, k, dimnames = list(nm, nm))
  P <- matrix(NA_real_, k, k, dimnames = list(nm, nm))
  N <- matrix(0L,       k, k, dimnames = list(nm, nm))
  for (i in seq_len(k)) for (j in seq_len(k)) {
    a <- suppressWarnings(as.numeric(series[[i]])); b <- suppressWarnings(as.numeric(series[[j]]))
    ok <- is.finite(a) & is.finite(b); nn <- sum(ok)
    N[i, j] <- nn
    if (nn >= 3) {
      ct <- tryCatch(suppressWarnings(stats::cor.test(a[ok], b[ok], method = method)), error = function(e) NULL)
      if (!is.null(ct)) { R[i, j] <- unname(ct$estimate); P[i, j] <- ct$p.value }
    }
  }
  list(method = method, names = nm, r = R, p = P, n = N)
}

# Lag search: correlate response x(t) with driver y(t - L) for L = 0..max_lag (does y lead x?).
# Returns the lag with the strongest |r|, plus the full table. NULL if too few points.
gf_lag_correlation <- function(x, y, max_lag = 2, method = "pearson") {
  x <- suppressWarnings(as.numeric(x)); y <- suppressWarnings(as.numeric(y))
  n <- length(x); if (n != length(y) || n < 4) return(NULL)
  rows <- list(); best <- NULL
  for (L in 0:max_lag) {
    if (L > n - 3) break
    xi <- x[(L + 1):n]; yi <- y[1:(n - L)]
    ok <- is.finite(xi) & is.finite(yi)
    if (sum(ok) < 3) next
    ct <- tryCatch(suppressWarnings(stats::cor.test(xi[ok], yi[ok], method = method)), error = function(e) NULL)
    if (is.null(ct)) next
    row <- list(lag = L, r = unname(ct$estimate), p = ct$p.value, n = sum(ok))
    rows[[length(rows) + 1]] <- row
    if (is.null(best) || (is.finite(row$r) && abs(row$r) > abs(best$r))) best <- row
  }
  if (!length(rows)) return(NULL)
  list(best = best, table = rows, method = method)
}

gf_correlation_html <- function(a, response = NULL) {
  if (is.null(a)) return("")
  nm <- a$names; k <- length(nm)
  esc <- function(x) { x <- as.character(x); x <- gsub("&", "&amp;", x, fixed = TRUE); x <- gsub("<", "&lt;", x, fixed = TRUE); gsub(">", "&gt;", x, fixed = TRUE) }
  cellcol <- function(r) if (!is.finite(r)) "#ffffff" else if (r > 0) sprintf("rgba(69,147,111,%.2f)", min(abs(r), 1) * 0.6) else sprintf("rgba(139,58,43,%.2f)", min(abs(r), 1) * 0.6)
  head_c <- paste0("<th>", esc(nm), "</th>", collapse = "")
  rows <- vapply(seq_len(k), function(i) {
    cells <- vapply(seq_len(k), function(j) {
      r <- a$r[i, j]; p <- a$p[i, j]
      # No significance star on the diagonal — a variable's correlation with itself is trivially 1.
      lbl <- if (!is.finite(r)) "—" else if (i == j) "1.00"
             else sprintf("%.2f%s", r, if (is.finite(p) && p < 0.05) "*" else "")
      sprintf("<td style='text-align:center;background:%s;'>%s</td>", cellcol(r), lbl)
    }, character(1))
    paste0("<tr><th style='text-align:left;'>", esc(nm[i]), "</th>", paste0(cells, collapse = ""), "</tr>")
  }, character(1))
  focus <- ""
  if (!is.null(response) && response %in% nm) {
    ri <- match(response, nm)
    frows <- vapply(setdiff(seq_len(k), ri), function(j) {
      r <- a$r[ri, j]; p <- a$p[ri, j]
      dirn <- if (!is.finite(r)) "—" else if (r > 0) "positive" else "negative"
      sig  <- if (is.finite(p) && p < 0.05) "significant" else "not significant"
      sprintf("<tr><td><b>%s</b></td><td>%s</td><td>%s</td><td>%s (%s, n=%d)</td></tr>",
              esc(nm[j]), if (is.finite(r)) sprintf("%.2f", r) else "—",
              if (is.finite(p)) sprintf("%.3f", p) else "—", dirn, sig, a$n[ri, j])
    }, character(1))
    focus <- paste0(
      "<h6 style='font-weight:600;color:#26333e;margin:10px 0 2px;'>", esc(response), " vs each driver</h6>",
      "<table class='table table-sm' style='font-size:11px;'><thead><tr><th>Driver</th><th>r</th><th>p</th><th>Relationship</th></tr></thead><tbody>",
      paste0(frows, collapse = ""), "</tbody></table>")
  }
  paste0(
    "<div style='font-size:11px;color:#5c6b73;margin:4px 0;'>", if (identical(a$method, "spearman")) "Spearman rank" else "Pearson linear",
    " correlation between the annual indicator series (over the shared years). <b>*</b> = p &lt; 0.05. Green = positive, red = negative.</div>",
    "<div style='overflow-x:auto;'><table class='table table-sm' style='font-size:11px;'><thead><tr><th></th>", head_c, "</tr></thead><tbody>",
    paste0(rows, collapse = ""), "</tbody></table></div>", focus,
    "<div style='font-size:10px;color:#8a97a0;margin-top:3px;'>Correlation is not causation, and short series (few years) give wide uncertainty — treat these as screening associations, not proof.</div>")
}

# Plain-language trend interpretation, calibrated to significance so a near-zero or
# non-significant slope is NOT reported as a confident "decline"/"rise". Pure (no GEE),
# so it is unit-testable. Inputs come from the OLS rate + the Mann-Kendall result.
#   - tau == 0 (no monotonic ordering)  -> "essentially flat"
#   - directional slope, significant    -> "declined" / "rose"
#   - directional slope, not significant -> "slight downward/upward tendency (not significant)"
# For benthic/coral indices it appends the macroalgae caveat and frames direction as
# reef-bottom brightness (darker = more coral/algae cover).
gf_trend_narrative <- function(rate, tau = NA, significant = FALSE, p_value = NA, n = NA, is_benthic = FALSE) {
  benthic_caveat <- if (isTRUE(is_benthic)) " Note this index cannot separate live coral from macroalgae (both darken the bottom), so it tracks benthic brightness rather than coral health directly — cross-check with the turbidity / chlorophyll indicators before concluding." else ""
  has_mk  <- isTRUE(is.finite(p_value)) || isTRUE(is.finite(tau))
  sig_ok  <- isTRUE(significant)
  is_flat <- isTRUE(is.finite(tau)) && abs(tau) < 1e-9
  verb_dn <- if (sig_ok) "declined" else if (has_mk) "showed a slight downward tendency (not statistically significant)" else "declined"
  verb_up <- if (sig_ok) "rose"     else if (has_mk) "showed a slight upward tendency (not statistically significant)" else "rose"
  dir_txt <- if (is_flat) {
               if (isTRUE(is_benthic)) paste0("Coral Health was essentially flat over the period — no meaningful net change in reef-bottom brightness.", benthic_caveat)
               else "The regional value was essentially flat over the period."
             } else if (isTRUE(rate < 0)) {
               if (isTRUE(is_benthic)) paste0("Coral Health ", verb_dn, " over the period — the reef bottom trended brighter (toward sand / rubble).", benthic_caveat)
               else paste0("The regional value ", verb_dn, " over the period.")
             } else if (isTRUE(rate > 0)) {
               if (isTRUE(is_benthic)) paste0("Coral Health ", verb_up, " over the period — the reef bottom trended darker (toward coral / algae / seagrass).", benthic_caveat)
               else paste0("The regional value ", verb_up, " over the period.")
             } else "There was no net change."
  sig_txt <- if (has_mk) {
               if (sig_ok) sprintf(" This trend is statistically significant (Mann-Kendall p = %.3f).", p_value)
               else sprintf(" Over this short record the change is within noise (Mann-Kendall p = %.3f, n = %d years); a longer series is needed to detect a real trend.", p_value, n)
             } else ""
  paste0(dir_txt, sig_txt)
}

# Accuracy-assessment HTML for the LULC report: standard headline (overall accuracy, kappa) +
# the good-practice metrics — area-weighted overall accuracy, Pontius & Millones (2011) quantity/
# allocation disagreement, and Olofsson et al. (2014) error-adjusted per-class area with 95% CI.
# Pure/testable. `rigor` is the list returned by lulc_rigorous_accuracy(), optionally with a
# Class_Name column merged into per_class; oa/kappa are the standard classifier metrics.
gf_lulc_accuracy_html <- function(oa = NA, kappa = NA, rigor = NULL) {
  esc <- function(x){x<-as.character(x);x<-gsub("&","&amp;",x,fixed=TRUE);x<-gsub("<","&lt;",x,fixed=TRUE);gsub(">","&gt;",x,fixed=TRUE)}
  pct <- function(v) if (is.null(v) || length(v)!=1 || !is.finite(v)) "—" else sprintf("%.1f%%", v*100)
  parts <- character(0)
  std <- character(0)
  if (isTRUE(is.finite(oa)))    std <- c(std, sprintf("<li>Overall accuracy: <b>%s</b></li>", pct(oa)))
  if (isTRUE(is.finite(kappa))) std <- c(std, sprintf("<li>Cohen's kappa: <b>%.3f</b> <span style='color:#8a97a0;'>(for reference; superseded by the disagreement metrics below)</span></li>", kappa))
  if (length(std)) parts <- c(parts, paste0("<ul style='font-size:12px;margin:4px 0 8px 18px;'>", paste(std, collapse=""), "</ul>"))
  if (is.null(rigor)) return(paste0(parts, collapse = ""))
  parts <- c(parts, sprintf(paste0(
    "<table><thead><tr><th>Metric</th><th>Value</th></tr></thead><tbody>",
    "<tr><td>Area-weighted overall accuracy</td><td>%s</td></tr>",
    "<tr><td>Quantity disagreement (Pontius &amp; Millones 2011)</td><td>%s</td></tr>",
    "<tr><td>Allocation disagreement</td><td>%s</td></tr>",
    "<tr><td>Total disagreement</td><td>%s</td></tr></tbody></table>",
    "<p class='note'>Quantity + Allocation disagreement replace the Kappa coefficient (now discouraged in the remote-sensing literature). <i>Quantity</i> = disagreement from the wrong total amount of a class; <i>Allocation</i> = right amount but wrong location. Total = 100%% &minus; area-weighted overall accuracy.%s</p>"),
    pct(rigor$oa_aw), pct(rigor$quantity), pct(rigor$allocation), pct(rigor$total_disagreement),
    if (isTRUE(rigor$unsampled_strata > 0)) sprintf(" Note: %d mapped class(es) had no validation samples, so their area could not be error-adjusted.", rigor$unsampled_strata) else ""))
  pc <- rigor$per_class
  if (!is.null(pc) && nrow(pc) > 0) {
    nm <- if ("Class_Name" %in% names(pc)) pc$Class_Name else as.character(pc$Class_ID)
    rows <- vapply(seq_len(nrow(pc)), function(i) {
      ci  <- if (isTRUE(is.finite(pc$CI95[i]))) sprintf("&plusmn; %.2f", pc$CI95[i]) else "—"
      rng <- if (isTRUE(is.finite(pc$CI95[i]))) sprintf("%.2f &ndash; %.2f", max(0, pc$Adj_Area[i]-pc$CI95[i]), pc$Adj_Area[i]+pc$CI95[i]) else "—"
      sprintf("<tr><td>%s</td><td>%.2f</td><td>%.2f</td><td>%s</td><td>%s</td></tr>",
              esc(nm[i]), pc$Mapped_Area[i], pc$Adj_Area[i], ci, rng)
    }, character(1))
    parts <- c(parts,
      "<h3>Error-adjusted area (Olofsson et al. 2014)</h3>",
      "<table><thead><tr><th>Class</th><th>Mapped (km²)</th><th>Adjusted (km²)</th><th>95% CI</th><th>Adjusted range (km²)</th></tr></thead><tbody>",
      paste(rows, collapse=""), "</tbody></table>",
      sprintf("<p class='note'>Error-adjusted area corrects the mapped area for classification error using the validation confusion matrix, with a 95%% confidence interval — so an area can be quoted honestly as a range. Total sampled area %s km² from %s validation points.</p>",
              if (isTRUE(is.finite(rigor$A_total))) sprintf("%.1f", rigor$A_total) else "?",
              if (isTRUE(is.finite(rigor$n_total))) as.character(rigor$n_total) else "?"))
  }
  paste0(parts, collapse = "")
}

# Choose one layer/indicator from the available names, honouring an optional user selection and
# falling back to a preference list then the first item. Pure/testable. Matching is exact first,
# then a substring match so a short catalog label (e.g. "Coral Health") resolves a stored long
# name (e.g. "Coral Health (blue-green bottom index)"). Used for BOTH the report's headline/
# composite layer and the correlation's response variable, so the two behave identically.
gf_pick_layer <- function(available, selected = "", prefer = character(0)) {
  if (is.null(available) || !length(available)) return(NULL)
  sel <- if (is.null(selected) || !length(selected)) "" else as.character(selected)[1]
  if (nzchar(sel)) {
    hit <- available[available == sel]
    if (!length(hit)) hit <- available[grepl(sel, available, fixed = TRUE)]
    if (length(hit)) return(hit[[1]])
  }
  for (p in prefer) {
    hit <- available[grepl(p, available, ignore.case = TRUE)]
    if (length(hit)) return(hit[[1]])
  }
  available[[1]]
}

# Reproducibility / provenance table (Parameter, Value) — the exact parameters needed to re-run a
# pipeline analysis. Pure/testable; shared by the report (HTML) and the data (Excel) exports so the
# two never drift. Rows whose value is empty are dropped so the table only shows what is known.
gf_provenance_df <- function(area_name = "", km2 = NA, date_range = "", agg = "",
                             dyn_scale = NA, thumb_dim = NA, chain = "",
                             datasets = character(0), generated = "",
                             crs = "EPSG:4326 (WGS84)", bbox = NULL,
                             params = "", versions = "") {
  s   <- function(x) if (is.null(x) || length(x) != 1 || is.na(x)) "" else as.character(x)
  numv <- function(x, d) { v <- suppressWarnings(as.numeric(x)); if (length(v) == 1 && is.finite(v)) as.character(round(v, d)) else "" }
  rows <- list(
    c("Study area", s(area_name)),
    c("Boundary area (km²)", numv(km2, 3)),
    c("Date range", s(date_range)),
    c("Compositing reducer", s(agg)),
    c("Adaptive spatial scale (m)", numv(dyn_scale, 1)),
    c("Composite thumbnail (px)", { v <- suppressWarnings(as.numeric(thumb_dim)); if (length(v) == 1 && is.finite(v)) as.character(as.integer(v)) else "" }),
    c("Processing chain", s(chain)),
    c("Datasets", if (length(datasets)) paste(datasets, collapse = "; ") else "Google Earth Engine catalog"),
    c("Cloud handling", "Sentinel-2: per-pixel SCL mask; image-level filter relaxed (<=90%); clear pixels reduced by the reducer above"),
    c("Coordinate reference system", s(crs)),
    c("Analysis parameters", if (nzchar(params)) params else "Shallow-water mask: green reflectance > 0.015; display stretch: 2-98th percentile; OISST anomaly raw x0.01 \u00b0C vs 1971-2000 climatology; Sentinel-2 image cloud filter <=90% + per-pixel SCL; benthic composite max cloud <=90%."),
    c("Bounding box (WGS84)", { if (is.null(bbox)) "" else { b <- suppressWarnings(as.numeric(bbox)); if (length(b) >= 4 && all(is.finite(b[1:4]))) sprintf("%.4f W, %.4f S, %.4f E, %.4f N", b[[1]], b[[2]], b[[3]], b[[4]]) else "" } }),
    c("Software versions", s(versions)),
    c("Platform", "Google Earth Engine (rgee) - Spatial Research Suite pipeline"),
    c("Generated", s(generated)))
  rows <- Filter(function(r) nzchar(r[[2]]), rows)
  data.frame(Parameter = vapply(rows, function(r) r[[1]], character(1)),
             Value     = vapply(rows, function(r) r[[2]], character(1)),
             check.names = FALSE, stringsAsFactors = FALSE)
}

# --- Change TRANSITION analysis + INTENSITY (Aldwaik & Pontius 2012), pure base R ---------------
# Given the from->to transition areas between two dates, builds the cross-tabulation matrix and the
# standard change budget per class:
#   * initial / final size, persistence (stayed the same)
#   * gross gain, gross loss, NET change (gain-loss), SWAP (2*min(gain,loss)) — Pontius' decomposition
#   * category-level INTENSITY: gain as a fraction of the class's FINAL extent, loss as a fraction of
#     its INITIAL extent, each compared with the map-wide uniform change intensity St = D/A. A class
#     whose gain (or loss) intensity exceeds St is "actively targeted" for gain (or loss); below = dormant.
# Inputs are parallel vectors (from, to labels + transition area, any single unit). Returns NULL if empty.
lulc_transition_analysis <- function(from, to, area) {
  df <- data.frame(from = as.character(from), to = as.character(to),
                   area = suppressWarnings(as.numeric(area)), stringsAsFactors = FALSE)
  df <- df[is.finite(df$area) & df$area > 0, , drop = FALSE]
  if (!nrow(df)) return(NULL)
  classes <- sort(unique(c(df$from, df$to)))
  M <- matrix(0, length(classes), length(classes), dimnames = list(classes, classes))
  for (i in seq_len(nrow(df))) M[df$from[i], df$to[i]] <- M[df$from[i], df$to[i]] + df$area[i]
  A <- sum(M); t1 <- rowSums(M); t2 <- colSums(M); persist <- diag(M)
  gain <- t2 - persist; loss <- t1 - persist
  net  <- t2 - t1; swap <- 2 * pmin(gain, loss)
  D <- A - sum(persist); St <- if (A > 0) D / A else NA_real_
  gain_int <- ifelse(t2 > 0, gain / t2, NA_real_)
  loss_int <- ifelse(t1 > 0, loss / t1, NA_real_)
  per_class <- data.frame(
    Class = classes, Initial = round(t1, 3), Final = round(t2, 3),
    Gain = round(gain, 3), Loss = round(loss, 3), Net = round(net, 3), Swap = round(swap, 3),
    Gain_intensity_pct = round(gain_int * 100, 1), Loss_intensity_pct = round(loss_int * 100, 1),
    Active_gain = as.logical(gain_int > St), Active_loss = as.logical(loss_int > St),
    stringsAsFactors = FALSE)
  list(matrix = M, classes = classes, per_class = per_class, total_area = A,
       changed_area = D, overall_change_pct = round(100 * St, 1), persistence = sum(persist))
}

gf_transition_html <- function(a) {
  if (is.null(a)) return("")
  pc <- a$per_class; cls <- a$classes
  esc <- function(x) { x <- as.character(x); x <- gsub("&", "&amp;", x, fixed = TRUE); x <- gsub("<", "&lt;", x, fixed = TRUE); gsub(">", "&gt;", x, fixed = TRUE) }
  # cross-tab matrix (from = rows, to = columns)
  head_to <- paste0("<th>", esc(cls), "</th>", collapse = "")
  mrows <- vapply(seq_along(cls), function(i) {
    cells <- vapply(seq_along(cls), function(j) {
      v <- a$matrix[i, j]; bold <- if (i == j) " style='background:#eef3ee;font-weight:600;'" else ""
      sprintf("<td%s>%s</td>", bold, if (v > 0) formatC(v, format = "f", digits = 3) else "·")
    }, character(1))
    paste0("<tr><th style='text-align:left;'>", esc(cls[i]), "</th>", paste0(cells, collapse = ""), "</tr>")
  }, character(1))
  budget <- vapply(seq_len(nrow(pc)), function(i) sprintf(
    "<tr><td><b>%s</b></td><td>%s</td><td>%s</td><td style='color:#45936f;'>+%s</td><td style='color:#8b3a2b;'>-%s</td><td><b>%+.3f</b></td><td>%s</td><td>%s%%%s</td><td>%s%%%s</td></tr>",
    esc(pc$Class[i]), formatC(pc$Initial[i], format = "f", digits = 3), formatC(pc$Final[i], format = "f", digits = 3),
    formatC(pc$Gain[i], format = "f", digits = 3), formatC(pc$Loss[i], format = "f", digits = 3), pc$Net[i],
    formatC(pc$Swap[i], format = "f", digits = 3),
    formatC(pc$Gain_intensity_pct[i], format = "f", digits = 1), if (isTRUE(pc$Active_gain[i])) " <b style='color:#45936f;'>▲</b>" else "",
    formatC(pc$Loss_intensity_pct[i], format = "f", digits = 1), if (isTRUE(pc$Active_loss[i])) " <b style='color:#8b3a2b;'>▲</b>" else ""), character(1))
  paste0(
    "<div style='font-size:12px;color:#5c6b73;margin:6px 0;'>Over the interval, <b>", formatC(a$changed_area, format = "f", digits = 3),
    "</b> of <b>", formatC(a$total_area, format = "f", digits = 3), "</b> changed class (<b>", a$overall_change_pct,
    "%</b> of the area). Uniform change intensity = ", a$overall_change_pct, "%; a class marked ▲ changed <i>more intensely</i> than that (actively targeted).</div>",
    "<h6 style='font-weight:600;color:#26333e;margin:8px 0 2px;'>Transition matrix (rows = from, columns = to; km²)</h6>",
    "<div style='overflow-x:auto;'><table class='table table-sm' style='font-size:11px;'><thead><tr><th>from \\ to</th>", head_to, "</tr></thead><tbody>",
    paste0(mrows, collapse = ""), "</tbody></table></div>",
    "<h6 style='font-weight:600;color:#26333e;margin:10px 0 2px;'>Change budget &amp; intensity (Aldwaik &amp; Pontius, 2012)</h6>",
    "<div style='overflow-x:auto;'><table class='table table-sm' style='font-size:11px;'><thead><tr><th>Class</th><th>Initial</th><th>Final</th><th>Gain</th><th>Loss</th><th>Net</th><th>Swap</th><th>Gain int.</th><th>Loss int.</th></tr></thead><tbody>",
    paste0(budget, collapse = ""), "</tbody></table></div>",
    "<div style='font-size:10px;color:#8a97a0;'><b>Net</b> = gain − loss (real expansion/contraction). <b>Swap</b> = area that left one place and arrived in another (2×min(gain,loss)) — same total, different location. <b>Gain/Loss intensity</b> = gain as % of the class's final extent / loss as % of its initial extent; ▲ = above the uniform rate.</div>")
}

gf_patch_html <- function(m) {
  if (is.null(m)) return("")
  f <- function(x, d = 1) if (!isTRUE(is.finite(x))) "—" else formatC(x, format = "f", digits = d, big.mark = ",")
  paste0(
    "<div style='margin-top:8px;background:#f2f5f7;border-radius:5px;padding:8px 10px;'>",
    "<div style='font-size:12px;font-weight:600;color:#26333e;margin-bottom:3px;'>Landscape configuration (patch structure)</div>",
    "<table class='table table-sm' style='font-size:12px;margin-bottom:2px;'><tbody>",
    sprintf("<tr><td>Number of patches</td><td><b>%s</b></td><td>Patch density (/100 ha)</td><td><b>%s</b></td></tr>", f(m$np, 0), f(m$pd, 2)),
    sprintf("<tr><td>Mean patch size (ha)</td><td><b>%s</b></td><td>Area-weighted mean patch (ha)</td><td><b>%s</b></td></tr>", f(m$mps_ha, 2), f(m$awmps_ha, 2)),
    sprintf("<tr><td>Largest Patch Index (%%)</td><td><b>%s</b></td><td>Edge density (m/ha)</td><td><b>%s</b></td></tr>", f(m$lpi, 1), f(m$ed, 1)),
    "</tbody></table>",
    "<div style='font-size:10px;color:#8a97a0;'>A <b>patch</b> is a contiguous block of one class (8-connected). Higher patch &amp; edge density with lower mean patch size = a more <b>fragmented</b> map; a high <b>Largest Patch Index</b> means one class forms a single big block. Approximate — computed on a coarsened grid (~3× the native pixel) to stay within Earth Engine memory, and single-patch pixel counts are capped at ~256 px, so very large patches (and LPI) can read low.</div>",
    "</div>")
}

# --- 5c. GEE RETRY-SAFE FETCH (moved here from mod_carto.R — was previously re-defined inside
# the module-server on every single "Render" click, instead of existing once, globally) ---------

# A getDownloadURL() call can succeed (returns a URL) while the actual fetch of that URL later
# fails with 400/503 — so both steps are retried together, at progressively coarser resolution,
# rather than just the URL-generation step. Uses httr2 (not download.file()) specifically so GEE's
# real JSON error body (e.g. "User memory limit exceeded", "no band named X") is captured and can
# be surfaced to the user/logs, instead of a bare, uninformative HTTP status code.
# crs is optional — pass it to let getDownloadURL() handle CRS-reprojection lazily as part of its
# own export computation, rather than via a separate eager reproject() call beforehand (the
# eager version is what caused "User memory limit exceeded" earlier in this app's history).
fetch_geotiff_safe <- function(image, roi_geom, initial_scale, crs = NULL) {
  attempt <- function(s) {
    params <- list(region = roi_geom, scale = s, format = "GEO_TIFF")
    if (!is.null(crs)) params$crs <- crs
    url <- image$getDownloadURL(params)
    resp <- httr2::req_perform(httr2::req_error(httr2::request(url), is_error = function(resp) FALSE))
    if (httr2::resp_status(resp) >= 400) {
      body_text <- tryCatch(httr2::resp_body_string(resp), error = function(e) "(no error body)")
      stop(sprintf("GEE GeoTIFF fetch failed (status %d): %s", httr2::resp_status(resp), body_text))
    }
    tmp <- tempfile(fileext = ".tif")
    writeBin(httr2::resp_body_raw(resp), tmp)
    tmp
  }
  tryCatch(attempt(initial_scale),
           error = function(e) tryCatch(attempt(initial_scale * 2),
                                        error = function(e2) attempt(initial_scale * 4)))
}

fetch_png_safe <- function(vis_image, roi_geom, initial_dim) {
  attempt <- function(d) {
    url <- vis_image$getThumbURL(list(region = roi_geom, dimensions = round(d), format = "png"))
    resp <- httr2::req_perform(httr2::req_error(httr2::request(url), is_error = function(resp) FALSE))
    if (httr2::resp_status(resp) >= 400) {
      body_text <- tryCatch(httr2::resp_body_string(resp), error = function(e) "(no error body)")
      stop(sprintf("GEE thumbnail fetch failed (status %d): %s", httr2::resp_status(resp), body_text))
    }
    tmp <- tempfile(fileext = ".png")
    writeBin(httr2::resp_body_raw(resp), tmp)
    tmp
  }
  tryCatch(attempt(initial_dim),
           error = function(e) tryCatch(attempt(initial_dim * 0.6),
                                        error = function(e2) attempt(initial_dim * 0.35)))
}

# --- 5d. GGPLOT MAP-BUILDING (categorical / continuous / final cartography styling) -------------

# Builds a ggplot map from a fetched raster PNG (img_arr) for a CATEGORICAL/classified result
# (e.g. LULC classes) — handles the raster-overlay, boundary outline, and the "invisible dummy
# points" trick needed to get a correct discrete legend from an already-rasterized PNG (since the
# PNG itself has no per-pixel class information ggplot could map a legend from directly).
build_categorical_map_plot <- function(img_arr, bbox, boundary_sf, class_labels, legend_title = "Legend", linewidth = 0.5, interpolate = FALSE) {
  legend_pal <- setNames(trimws(class_labels$Class_Color), trimws(class_labels$Class_Name))
  dummy_df <- data.frame(
    x = as.numeric(bbox["xmin"]), y = as.numeric(bbox["ymin"]),
    Class_Name = factor(class_labels$Class_Name, levels = class_labels$Class_Name)
  )
  ggplot() +
    annotation_custom(grid::rasterGrob(img_arr, width = unit(1, "npc"), height = unit(1, "npc"), interpolate = interpolate),
                      xmin = as.numeric(bbox["xmin"]), xmax = as.numeric(bbox["xmax"]), ymin = as.numeric(bbox["ymin"]), ymax = as.numeric(bbox["ymax"])) +
    geom_sf(data = sf::st_as_sf(boundary_sf), fill = NA, color = "black", linewidth = linewidth) +
    geom_point(data = dummy_df, aes(x = x, y = y, fill = Class_Name), shape = 22, size = 0.1, alpha = 0, color = "transparent") +
    scale_fill_manual(name = legend_title, values = legend_pal, drop = FALSE) +
    guides(fill = guide_legend(override.aes = list(size = 5, alpha = 1, color = "black")))
}

# Same idea, but for a CONTINUOUS result (e.g. Confidence Map, LST, NDVI) — uses a color-gradient
# legend/colorbar instead of a discrete one, via the same "invisible dummy points" trick.
build_continuous_map_plot <- function(img_arr, bbox, boundary_sf, map_min, map_max, palette, legend_title = "Legend", linewidth = 0.5, base_fill = NA) {
  # Guard degenerate ranges (min >= max makes the colorbar/stretch collapse to one flat colour).
  if (is.na(map_min) || is.na(map_max) || map_min >= map_max) { map_min <- 0; map_max <- 1 }
  dummy_df <- data.frame(x = as.numeric(bbox["xmin"]), y = as.numeric(bbox["ymin"]), val = c(map_min, map_max))
  p <- ggplot()
  # Optional base fill: when the data layer is heavily masked (e.g. only shallow reef
  # pixels survive), a light study-area fill UNDER the raster keeps the map from
  # reading as blank/transparent. Drawn first so the raster sits on top.
  if (!is.na(base_fill)) p <- p + geom_sf(data = sf::st_as_sf(boundary_sf), fill = base_fill, color = NA)
  p +
    annotation_custom(grid::rasterGrob(img_arr, width = unit(1, "npc"), height = unit(1, "npc"), interpolate = TRUE),
                      xmin = as.numeric(bbox["xmin"]), xmax = as.numeric(bbox["xmax"]), ymin = as.numeric(bbox["ymin"]), ymax = as.numeric(bbox["ymax"])) +
    geom_sf(data = sf::st_as_sf(boundary_sf), fill = NA, color = "black", linewidth = linewidth) +
    geom_point(data = dummy_df, aes(x = x, y = y, color = val), alpha = 0, size = 0.1) +
    scale_color_gradientn(colors = palette, limits = c(map_min, map_max), name = legend_title) +
    guides(color = guide_colorbar(barwidth = 1.5, barheight = 12, frame.colour = "black", ticks.colour = "black"))
}

# Discrete sibling of build_continuous_map_plot: draws a CLASSIFIED raster with a
# swatch legend (one box per class) instead of a continuous colourbar, so the map's
# colours line up 1:1 with a per-class area table. `class_colors` and `class_labels`
# are parallel vectors in class order (lowest -> highest). interpolate = FALSE keeps
# class edges crisp rather than smearing colours across boundaries.
build_classified_map_plot <- function(img_arr, bbox, boundary_sf, class_colors, class_labels,
                                      legend_title = "Class", linewidth = 0.5, base_fill = "#eaeef1") {
  bsf <- sf::st_as_sf(boundary_sf)
  labs_f <- factor(class_labels, levels = class_labels)
  dummy_df <- data.frame(x = as.numeric(bbox["xmin"]), y = as.numeric(bbox["ymin"]), cls = labs_f)
  p <- ggplot()
  if (!is.na(base_fill)) p <- p + geom_sf(data = bsf, fill = base_fill, color = NA)
  p +
    annotation_custom(grid::rasterGrob(img_arr, width = unit(1, "npc"), height = unit(1, "npc"), interpolate = FALSE),
                      xmin = as.numeric(bbox["xmin"]), xmax = as.numeric(bbox["xmax"]), ymin = as.numeric(bbox["ymin"]), ymax = as.numeric(bbox["ymax"])) +
    geom_sf(data = bsf, fill = NA, color = "black", linewidth = linewidth) +
    geom_point(data = dummy_df, aes(x = x, y = y, color = cls), alpha = 0, size = 0.1) +
    scale_color_manual(values = stats::setNames(class_colors, class_labels), name = legend_title, drop = FALSE) +
    guides(color = guide_legend(override.aes = list(alpha = 1, size = 5, shape = 15)))
}

# -------------------------------------------------------------------------
# Report CHARTS (pure data -> ggplot). No GEE / no network: they take the
# numbers the pipeline already produced and draw them, so they are unit-
# testable and can never stall the report. Both return a ggplot on success
# and NULL on degenerate input (too few finite points) so the caller can
# simply skip the figure.
# -------------------------------------------------------------------------

# Yearly regional trend: points + connecting line + a dashed OLS fit line.
# series_df must have numeric columns Year and Value.
gf_trend_line_plot <- function(series_df, label = "Value", units = "") {
  if (is.null(series_df) || !all(c("Year", "Value") %in% names(series_df))) return(NULL)
  d <- data.frame(Year = suppressWarnings(as.numeric(series_df$Year)),
                  Value = suppressWarnings(as.numeric(series_df$Value)))
  d <- d[is.finite(d$Year) & is.finite(d$Value), , drop = FALSE]
  if (nrow(d) < 2) return(NULL)
  ylab <- if (nzchar(units)) sprintf("%s (%s)", label, units) else label
  fit <- tryCatch(stats::lm(Value ~ Year, data = d), error = function(e) NULL)
  p <- ggplot(d, aes(x = Year, y = Value)) +
    geom_line(color = "#8aa0ab", linewidth = 0.5) +
    geom_point(color = "#2c5a4a", size = 2.6)
  if (!is.null(fit))
    p <- p + geom_abline(intercept = stats::coef(fit)[[1]], slope = stats::coef(fit)[[2]],
                         color = "#8b3a2b", linewidth = 0.7, linetype = "dashed")
  p +
    scale_x_continuous(breaks = unique(d$Year)) +
    labs(x = NULL, y = ylab, title = sprintf("%s — yearly regional trend", label),
         subtitle = "Points = annual regional mean; dashed line = OLS fit") +
    theme_minimal(base_size = 11) +
    theme(panel.grid.minor = element_blank(),
          plot.title = element_text(size = 12, face = "bold", color = "#2c5a4a"),
          plot.subtitle = element_text(size = 9, color = "#5c6b73"))
}

# Response-vs-driver scatters (one facet per driver), points labelled by year,
# with a per-facet linear fit. `series` is a named list of numeric annual
# vectors; `years` a parallel year vector; `response` the key to put on the y-axis.
gf_correlation_scatter_plot <- function(series, years, response) {
  if (is.null(series) || is.null(response) || is.null(series[[response]])) return(NULL)
  drivers <- setdiff(names(series), response)
  if (!length(drivers)) return(NULL)
  y  <- suppressWarnings(as.numeric(series[[response]]))
  yr <- suppressWarnings(as.numeric(years))
  rows <- list()
  for (dn in drivers) {
    xv <- suppressWarnings(as.numeric(series[[dn]]))
    n  <- min(length(xv), length(y), length(yr))
    if (n < 3) next
    idx <- seq_len(n); ok <- is.finite(xv[idx]) & is.finite(y[idx])
    if (sum(ok) < 3) next
    rows[[dn]] <- data.frame(driver = dn, x = xv[idx][ok], yv = y[idx][ok],
                             year = yr[idx][ok], stringsAsFactors = FALSE)
  }
  if (!length(rows)) return(NULL)
  d <- do.call(rbind, rows)
  d$driver <- factor(d$driver, levels = drivers[drivers %in% d$driver])
  ggplot(d, aes(x = x, y = yv)) +
    geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "#8b3a2b", linewidth = 0.6) +
    geom_point(color = "#2c5a4a", size = 2.3) +
    geom_text(aes(label = year), size = 2.5, vjust = -0.8, color = "#5c6b73") +
    facet_wrap(~ driver, scales = "free_x") +
    labs(x = "Driver value (annual)", y = response,
         title = sprintf("%s vs each driver", response),
         subtitle = "Each point = one year; red line = linear fit") +
    theme_minimal(base_size = 10) +
    theme(strip.text = element_text(face = "bold", size = 9, color = "#26333e"),
          plot.title = element_text(size = 12, face = "bold", color = "#2c5a4a"),
          plot.subtitle = element_text(size = 9, color = "#5c6b73"))
}

# Applies the final, shared "publication cartography" finishing touches to ANY map plot built by
# either function above: coordinate framing, scale bar, north arrow, background-theme choice
# (Clear White / Shaded Light / Dark Minimal), and title. This is the block that was previously
# duplicated at the end of every branch in mod_carto.R's render handler.
apply_cartography_styling <- function(p, bbox, bg_choice = "Clear White", scale_pos = "bl", north_pos = "tr", title = "") {
  bg_fill <- "white"; grid_col <- "grey80"; text_col <- "black"
  if (bg_choice == "Shaded Light") {
    bg_fill <- "#f4f6f9"; grid_col <- "white"
  } else if (bg_choice == "Dark Minimal") {
    bg_fill <- "#2c3e50"; grid_col <- "#34495e"; text_col <- "white"
  }

  p + coord_sf(xlim = c(as.numeric(bbox["xmin"]), as.numeric(bbox["xmax"])), ylim = c(as.numeric(bbox["ymin"]), as.numeric(bbox["ymax"])), expand = FALSE, crs = 4326, default_crs = 4326) +
    ggspatial::annotation_scale(location = scale_pos, width_hint = 0.25, text_cex = 0.8, text_col = text_col) +
    ggspatial::annotation_north_arrow(location = north_pos, which_north = "true",
                                      pad_x = unit(0.1, "in"), pad_y = unit(0.1, "in"),
                                      style = ggspatial::north_arrow_fancy_orienteering(text_col = text_col, line_col = text_col)) +
    theme_minimal(base_family = "sans") +
    theme(
      panel.background = element_rect(fill = bg_fill, color = text_col, linewidth = 1),
      plot.background = element_rect(fill = bg_fill, color = NA),
      panel.grid.major = element_line(color = grid_col, linetype = "dashed"),
      axis.text = element_text(size = 10, color = text_col, face = "bold"),
      axis.title = element_blank(),
      legend.position = "right",
      legend.background = element_rect(fill = alpha("white", 0.8), color = "black"),
      legend.title = element_text(face = "bold", size = 12),
      legend.text = element_text(size = 11, color = "black"),
      plot.title = element_text(face = "bold", size = 18, color = text_col, hjust = 0.5, margin = margin(b = 15))
    ) +
    labs(title = title)
}

# --- 5e. VECTOR/BOUNDARY HELPERS (extraction, dissolve, selection) -------------------------------

# Ensures an sf object is in EPSG:4326 — sets it if genuinely missing (assumes the data actually
# IS lon/lat but just lacked a CRS tag, which was true for every shapefile source in this app),
# or reprojects if it's in some other known CRS. This exact if/else-if block was repeated 6+ times
# across mod_extractor.R and mod_lulc.R — the single most duplicated pattern found in this app.
ensure_crs_4326 <- function(sf_obj) {
  if (is.na(sf::st_crs(sf_obj))) {
    sf::st_crs(sf_obj) <- 4326
  } else if (sf::st_crs(sf_obj) != sf::st_crs(4326)) {
    sf_obj <- sf::st_transform(sf_obj, 4326)
  }
  sf_obj
}

# Dissolves (unions) an sf object's geometries into one feature if requested, otherwise returns it
# unchanged. Used wherever a user has a "Merge/Dissolve selected boundaries" checkbox.
dissolve_if_requested <- function(sf_obj, should_dissolve) {
  if (isTRUE(should_dissolve)) {
    return(sf_obj %>% dplyr::summarise(geometry = sf::st_union(geometry)))
  }
  sf_obj
}

# Returns just the rows matching selected_ids (by id_col) if any are selected, otherwise returns
# the WHOLE active_sf unfiltered — the common "act on selection, or on everything if nothing is
# specifically selected" pattern used by the Shapefile Extractor's clipboard/workspace actions.
# --- 5f. GEE COMPUTE SERVICE (async httr2 request/response helpers) ------------------------------

# Builds an httr2 request to the GEE Compute Service, with the auth bearer token attached if
# available. Does NOT perform the request — callers still call httr2::req_perform_promise() and
# write their own .then()/.catch() handlers, since error-message customization (e.g. "no bands"
# rewritten to a friendlier message) genuinely differs per call-site. This just consolidates the
# identical request-construction boilerplate (URL path, body encoding, timeout, error-passthrough,
# auth token) that was previously repeated at 3 call sites.
build_compute_service_request <- function(path, body_list, is_json = FALSE, timeout_secs = 300) {
  req <- httr2::request(GEE_COMPUTE_API_URL) |> httr2::req_url_path_append(path)
  req <- if (is_json) {
    httr2::req_body_json(req, body_list)
  } else {
    do.call(httr2::req_body_form, c(list(req), body_list))
  }
  req <- req |> httr2::req_timeout(timeout_secs) |> httr2::req_error(is_error = function(resp) FALSE)
  token <- get_gee_service_identity_token()
  if (!is.null(token)) req <- req |> httr2::req_auth_bearer_token(token)
  req
}

# Validates and parses a compute-service httr2 response: confirms it's actually JSON (a non-JSON
# response usually means the request wasn't authenticated), checks for a >=400 status or an
# embedded {error: ...} field, and returns the parsed result list on success. Throws with a
# descriptive message otherwise — this was a word-for-word identical block repeated at 3 call
# sites (analytics, trend, batch), each of which can still layer their OWN call-specific
# error-message rewrites around this in their own tryCatch/%...!% handler.
parse_compute_service_response <- function(resp) {
  ct <- httr2::resp_content_type(resp)
  if (!grepl("json", ct, ignore.case = TRUE)) {
    stop(sprintf("Compute service returned an unexpected response (status %d, type %s) instead of JSON \u2014 this usually means the request wasn't authenticated (common when testing locally against the deployed service; point GEE_COMPUTE_API_URL at a local Plumber server for local testing instead).", httr2::resp_status(resp), ct))
  }
  result <- httr2::resp_body_json(resp)
  if (httr2::resp_status(resp) >= 400 || !is.null(result$error)) {
    stop(result$error %||% paste("Compute service returned status", httr2::resp_status(resp)))
  }
  result
}

# --- 5g. BATCH ZONAL STATS DISPLAY -----------------------------------------------------------

# Renames a batch-zonal-stats results data.frame's columns to their final display names. The base
# 6 columns are always the same, but "Category B" feature-specific metrics (Total Population, %
# Vegetated Area, UHI Intensity) can add extra columns beyond that — this preserves whatever extra
# column names already exist on the data.frame rather than assuming a fixed total column count
# (which was the bug: a fixed-length rename vector silently mismatched once extra columns existed).
rename_batch_results_columns <- function(df, units) {
  unit_suffix <- if (!is.null(units) && nzchar(units)) paste0(" (", units, ")") else ""
  base_names <- c("Boundary", "Area (sq.km)", paste0("Mean", unit_suffix), "95% CI", paste0("Min", unit_suffix), paste0("Max", unit_suffix), paste0("Std. Deviation", unit_suffix))
  extra_names <- if (ncol(df) > 7) names(df)[8:ncol(df)] else character(0)
  names(df) <- c(base_names, extra_names)
  df
}

# Returns just the rows matching selected_ids (by id_col) if any are selected, otherwise returns
# the WHOLE active_sf unfiltered — the common "act on selection, or on everything if nothing is
# specifically selected" pattern used by the Shapefile Extractor's clipboard/workspace actions.
get_selected_or_all <- function(active_sf, selected_ids, id_col = "internal_ext_id") {
  if (length(selected_ids) > 0) {
    return(active_sf[active_sf[[id_col]] %in% selected_ids, ])
  }
  active_sf
}

# --- 5i. EXTRACTED MODULE-INTERNAL LOGIC (Option B: pulled out of moduleServer() closures so it
# can be unit-tested independently, following the same pattern as the earlier shared-function
# consolidation) ---------------------------------------------------------------------------------

# Given an uploaded training-data sf object, detects which column holds the class ID (trying common
# names first, falling back to auto-generating one from the first column if nothing numeric is
# found), and builds the initial Class_ID/Class_Name/Class_Color label table from whatever
# name/color columns exist in the source data (or sensible defaults otherwise). Returns the
# (possibly modified, now with a `uid` column added) train_vect alongside the detected column name
# and label table — the caller (mod_lulc.R) is responsible for assigning these into its own
# reactive values; this function itself has no reactive/session dependency at all.
detect_class_column_and_labels <- function(train_vect) {
  col_names <- names(train_vect)
  possible_names <- c("class_id", "id", "class", "value", "gridcode", "cid")
  target_col <- col_names[tolower(col_names) %in% possible_names][1]
  if (is.na(target_col)) target_col <- col_names[1]
  if (!is.numeric(train_vect[[target_col]])) {
    train_vect$auto_id <- as.integer(as.factor(train_vect[[target_col]]))
    target_col <- "auto_id"
  }

  name_cols <- c("class_name", "classname", "name", "desc", "label", "lulc")
  name_col <- col_names[tolower(col_names) %in% name_cols][1]
  col_cols <- c("color", "colour", "hex", "class_color")
  col_col <- col_names[tolower(col_names) %in% col_cols][1]

  u_ids <- as.integer(as.character(train_vect[[target_col]]))
  u_ids <- sort(unique(u_ids[!is.na(u_ids)]))
  if (length(u_ids) == 0) u_ids <- 1
  def_cols <- colorRampPalette(c("#1E90FF", "#228B22", "#FFD700", "#DC143C", "#8B4513"))(length(u_ids))
  df_labels <- data.frame(Class_ID = integer(), Class_Name = character(), Class_Color = character(), stringsAsFactors = FALSE)

  train_vect$uid <- paste0("up_", sample(10000:99999, nrow(train_vect), replace = TRUE), "_", seq_len(nrow(train_vect)))

  for (i in seq_along(u_ids)) {
    uid <- u_ids[i]
    c_name <- paste("Class", uid)
    c_col <- def_cols[i]
    match_row <- train_vect[train_vect[[target_col]] == uid, ]
    if (nrow(match_row) > 0) {
      if (!is.na(name_col)) {
        ext_name <- as.character(match_row[[name_col]][1])
        if (!is.na(ext_name) && trimws(ext_name) != "") c_name <- trimws(ext_name)
      }
      if (!is.na(col_col)) {
        ext_col <- as.character(match_row[[col_col]][1])
        if (!is.na(ext_col) && trimws(ext_col) != "") c_col <- trimws(ext_col)
      }
    }
    df_labels <- rbind(df_labels, data.frame(Class_ID = as.integer(uid), Class_Name = as.character(c_name), Class_Color = as.character(c_col), stringsAsFactors = FALSE))
  }

  list(train_vect = train_vect, train_col = target_col, class_labels = df_labels)
}

# Decides whether a Publication Map's selected map-type is one of the LULC-related types (which
# must use the boundary SNAPSHOT taken at classifier-training time, not whatever rv$mask_vect
# currently holds) — extracted from mod_carto.R so this decision itself is directly testable.
is_lulc_map_type <- function(carto_map_type) {
  carto_map_type == "LULC Classification Map" ||
    grepl("^LULC - ", carto_map_type) ||
    carto_map_type == "LULC Classification Confidence Map" ||
    carto_map_type == "LULC Change Detection Map"
}

# Builds the Zonal Statistics display table (with a 95% CI computed from pixel-level standard
# error) from a GEE Compute Service "run-analytics" result's stats block — extracted from
# mod_gee.R's render_result() so this calculation is directly testable without needing a live
# compute-service response.
build_zonal_stats_df <- function(stats) {
  ci_95 <- if (!is.na(stats$std_dev) && !is.na(stats$valid_pixels) && stats$valid_pixels > 1) {
    round(1.96 * stats$std_dev / sqrt(stats$valid_pixels), 4)
  } else {
    NA_real_
  }

  data.frame(
    Metric = c("Mean", "95% CI (\u00b1)", "Median", "Minimum", "Maximum", "Std. Deviation", "Total Sum", "Valid Pixels"),
    Value = c(
      round(stats$mean, 3), ci_95, round(stats$median, 3), round(stats$minimum, 3),
      round(stats$maximum, 3), round(stats$std_dev, 3), round(stats$sum, 3), round(stats$valid_pixels, 0)
    )
  )
}

# Builds ONE row of the Batch Zonal Statistics results table from a single compute-service result
# item, including any "Category B" feature-specific extra columns (Total Population, % Vegetated
# Area, UHI Intensity) that are only present for certain features — extracted from mod_gee.R's
# run_batch_stats() row-building lambda so this per-row logic is directly testable.
build_batch_results_row <- function(item, area_lookup) {
  area_val <- if (item$name %in% names(area_lookup)) area_lookup[[item$name]] else NA
  base_row <- data.frame(
    Boundary = item$name,
    `Area (sq.km)` = area_val,
    Mean = round(item$mean %||% NA, 3),
    `95% CI` = if (!is.null(item$ci_95)) round(item$ci_95, 3) else NA,
    Min = round(item$min %||% NA, 3),
    Max = round(item$max %||% NA, 3),
    StdDev = round(item$std_dev %||% NA, 3),
    check.names = FALSE
  )
  if (!is.null(item$total_population)) base_row$`Total Population` <- round(item$total_population, 0)
  if (!is.null(item$percent_vegetated)) base_row$`% Vegetated Area` <- round(item$percent_vegetated, 1)
  if (!is.null(item$uhi_intensity)) base_row$`UHI Intensity (vs batch avg)` <- round(item$uhi_intensity, 2)
  base_row
}

# Computes Z-score-based outlier flags across a set of boundaries' mean values — extracted from
# mod_stats.R's run_outliers() so the statistical logic itself (not the reactive wiring around it)
# is directly testable.
compute_zscore_outliers <- function(boundary_names, mean_col, threshold = 2) {
  z_scores <- (mean_col - mean(mean_col, na.rm = TRUE)) / stats::sd(mean_col, na.rm = TRUE)
  data.frame(
    Boundary = boundary_names,
    Mean = round(mean_col, 3),
    `Z-Score` = round(z_scores, 2),
    Flag = ifelse(abs(z_scores) > threshold, "\u26a0 Outlier", "Normal"),
    check.names = FALSE
  )
}

# Classifies a Pearson correlation's strength/direction/statistical-significance — extracted from
# mod_stats.R's correlation_summary renderUI so this interpretation logic is directly testable
# independent of the HTML-building around it.
interpret_correlation <- function(r, p) {
  strength <- if (abs(r) > 0.7) "Strong" else if (abs(r) > 0.3) "Moderate" else "Weak"
  direction <- if (r > 0) "positive" else "negative"
  list(strength = strength, direction = direction, significant = p < 0.05)
}

# Computes a 95% confidence interval for a mean, given its standard deviation and sample size —
# extracted from mod_stats.R's run_ci() so this calculation (and its "not enough data" guard) is
# directly testable. Throws (rather than returning NA silently) when there isn't enough data,
# matching the original function's error-first behavior.
compute_confidence_interval <- function(mean_val, sd_val, n_val) {
  if (length(mean_val) == 0 || length(sd_val) == 0 || length(n_val) == 0 || is.na(n_val) || n_val <= 1) {
    stop("Not enough data from the last analysis to compute a confidence interval.")
  }
  se <- sd_val / sqrt(n_val)
  list(se = se, lower = mean_val - 1.96 * se, upper = mean_val + 1.96 * se)
}

# Validates that all required boundary-selection inputs are filled in before Shapefile Extractor
# attempts to load boundaries — extracted from mod_extractor.R's ext_load_map observer so this
# validation logic is directly testable. Returns NULL when everything required is present/valid,
# or a user-facing message string describing the FIRST missing/invalid field otherwise.
validate_extractor_load_inputs <- function(db_source, global_level = NULL, global_country = NULL, ext_level = NULL, sel_state = NULL, sel_dist = NULL, sel_taluka = NULL) {
  if (db_source == "global") {
    if (identical(global_level, "states") && (is.null(global_country) || global_country == "Select...")) {
      return("Please select a Country.")
    }
  } else {
    if (is.null(ext_level)) return("Please select an extraction level.")
    if (ext_level %in% c("ADM2", "ADM3", "ADM5") && (is.null(sel_state) || sel_state == "Select...")) {
      return("Please select a State.")
    }
    if (ext_level %in% c("ADM3", "ADM5") && (is.null(sel_dist) || sel_dist == "Select...")) {
      return("Please select a District.")
    }
    if (ext_level == "ADM5" && (is.null(sel_taluka) || sel_taluka == "Select...")) {
      return("Please select a Taluka.")
    }
  }
  NULL
}

