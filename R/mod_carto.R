# =========================================================================
# MODULE 3: CARTOGRAPHY STUDIO (NATIVE GEE RENDERING & SAFE SCALE FIX)
# =========================================================================
mod_carto_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(width = 3, class = "sidebar-panel-custom",
             div(class = "step-card", style = "border-left: 3px solid #2c7a6b;",
                 h4("Cartography Studio", info_tooltip("Generates a publication-ready map (with north arrow, scale bar, legend, and coordinates) from any analysis you've already run in GEE Cloud Analytics, Trend Analysis, or LULC Classification."), class="step-title"),
                 p("Generate publication-ready maps with geographic coordinates, north arrow, and accurate scale bar.", style="font-size:12px; color:#5c6b73; margin-bottom:15px;"),
                 
                 uiOutput(ns("dynamic_carto_selector")),
                 textInput(ns("carto_map_title"), "2. Map Title:", value = "Spatial Analysis Map"),
                 textInput(ns("carto_legend_title"), "3. Legend Title:", value = "Legend"),
                 selectInput(ns("carto_bg"), "4. Map Background:", choices = c("Clear White", "Shaded Light", "Dark Minimal")),
                 
                 hr(style="margin: 10px 0; border-top: 1px dashed #ccc;"),
                 p("Element Positioning:", style="font-weight:600; font-size:12px; margin-bottom:5px; color:#26333e;"),
                 fluidRow(
                   column(6, selectInput(ns("carto_scale_pos"), "Scale Bar:", choices = c("Bottom Left" = "bl", "Bottom Right" = "br", "Top Left" = "tl", "Top Right" = "tr"), selected="bl")),
                   column(6, selectInput(ns("carto_north_pos"), "North Arrow:", choices = c("Top Right" = "tr", "Top Left" = "tl", "Bottom Right" = "br", "Bottom Left" = "bl"), selected="tr"))
                 ),
                 
                 hr(style="margin: 15px 0; border-top: 1px solid #eee;"),
                 actionButton(ns("generate_carto_map"), "Render Publication Map", class="btn-primary btn-custom", style="border:none; padding:12px; font-size:14px;"),
                 
                 hr(style="margin: 20px 0; border-top: 1px dashed #ccc;"),
                 h5("Export Options", style="color:#26333e; font-weight:600; font-size:14px; margin-bottom:10px;"),
                 fluidRow(
                   column(6, actionButton(ns("add_carto_png"), "Save PNG to Manager", class="btn-success btn-custom", style="font-size:12px; padding:6px;")),
                   column(6, actionButton(ns("add_carto_tif"), "Save GeoTIFF to Manager", class="btn-success btn-custom", style="font-size:12px; padding:6px;"))
                 )
             )
      ),
      column(width = 9, class = "main-panel-custom",
             div(style = "background-color: #f3e6de; color: #8b4a2e; padding: 10px 15px; border-left: 5px solid #c1683b; border-radius: 4px; margin-bottom: 15px; font-weight: 500; font-size: 13px;",
                 tags$b("Preview:"), " The faint Gisforus watermark shown on the map below is for preview purposes only and is entirely removed from your final downloaded high-resolution asset."
             ),
             div(style="background:#fff; padding:20px; border-radius:6px; border:1px solid #d8d4c8;", 
                 h4("Map Preview", style="color:#26333e; font-weight:600; margin-top:0; font-size:16px; border-bottom:1px solid #eee; padding-bottom:10px; margin-bottom:15px;"),
                 div(class="protect-wrap", oncontextmenu="return false;",
                     withSpinner(plotOutput(ns("carto_hd_plot"), height="700px"), type=8, color="#26333e"),
                     div(class="glass-overlay")
                 )
             ),
             # =====================================================================
             # MODULAR LAYOUT CANVAS (Phase 1) — add the publication map, charts and
             # galleries as draggable/resizable cards, each with an editable caption.
             # Composition/export is Phase 2. Drag-to-reorder uses SortableJS (CDN) and
             # resize uses a native CSS handle — NO new R package, so nothing for CI/pak
             # to install and no Docker rebuild; it works on the current deployed image.
             # =====================================================================
             tags$style(HTML("
               .gf-canvas { min-height: 120px; display:flex; flex-wrap:wrap; gap:14px; padding:6px; background:#f7f6f2; border:1px dashed #d8d4c8; border-radius:8px; }
               .gf-canvas:empty::before { content:'Add elements above — they appear here as draggable, resizable cards.'; color:#8a97a0; font-size:12.5px; padding:22px; margin:auto; }
               /* Native CSS resize handle (bottom-right) — no JS library needed. */
               .gf-canvas-card { background:#fff; border:1px solid #d8d4c8; border-radius:8px; padding:8px; width:340px; box-shadow:0 2px 8px rgba(38,51,62,0.06); display:flex; flex-direction:column; resize:both; overflow:hidden; min-width:280px; min-height:230px; }
               .gf-canvas-card .gf-cc-head { display:flex; justify-content:space-between; align-items:center; cursor:move; padding:2px 4px 6px; border-bottom:1px solid #eee; margin-bottom:6px; }
               .gf-canvas-card .gf-cc-title { font-size:12px; font-weight:700; color:#26333e; }
               .gf-canvas-card .gf-cc-rm { border:none; background:transparent; color:#8b3a2b; font-weight:700; font-size:15px; cursor:pointer; line-height:1; }
               .gf-canvas-card textarea { font-size:12px; }
               .gf-canvas-card .shiny-plot-output { flex:1 1 auto; min-height:0; }
               .gf-cc-ghost { opacity:0.45; }
             ")),
             # SortableJS (CDN) provides drag-to-reorder; resize is native CSS above.
             # No new R package -> nothing for CI/pak to install; works on the current image.
             tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/Sortable/1.15.2/Sortable.min.js"),
             tags$script(HTML("
               (function(){
                 function reportOrder(el){
                   try {
                     var ids = Array.prototype.slice.call(el.children).map(function(c){ return c.getAttribute('data-uid'); }).filter(Boolean);
                     var inputId = el.id.replace(/canvas_area$/, 'canvas_order');
                     if (window.Shiny && Shiny.setInputValue) Shiny.setInputValue(inputId, ids, {priority:'event'});
                   } catch(e){}
                 }
                 function initCanvasSortable(){
                   if (!window.Sortable) return;
                   document.querySelectorAll('.gf-canvas').forEach(function(el){
                     if (el._gfSortable) return;
                     el._gfSortable = Sortable.create(el, { handle: '.gf-cc-head', animation: 150, ghostClass: 'gf-cc-ghost',
                       onEnd: function(){ reportOrder(el); } });
                   });
                 }
                 document.addEventListener('shiny:connected', function(){ setTimeout(initCanvasSortable, 800); });
                 setInterval(initCanvasSortable, 1500);
               })();
             ")),
             div(style="background:#fff; padding:20px; border-radius:6px; border:1px solid #d8d4c8; margin-top:15px;",
                 div(style="display:flex; justify-content:space-between; align-items:center; flex-wrap:wrap; gap:10px; border-bottom:1px solid #eee; padding-bottom:10px; margin-bottom:12px;",
                     h4("Modular Layout Canvas", style="margin:0; color:#26333e; font-weight:600; font-size:16px;"),
                     div(style="display:flex; gap:8px; align-items:center; flex-wrap:wrap;",
                         div(style="width:210px;", selectInput(ns("canvas_source"), NULL, width="100%",
                             choices = c("Publication Map (current)", "Correlation Chart", "Multivariate Heatmap", "PCA Biplot", "LULC Temporal Gallery", "GEE Temporal Gallery", "Sequential Pipeline (Final Map)"))),
                         actionButton(ns("canvas_add"), "Add Element", class="btn-success", style="padding:8px 14px; border-radius:6px;"),
                         actionButton(ns("canvas_add_text"), "Add Text", class="btn-custom", style="width:auto; margin-bottom:0; padding:8px 14px; background:#eef2f0; border:1px solid #d8d4c8; color:#26333e;")
                     )
                 ),
                 p("Drag a card by its header to reorder, drag its bottom-right corner to resize, and type a caption under each element. Then compose the arranged layout into a single publication figure.", style="font-size:12px; color:#5c6b73; margin-bottom:12px;"),
                 div(id = ns("canvas_area"), class = "gf-canvas"),
                 # ---- Compose & export ----
                 div(style="display:flex; gap:8px; align-items:center; flex-wrap:wrap; margin-top:14px; padding-top:12px; border-top:1px dashed #d8d4c8;",
                     actionButton(ns("canvas_compose"), "Compose Layout", class="btn-primary", style="padding:8px 16px; border-radius:6px; border:none;"),
                     downloadButton(ns("canvas_png"), "Download PNG", class="btn-custom", style="width:auto; margin-bottom:0; padding:8px 14px; background:#eef2f0; border:1px solid #d8d4c8; color:#26333e;"),
                     downloadButton(ns("canvas_pdf"), "Download PDF", class="btn-custom", style="width:auto; margin-bottom:0; padding:8px 14px; background:#eef2f0; border:1px solid #d8d4c8; color:#26333e;"),
                     actionButton(ns("canvas_save_ws"), "Save to Export Manager", class="btn-success", style="padding:8px 14px; border-radius:6px;")
                 ),
                 div(style="margin-top:14px;",
                     withSpinner(plotOutput(ns("canvas_composed_preview"), height="520px"), type=8, color="#26333e")
                 )
             )
      )
    )
  )
}
mod_carto_server <- function(id, rv, gee_rv, carto_rv, cart_rv, add_to_workspace, stats_rv = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    log_err <- function(msg, fix) { showModal(modalDialog(title = HTML("<b style='color:#8b3a2b;'>Action Required</b>"), HTML(paste0("<div style='font-size: 14px;'><p><b>Error:</b> ", msg, "</p><hr><p style='color:#45936f;'><b>Suggestion:</b> ", fix, "</p></div>")), size = "m", easyClose = TRUE, footer = modalButton("Okay"))) }
    
    output$dynamic_carto_selector <- renderUI({
      if(length(rv$available_maps) == 0) {
        selectInput(ns("carto_map_type"), "1. Select Map Category:", choices = c("Run Analysis First" = ""))
      } else {
        selectInput(ns("carto_map_type"), "1. Select Map Category:", choices = rv$available_maps)
      }
    })
    
    # 🚀 REFACTOR: fetch_geotiff_safe() and fetch_png_safe() moved to global.R (shared function
    # library) — previously these were re-defined from scratch inside this observer on EVERY
    # single "Render" click. They now exist once, globally, and are simply called by name here.
    
    observeEvent(input$generate_carto_map, {
      req(input$carto_map_type != "")

      # --- Sequential Pipeline final composite: render the stored ggplot directly
      #     (it is already a finished map layer; just add title/scale/north/bg). ---
      if (identical(input$carto_map_type, "Sequential Pipeline (Final Map)")) {
        tryCatch({
          p0 <- gee_rv$pipeline_final_plot
          if (is.null(p0)) return(showNotification("Run the Sequential Pipeline (GEE module) first — there's no final map yet.", type = "error"))
          bb <- gee_rv$pipeline_final_bbox
          p_clean <- if (!is.null(bb))
            apply_cartography_styling(p0, bb, bg_choice = input$carto_bg, scale_pos = input$carto_scale_pos,
                                      north_pos = input$carto_north_pos, title = input$carto_map_title)
            else p0
          carto_rv$plot_clean   <- p_clean
          carto_rv$plot_preview <- p_clean +
            annotation_custom(grid::textGrob("Gisforus", x = 0.5, y = 0.5, rot = 30,
              gp = grid::gpar(col = "#26333e", fontsize = 54, fontface = "bold", alpha = 0.18)))
          carto_rv$plot <- p_clean
          showNotification("Pipeline final map rendered.", type = "message", duration = 4)
        }, error = function(e) showNotification(paste("Pipeline map render failed:", conditionMessage(e)), type = "error"))
        return()
      }

      if(is.null(rv$mask_vect)) return(log_err("Missing Boundary", "Please upload or fetch a boundary in previous steps."))
      if(!check_rate_limit(rv, "generate_carto_map", cooldown_seconds = 10)) return()
      
      # 🚀 FIX: boundary-confusion fix — LULC-related map types use the boundary SNAPSHOT taken
      # at classifier-training time (rv$lulc_boundary_used), not whatever rv$mask_vect currently
      # holds (which other modules like GEE Cloud Analytics can silently change afterward).
      # 🚀 REFACTOR (Option B): uses the extracted, independently unit-tested is_lulc_map_type()
      # (global.R) instead of inline logic repeated at this decision point.
      is_lulc_map <- is_lulc_map_type(input$carto_map_type)
      boundary_for_render <- if (is_lulc_map) (rv$lulc_boundary_used %||% rv$mask_vect) else rv$mask_vect
      
      log_event("INFO", "mod_carto", "publication_map_requested", session_id = session$token,
                map_type = input$carto_map_type, is_lulc_map = is_lulc_map,
                used_snapshot_boundary = is_lulc_map && !is.null(rv$lulc_boundary_used))
      
      dyn_scale <- get_dynamic_scale(boundary_for_render)
      
      bbox_m <- sf::st_bbox(sf::st_transform(boundary_for_render, 3857))
      max_dim_m <- max(as.numeric(bbox_m["xmax"] - bbox_m["xmin"]), as.numeric(bbox_m["ymax"] - bbox_m["ymin"]))
      safe_scale <- max(dyn_scale, max_dim_m / 1000) 
      
      # 🚀 COST FIX: print/document-optimized resolution tiers (150 DPI-equivalent) — far fewer
      # total pixels to fetch from GEE and render locally than screen-zoom quality, with no
      # visible quality loss at normal reading distance in a report or PDF.
      smart_dim <- if (max_dim_m > 700000) 300 else if (max_dim_m > 300000) 450 else if (max_dim_m > 200000) 600 else if (max_dim_m > 50000) 900 else 1200
      
      b <- sf::st_bbox(sf::st_transform(boundary_for_render, 4326))
      v <- terra::vect(boundary_for_render)
      
      old_to <- getOption("timeout")
      options(timeout = 3600) # 1 hour
      on.exit(options(timeout = old_to))
      
      withProgress(message = "Rendering High-Res Cartography...", value=0.5, {
        tryCatch({
          ee_roi <- sf_as_ee(boundary_for_render)
          p <- NULL
          r_final <- NULL
          
          if(input$carto_map_type == "LULC Classification Map" || grepl("^LULC - ", input$carto_map_type)) {
            
            if(input$carto_map_type == "LULC Classification Map") {
              img_hd <- rv$gee_landsat_img
            } else {
              parts <- strsplit(input$carto_map_type, "- ")[[1]][2]
              mo_str <- strsplit(parts, " ")[[1]][1]
              yr_str <- strsplit(parts, " ")[[1]][2]
              mo <- match(mo_str, month.abb)
              yr <- as.numeric(yr_str)
              
              s_d <- sprintf("%04d-%02d-01", yr, mo)
              e_d <- as.character(seq(as.Date(s_d), length=2, by="4 months")[2] - 1)
              
              col <- get_landsat89_collection(s_d, e_d, ee_roi)
              if (is.null(col) || col$size()$getInfo() == 0) stop("No Landsat imagery available to re-render this year.")
              img_hd <- col$median()$clip(ee_roi)
            }
            
            # 🚀 REFACTOR: uses shared add_selected_indices() (global.R) instead of inline-repeating
            # the NDVI/NDWI/EVI/NDBI if-blocks (this exact sequence was duplicated 3 times before
            # being consolidated: here, in mod_lulc.R's Temporal Grid, and in the Change Detection
            # branch below).
            idx_res <- add_selected_indices(img_hd, rv$training_indices_used %||% character(0))
            img_hd <- idx_res$img
            
            # 🚀 REFACTOR: uses shared apply_saved_normalization() (global.R) — same normalization
            # applied at training time, instead of a hand-repeated mins/maxs constant-image block.
            img_hd_norm <- apply_saved_normalization(img_hd, rv$training_bands, rv$norm_mins, rv$norm_maxs)
            
            classified_no_reproject <- img_hd_norm$classify(rv$trained_classifier)$select(0)$toInt()$clip(ee_roi)
            
            # 🚀 REFACTOR: uses shared get_classification_vis() (global.R) — same remap/palette
            # logic previously duplicated here and (twice more) in mod_lulc.R.
            vis <- get_classification_vis(classified_no_reproject, rv$class_labels)
            
            tmp_png <- fetch_png_safe(vis$vis_img, ee_roi$geometry(), smart_dim)
            img_arr <- png::readPNG(tmp_png)
            
            # 🚀 REFACTOR: uses shared build_categorical_map_plot() (global.R)
            p <- build_categorical_map_plot(img_arr, b, boundary_for_render, rv$class_labels, legend_title = input$carto_legend_title)
            
            # 🚀 FIX: "User memory limit exceeded" — no eager reproject() before GeoTIFF export;
            # crs is passed directly to fetch_geotiff_safe(), letting GEE handle CRS-reprojection
            # lazily as part of its own export computation instead of a separate, memory-heavy
            # eager step beforehand.
            tmp_tif <- fetch_geotiff_safe(classified_no_reproject, ee_roi$geometry(), safe_scale, crs = "EPSG:4326")
            r <- terra::rast(tmp_tif)
            if(terra::crs(r) != terra::crs(v)) v <- terra::project(v, terra::crs(r))
            r_final <- terra::mask(terra::crop(r, v), v)
            
          } else if (input$carto_map_type == "LULC Classification Confidence Map") {
            if (is.null(rv$trained_classifier_prob) || is.null(rv$classification_ready_img)) stop("Confidence map data not available — generate it first in LULC Engine ('Show Classification Confidence').")
            
            img <- rv$classification_ready_img$select(rv$training_bands)$classify(rv$trained_classifier_prob)$arrayReduce(ee$Reducer$max(), list(0L))$arrayFlatten(list(list("confidence")))$clip(ee_roi)
            map_min <- 0; map_max <- 1
            pal_use <- c('#d73027', '#fee08b', '#1a9850')
            
            tmp_tif <- fetch_geotiff_safe(img$toFloat(), ee_roi$geometry(), safe_scale)
            r <- terra::rast(tmp_tif)
            if(terra::crs(r) != terra::crs(v)) v <- terra::project(v, terra::crs(r))
            r_final <- terra::mask(terra::crop(r, v), v)
            
            vis_img <- img$visualize(min = map_min, max = map_max, palette = pal_use)
            tmp_png <- fetch_png_safe(vis_img, ee_roi$geometry(), smart_dim)
            img_arr <- png::readPNG(tmp_png)
            
            # 🚀 REFACTOR: uses shared build_continuous_map_plot() (global.R)
            p <- build_continuous_map_plot(img_arr, b, boundary_for_render, map_min, map_max, pal_use, legend_title = input$carto_legend_title)
            
          } else if (input$carto_map_type == "LULC Change Detection Map") {
            if (is.null(rv$change_detection_years) || is.null(rv$trained_classifier) || is.null(boundary_for_render)) stop("Change detection data not available — generate it first in LULC Engine (Temporal Gallery).")
            
            y1_str <- rv$change_detection_years[1]; y2_str <- rv$change_detection_years[2]
            
            # 🚀 REFACTOR: uses shared fetch_year_landsat_image() (global.R) — combines collection
            # fetch + median + clip + indices + normalization in one call, replacing a hand-repeated
            # inline sequence that was duplicated across this branch, the LULC branch above, and
            # mod_lulc.R's Temporal Grid.
            classify_year <- function(yr_str) {
              img_ready <- fetch_year_landsat_image(as.numeric(yr_str), 1, ee_roi, rv$training_indices_used %||% character(0), rv$training_bands, rv$norm_mins, rv$norm_maxs)
              img_ready$classify(rv$trained_classifier)
            }
            
            img1 <- classify_year(y1_str)
            img2 <- classify_year(y2_str)
            # 🚀 REFACTOR: uses shared compute_class_change_mask() (global.R)
            img <- compute_class_change_mask(img1, img2, min_patch_pixels = 9)$rename("change")
            
            tmp_tif <- fetch_geotiff_safe(img$toFloat(), ee_roi$geometry(), safe_scale)
            r <- terra::rast(tmp_tif)
            if(terra::crs(r) != terra::crs(v)) v <- terra::project(v, terra::crs(r))
            r_final <- terra::mask(terra::crop(r, v), v)
            
            vis_img <- img$visualize(palette = c('#e74c3c'))
            tmp_png <- fetch_png_safe(vis_img, ee_roi$geometry(), smart_dim)
            img_arr <- png::readPNG(tmp_png)
            
            p <- ggplot() +
              annotation_custom(grid::rasterGrob(img_arr, width=unit(1,"npc"), height=unit(1,"npc"), interpolate=TRUE),
                                xmin = as.numeric(b["xmin"]), xmax = as.numeric(b["xmax"]), ymin = as.numeric(b["ymin"]), ymax = as.numeric(b["ymax"])) +
              geom_sf(data = sf::st_as_sf(boundary_for_render), fill = NA, color = "black", linewidth = 0.5) +
              labs(caption = sprintf("Red = changed class (%s \u2192 %s)", y1_str, y2_str))
            
          } else {
            recipe <- gee_rv$saved_images[[input$carto_map_type]]
            if(is.null(recipe)) stop("Error: Analyzed image not found in memory.")
            
            # 🚀 FIX: date-specific/GEE-analytics map boundary mismatch — img (from
            # resolve_saved_image()) is computed against recipe$roi_wkt (the boundary that was
            # ACTIVE when the analysis was originally run), but the crop/frame/export variables
            # (ee_roi, b, v, safe_scale, smart_dim) computed OUTSIDE this branch are based on
            # whatever rv$mask_vect is CURRENTLY set to. If the user has since switched to a
            # different boundary elsewhere in the app (e.g. trained LULC on one boundary, then
            # selected a different one in GEE Cloud Analytics), those two would silently mismatch
            # — the actual data clipped to one geometry, the export/render frame built from
            # another — producing a misaligned, wrongly-cropped, or blank-looking map. Re-deriving
            # every geometry-related variable from recipe$roi_wkt specifically for this branch
            # guarantees the data and the frame always describe the SAME boundary.
            recipe_boundary <- sf::st_as_sf(sf::st_as_sfc(recipe$roi_wkt, crs = 4326))
            ee_roi <- sf_as_ee(recipe_boundary)
            b <- sf::st_bbox(sf::st_transform(recipe_boundary, 4326))
            v <- terra::vect(recipe_boundary)
            
            # Diagnostic only — confirms whether the CURRENT rv$mask_vect differs from the
            # boundary this recipe was actually run against, so this exact class of mismatch is
            # visible in Cloud Run logs going forward rather than only surfacing as a silent
            # misrendered map.
            current_boundary_differs <- tryCatch({
              !isTRUE(all.equal(sf::st_bbox(sf::st_transform(rv$mask_vect, 4326)), b, tolerance = 1e-6))
            }, error = function(e) NA)
            log_event("INFO", "mod_carto", "gee_recipe_boundary_check", session_id = session$token,
                      map_type = input$carto_map_type, current_boundary_differs_from_recipe = current_boundary_differs)
            
            recipe_bbox_m <- sf::st_bbox(sf::st_transform(recipe_boundary, 3857))
            recipe_max_dim_m <- max(as.numeric(recipe_bbox_m["xmax"] - recipe_bbox_m["xmin"]), as.numeric(recipe_bbox_m["ymax"] - recipe_bbox_m["ymin"]))
            safe_scale <- max(get_dynamic_scale(recipe_boundary), recipe_max_dim_m / 1000)
            smart_dim <- if (recipe_max_dim_m > 700000) 300 else if (recipe_max_dim_m > 300000) 450 else if (recipe_max_dim_m > 200000) 600 else if (recipe_max_dim_m > 50000) 900 else 1200
            
            img <- resolve_saved_image(recipe)
            
            base_feature_name <- gsub(" - .*$", "", input$carto_map_type)
            saved_vis <- gee_rv$saved_vis[[input$carto_map_type]]
            if(is.null(saved_vis)) saved_vis <- gee_rv$saved_vis[[base_feature_name]]
            if(is.null(saved_vis) || is.null(saved_vis$pal)) stop("Visualization palette missing.")
            
            # 🚀 FIX: crs passed directly (global.R's get_feature_img() no longer reprojects
            # eagerly — same "User memory limit exceeded" fix as the LULC branch above).
            tmp_tif <- fetch_geotiff_safe(img$toFloat(), ee_roi$geometry(), safe_scale, crs = "EPSG:4326")
            r <- terra::rast(tmp_tif)
            if(terra::crs(r) != terra::crs(v)) v <- terra::project(v, terra::crs(r))
            r_final <- terra::mask(terra::crop(r, v), v)
            
            df <- as.data.frame(r_final, xy = TRUE, na.rm = TRUE)
            colnames(df)[3] <- "Value"
            
            local_p2 <- as.numeric(quantile(df$Value, 0.02, na.rm = TRUE))
            local_p98 <- as.numeric(quantile(df$Value, 0.98, na.rm = TRUE))
            
            map_min <- if(is.na(local_p2)) saved_vis$min else local_p2
            map_max <- if(is.na(local_p98)) saved_vis$max else local_p98
            if(map_min >= map_max) map_max <- map_min + 1
            
            vis_img <- img$visualize(min=map_min, max=map_max, palette=saved_vis$pal)
            tmp_png <- fetch_png_safe(vis_img, ee_roi$geometry(), smart_dim)
            img_arr <- png::readPNG(tmp_png)
            
            # 🚀 REFACTOR: uses shared build_continuous_map_plot() (global.R). Uses
            # recipe_boundary (not boundary_for_render) as the outline drawn on the map, matching
            # the recipe-derived b/ee_roi/v above.
            p <- build_continuous_map_plot(img_arr, b, recipe_boundary, map_min, map_max, saved_vis$pal, legend_title = input$carto_legend_title)
          }
          
          # 🚀 REFACTOR: uses shared apply_cartography_styling() (global.R) — the coord_sf/
          # scale-bar/north-arrow/background-theme/title "finishing" block previously duplicated
          # at the end of every branch above.
          p_clean <- apply_cartography_styling(p, b, bg_choice = input$carto_bg, scale_pos = input$carto_scale_pos, north_pos = input$carto_north_pos, title = input$carto_map_title)
          
          watermark_text <- "Gisforus"
          p_preview <- p_clean +
            annotation_custom(grid::textGrob(watermark_text, x = 0.5, y = 0.5, rot = 30, gp = grid::gpar(col = "#26333e", fontsize = 54, fontface = "bold", alpha = 0.18)))
          
          carto_rv$plot_clean <- p_clean
          carto_rv$plot_preview <- p_preview
          carto_rv$plot <- p_clean
          carto_rv$raster <- r_final
          
          log_event("INFO", "mod_carto", "publication_map_rendered", session_id = session$token,
                    map_type = input$carto_map_type, smart_dim = smart_dim, safe_scale = round(safe_scale, 1))
          
        }, error = function(e) { 
          log_event("ERROR", "mod_carto", "publication_map_failed", session_id = session$token,
                    map_type = input$carto_map_type, error = conditionMessage(e))
          showNotification(paste("Failed to generate publication map:", e$message), type="error") 
        })
      })
    })
    
    output$carto_hd_plot <- renderPlot({
      shiny::validate(shiny::need(carto_rv$plot_preview, "Select a map and click Render Publication Map."))
      carto_rv$plot_preview
    })

    # =====================================================================
    # MODULAR LAYOUT CANVAS (Phase 2) — server
    # The canvas MODEL (items, order, caption/body text) lives in carto_rv so it
    # rides the reactiveValues snapshot cache and can be rebuilt after a
    # reconnect/reload. Figure cards re-render their backing ggplot (no
    # recomputation). "Compose" arranges the cards + captions into a single
    # cowplot grid (no new package) for print-quality PNG/PDF export and the
    # Export Manager. Drag-reorder = SortableJS (order reported to the server);
    # resize = native CSS.
    # =====================================================================
    # Canvas MODEL is LOCAL (not in the shared carto_rv) so it never touches the
    # boot-time snapshot reactive graph. Only the COMPOSED figure is published to
    # carto_rv (for the Export Manager ZIP). Cross-reload canvas persistence is a
    # deliberate follow-up — kept out of here to keep app boot rock-stable.
    canvas_rv  <- reactiveValues(items = list(), order = NULL)
    canvas_seq <- reactiveVal(0)

    # Map a palette label to its backing ggplot (NULL if not generated yet).
    canvas_fig_for <- function(src) {
      switch(src,
        "Publication Map (current)" = carto_rv$plot_clean,
        "Correlation Chart"         = if (!is.null(stats_rv)) stats_rv$corr_plot else NULL,
        "Multivariate Heatmap"      = if (!is.null(stats_rv) && !is.null(stats_rv$mv_res)) tryCatch(mv_corr_heatmap(stats_rv$mv_res), error = function(e) NULL) else NULL,
        "PCA Biplot"                = if (!is.null(stats_rv) && !is.null(stats_rv$mv_res)) tryCatch(mv_pca_biplot(stats_rv$mv_res), error = function(e) NULL) else NULL,
        "LULC Temporal Gallery"     = rv$temporal_combined_plot,
        "GEE Temporal Gallery"      = gee_rv$temporal_combined_plot,
        "Sequential Pipeline (Final Map)" = gee_rv$pipeline_final_plot,
        NULL)
    }

    # Insert one card into the DOM and register its outputs/observers. Used BOTH
    # for a fresh add and for rebuilding from a restored snapshot (initial values
    # come from the stored model so captions/notes survive a reload).
    render_card <- function(uid, type, src, caption = "", body_text = "") {
      card_id <- ns(paste0("card_", uid))
      if (identical(type, "figure")) {
        local({
          u <- uid; s <- src
          output[[paste0("fig_", u)]] <- renderPlot({
            p <- canvas_fig_for(s)
            shiny::validate(shiny::need(!is.null(p), paste0("'", s, "' isn't ready yet — generate it in its own module first.")))
            p
          })
        })
        body  <- plotOutput(ns(paste0("fig_", uid)), height = "260px")
        title <- src
      } else {
        body  <- textAreaInput(ns(paste0("txt_", uid)), NULL, value = body_text %||% "", placeholder = "Section heading or note…", width = "100%", height = "120px")
        title <- "Text / Note"
      }
      card <- div(id = card_id, class = "gf-canvas-card", `data-uid` = uid,
        div(class = "gf-cc-head",
            tags$span(class = "gf-cc-title", title),
            tags$button(class = "gf-cc-rm", type = "button",
                        onclick = sprintf("Shiny.setInputValue('%s', '%s', {priority:'event'})", ns("canvas_remove"), uid), HTML("&times;"))),
        body,
        textAreaInput(ns(paste0("cap_", uid)), NULL, value = caption %||% "", placeholder = "Add a caption / description…", width = "100%", height = "56px")
      )
      insertUI(selector = paste0("#", ns("canvas_area")), where = "beforeEnd", immediate = TRUE, ui = card)
      local({
        u <- uid
        observeEvent(input[[paste0("cap_", u)]], {
          if (!is.null(canvas_rv$items[[u]])) canvas_rv$items[[u]]$caption <- input[[paste0("cap_", u)]]
        }, ignoreInit = TRUE)
        if (identical(type, "text")) observeEvent(input[[paste0("txt_", u)]], {
          if (!is.null(canvas_rv$items[[u]])) canvas_rv$items[[u]]$body <- input[[paste0("txt_", u)]]
        }, ignoreInit = TRUE)
      })
    }

    add_card <- function(type, src = NULL) {
      n <- canvas_seq() + 1; canvas_seq(n)
      uid <- paste0("cc", n)
      canvas_rv$items[[uid]] <- list(type = type, source = src, caption = "", body = "")
      render_card(uid, type, src)
    }

    observeEvent(input$canvas_add,      add_card("figure", input$canvas_source))
    observeEvent(input$canvas_add_text, add_card("text"))
    observeEvent(input$canvas_remove, {
      uid <- input$canvas_remove
      if (is.null(uid)) return()
      canvas_rv$items[[uid]] <- NULL
      removeUI(selector = paste0("#", ns(paste0("card_", uid))), immediate = TRUE)
    })
    # SortableJS reports the arranged order of card uids after each drag.
    observeEvent(input$canvas_order, { canvas_rv$order <- input$canvas_order })

    # ---- Compose the arranged cards + captions into one figure (cowplot) ----
    compose_canvas <- function() {
      items <- canvas_rv$items
      if (is.null(items) || !length(items)) return(NULL)
      ord  <- canvas_rv$order
      uids <- if (!is.null(ord) && length(ord)) c(ord[ord %in% names(items)], setdiff(names(items), ord)) else names(items)
      panels <- lapply(uids, function(u) {
        it <- items[[u]]; if (is.null(it)) return(NULL)
        if (identical(it$type, "figure")) {
          p <- canvas_fig_for(it$source)
          if (is.null(p)) return(NULL)
          cap <- it$caption
          if (!is.null(cap) && nzchar(cap)) p <- p + ggplot2::labs(caption = cap) +
            ggplot2::theme(plot.caption = ggplot2::element_text(hjust = 0, size = 9, colour = "#5c6b73"))
          p
        } else {
          txt <- it$body; if (is.null(txt) || !nzchar(txt)) txt <- "(empty note)"
          cowplot::ggdraw() + cowplot::draw_label(txt, x = 0.02, y = 0.95, hjust = 0, vjust = 1, size = 12, colour = "#26333e")
        }
      })
      panels <- Filter(Negate(is.null), panels)
      if (!length(panels)) return(NULL)
      cowplot::plot_grid(plotlist = panels, ncol = min(2L, length(panels)))
    }

    observeEvent(input$canvas_compose, {
      comp <- tryCatch(compose_canvas(), error = function(e) NULL)
      if (is.null(comp)) return(showNotification("Add at least one element with generated content before composing.", type = "warning"))
      carto_rv$canvas_composed <- comp
      showNotification("Layout composed — preview below; export as PNG/PDF or save to the Export Manager.", type = "message", duration = 6)
    })
    output$canvas_composed_preview <- renderPlot({
      shiny::validate(shiny::need(!is.null(carto_rv$canvas_composed), "Click 'Compose Layout' to preview the combined figure here."))
      carto_rv$canvas_composed
    })

    output$canvas_png <- downloadHandler(
      filename = function() paste0("publication_canvas_", format(Sys.time(), "%Y%m%d_%H%M"), ".png"),
      content  = function(file) {
        comp <- carto_rv$canvas_composed %||% tryCatch(compose_canvas(), error = function(e) NULL)
        shiny::validate(shiny::need(!is.null(comp), "Nothing to export — compose the layout first."))
        ggplot2::ggsave(file, comp, width = 11, height = 8.5, dpi = 200, bg = "white", limitsize = FALSE)
      }
    )
    output$canvas_pdf <- downloadHandler(
      filename = function() paste0("publication_canvas_", format(Sys.time(), "%Y%m%d_%H%M"), ".pdf"),
      content  = function(file) {
        comp <- carto_rv$canvas_composed %||% tryCatch(compose_canvas(), error = function(e) NULL)
        shiny::validate(shiny::need(!is.null(comp), "Nothing to export — compose the layout first."))
        ggplot2::ggsave(file, comp, width = 11, height = 8.5, device = "pdf", bg = "white", limitsize = FALSE)
      }
    )
    observeEvent(input$canvas_save_ws, {
      comp <- carto_rv$canvas_composed %||% tryCatch(compose_canvas(), error = function(e) NULL)
      if (is.null(comp)) return(showNotification("Compose the layout first, then save it.", type = "error"))
      carto_rv$canvas_composed <- comp
      add_to_workspace(paste0("carto_canvas_", as.integer(Sys.time())), "Publication Canvas Composition (.png)", 0.50)
    })
    
    observeEvent(input$add_carto_png, { 
      if(!is.null(carto_rv$plot_clean)) {
        add_to_workspace(paste0("carto_png_", as.integer(Sys.time())), paste("Publication Map (PNG):", input$carto_map_type), 0.50) 
      } else {
        showNotification("Generate a Map first.", type="error") 
      }
    })
    
    observeEvent(input$add_carto_tif, { 
      if(!is.null(carto_rv$raster)) {
        add_to_workspace(paste0("carto_tif_", as.integer(Sys.time())), paste("Publication Map (GeoTIFF):", input$carto_map_type), 0.50) 
      } else {
        showNotification("Generate a Map first.", type="error") 
      }
    })
  })
}