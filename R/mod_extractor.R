# =========================================================================
# MODULE 1: SHAPEFILE EXTRACTOR (LAZY LOAD & CRASH-PROOF)
# =========================================================================
mod_extractor_ui <- function(id) {
  ns <- NS(id)
  tagList(
    useShinyjs(), 
    fluidRow(
      column(width = 4, class = "sidebar-panel-custom",
             tabsetPanel(
               tabPanel("Database Extractor",
                        div(class = "step-card", style = "margin-top:15px; border-left: 3px solid #c1683b;", 
                            h4("1. Data Source", info_tooltip("Choose which administrative boundary dataset to browse — India's admin levels (State/District/Tehsil) or Global country/state boundaries."), class = "step-title"), 
                            radioButtons(ns("db_source"), NULL, choices = c("India Database (Local)" = "local", "Global Repository (Natural Earth)" = "global"), inline = FALSE),
                            actionButton(ns("ext_connect_db"), "Initialize Database", class = "btn-warning btn-custom", style="color:#fff;")
                        ),
                        div(class = "step-card", style = "border-left: 3px solid #4a83c4;", 
                            h4("2. Extractor", info_tooltip("Search or click on the map to select one or more boundary features. Selected features highlight on the map and can be sent to the Clipboard for use elsewhere in the app."), class = "step-title"), 
                            
                            # LOCAL INDIA UI
                            conditionalPanel(sprintf("input['%s'] == 'local'", ns("db_source")),
                                             selectInput(ns("ext_level"), "Extraction Level:", choices = c("Level 0: Country (India)" = "ADM0", "Level 1: State(s)" = "ADM1", "Level 2: District(s)" = "ADM2", "Level 3: Taluka(s) / Sub-Districts" = "ADM3", "Level 5: Village(s)" = "ADM5"), selected = "ADM2"), 
                                             conditionalPanel(sprintf("input['%s'] == 'ADM2' || input['%s'] == 'ADM3' || input['%s'] == 'ADM5'", ns("ext_level"), ns("ext_level"), ns("ext_level")), selectizeInput(ns("sel_state"), "Filter State:", choices = NULL, options = list(placeholder = "Type to search a state…"))),
                                             conditionalPanel(sprintf("input['%s'] == 'ADM3' || input['%s'] == 'ADM5'", ns("ext_level"), ns("ext_level")), selectizeInput(ns("sel_dist"), "Filter District:", choices = NULL, options = list(placeholder = "Type to search a district…"))),
                                             conditionalPanel(sprintf("input['%s'] == 'ADM5'", ns("ext_level")), selectizeInput(ns("sel_taluka"), "Filter Taluka:", choices = NULL, options = list(placeholder = "Type to search a taluka…")))
                            ),
                            
                            # GLOBAL NATURAL EARTH UI
                            conditionalPanel(sprintf("input['%s'] == 'global'", ns("db_source")),
                                             selectInput(ns("global_level"), "Extraction Level:", choices = c("Level 0: World Countries" = "world", "Level 1: States / Provinces" = "states"), selected = "states"),
                                             conditionalPanel(sprintf("input['%s'] == 'states'", ns("global_level")),
                                                              selectizeInput(ns("global_country"), "Filter Country:", choices = NULL, options = list(placeholder = "Type to search a country…"))
                                             )
                            ),
                            
                            hr(style="margin: 15px 0; border-top: 1px solid #eee;"), 
                            actionButton(ns("ext_load_map"), "Load Boundaries on Map", class = "btn-primary btn-custom")
                        ),
                        div(class = "step-card", style = "border-left: 3px solid #45936f;", h4("3. Selection & Actions", class = "step-title"), p("Selected Features:", style = "font-weight:500; margin-bottom:5px; color:#3d4f5c;"), verbatimTextOutput(ns("ext_selected_names"), placeholder = TRUE), uiOutput(ns("ext_metrics")), hr(style="margin: 15px 0; border-top: 1px solid #eee;"), checkboxInput(ns("ext_dissolve"), tags$b("Merge Selected Boundaries (Dissolve)"), value = FALSE), actionButton(ns("send_floating_ext"), "Send Boundary to Clipboard", class="btn-custom", style="background-color: #e8f0ea; border: 1px solid #d8d4c8; color: #26333e;"), fluidRow(column(5, selectInput(ns("ext_export_fmt"), label = NULL, choices = c("Shapefile (.zip)"="shp", "GeoJSON"="geojson", "KML"="kml"))), column(7, actionButton(ns("add_cart_ext_shp"), "Save to Workspace", class="btn-success", style="width:100%; height: 38px; border-radius: 4px;"))), actionButton(ns("ext_clear_sel"), "Clear Selection", class="btn-light btn-custom", style="border: 1px solid #ccc; margin-bottom:0;"))
               ),
               tabPanel("Custom Draw ROI",
                        div(class = "step-card", style = "margin-top:15px; border-left: 3px solid #6b4c7a;",
                            h5("Draw your custom area directly on the map using the polygon tool.", style="color:#5c6b73; font-size:12px; line-height:1.4;"), hr(style="margin: 10px 0;"),
                            textInput(ns("custom_roi_name"), "Name your Region:", value = "My_Custom_ROI"),
                            actionButton(ns("save_drawn_roi"), "Save Drawn ROI to Clipboard", class="btn-custom", style="background:#6b4c7a; color:#fff; border:none;"),
                            actionButton(ns("add_cart_drawn_roi"), "Save ROI to Workspace", class="btn-success btn-custom"),
                            actionButton(ns("clear_draw"), "Clear Drawing", class="btn-light btn-custom", style="border:1px solid #ccc;")
                        )
               )
             )
      ),
      column(width = 8, class = "main-panel-custom",
             div(style = "background-color: #ffffff; padding: 12px 20px; border-radius: 6px; border: 1px solid #d8d4c8; margin-bottom: 15px; display: flex; justify-content: space-between; align-items: center;", div(h4("Reference Map", style="margin:0; font-weight:600; font-size:16px; color:#26333e;"), p("Navigate and click on polygons to extract or draw your own.", style="margin:4px 0 0 0; font-size:12px; color:#5c6b73;")), div(style = "width: 280px;", selectizeInput(ns("ext_search"), label = NULL, choices = NULL, options = list(placeholder = 'Search Map Index...')))),
             withSpinner(leafletOutput(ns("ext_map"), height="70vh"), type = 8, color = "#26333e")
      )
    )
  )
}
mod_extractor_server <- function(id, ext_rv, floating_rv, cart_rv, add_to_workspace) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    log_err <- function(msg, fix) { showModal(modalDialog(title = HTML("<b style='color:#8b3a2b;'>Action Required</b>"), HTML(paste0("<div style='font-size: 14px;'><p><b>Error:</b> ", msg, "</p><hr><p style='color:#45936f;'><b>Suggestion:</b> ", fix, "</p></div>")), size = "m", easyClose = TRUE, footer = modalButton("Okay"))) }
    
    # 🚀 ANTI-HANG FIX: Render Leaflet immediately without req()
    output$ext_map <- renderLeaflet({ 
      leaflet() %>% 
        addProviderTiles(providers$CartoDB.Positron, group="Clean Map") %>% 
        addProviderTiles(providers$Esri.WorldTopoMap, group="Physical") %>% 
        addProviderTiles(providers$Esri.WorldImagery, group="Satellite") %>% 
        addLayersControl(baseGroups=c("Clean Map", "Physical", "Satellite")) %>% 
        setView(78.96, 20.59, 5) %>% 
        addDrawToolbar(targetGroup="drawn_roi", polylineOptions=FALSE, polygonOptions=drawPolygonOptions(), circleOptions=FALSE, rectangleOptions=drawRectangleOptions(), markerOptions=FALSE, circleMarkerOptions=FALSE) %>% 
        htmlwidgets::onRender(js_coords) %>% 
        inject_map_elements("Boundary Extractor")
    })
    
    observeEvent(input$ext_map_draw_new_feature, {
      feat <- input$ext_map_draw_new_feature
      poly_sf <- geojsonio::geojson_sf(jsonlite::toJSON(feat, auto_unbox=TRUE, force=TRUE))
      ext_rv$drawn_poly <- poly_sf
      showNotification("Custom Polygon Captured!", type="message")
    })
    
    observeEvent(input$clear_draw, { ext_rv$drawn_poly <- NULL; leafletProxy("ext_map") %>% clearGroup("drawn_roi") })
    
    # 🚀 MOBILE FIX (loading trap): this used to run st_read() SYNCHRONOUSLY inside
    # withProgress(), blocking the single R thread for several seconds. On mobile the block
    # outlasts the browser's websocket tolerance, so Shiny shows its permanent grey
    # "disconnected" overlay. Dispatching to a future worker (same pattern as ext_load_map)
    # keeps the main thread free and the connection alive, so the overlay never appears.
    observeEvent(input$ext_connect_db, {
      db_source <- input$db_source
      set_busy(session, "Initializing database... (app stays usable — feel free to keep working)")
      progress <- shiny::Progress$new(session)
      progress$set(message = if (db_source == "global") "Connecting to Global API..." else "Initializing Secure Database...", value = NULL)

      fut <- future::future({
        if (db_source == "global") {
          world_sf <- rnaturalearth::ne_countries(scale = 50, returnclass = "sf")
          list(kind = "global", world_sf = world_sf, countries = sort(unique(world_sf$admin)))
        } else {
          all_shps <- list.files(path = ".", pattern = "\\.shp$", recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
          shp1_files <- all_shps[grepl("ADM1", all_shps, ignore.case = TRUE)]
          shp2_files <- all_shps[grepl("ADM2", all_shps, ignore.case = TRUE)]
          if (length(shp1_files) == 0 || length(shp2_files) == 0) stop("Server Error: ADM1 or ADM2 shapefiles not found.")
          tmp  <- sf::st_read(shp1_files[1], quiet = TRUE); tmp$shapeName  <- get_shape_name(tmp);  tmp  <- ensure_crs_4326(tmp)
          tmp2 <- sf::st_read(shp2_files[1], quiet = TRUE); tmp2$shapeName <- get_shape_name(tmp2); tmp2 <- ensure_crs_4326(tmp2)
          list(kind = "local", adm1_sf = tmp, adm2_sf = tmp2)
        }
      }, seed = TRUE)

      prom <- promises::as.promise(fut)
      prom %...>% (function(res) {
        if (identical(res$kind, "global")) {
          ext_rv$world_sf <- res$world_sf
          updateSelectInput(session, "global_country", choices = c("Select...", res$countries))
          updateSelectizeInput(session, "ext_search", choices = c("", res$countries), server = TRUE)
          showNotification("Global Database Connected!", type = "message")
        } else {
          ext_rv$adm1_sf <- res$adm1_sf
          ext_rv$adm2_sf <- res$adm2_sf
          states <- sort(unique(as.character(res$adm1_sf$shapeName)))
          updateSelectInput(session, "sel_state", choices = c("Select...", states))
          safe_names <- unique(c(as.character(res$adm1_sf$shapeName), as.character(res$adm2_sf$shapeName)))
          updateSelectizeInput(session, "ext_search", choices = c("", sort(safe_names)), server = TRUE)
          showNotification("Local Database connected!", type = "message")
        }
        clear_busy(session); progress$close()
      }) %...!% (function(e) {
        log_err("Database Loader Error", conditionMessage(e))
        clear_busy(session); progress$close()
      })
    })
    
    observeEvent(input$sel_state, { 
      req(ext_rv$adm1_sf, ext_rv$adm2_sf) 
      if(input$db_source == "local" && !is.null(input$sel_state) && input$sel_state != "Select...") { 
        withProgress(message = 'Locating...', value = 0.5, { 
          state_geom <- ext_rv$adm1_sf %>% filter(shapeName == input$sel_state)
          districts_in_state <- st_filter(ext_rv$adm2_sf, state_geom, .predicate = st_intersects)
          dist_names <- sort(unique(as.character(districts_in_state$shapeName)))
          updateSelectInput(session, "sel_dist", choices = c("Select...", dist_names)) 
        }) 
      } 
    })
    
    observeEvent(input$sel_dist, {
      req(ext_rv$adm2_sf)
      if(input$db_source == "local" && !is.null(input$sel_dist) && input$sel_dist != "Select..." && !is.null(input$ext_level) && input$ext_level == "ADM5") {
        withProgress(message = 'Locating...', value = 0.5, {
          dist_geom <- ext_rv$adm2_sf %>% filter(shapeName == input$sel_dist)
          bbox <- st_bbox(dist_geom); wkt_str <- st_as_text(st_as_sfc(bbox))
          all_shps <- list.files(path = ".", pattern = "\\.shp$", recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
          tgt_files <- all_shps[grepl("ADM3", all_shps, ignore.case = TRUE)]
          if(length(tgt_files) > 0) {
            raw_adm3 <- st_read(tgt_files[1], wkt_filter = wkt_str, quiet = TRUE)
            raw_adm3 <- ensure_crs_4326(raw_adm3)
            talukas_in_dist <- st_filter(raw_adm3, dist_geom, .predicate = st_intersects)
            talukas_in_dist$shapeName <- get_shape_name(talukas_in_dist); ext_rv$adm3_sf_temp <- talukas_in_dist
            t_names <- sort(unique(as.character(talukas_in_dist$shapeName))); updateSelectInput(session, "sel_taluka", choices = c("Select...", t_names))
          } else { showNotification("Taluka data missing on server.", type="warning") }
        })
      }
    })
    
    observeEvent(input$ext_load_map, {
      # Quick validation checks stay synchronous (immediate feedback, no need to dispatch to a worker)
      db_source <- input$db_source
      global_level <- input$global_level
      global_country <- input$global_country
      ext_level <- input$ext_level
      sel_state <- input$sel_state
      sel_dist <- input$sel_dist
      sel_taluka <- input$sel_taluka
      
      # 🚀 REFACTOR (Option B): the required-field validation previously lived entirely inline
      # here — now extracted to validate_extractor_load_inputs() (global.R) so it's independently
      # unit-tested. Returns NULL when everything required is present, or a message describing
      # the first missing field otherwise.
      validation_error <- validate_extractor_load_inputs(db_source, global_level, global_country, ext_level, sel_state, sel_dist, sel_taluka)
      if (!is.null(validation_error)) return(showNotification(validation_error, type = "warning"))
      
      # Snapshot needed reactive data as plain values BEFORE dispatching to the background worker —
      # reactive values (ext_rv$...) only exist in this session, not inside a multisession worker.
      world_sf_snapshot <- ext_rv$world_sf
      adm1_sf_snapshot <- ext_rv$adm1_sf
      adm2_sf_snapshot <- ext_rv$adm2_sf
      adm3_sf_temp_snapshot <- ext_rv$adm3_sf_temp
      
      set_busy(session, "Loading boundaries... (app stays usable — feel free to keep working)")
      progress <- shiny::Progress$new(session)
      progress$set(message = "Loading boundaries...", value = NULL)
      
      fut <- future::future({
        target_sf <- NULL
        
        if (db_source == "global") {
          if (global_level == "world") {
            target_sf <- world_sf_snapshot
            target_sf$shapeName <- target_sf$admin
          } else if (global_level == "states") {
            target_sf <- rnaturalearth::ne_states(country = global_country, returnclass = "sf")
            target_sf$shapeName <- target_sf$name
          }
        } else {
          lvl <- ext_level
          all_shps <- list.files(path = ".", pattern = "\\.shp$", recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
          if (lvl == "ADM0") {
            tgt_files <- all_shps[grepl("ADM0", all_shps, ignore.case = TRUE)]; tmp <- sf::st_read(tgt_files[1], quiet = TRUE) %>% sf::st_zm(); tmp$shapeName <- get_shape_name(tmp)
            tmp <- ensure_crs_4326(tmp); target_sf <- tmp
          } else if (lvl == "ADM1") {
            target_sf <- adm1_sf_snapshot
          } else if (lvl == "ADM2") {
            state_geom <- adm1_sf_snapshot %>% dplyr::filter(shapeName == sel_state); target_sf <- sf::st_filter(adm2_sf_snapshot, state_geom, .predicate = sf::st_intersects)
          } else if (lvl == "ADM3") {
            dist_geom <- adm2_sf_snapshot %>% dplyr::filter(shapeName == sel_dist)
            bbox <- sf::st_bbox(dist_geom); wkt_str <- sf::st_as_text(sf::st_as_sfc(bbox))
            tgt_files <- all_shps[grepl("ADM3", all_shps, ignore.case = TRUE)]; if (length(tgt_files) == 0) stop("ADM3 data missing.")
            raw_sf <- sf::st_read(tgt_files[1], wkt_filter = wkt_str, quiet = TRUE) %>% sf::st_zm(); raw_sf$shapeName <- get_shape_name(raw_sf)
            raw_sf <- ensure_crs_4326(raw_sf)
            target_sf <- sf::st_filter(raw_sf, dist_geom, .predicate = sf::st_intersects)
          } else if (lvl == "ADM5") {
            if (is.null(adm3_sf_temp_snapshot)) stop("Taluka mapping failed.")
            taluka_geom <- adm3_sf_temp_snapshot %>% dplyr::filter(shapeName == sel_taluka)
            bbox <- sf::st_bbox(taluka_geom); wkt_str <- sf::st_as_text(sf::st_as_sfc(bbox))
            tgt_files <- all_shps[grepl("ADM5", all_shps, ignore.case = TRUE)]; if (length(tgt_files) == 0) stop("Village (ADM5) data missing.")
            raw_sf <- sf::st_read(tgt_files[1], wkt_filter = wkt_str, quiet = TRUE) %>% sf::st_zm(); raw_sf$shapeName <- get_shape_name(raw_sf)
            raw_sf <- ensure_crs_4326(raw_sf)
            target_sf <- sf::st_filter(raw_sf, taluka_geom, .predicate = sf::st_intersects)
          }
        }
        
        target_sf
      }, seed = TRUE)
      
      prom <- promises::as.promise(fut)
      
      prom %...>% (function(target_sf) {
        if (!is.null(target_sf) && nrow(target_sf) > 0) {
          target_sf <- target_sf[!st_is_empty(target_sf), ]
          target_sf$internal_ext_id <- seq_len(nrow(target_sf)); ext_rv$active_sf <- target_sf; ext_rv$selected_ids <- c()
          current_names <- as.character(target_sf$shapeName)
          
          global_names <- if (db_source == "local") {
            unique(c(as.character(adm1_sf_snapshot$shapeName), as.character(adm2_sf_snapshot$shapeName), current_names))
          } else {
            sort(current_names)
          }
          
          updateSelectizeInput(session, "ext_search", choices = c("", sort(global_names)), selected = "", server = TRUE)
          bbox <- st_bbox(target_sf)
          leafletProxy("ext_map") %>% clearShapes() %>% clearControls() %>% fitBounds(lng1 = as.numeric(bbox["xmin"]), lat1 = as.numeric(bbox["ymin"]), lng2 = as.numeric(bbox["xmax"]), lat2 = as.numeric(bbox["ymax"])) %>% addPolygons(data = ext_rv$active_sf, layerId = ~internal_ext_id, fillColor = "#4a83c4", fillOpacity = 0.2, color = "#26333e", weight = 1, highlightOptions = highlightOptions(weight = 3, color = "#8b3a2b", fillOpacity = 0.7, bringToFront = TRUE), label = ~as.character(shapeName)) %>% inject_map_elements("Boundary Extractor")
          log_event("INFO", "mod_extractor", "boundary_loaded", session_id = session$token,
                    db_source = db_source, n_features = nrow(target_sf))
        } else {
          log_event("WARN", "mod_extractor", "boundary_load_empty", session_id = session$token, db_source = db_source)
          showNotification("No boundaries found.", type = "warning")
        }
        clear_busy(session)
        progress$close()
      }) %...!% (function(e) {
        log_event("ERROR", "mod_extractor", "boundary_load_failed", session_id = session$token,
                  db_source = db_source, error = conditionMessage(e))
        showNotification(paste("Extraction Error:", conditionMessage(e)), type = "error")
        clear_busy(session)
        progress$close()
      })
    })
    
    observeEvent(input$ext_search, {
      if (is.null(input$ext_search) || input$ext_search == "") return(); search_val <- input$ext_search; target_poly <- NULL
      if(!is.null(ext_rv$active_sf) && search_val %in% ext_rv$active_sf$shapeName) { target_poly <- ext_rv$active_sf %>% filter(shapeName == search_val)
      } else if(input$db_source == "local") {
        if(!is.null(ext_rv$adm2_sf) && search_val %in% ext_rv$adm2_sf$shapeName) { target_poly <- ext_rv$adm2_sf %>% filter(shapeName == search_val)
        } else if(!is.null(ext_rv$adm1_sf) && search_val %in% ext_rv$adm1_sf$shapeName) { target_poly <- ext_rv$adm1_sf %>% filter(shapeName == search_val) }
      }
      if(!is.null(target_poly) && nrow(target_poly) > 0) {
        target_poly$internal_ext_id <- seq_len(nrow(target_poly)) + 999900; ext_rv$active_sf <- target_poly; ext_rv$selected_ids <- target_poly$internal_ext_id
        bbox <- st_bbox(target_poly); leafletProxy("ext_map") %>% clearShapes() %>% clearGroup("extracted_overlay") %>% fitBounds(lng1 = as.numeric(bbox["xmin"]), lat1 = as.numeric(bbox["ymin"]), lng2 = as.numeric(bbox["xmax"]), lat2 = as.numeric(bbox["ymax"])) %>% addPolygons(data = target_poly, layerId = ~internal_ext_id, fillColor = "#c1683b", fillOpacity = 0.8, color = "#26333e", weight = 2, highlightOptions = highlightOptions(weight = 3, color = "#8b3a2b", fillOpacity = 0.7, bringToFront = TRUE), label = ~as.character(shapeName))
      }
    })
    
    observeEvent(input$ext_map_shape_click, { 
      click <- input$ext_map_shape_click; if(is.null(click$id) || is.null(ext_rv$active_sf)) return(); clicked_id <- click$id
      if (clicked_id %in% ext_rv$selected_ids) { ext_rv$selected_ids <- ext_rv$selected_ids[ext_rv$selected_ids != clicked_id]; new_color <- "#4a83c4"; new_opacity <- 0.2 
      } else { ext_rv$selected_ids <- c(ext_rv$selected_ids, clicked_id); new_color <- "#c1683b"; new_opacity <- 0.8 }
      clicked_poly <- ext_rv$active_sf %>% filter(internal_ext_id == clicked_id)
      leafletProxy("ext_map") %>% addPolygons(data = clicked_poly, layerId = ~internal_ext_id, fillColor = new_color, fillOpacity = new_opacity, color = "#26333e", weight = if(clicked_id %in% ext_rv$selected_ids) 2 else 1, label = ~as.character(shapeName)) 
    })
    
    output$ext_selected_names <- renderText({ if (length(ext_rv$selected_ids) == 0) { return("No features selected yet.") } else { sel_data <- ext_rv$active_sf %>% filter(internal_ext_id %in% ext_rv$selected_ids); names_list <- sel_data %>% pull(shapeName) %>% as.character(); return(paste(names_list, collapse = ", ")) } })
    output$ext_metrics <- renderUI({ if(length(ext_rv$selected_ids) == 0) { HTML("<div style='font-size:12px; color:#5c6b73; background:#e8f0ea; padding:8px; border-radius:4px;'><b>Area:</b> 0.00 sq. km | <b>Features:</b> 0</div>") } else { sel <- ext_rv$active_sf %>% filter(internal_ext_id %in% ext_rv$selected_ids); area_m2 <- sum(st_area(sel)); area_km2 <- as.numeric(area_m2) / 1e6; HTML(sprintf("<div style='font-size:12px; color:#45936f; font-weight:bold; background:#e8f0ea; padding:8px; border-radius:4px; border:1px solid #9bc4ab;'>Area: %.2f sq. km | Features: %d</div>", area_km2, nrow(sel))) } })
    observeEvent(input$ext_clear_sel, { if (is.null(ext_rv$active_sf)) return(); ext_rv$selected_ids <- c(); updateSelectizeInput(session, "ext_search", selected = ""); leafletProxy("ext_map") %>% clearGroup("extracted_overlay") %>% clearShapes() %>% addPolygons(data = ext_rv$active_sf, layerId = ~internal_ext_id, fillColor = "#4a83c4", fillOpacity = 0.2, color = "#26333e", weight = 1, highlightOptions = highlightOptions(weight = 3, color = "#8b3a2b", fillOpacity = 0.7, bringToFront = TRUE), label = ~as.character(shapeName)) })
    
    # 🚀 REFACTOR: uses shared get_selected_or_all() + dissolve_if_requested() (global.R) instead
    # of the same "filter-if-selected-else-all, then dissolve-if-checked" logic being hand-written
    # separately in this handler AND in add_cart_ext_shp below.
    observeEvent(input$send_floating_ext, {
      if(is.null(ext_rv$active_sf)) return(showNotification("No boundary selected.", type="error"))
      export_sf <- get_selected_or_all(ext_rv$active_sf, ext_rv$selected_ids)
      export_sf <- dissolve_if_requested(export_sf, input$ext_dissolve)
      curr <- floating_rv$files; new_id <- paste0("file_", as.integer(Sys.time()), "_", sample(1:1000, 1))
      region_tag <- get_region_label(export_sf)
      item_name <- paste0(region_tag, " Boundary: ", ifelse(input$ext_dissolve, "Merged", paste(nrow(export_sf), "Features")))
      curr[[new_id]] <- list(id = new_id, name = item_name, data = export_sf, type = "Shapefile (.shp, .shx, .dbf, .prj)")
      floating_rv$files <- curr
      showNotification(sprintf("Sent to Clipboard: %s", item_name), type="message", duration = 6)
    })
    
    observeEvent(input$save_drawn_roi, {
      if(is.null(ext_rv$drawn_poly)) return(showNotification("Draw a polygon on map first!", type="error"))
      curr <- floating_rv$files; new_id <- paste0("roi_", as.integer(Sys.time()), "_", sample(1:1000, 1))
      region_tag <- get_region_label(ext_rv$drawn_poly)
      item_name <- paste0(region_tag, "_", input$custom_roi_name)
      curr[[new_id]] <- list(id = new_id, name = item_name, data = ext_rv$drawn_poly, type = "Shapefile (.shp, .shx, .dbf, .prj)")
      floating_rv$files <- curr
      showNotification("Custom ROI saved to Clipboard!", type="message")
    })
    
    observeEvent(input$add_cart_ext_shp, { 
      if(is.null(ext_rv$active_sf)) return(showNotification("No boundary data loaded.", type="error")) 
      export_sf <- get_selected_or_all(ext_rv$active_sf, ext_rv$selected_ids)
      export_sf <- dissolve_if_requested(export_sf, input$ext_dissolve)
      ext_rv$ext_shp_export <- export_sf
      item_name <- if(input$ext_dissolve) "Merged Spatial Boundary" else sprintf("Extracted Boundaries (%d features)", if(length(ext_rv$selected_ids)>0) length(ext_rv$selected_ids) else nrow(ext_rv$active_sf))
      add_to_workspace("ext_shp", item_name, 0.50) 
    })
    
    observeEvent(input$add_cart_drawn_roi, { 
      if(!is.null(ext_rv$drawn_poly)) {
        ext_rv$drawn_roi_export <- ext_rv$drawn_poly
        add_to_workspace("drawn_roi", "Custom Drawn ROI Boundary", 0.50) 
      } else showNotification("Draw ROI first!", type="error") 
    })
  })
}
