# =========================================================================
# MODULE 3: LULC ENGINE (LAZY LOAD, FULL TABS & ANTI-CRASH)
# =========================================================================

# -------------------------------------------------------------------------
# Research-standard accuracy & area estimation (pure base R, no packages).
# Given a validation confusion matrix + the mapped area of each class, this
# returns the metrics reviewers now expect INSTEAD of Kappa:
#   * Olofsson et al. (2014, RSE 148:42) stratified error-adjusted AREA of
#     each class, with a 95% confidence interval — the "good practices"
#     standard for area estimation from a classified map.
#   * Pontius & Millones (2011, IJRS 32:4407) QUANTITY and ALLOCATION
#     disagreement, the recommended replacement for the Kappa coefficient.
#
# Convention: Earth Engine's errorMatrix has ROWS = reference (actual) and
# COLUMNS = predicted (map class); `cm_order` lists the class IDs along both
# axes. `area_by_id` is a named numeric vector (names = class IDs as strings)
# giving each MAP class's area in any single unit; results come back in that
# same unit. Returns NULL if inputs are unusable (caller shows a fallback).
lulc_rigorous_accuracy <- function(cm_array, cm_order, area_by_id, z = 1.96) {
  if (is.null(cm_array) || is.null(cm_order) || is.null(area_by_id)) return(NULL)
  ord <- suppressWarnings(as.integer(unlist(cm_order)))
  K <- length(ord)
  if (K < 2) return(NULL)
  M <- tryCatch(do.call(rbind, lapply(cm_array, function(r) as.numeric(unlist(r)))),
                error = function(e) NULL)
  if (is.null(M) || nrow(M) != K || ncol(M) != K || any(!is.finite(M))) return(NULL)
  # rows = reference (actual), cols = predicted (map)
  areas <- suppressWarnings(as.numeric(area_by_id[as.character(ord)]))
  areas[is.na(areas)] <- 0
  A_total <- sum(areas)
  if (!isTRUE(A_total > 0)) return(NULL)
  W <- areas / A_total                       # map-class weights, sum to 1
  n_map <- colSums(M)                          # validation samples per MAP class
  # Estimated population matrix P[i_map, j_ref] = W_i * n_ij / n_i.
  P <- matrix(0, K, K)
  for (i in seq_len(K)) if (n_map[i] > 0 && W[i] > 0)
    for (j in seq_len(K)) P[i, j] <- W[i] * M[j, i] / n_map[i]
  rowT <- rowSums(P)          # map-class area proportions (= W)
  colT <- colSums(P)          # estimated reference-area proportions p_.j
  diagP <- diag(P)
  oa_aw <- sum(diagP)         # area-weighted overall accuracy
  # Olofsson SE of each reference-class area proportion.
  se_pj <- vapply(seq_len(K), function(j) {
    v <- 0
    for (i in seq_len(K)) if (n_map[i] > 1 && W[i] > 0) {
      r <- M[j, i] / n_map[i]
      v <- v + W[i]^2 * r * (1 - r) / (n_map[i] - 1)
    }
    sqrt(v)
  }, numeric(1))
  # Pontius & Millones disagreement, from the estimated population matrix.
  quantity   <- 0.5 * sum(abs(rowT - colT))
  allocation <- sum(pmin(rowT - diagP, colT - diagP))
  per_class <- data.frame(
    Class_ID    = ord,
    Mapped_Area = round(areas, 3),
    Adj_Area    = round(colT * A_total, 3),
    CI95        = round(z * se_pj * A_total, 3),
    Users_Acc   = round(ifelse(rowT > 0, diagP / rowT, NA_real_) * 100, 1),
    Prod_Acc    = round(ifelse(colT > 0, diagP / colT, NA_real_) * 100, 1),
    stringsAsFactors = FALSE
  )
  list(per_class = per_class, oa_aw = oa_aw, quantity = quantity,
       allocation = allocation, total_disagreement = quantity + allocation,
       A_total = A_total, n_total = sum(M),
       unsampled_strata = sum(areas > 0 & n_map == 0))
}

mod_lulc_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$style(HTML("
      .mini-popup .leaflet-popup-content-wrapper { padding: 0px; border-radius: 4px; box-shadow: 0 3px 14px rgba(0,0,0,0.2); }
      .mini-popup .leaflet-popup-content { margin: 8px; line-height: 1.2; }
      .mini-popup .leaflet-popup-close-button { display: none; }
    ")),
    fluidRow(
      column(width = 4, class = "sidebar-panel-custom",
             div(class = "step-card", style = "border-left: 3px solid #8b3a2b;", 
                 h4("Step 1: Study Area", info_tooltip("Set the area you want to classify. Upload a shapefile or fetch a boundary from the Data Clipboard."), class = "step-title"), 
                 div(style = "background-color: #e8f0ea; color: #45936f; padding: 8px 12px; border-radius: 4px; font-size: 12px; margin-bottom: 15px; border: 1px solid #9bc4ab;", 
                     "System Note: Dynamic Scaling Active. Safely handles any district or state size."), 
                 uiOutput(ns("lulc_mask_status")), 
                 fileInput(ns("mask_file"), "Upload Target Area (.shp, .shx, .dbf, .prj)", multiple = TRUE, accept = c(".shp", ".shx", ".dbf", ".prj")), 
                 actionButton(ns("process_mask_btn"), "Set Uploaded Boundary", class = "btn-primary btn-custom", style="margin-bottom:0;")
             ),
             
             div(class = "step-card", style = "border-left: 3px solid #4a83c4;", 
                 h4("Step 2: Landsat Engine", info_tooltip("Fetches cloud-masked Landsat 8/9 imagery for your boundary and date range — this becomes the input for both spectral indices and the classifier."), class = "step-title"), 
                 p("Rendered on Google Servers with Pixel-wise Cloud Masking.", style="font-size:11px; color:#5c6b73; margin-bottom:10px;"), 
                 dateRangeInput(ns("stac_dates"), "Select Date Range:", start = as.Date("2015-01-01"), end = Sys.Date(), min = as.Date("1984-01-01"), max = Sys.Date()), 
                 sliderInput(ns("stac_cloud"), "Scene Cloud Tolerance (%):", min = 10, max = 100, value = 60, step = 10), 
                 div(style = "background: #f7f6f2; padding: 12px; border-radius: 4px; border: 1px solid #d8d4c8; margin-bottom: 12px;", 
                     tags$label("Band Combination Setup:", style="font-size:12px; color:#5c6b73; margin-bottom:8px; display:block;"), 
                     fluidRow(
                       column(4, numericInput(ns("r_band"), "R:", value = 4, min = 1, max = 5)), 
                       column(4, numericInput(ns("g_band"), "G:", value = 3, min = 1, max = 5)), 
                       column(4, numericInput(ns("b_band"), "B:", value = 2, min = 1, max = 5))
                     ), 
                     fluidRow(
                       column(6, numericInput(ns("nir_band"), "NIR:", value = 5, min = 1, max = 7)),
                       column(6, numericInput(ns("swir_band"), "SWIR:", value = 6, min = 1, max = 7))
                     ),
                     uiOutput(ns("band_combo_hint"))
                 ),
                 actionButton(ns("fetch_stac_btn"), "Fetch Landsat Image", class = "btn-info btn-custom", style="color:#fff; border:none;")
             ),
             
             div(class = "step-card", style = "border-left: 3px solid #c1683b;",
                 h4("Step 3: Ground Truth", info_tooltip("Draw training points/polygons on the map and label each with a land-cover class (e.g. Forest, Water, Urban) — the classifier learns from these examples."), class = "step-title"),
                 radioButtons(ns("gt_method"), "Select Input Method:", choices = list("Upload Data" = "upload", "Draw on Map" = "draw"), inline = TRUE), 
                 hr(style="margin: 10px 0; border-top: 1px solid #eee;"),
                 
                 conditionalPanel(sprintf("input['%s'] == 'upload'", ns("gt_method")),
                                  fileInput(ns("training_file"), "Upload (.shp, .shx, .dbf, .prj OR .geojson)", multiple = TRUE),
                                  actionButton(ns("edit_shp_labels_btn"), "Manage Classes & Colors", class = "btn-warning btn-custom", style="color:#fff; margin-bottom:0; border:none;")
                 ),
                 
                 conditionalPanel(sprintf("input['%s'] == 'draw'", ns("gt_method")),
                                  div(style="background:#f7f6f2; border:1px solid #c1683b; padding:8px; border-radius:4px; margin-bottom:10px;",
                                      p("Left-Click to start drawing points or polygons.", style="font-size:11px; color:#c1683b; font-weight:bold; margin-bottom:4px;"),
                                      p("Click on the first point to finish the polygon.", style="font-size:11px; color:#45936f; font-weight:bold; margin-bottom:0;")
                                  ),
                                  actionButton(ns("edit_pts_btn"), "Manage Drawn Data (Table)", class = "btn-info btn-custom", style="color:#fff; border:none; margin-bottom:10px;"),
                                  fluidRow(
                                    column(6, actionButton(ns("send_basket_pts"), "To Clipboard", class="btn-custom", style = "background-color: #e8f0ea; border: 1px solid #d8d4c8; color: #26333e; font-size:12px; padding:6px;")),
                                    column(6, downloadButton(ns("download_drawn_data"), "Save Shapefile (ZIP)", class="btn btn-custom", style = "background-color: #3d4f5c; border: none; color: #fff; font-size:12px; padding:6px; width:100%; display:block; text-align:center;"))
                                  ),
                                  actionButton(ns("clear_points_btn"), "Clear All Training Data", class = "btn-danger btn-custom", style="margin-bottom:0; border:none;")
                 )
             ),
             
             div(class = "step-card", style = "border-left: 3px solid #45936f; background-color: #f7f6f2;", 
                 h4("Step 4: Machine Learning", info_tooltip("Trains a classifier (Random Forest, CART, or SVM) on your labeled training points, then applies it to classify the whole boundary into land-cover classes. Also computes accuracy, Kappa, and (for RF/CART) a confidence map."), class = "step-title", style="border-bottom-color: #45936f;"), 
                 selectInput(ns("classifier_type"), "Classifier Algorithm:", choices = c("Random Forest" = "rf", "CART (Decision Tree)" = "cart", "Support Vector Machine" = "svm"), selected = "rf"),
                 div(style = "margin:-4px 0 8px;", actionLink(ns("show_algo_guide"), tagList(icon("question-circle"), " Which algorithm should I use?"), style = "font-size:12px; color:#4a83c4; font-weight:600;")),
                 uiOutput(ns("classifier_reco")),
                 conditionalPanel(condition = sprintf("input['%s'] == 'rf'", ns("classifier_type")), numericInput(ns("rf_trees"), "Number of Trees (Random Forest):", value=50, min=10, max=500)),
                 checkboxGroupInput(ns("calc_indices"), strong("Generate Spectral Indices:"), choices = c("NDVI", "NDWI", "EVI", "NDBI"), selected = c("NDVI", "NDWI"), inline = TRUE), 
                 actionButton(ns("run_model"), "Execute Classification Model", class = "btn-primary btn-custom", style = "padding: 12px; font-size: 14px; margin-bottom:0; border:none;")
             ),
             
             div(class = "step-card", style = "border-left: 3px solid #3d4f5c; background-color: #f7f6f2;", 
                 h4("Step 5: Spatiotemporal Automation", info_tooltip("Re-runs your trained classifier across multiple years to show how land cover changed over time, with an auto-generated transition matrix and spatial Change Detection Map."), class = "step-title", style="border-bottom-color: #3d4f5c;"), 
                 p("Generate Temporal Gallery & Auto-Transition Matrix.", style="font-size:11px; color:#5c6b73; margin-bottom:5px;"), 
                 p("Limited to 2013+ (Landsat 8/9) — classification accuracy isn't reliable on older sensors (Landsat 5/7) since the model is trained on Landsat 8/9's specific spectral characteristics.", style="font-size:10px; color:#5c6b73; margin-bottom:12px; font-style:italic;"), 
                 fluidRow(
                   column(6, numericInput(ns("lulc_temp_start"), "Start Year:", value=2015, min=2013, max=as.numeric(format(Sys.Date(), "%Y")))), 
                   column(6, numericInput(ns("lulc_temp_end"), "End Year:", value=as.numeric(format(Sys.Date(), "%Y")), min=2013, max=as.numeric(format(Sys.Date(), "%Y"))))
                 ), 
                 fluidRow(
                   column(6, selectInput(ns("lulc_temp_gap"), "Time Interval:", choices=c("1 Year"=1, "2 Years"=2, "3 Years"=3, "4 Years"=4, "5 Years"=5, "10 Years"=10), selected=3)), 
                   column(6, selectInput(ns("lulc_temp_month"), "Select Month:", choices = setNames(1:12, month.name), selected = 1))
                 ), 
                 actionButton(ns("run_lulc_temporal"), "Generate Grid & Matrix", class="btn-primary btn-custom", style="margin-bottom:10px; border:none;"), 
                 actionButton(ns("add_cart_lulc_gallery"), "Save Gallery to Workspace", class="btn-success btn-custom", style="margin-bottom:0; border:none;")
             )
      ),
      
      column(width = 8, class = "main-panel-custom",
             tabsetPanel(id = ns("main_tabs"),
                         tabPanel(title = "Study Area Map", value = "draw_tab", 
                                  div(style="margin-top: 15px;", withSpinner(leafletOutput(ns("interactive_map")), type=8, color="#26333e"))
                         ),
                         
                         tabPanel(title = "LULC Classification Map", value = "final_tab", 
                                  div(style="margin-top: 15px; margin-bottom: 10px;", 
                                      actionButton(ns("show_confidence_map"), "Show Classification Confidence", class="btn-sm btn-outline-secondary", style="border:1px solid #5c6b73; color:#26333e; background:white;"),
                                      actionButton(ns("show_classes_map"), "Back to Classes", class="btn-sm btn-outline-secondary", style="border:1px solid #5c6b73; color:#26333e; background:white; margin-left:8px;")
                                  ),
                                  div(style="margin-top: 0px;", class="gf-map-shell",
                                      withSpinner(leafletOutput(ns("lulc_pred_map")), type=8, color="#26333e"),
                                      insights_drawer_ui(ns("lulc_insights"), title = "LULC Insights"))
                         ),
                         
                         tabPanel(title = "Change Detection Map", value = "change_map_tab",
                                  div(style="padding:15px; background:#fff; margin-top:15px; border-radius:6px; border:1px solid #eee;",
                                      p("Spatial map of WHERE land cover changed between the first and last year of your Temporal Gallery (run 'Generate Grid & Matrix' first). Highlighted areas changed class; the rest stayed the same.", style="color:#5c6b73; font-size:12px; margin-bottom:10px;"),
                                      withSpinner(leafletOutput(ns("change_map")), type=8, color="#26333e"),
                                      uiOutput(ns("change_significance_ui"))
                                  )
                         ),
                         
                         tabPanel(title = "LULC Temporal Grid", value = "lulc_grid_tab",
                                  div(style="padding:20px; background:#fff; margin-top:15px; border-radius:6px; border:1px solid #eee;",
                                      div(style="display: flex; justify-content: space-between; align-items: center; margin-bottom: 15px;",
                                          h4("Spatiotemporal LULC Change Grid", style="font-size:16px; font-weight:600; color:#26333e; margin:0;"),
                                          actionButton(ns("add_cart_temporal_grid_btn"), "Save Temporal Grid", class="btn-sm btn-success", style="font-weight:500; border:none;")
                                      ),
                                      withSpinner(uiOutput(ns("lulc_temporal_grid_ui_wrapper")), type=8, color="#26333e"),
                                      hr(style="margin: 30px 0; border-top: 1px dashed #d8d4c8;"),
                                      
                                      div(style="display: flex; justify-content: space-between; align-items: center; margin-bottom: 15px;",
                                          h4("Temporal LULC Area Statistics", style="color:#26333e; font-weight:600; margin: 0; font-size:16px;"),
                                          actionButton(ns("add_cart_temporal_plot"), "Save Temporal Chart", class="btn-sm btn-success", style="font-weight:500; border:none;")
                                      ),
                                      div(class="protect-wrap", oncontextmenu="return false;", 
                                          withSpinner(plotOutput(ns("temporal_area_plot"), height="350px"), type=8, color="#26333e")
                                      ), 
                                      br(),
                                      
                                      div(style="display: flex; justify-content: space-between; align-items: center; margin-bottom: 15px;",
                                          h4("Temporal LULC Area Data", style="color:#26333e; font-weight:600; margin: 0; font-size:16px;"),
                                          actionButton(ns("add_cart_temporal_csv"), "Save Pivot Table", class="btn-sm btn-success", style="font-weight:500; border:none;")
                                      ),
                                      div(class="secure-content", style="border: 1px solid #eee; padding: 10px; border-radius: 4px; background: #f7f6f2; overflow-x: auto;", 
                                          withSpinner(DTOutput(ns("temporal_area_tbl")), type=8, color="#26333e")
                                      )
                                  )
                         ),
                         
                         tabPanel(title = "Change Matrix", value = "sankey_tab", 
                                  div(style="padding: 20px; background: #fff; margin-top: 15px; border-radius: 6px; border: 1px solid #d8d4c8;", 
                                      div(style="display: flex; justify-content: space-between; align-items: center; margin-bottom: 15px;", 
                                          h4("LULC Transition Heatmap", style="color:#26333e; font-weight:600; margin: 0; font-size:16px;"), 
                                          actionButton(ns("add_cart_sankey"), "Save Matrix Data", class="btn-sm btn-success", style="font-weight:500; border:none;")
                                      ), 
                                      p("Shows the precise area (Sq.Km) that transitioned from a base classification to a target classification.", style="font-size:12px; color:#5c6b73; margin-bottom:15px;"), 
                                      div(class="protect-wrap", style="border:1px solid #eee; padding:20px; border-radius:4px; background:#fff;", 
                                          withSpinner(plotOutput(ns("transition_plot"), height="400px"), type=8, color="#6b4c7a")
                                      ), 
                                      hr(style="margin: 30px 0; border-top: 1px dashed #d8d4c8;"), 
                                      h4("Transition Matrix Table (Sq.Km)", style="color:#26333e; font-weight:600; margin-bottom:15px; font-size:16px;"),
                                      div(class="secure-content", style="border: 1px solid #eee; padding: 10px; border-radius: 4px; background: #f7f6f2; overflow-x: auto;",
                                          withSpinner(DTOutput(ns("sankey_table")), type=8, color="#6b4c7a")
                                      ),
                                      hr(style="margin: 24px 0; border-top: 1px dashed #d8d4c8;"),
                                      h4(HTML("Change Budget &amp; Intensity Analysis &nbsp;<span style='font-size:11px;font-weight:600;color:#45936f;background:#e6f3ec;padding:2px 8px;border-radius:10px;'>Research Metrics</span>"),
                                         style="color:#26333e; font-weight:600; margin-bottom:8px; font-size:16px;"),
                                      uiOutput(ns("transition_analysis_ui"))
                                  )
                         ),
                         
                         tabPanel(title = "LULC Area Statistics", value = "stats_tab", 
                                  div(style="padding: 20px; background: #fff; margin-top: 15px; border-radius: 6px; border: 1px solid #d8d4c8;", 
                                      div(style="display: flex; justify-content: space-between; align-items: center; margin-bottom: 15px;", 
                                          h4("Area Distribution Chart", style="color:#26333e; font-weight:600; margin: 0; font-size:16px;"), 
                                          actionButton(ns("add_cart_plot"), "Save Chart", class="btn-sm btn-success", style="font-weight:500; border:none;")
                                      ), 
                                      div(class="protect-wrap", oncontextmenu="return false;", 
                                          withSpinner(plotOutput(ns("stats_bar_plot"), height = "350px"), type=8, color="#26333e")
                                      ), 
                                      hr(style="margin: 30px 0; border-top: 1px dashed #d8d4c8;"), 
                                      div(style="display: flex; justify-content: space-between; align-items: center; margin-bottom: 15px;", 
                                          h4("Tabular Zonal Metrics", style="color:#26333e; font-weight:600; margin: 0; font-size:16px;"), 
                                          actionButton(ns("add_cart_csv"), "Save CSV File", class="btn-sm btn-success", style="font-weight:500; border:none;")
                                      ), 
                                      div(class="secure-content", style="border: 1px solid #eee; padding: 10px; border-radius: 4px; background: #f7f6f2; overflow-x: auto;", 
                                          withSpinner(DTOutput(ns("stats_table")), type=8, color="#26333e")
                                      )
                                  )
                         ),
                         
                         tabPanel(title = "Model Validation", value = "acc_tab", 
                                  div(style="padding: 20px; background: #fff; margin-top: 15px; border-radius: 6px; border: 1px solid #d8d4c8;", 
                                      div(style="display: flex; justify-content: space-between; align-items: center; margin-bottom: 15px;", 
                                          h4("Random Forest Validation Metrics", style="color:#26333e; font-weight:600; margin: 0; font-size:16px;"), 
                                          actionButton(ns("add_cart_report"), "Save Report", class="btn-sm btn-success", style="font-weight:500; border:none;")
                                      ), 
                                      h5("Class-wise Accuracy", style="color:#3d4f5c; font-weight:600; margin-top:15px;"), 
                                      div(class="secure-content", style="border: 1px solid #eee; padding: 10px; border-radius: 4px; background: #f7f6f2; margin-bottom: 20px;", 
                                          withSpinner(DTOutput(ns("acc_table")), type=8, color="#26333e")
                                      ), 
                                      h5("System Logs", style="color:#3d4f5c; font-weight:600;"), 
                                      div(class="secure-content", style="background:#f7f6f2; color:#26333e; border: 1px solid #eee; padding: 15px; border-radius: 4px; font-family: monospace; font-size: 13px; line-height: 1.5; overflow-x: auto;", 
                                          verbatimTextOutput(ns("acc_log"))
                                      )
                                  )
                         ),
                         
                         # ============================================================
                         # NEW TAB — Insights & Analytics (non-destructive; passively
                         # summarizes the latest classification / temporal run).
                         # ============================================================
                         tabPanel(title = "Insights & Analytics", value = "insights_tab",
                                  div(style="padding:18px; background:#fff; margin-top:15px; border-radius:6px; border:1px solid #d8d4c8;",
                                      div(style="display:flex; justify-content:space-between; align-items:center; border-bottom:1px solid #eee; padding-bottom:10px; margin-bottom:14px;",
                                          h4(HTML("Insights &amp; Analytics &nbsp;<span style='font-size:11px;font-weight:600;color:#45936f;background:#e6f3ec;padding:2px 8px;border-radius:10px;'>Research Metrics</span>"),
                                             style="margin:0; color:#26333e; font-weight:600; font-size:16px;"),
                                          span("Passively reads your latest classification & temporal run — nothing to click.", style="font-size:11px; color:#5c6b73;")),
                                      div(style="display:flex; gap:10px; margin-bottom:14px;",
                                          downloadButton(ns("lulc_dl_report"), "Report (HTML)", class="btn-sm",
                                                         style="background:#2c5a4a;color:#fff;border:none;font-size:12px;padding:6px 12px;"),
                                          downloadButton(ns("lulc_dl_data"), "Data (Excel)", class="btn-sm",
                                                         style="background:#3d4f5c;color:#fff;border:none;font-size:12px;padding:6px 12px;")),
                                      uiOutput(ns("insights_headline")),
                                      h5("1 · Area & Proportional Metrics", style="color:#3d4f5c; font-weight:600; margin-top:16px;"),
                                      uiOutput(ns("insights_area_note")),
                                      div(class="secure-content", style="border:1px solid #eee; padding:10px; border-radius:4px; background:#f7f6f2;",
                                          withSpinner(DTOutput(ns("insights_area_table")), type=8, color="#26333e")),
                                      uiOutput(ns("insights_diversity")),
                                      h5("2 · Rate of Change (Temporal Dynamics)", style="color:#3d4f5c; font-weight:600; margin-top:18px;"),
                                      uiOutput(ns("insights_rate")),
                                      h5("3 · Validation & Robustness", style="color:#3d4f5c; font-weight:600; margin-top:18px;"),
                                      uiOutput(ns("insights_validation_note")),
                                      div(class="secure-content", style="border:1px solid #eee; padding:10px; border-radius:4px; background:#f7f6f2;",
                                          withSpinner(DTOutput(ns("insights_validation_table")), type=8, color="#26333e")),
                                      h5("4 · Rigorous Accuracy & Area (research-standard)", style="color:#3d4f5c; font-weight:600; margin-top:18px;"),
                                      uiOutput(ns("insights_disagreement")),
                                      uiOutput(ns("insights_area_ci")),
                                      div(style="margin-top:16px; border-left:3px solid #6b4c7a; background:#f4f0f7; border-radius:4px; padding:10px 12px; font-size:11px; color:#5c6b73;",
                                          HTML("<b>Coming next:</b> class-transition matrices (which class became which) with intensity analysis, landscape fragmentation (patch density, mean patch size, edge density, Shannon diversity), and a spatial map of where the model is least certain."))
                                  )
                         ),

                         tabPanel(title = "Process Logs", value = "logs_tab",
                                  div(class="secure-content", style="margin-top:15px; background:#f7f6f2; padding:15px; border-radius:6px; border: 1px solid #eee; font-family: monospace; font-size:12px; overflow-x: auto;",
                                      verbatimTextOutput(ns("logs"))
                                  )
                         )
             )
      )
    )
  )
}
mod_lulc_server <- function(id, rv, floating_rv, cart_rv, log_msg, add_to_workspace, analysis_registry = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    log_err <- function(msg, fix) { 
      showModal(modalDialog(
        title = HTML("Action Required"), 
        HTML(paste0("Error: ", msg, "<hr>Suggestion: ", fix, "")), 
        size = "m", easyClose = TRUE, footer = modalButton("Okay")
      )) 
    }
    
    pending_geom <- reactiveVal(NULL)

    # ============================ BATCH 3: LULC INSIGHTS ============================
    # (b) Band-combination helper — interpret the current R/G/B Landsat SR band
    # numbers into a plain-language description of what the composite visualizes.
    # Landsat 8/9 Surface-Reflectance bands: 2=Blue 3=Green 4=Red 5=NIR 6=SWIR1 7=SWIR2.
    .band_combo_desc <- function(r, g, b) {
      key <- paste(r, g, b, sep = "-")
      lut <- list(
        "4-3-2" = list("True / Natural Colour", "how the land looks to the human eye — good for a general orientation basemap."),
        "5-4-3" = list("Colour Infrared (vegetation)", "healthy vegetation glows bright red; excellent for separating vigorous crops/forest from bare soil and water."),
        "6-5-2" = list("False Colour (urban / SWIR)", "built-up areas and bare soil stand out; vegetation is green — a strong general land-cover combination."),
        "6-5-4" = list("Land / Water (SWIR-NIR-Red)", "sharp land–water boundaries and soil/vegetation moisture contrast."),
        "5-6-2" = list("Agriculture", "highlights crop vigour and field boundaries; healthy crops appear vivid green."),
        "7-6-4" = list("Shortwave Infrared", "penetrates smoke/haze; good for burn scars, geology and soil/mineral differences."),
        "7-5-3" = list("False Colour (alt. urban)", "distinguishes urban fabric, bare ground and vegetation with strong contrast.")
      )
      hit <- lut[[key]]
      if (is.null(hit)) return(list("Custom combination", "a non-standard band mix — the R/G/B channels show whichever bands you selected. Try 4-3-2 (natural), 5-4-3 (vegetation) or 6-5-2 (urban) for interpretable composites."))
      hit
    }
    output$band_combo_hint <- renderUI({
      d <- .band_combo_desc(input$r_band, input$g_band, input$b_band)
      div(style = "margin-top:8px; padding:8px 10px; background:#eef2f0; border-left:3px solid #45936f; border-radius:4px; font-size:11.5px; line-height:1.5; color:#3d4f5c;",
          tags$b(style = "color:#26333e;", sprintf("R-G-B %d-%d-%d → %s. ", input$r_band, input$g_band, input$b_band, d[[1]])),
          d[[2]])
    })

    # (e) Rule-based recommendation for the chosen classifier + a full algorithm guide modal.
    output$classifier_reco <- renderUI({
      idx <- input$calc_indices
      idx_txt <- if (length(idx)) paste(idx, collapse = " + ") else "at least NDVI + NDWI"
      msg <- switch(input$classifier_type %||% "rf",
        "rf"   = sprintf("Random Forest is the robust default — it handles mixed, noisy land cover well and rarely overfits. Pairing NDVI with NDWI (you have: %s) typically yields the best separability of vegetation, water and built-up.", idx_txt),
        "cart" = "A single CART tree is fast and fully interpretable, but more prone to overfitting than Random Forest — best for a quick look or a small number of very distinct classes.",
        "svm"  = "SVM can draw sharp boundaries between spectrally similar classes, but is sensitive to scaling and slower to train — give it clean, well-separated training samples and normalized indices.",
        "Random Forest is a solid default for most land-cover mapping.")
      col <- switch(input$classifier_type %||% "rf", "rf" = "#45936f", "cart" = "#c1683b", "svm" = "#4a83c4", "#45936f")
      div(style = sprintf("margin:0 0 10px; padding:8px 10px; background:#f7f6f2; border-left:3px solid %s; border-radius:4px; font-size:11.5px; line-height:1.5; color:#3d4f5c;", col),
          tags$b(style = "color:#26333e;", "Recommendation: "), msg)
    })
    observeEvent(input$show_algo_guide, {
      showModal(modalDialog(
        title = HTML("<b style='color:#26333e;'>Choosing a Classifier Algorithm</b>"), size = "l", easyClose = TRUE,
        footer = modalButton("Got it"),
        HTML(
          "<div style='font-size:13.5px; line-height:1.6; color:#3d4f5c;'>",
          "<p>All three learn land-cover classes from your labelled training samples, then classify every pixel in the boundary. They differ in how they draw the decision boundaries:</p>",
          "<div style='padding:10px 14px; background:#eef2f0; border-left:4px solid #45936f; border-radius:0 6px 6px 0; margin-bottom:10px;'>",
          "<b style='color:#26333e;'>Random Forest (recommended default)</b><br>An ensemble of many decision trees that vote. Robust to noisy training data, handles many spectral bands/indices, resists overfitting, and produces a per-pixel <i>confidence</i> map. Best all-round choice for mixed landscapes. Tip: 50–100 trees is usually plenty.</div>",
          "<div style='padding:10px 14px; background:#faf1ec; border-left:4px solid #c1683b; border-radius:0 6px 6px 0; margin-bottom:10px;'>",
          "<b style='color:#26333e;'>CART (single Decision Tree)</b><br>One transparent if-then tree. Fast and easy to interpret, but a single tree overfits more readily and can be unstable. Good for a quick result or a few very distinct classes.</div>",
          "<div style='padding:10px 14px; background:#eef2f6; border-left:4px solid #4a83c4; border-radius:0 6px 6px 0; margin-bottom:10px;'>",
          "<b style='color:#26333e;'>Support Vector Machine (SVM)</b><br>Finds the maximum-margin boundary between classes. Can excel at separating spectrally similar classes, but is sensitive to feature scaling, needs clean well-separated samples, is slower to train, and gives no confidence map here.</div>",
          "<p style='margin-top:12px;'><b>Rule of thumb:</b> start with <b>Random Forest + NDVI + NDWI</b>; switch to SVM only if two classes keep getting confused and you have clean training data; use CART when you need a simple, explainable tree.</p>",
          "</div>"
        )
      ))
    })

    draw_bridge_js <- sprintf("
      function(el, x) {
        var map = this;
        map.on(L.Draw.Event.CREATED, function(e) {
          var layer = e.layer, type = e.layerType;
          var uid = 'draw_' + Date.now() + '_' + Math.floor(Math.random()*100000);
          if (type === 'marker') {
            var ll = layer.getLatLng();
            Shiny.setInputValue('%s', {type: 'marker', coords: [ll.lng, ll.lat], uid: uid}, {priority: 'event'});
          } else if (type === 'polygon' || type === 'rectangle') {
            var latlngs = layer.getLatLngs()[0];
            var ring = latlngs.map(function(ll) { return [ll.lng, ll.lat]; });
            Shiny.setInputValue('%s', {type: 'polygon', coords: [ring], uid: uid}, {priority: 'event'});
          }
        });
      }
    ", ns("custom_clean_draw_event"), ns("custom_clean_draw_event"))
    
    output$interactive_map <- renderLeaflet({
      tryCatch({ 
        leaflet(options = leafletOptions(maxZoom = 24)) %>% 
          addProviderTiles(providers$Esri.WorldImagery) %>% 
          setView(lng = 78.9629, lat = 20.5937, zoom = 5) %>% 
          addDrawToolbar(
            targetGroup = "draw_layer",
            polylineOptions = FALSE, circleOptions = FALSE, circleMarkerOptions = FALSE,
            markerOptions = drawMarkerOptions(),
            polygonOptions = drawPolygonOptions(),
            rectangleOptions = drawRectangleOptions(),
            editOptions = editToolbarOptions()
          ) %>%
          htmlwidgets::onRender(js_coords) %>% 
          htmlwidgets::onRender(draw_bridge_js) %>%
          inject_map_elements("Study Area")
      }, error = function(e) { 
        leaflet() %>% addProviderTiles(providers$Esri.WorldImagery) 
      })
    })
    
    output$lulc_pred_map <- renderLeaflet({
      tryCatch({ 
        leaflet(options = leafletOptions(maxZoom = 24)) %>% 
          addProviderTiles(providers$Esri.WorldImagery) %>% 
          setView(78.96, 20.59, 5) %>% 
          htmlwidgets::onRender(js_coords) %>% 
          inject_map_elements("LULC Layout") 
      }, error = function(e) { 
        leaflet() %>% addProviderTiles(providers$Esri.WorldImagery) 
      })
    })
    
    output$change_map <- renderLeaflet({
      tryCatch({ 
        leaflet(options = leafletOptions(maxZoom = 24)) %>% 
          addProviderTiles(providers$Esri.WorldImagery) %>% 
          setView(78.96, 20.59, 5) %>% 
          inject_map_elements("Change Detection") 
      }, error = function(e) { 
        leaflet() %>% addProviderTiles(providers$Esri.WorldImagery) 
      })
    })
    
    observeEvent(input$gt_method, { 
      if (input$gt_method == "draw") { 
        updateTabsetPanel(session, "main_tabs", selected = "draw_tab") 
      } 
    })
    
    observeEvent(input$process_mask_btn, { 
      if(is.null(input$mask_file)) return(log_err("No files uploaded.", "Browse all 4 shapefile components."))
      if(!any(grepl("\\.shp$", tolower(input$mask_file$name)))) return(log_err("Missing .shp file", "Select .shp, .shx, .dbf, and .prj together."))
      
      tryCatch({
        td <- tempdir()
        for(i in 1:nrow(input$mask_file)) {
          file.copy(input$mask_file$datapath[i], file.path(td, input$mask_file$name[i]), overwrite=TRUE)
        }
        p <- file.path(td, input$mask_file$name[grep("\\.shp$", tolower(input$mask_file$name))][1])
        
        v <- st_read(p, quiet=TRUE) %>% st_make_valid() %>% st_zm()
        v <- ensure_crs_4326(v)
        if (!validate_roi_size(v)) return()
        
        dyn_scale <- get_dynamic_scale(v)
        rv$mask_vect <- v
        zoom_to_boundary("interactive_map", v)
        area_sqkm <- tryCatch(round(as.numeric(sum(sf::st_area(v))) / 1e6, 1), error = function(e) NA)
        log_event("INFO", "mod_lulc", "boundary_set", session_id = session$token,
                  area_sqkm = area_sqkm, scale_m = dyn_scale)
        output$lulc_mask_status <- renderUI({ 
          HTML(sprintf("<div style='color:#45936f; font-weight:600; font-size:12px; margin-bottom:10px;'>Boundary Set. Scale: %dm.</div>", dyn_scale)) 
        })
        showNotification("Boundary Set!", type="message") 
      }, error=function(e) {
        log_event("ERROR", "mod_lulc", "boundary_upload_failed", session_id = session$token, error = conditionMessage(e))
        showNotification(paste("Boundary Error:", conditionMessage(e)), type="error")
      })
    })
    
    # Guarded so the app can BOOT without a live Python/rgee binding (CI headless
    # boot + the shinytest2 subprocess don't have Earth Engine's Python available).
    # In production (Docker) Python IS present, so this binds the real pyfunc; in CI
    # it falls back to NULL and the Landsat-fetch path (which needs GEE anyway) is
    # simply not exercised. Prevents a boot-time reticulate crash.
    cloud_mask_func <- tryCatch(rgee::ee_utils_pyfunc(function(image) {
      qa <- image$select('QA_PIXEL')
      mask <- qa$bitwiseAnd(24)$eq(0)
      return(image$updateMask(mask))
    }), error = function(e) NULL)
    
    observeEvent(input$fetch_stac_btn, {
      if(is.null(rv$mask_vect)) return(log_err("Missing Boundary.", "Set boundary first."))
      
      withProgress(message='Fetching Landsat...', {
        tryCatch({
          ee_roi <- sf_as_ee(rv$mask_vect)
          s_d <- as.character(input$stac_dates[1])
          e_d <- as.character(input$stac_dates[2])
          
          col <- get_landsat89_collection(s_d, e_d, ee_roi)
          col <- if (!is.null(col)) col$filterMetadata('CLOUD_COVER', 'less_than', input$stac_cloud) else NULL
          
          if (is.null(col) || col$size()$getInfo() == 0) {
            stop("No Landsat images found for this date range and boundary. Try expanding the dates or increasing cloud tolerance.")
          }
          
          img <- col$median()$clip(ee_roi)
          actual_bands <- c(sprintf('SR_B%d', input$r_band), sprintf('SR_B%d', input$g_band), sprintf('SR_B%d', input$b_band))
          vis <- list(bands=actual_bands, min=7000, max=15000)
          map_info <- img$getMapId(vis)
          bbox <- st_bbox(st_transform(rv$mask_vect, 4326))
          
          leafletProxy("interactive_map", session = session) %>% 
            clearGroup("Landsat Image") %>% 
            clearGroup("Boundary") %>% 
            addTiles(urlTemplate=map_info$tile_fetcher$url_format, group="Landsat Image", options = tileOptions(opacity = 0.35)) %>% 
            addPolygons(data=st_transform(rv$mask_vect, 4326), fill=FALSE, color="#8b3a2b", weight=3, group="Boundary", options = pathOptions(clickable = FALSE)) %>% 
            fitBounds(as.numeric(bbox["xmin"]), as.numeric(bbox["ymin"]), as.numeric(bbox["xmax"]), as.numeric(bbox["ymax"])) %>% 
            addLayersControl(baseGroups = "Satellite", overlayGroups = c("Boundary", "Landsat Image", "Training Data"), options = layersControlOptions(collapsed = FALSE))
          
          rv$gee_landsat_img <- img
          log_msg("Landsat Fetch Complete!")
          showNotification("Cloud Render Complete!", type="message")
        }, error=function(e) log_err(e$message, "Adjust Dates/Cloud Tolerance %"))
      })
    })
    
    open_shp_manager <- function() { 
      showModal(modalDialog(
        title = "Shapefile Data Manager", 
        tabsetPanel(
          tabPanel("1. Legend & Colors", p("Assign names and colors.", style="margin-top:10px; color:#8b3a2b; font-weight:bold;"), div(style = "max-height: 350px; overflow-y: auto; border: 1px solid #eee;", DTOutput(ns("shp_labels_table")))), 
          tabPanel("2. Full Attribute Table", p("Double-click any cell to edit.", style="margin-top:10px; color:#8b3a2b; font-weight:bold;"), div(style = "max-height: 350px; overflow-y: auto; border: 1px solid #eee;", DTOutput(ns("shp_attr_table"))))
        ), 
        size = "l", footer = tagList(actionButton(ns("delete_class_btn"), "Delete Selected Class", class = "btn-danger"), modalButton("Save & Close"))
      )) 
    }
    
    observeEvent(input$edit_shp_labels_btn, {
      open_shp_manager()
    })
    
    sync_training_data <- function(train_vect) {
      detected <- detect_class_column_and_labels(train_vect)
      
      rv$class_labels <- detected$class_labels
      rv$train_vect <- detected$train_vect
      rv$train_col <- detected$train_col
      rv$table_update <- rv$table_update + 1 
      showNotification("Training Data Synced!", type = "message")
      open_shp_manager()
      redraw_features()
    }
    
    observeEvent(input$training_file, { 
      if (is.null(input$training_file)) return()
      is_geojson <- any(grepl("\\.geojson$", tolower(input$training_file$name)))
      is_shapefile <- any(grepl("\\.shp$", tolower(input$training_file$name)))
      if(!is_geojson && !is_shapefile) { return(log_err("Invalid File", "Upload a .geojson file OR all 4 Shapefile components (.shp, .shx, .dbf, .prj)")) }
      
      tryCatch({ 
        temp_dir <- tempdir()
        for (i in 1:nrow(input$training_file)) {
          file.copy(input$training_file$datapath[i], file.path(temp_dir, input$training_file$name[i]), overwrite = TRUE)
        }
        if(is_geojson) { 
          target_file <- file.path(temp_dir, input$training_file$name[grep("\\.geojson$", tolower(input$training_file$name))][1])
        } else { 
          target_file <- file.path(temp_dir, input$training_file$name[grep("\\.shp$", tolower(input$training_file$name))][1]) 
        }
        train_vect <- st_read(target_file, quiet = TRUE) %>% st_zm()
        sync_training_data(train_vect) 
      }, error = function(e) { log_err(e$message, "Ensure files are valid.") }) 
    })
    
    output$shp_labels_table <- renderDT({ 
      shiny::validate(shiny::need(nrow(rv$class_labels) > 0, "No labels available."))
      datatable(rv$class_labels, selection = list(mode = "multiple", target = "row"), rownames = FALSE, editable = list(target = "cell", disable = list(columns = c(0))), options = list(paging = FALSE, dom = 't', scrollY = "300px", scrollCollapse = TRUE)) %>%
        formatStyle('Class_Color', backgroundColor = styleEqual(rv$class_labels$Class_Color, rv$class_labels$Class_Color), color = 'transparent')
    }, server = TRUE)
    
    observeEvent(input$shp_labels_table_cell_edit, { 
      tryCatch({ 
        info <- input$shp_labels_table_cell_edit
        if(is.null(info$value) || trimws(info$value) == "") info$value <- "#808080"
        rv$class_labels <- DT::editData(rv$class_labels, info, rownames = FALSE)
        rv$class_labels$Class_Color <- trimws(rv$class_labels$Class_Color)
        rv$class_labels$Class_Color[rv$class_labels$Class_Color == "" | is.na(rv$class_labels$Class_Color)] <- "#808080"
        redraw_features() 
      }, error = function(e) { showNotification("Edit ignored: Invalid format.", type="warning") }) 
    })
    
    observeEvent(input$delete_class_btn, {
      sel <- input$shp_labels_table_rows_selected
      if (is.null(sel) || length(sel) == 0) return(showNotification("Select at least one class row first.", type = "warning"))
      
      deleted_ids <- rv$class_labels$Class_ID[sel]
      rv$class_labels <- rv$class_labels[-sel, ]
      
      n_pts_removed <- 0
      if (!is.null(rv$train_vect) && !is.null(rv$train_col) && rv$train_col %in% names(rv$train_vect)) {
        matches <- rv$train_vect[[rv$train_col]] %in% deleted_ids
        n_pts_removed <- sum(matches)
        if (n_pts_removed > 0) rv$train_vect <- rv$train_vect[!matches, ]
      }
      
      redraw_features()
      showNotification(sprintf("Deleted %d class(es)%s.", length(deleted_ids), if (n_pts_removed > 0) sprintf(" and %d associated training point(s)/polygon(s)", n_pts_removed) else ""), type = "message")
    })
    
    output$shp_attr_table <- renderDT({
      shiny::validate(shiny::need(rv$train_vect, "No attribute table to show."))
      attr_data <- sf::st_drop_geometry(rv$train_vect)
      # 🚀 (c) Per-feature Area. Geodesic area via sf (works on lon/lat); polygons get a
      # real area, points/lines come out ~0. Appended AFTER the editable attribute columns
      # so the existing cell-edit indexing is unaffected; these 3 trailing columns are
      # marked non-editable below.
      areas_m2 <- tryCatch(as.numeric(sf::st_area(rv$train_vect)), error = function(e) rep(NA_real_, nrow(attr_data)))
      attr_data[["Area (ha)"]]    <- round(areas_m2 / 1e4, 3)
      attr_data[["Area (sq km)"]] <- round(areas_m2 / 1e6, 4)
      attr_data$Geometry_Type <- as.character(sf::st_geometry_type(rv$train_vect))
      disable_cols <- (ncol(attr_data) - 3):(ncol(attr_data) - 1)  # 0-indexed: Area(ha), Area(sqkm), Geometry_Type
      datatable(
        attr_data,
        selection = "multiple",
        rownames = FALSE,
        editable = list(target = "cell", disable = list(columns = disable_cols)),
        options = list(paging = FALSE, dom = 'ft', scrollX = TRUE, scrollY = "300px", scrollCollapse = TRUE)
      )
    }, server = FALSE)
    
    observeEvent(input$shp_attr_table_cell_edit, { 
      tryCatch({ 
        df <- sf::st_drop_geometry(rv$train_vect)
        info <- input$shp_attr_table_cell_edit
        df <- DT::editData(df, info, rownames = FALSE)
        rv$train_vect <- sf::st_set_geometry(df, sf::st_geometry(rv$train_vect))
        redraw_features() 
      }, error = function(e) { showNotification("Edit ignored: Type mismatch.", type="warning") }) 
    })
    
    show_smart_popup <- function(lng, lat, geom_data, type = "point") {
      pending_geom(list(type = type, data = geom_data)) 
      
      options_html <- ""
      show_new_default <- "block" 
      
      if(nrow(rv$class_labels) > 0) {
        show_new_default <- "none" 
        for(i in 1:nrow(rv$class_labels)) {
          cls <- rv$class_labels[i, ]
          val_str <- paste(cls$Class_ID, cls$Class_Name, cls$Class_Color, sep="|")
          selected_attr <- ifelse(cls$Class_ID == rv$last_id, "selected", "")
          options_html <- paste0(options_html, sprintf("<option value='%s' %s>%s (ID: %s)</option>", val_str, selected_attr, cls$Class_Name, cls$Class_ID))
        }
      }
      
      new_selected <- ifelse(nrow(rv$class_labels) == 0, "selected", "")
      options_html <- paste0(options_html, sprintf("<option value='new' %s>Create New Class...</option>", new_selected))
      
      next_id <- ifelse(nrow(rv$class_labels) > 0, max(rv$class_labels$Class_ID, na.rm=TRUE) + 1, 1)
      
      onclick_js <- sprintf("var sel=document.getElementById('class_selector').value; var f_id,f_name,f_col; if(sel==='new' || sel===''){ f_id=document.getElementById('pop_id').value; f_name=document.getElementById('pop_name').value; f_col=document.getElementById('pop_col').value; } else { var p=sel.split('|'); f_id=p[0]; f_name=p[1]; f_col=p[2]; } Shiny.setInputValue('%s', {id: f_id, name: f_name, color: f_col}, {priority: 'event'});", ns("save_point_popup"))
      cancel_js <- sprintf("Shiny.setInputValue('%s', Math.random(), {priority: 'event'});", ns("cancel_draw"))
      
      html_str <- sprintf("
        <div style='width: 170px; font-family: sans-serif;'>
          <div style='font-size:12px; font-weight:bold; color:#26333e; border-bottom:1px solid #eee; padding-bottom:5px; margin-bottom:5px; text-align:center;'>Tag Feature</div>
          <div style='padding: 2px;'>
            <label style='font-size:10px; font-weight:bold; margin-bottom:2px; display:block; color:#5c6b73;'>Class:</label>
            <select id='class_selector' style='width:100%%; border:1px solid #ccc; border-radius:3px; padding:3px; font-size:11px; margin-bottom:5px;' onchange=\"document.getElementById('new_class_div').style.display = (this.value === 'new' || this.value === '') ? 'block' : 'none';\">
              %s
            </select>
            <div id='new_class_div' style='display:%s; background:#f7f6f2; padding:5px; border-radius:3px; margin-bottom:5px; border: 1px solid #eee;'>
              <div style='margin-bottom:4px;'><label style='font-size:10px; display:inline-block; width:35px;'>ID:</label><input type='number' id='pop_id' value='%s' style='width:100px; font-size:10px; padding:2px;'></div>
              <div style='margin-bottom:4px;'><label style='font-size:10px; display:inline-block; width:35px;'>Name:</label><input type='text' id='pop_name' value='New_Class' style='width:100px; font-size:10px; padding:2px;'></div>
              <div><label style='font-size:10px; display:inline-block; width:35px;'>Color:</label><input type='color' id='pop_col' value='#e74c3c' style='width:100px; height:18px; border:none; padding:0; cursor:pointer;'></div>
            </div>
            <div style='display:flex; gap: 4px; margin-top:5px;'>
              <button onclick=\"%s\" style='flex:1; background:#45936f; color:white; border:none; padding:5px; border-radius:3px; font-size:11px; font-weight:bold; cursor:pointer;'>Save</button>
              <button onclick=\"%s\" style='flex:1; background:#8b3a2b; color:white; border:none; padding:5px; border-radius:3px; font-size:11px; font-weight:bold; cursor:pointer;'>Cancel</button>
            </div>
          </div>
        </div>
      ", options_html, show_new_default, next_id, onclick_js, cancel_js)
      
      leafletProxy("interactive_map", session = session) %>% 
        addPopups(lng = lng, lat = lat, popup = HTML(html_str), layerId = "draw_popup", options = popupOptions(closeButton = FALSE, className = "mini-popup"))
    }
    
    processed_draw_events <- reactiveVal(c())
    
    observeEvent(input$custom_clean_draw_event, {
      req(input$gt_method == 'draw')
      data <- input$custom_clean_draw_event
      if (data$uid %in% processed_draw_events()) return()
      processed_draw_events(c(processed_draw_events(), data$uid))
      gtype <- data$type
      coords <- data$coords
      
      if (gtype == "marker" || gtype == "point") {
        lng <- as.numeric(coords[[1]]); lat <- as.numeric(coords[[2]])
        geom <- st_point(c(lng, lat))
        show_smart_popup(lng, lat, geom, "point")
      } else if (gtype %in% c("polygon", "rectangle")) {
        ring <- coords[[1]]
        mat <- do.call(rbind, lapply(ring, function(pt) c(as.numeric(pt[[1]]), as.numeric(pt[[2]]))))
        if (nrow(mat) > 1) {
          dists <- sqrt(diff(mat[,1])^2 + diff(mat[,2])^2)
          mat <- mat[c(TRUE, dists > 1e-7), , drop = FALSE]
        }
        if (nrow(mat) > 0 && !identical(mat[1, ], mat[nrow(mat), ])) { mat <- rbind(mat, mat[1, ]) }
        if (nrow(mat) < 4) {
          leafletProxy("interactive_map", session = session) %>% clearGroup("draw_layer")
          showNotification("Shape is too small or invalid. Please redraw.", type="warning")
          return()
        }
        geom <- tryCatch({ st_make_valid(st_polygon(list(mat))) }, error = function(e) { NULL })
        if (is.null(geom)) {
          leafletProxy("interactive_map", session = session) %>% clearGroup("draw_layer")
          showNotification("Invalid geometry detected. Please redraw.", type="error")
          return()
        }
        last_idx <- max(1, nrow(mat) - 1)
        pop_lng <- mat[last_idx, 1]
        pop_lat <- mat[last_idx, 2]
        show_smart_popup(pop_lng, pop_lat, geom, "polygon")
      }
    })
    
    observeEvent(input$cancel_draw, {
      pg <- pending_geom()
      if(!is.null(pg)) {
        leafletProxy("interactive_map", session = session) %>% clearGroup("draw_layer") %>% removePopup("draw_popup")
        shinyjs::runjs("var map = HTMLWidgets.find('#' + 'lulc_1-interactive_map').getMap(); map.eachLayer(function(l){ if(l.options && l.options.temp_draw_layer){ map.removeLayer(l); } });")
      }
      pending_geom(NULL)
    })
    
    observeEvent(input$save_point_popup, {
      d <- input$save_point_popup
      pg <- pending_geom()
      if(is.null(pg)) return()
      
      rv$last_id <- as.numeric(d$id); rv$last_name <- d$name; rv$last_color <- d$color
      uid_val <- paste0("uid_", sample(1000:9999, 1), "_", as.integer(Sys.time()))
      
      new_feat <- st_sf(uid = uid_val, class_id = rv$last_id, class_name = rv$last_name, color = rv$last_color, geometry = st_sfc(pg$data), crs = 4326)
      
      if(is.null(rv$train_vect)) {
        rv$train_vect <- new_feat
        rv$train_col <- "class_id"
      } else {
        if(!"geometry" %in% names(new_feat)) st_geometry(new_feat) <- "geometry"
        rv$train_vect <- bind_rows(rv$train_vect, new_feat)
      }
      
      new_lbl <- data.frame(Class_ID = rv$last_id, Class_Name = rv$last_name, Class_Color = rv$last_color, stringsAsFactors=FALSE)
      rv$class_labels <- distinct(rbind(rv$class_labels, new_lbl), Class_ID, .keep_all=TRUE)
      
      pending_geom(NULL) 
      leafletProxy("interactive_map", session = session) %>% clearGroup("draw_layer") %>% removePopup("draw_popup")
      shinyjs::runjs("var map = HTMLWidgets.find('#' + 'lulc_1-interactive_map').getMap(); map.eachLayer(function(l){ if(l.options && l.options.temp_draw_layer){ map.removeLayer(l); } });")
      redraw_features()
      showNotification(paste("Saved:", d$name), type="message", duration=2)
    })
    
    observeEvent(input$delete_map_feature, {
      uid_to_del <- input$delete_map_feature
      if(!is.null(rv$train_vect) && nrow(rv$train_vect) > 0) {
        rv$train_vect <- rv$train_vect[rv$train_vect$uid != uid_to_del, ]
        leafletProxy("interactive_map", session = session) %>% removePopup("draw_popup")
        redraw_features()
        showNotification("Feature removed.", type="message")
      }
    })
    
    redraw_features <- function() { 
      leafletProxy("interactive_map", session = session) %>% clearGroup("training_data") 
      if(!is.null(rv$train_vect) && nrow(rv$train_vect) > 0) { 
        pts <- rv$train_vect[st_geometry_type(rv$train_vect) == "POINT", ]
        if(nrow(pts) > 0) {
          coords <- st_coordinates(pts)
          popup_html <- paste0("<div style='text-align:center; min-width:100px;'><b style='font-size:12px;'>", pts$class_name, "</b><br><span style='color:#5c6b73; font-size:10px;'>ID: ", pts$class_id, "</span><br><hr style='margin:4px 0;'><button class='btn btn-danger btn-sm' style='padding:2px 8px; font-size:10px; width:100%; border-radius:3px;' onclick=\"Shiny.setInputValue('", ns("delete_map_feature"), "', '", pts$uid, "', {priority:'event'})\">Remove</button></div>")
          leafletProxy("interactive_map", session = session) %>% addCircleMarkers(lng = coords[,1], lat = coords[,2], radius = 6, color = "white", weight = 2, fillColor = pts$color, fillOpacity = 1, popup = popup_html, group="training_data") 
        }
        polys <- rv$train_vect[st_geometry_type(rv$train_vect) %in% c("POLYGON", "MULTIPOLYGON"), ]
        if(nrow(polys) > 0) {
          popup_html <- ~paste0("<div style='text-align:center; min-width:100px;'><b style='font-size:12px;'>", class_name, "</b><br><span style='color:#5c6b73; font-size:10px;'>ID: ", class_id, "</span><br><hr style='margin:4px 0;'><button class='btn btn-danger btn-sm' style='padding:2px 8px; font-size:10px; width:100%; border-radius:3px;' onclick=\"Shiny.setInputValue('", ns("delete_map_feature"), "', '", uid, "', {priority:'event'})\">Remove</button></div>")
          leafletProxy("interactive_map", session = session) %>% addPolygons(data = polys, fillColor = ~color, fillOpacity = 0.6, color = ~color, weight = 2, group="training_data", popup = popup_html)
        }
      } 
    }
    
    observeEvent(input$edit_pts_btn, { 
      showModal(modalDialog(
        title = "Manage Training Data", p("Double-click any cell to edit.", style="color:#555;"), 
        uiOutput(ns("class_count_summary")),
        DTOutput(ns("shp_attr_table")), 
        footer = tagList(actionButton(ns("delete_pt_btn"), "Delete Selected", class = "btn-danger"), modalButton("Close")),
        size = "l"
      )) 
    })
    
    output$class_count_summary <- renderUI({
      req(rv$train_vect, rv$train_col)
      counts_df <- as.data.frame(table(rv$train_vect[[rv$train_col]]), stringsAsFactors = FALSE)
      names(counts_df) <- c("Class_ID_Chr", "Count")
      
      badges <- lapply(seq_len(nrow(counts_df)), function(i) {
        class_id_chr <- counts_df$Class_ID_Chr[i]
        n <- counts_df$Count[i]
        match_row <- rv$class_labels[as.character(rv$class_labels$Class_ID) == class_id_chr, ]
        name <- if (nrow(match_row) > 0) match_row$Class_Name[1] else paste("Class", class_id_chr)
        is_thin <- n < 3
        tags$span(
          style = sprintf("display:inline-block; margin:2px 6px 2px 0; padding:4px 10px; border-radius:12px; font-size:12px; font-weight:600; background:%s; color:%s;",
                          if (is_thin) "#f5e6e3" else "#e8f0ea", if (is_thin) "#8b3a2b" else "#45936f"),
          sprintf("%s: %d%s", name, n, if (is_thin) " \u26a0" else "")
        )
      })
      
      div(style = "margin-bottom: 12px;", badges)
    })
    
    observeEvent(input$delete_pt_btn, { 
      if(is.null(input$shp_attr_table_rows_selected)) return()
      rv$train_vect <- rv$train_vect[-input$shp_attr_table_rows_selected, ]
      redraw_features() 
    })
    
    observeEvent(input$clear_points_btn, { 
      rv$train_vect <- NULL
      rv$class_labels <- data.frame(Class_ID = integer(), Class_Name = character(), Class_Color = character(), stringsAsFactors = FALSE)
      leafletProxy("interactive_map", session = session) %>% clearGroup("training_data") %>% clearGroup("draw_layer") 
    })
    
    output$download_drawn_data <- downloadHandler(
      filename = function() { paste0("LULC_Training_Data_", Sys.Date(), ".zip") },
      content = function(file) {
        if(is.null(rv$train_vect) || nrow(rv$train_vect) == 0) { writeLines("No data", file); return() }
        tmp_dir <- tempfile("export_")
        dir.create(tmp_dir)
        owd <- setwd(tmp_dir)
        on.exit({ setwd(owd); unlink(tmp_dir, recursive = TRUE) })
        geom_types <- as.character(sf::st_geometry_type(rv$train_vect))
        pts <- rv$train_vect[geom_types %in% c("POINT", "MULTIPOINT"), ]
        polys <- rv$train_vect[geom_types %in% c("POLYGON", "MULTIPOLYGON", "GEOMETRYCOLLECTION"), ]
        if(nrow(pts) > 0) { pts <- suppressWarnings(sf::st_cast(pts, "POINT")); sf::st_write(pts, "LULC_Points.shp", driver = "ESRI Shapefile", quiet = TRUE) }
        if(nrow(polys) > 0) { polys <- suppressWarnings(sf::st_cast(polys, "POLYGON")); sf::st_write(polys, "LULC_Polygons.shp", driver = "ESRI Shapefile", quiet = TRUE) }
        files_to_zip <- list.files()
        zip::zip(zipfile = file, files = files_to_zip)
      }
    )
    
    # =========================================================================
    # ML MODEL EXECUTION (ANTI-CRASH FIXES ADDED)
    # =========================================================================
    observeEvent(input$run_model, {
      if(is.null(rv$mask_vect)) return(log_err("Missing Boundary", "Step 1"))
      if(is.null(rv$gee_landsat_img)) return(log_err("Missing Landsat", "Step 2"))
      if(is.null(rv$train_vect) || nrow(rv$train_vect) < 2) return(log_err("Training Data Needed", "Draw at least 2 points/polygons."))
      if(nrow(rv$class_labels) == 0) return(log_err("Missing Training Data", "Ensure classes and colors are synced before running the model."))
      n_distinct_classes <- length(unique(rv$train_vect[[rv$train_col]]))
      if (n_distinct_classes < 2) return(log_err("Only One Class Found", sprintf("Your training data only has 1 distinct class (%d points/polygons, all the same label). Draw training samples for at least 2 different land-cover classes.", nrow(rv$train_vect))))
      if(!check_rate_limit(rv, "run_lulc_model", cooldown_seconds = 10)) return()
      
      rv$stats_df <- NULL
      rv$acc_df <- NULL
      rv$trained_classifier <- NULL
      
      old_to <- getOption("timeout")
      options(timeout = 3600)
      on.exit(options(timeout = old_to))
      
      
      ee_pts <- sf_as_ee(rv$train_vect)
      t_prop <- rv$train_col
      
      updateTabsetPanel(session, "main_tabs", selected="final_tab")
      
      classifier_label <- switch(input$classifier_type, "cart" = "CART (Decision Tree)", "svm" = "Support Vector Machine", "Random Forest")
      withProgress(message=paste0("Training ", classifier_label, "..."), value=0.3, {
        tryCatch({
          ee_roi <- sf_as_ee(rv$mask_vect)
          
          res <- add_selected_indices(rv$gee_landsat_img, input$calc_indices)
          img <- res$img
          bands <- res$bands
          dyn_scale <- get_dynamic_scale(rv$mask_vect)
          
          band_stats <- tryCatch(
            img$select(bands)$reduceRegion(reducer = ee$Reducer$minMax(), geometry = ee_roi, scale = dyn_scale * 2, maxPixels = 1e13, bestEffort = TRUE, tileScale = 16)$getInfo(),
            error = function(e) NULL
          )
          if (!is.null(band_stats)) {
            b_mins <- sapply(bands, function(b) { v <- band_stats[[paste0(b, "_min")]]; if (is.null(v)) 0 else as.numeric(v) })
            b_maxs <- sapply(bands, function(b) { v <- band_stats[[paste0(b, "_max")]]; if (is.null(v)) 1 else as.numeric(v) })
            b_maxs <- ifelse(b_maxs > b_mins, b_maxs, b_mins + 1)
            img <- apply_saved_normalization(img, bands, b_mins, b_maxs)
          } else {
            img <- img$select(bands)
          }
          
          training <- img$select(bands)$sampleRegions(collection=ee_pts, properties=list(t_prop), scale=dyn_scale, tileScale=16)
          
          n_pts <- nrow(rv$train_vect)
          use_holdout <- n_pts >= 20
          
          if (use_holdout) {
            training_with_random <- training$randomColumn('random', seed = 42)
            train_split <- training_with_random$filter(ee$Filter$lt('random', 0.7))
            test_split <- training_with_random$filter(ee$Filter$gte('random', 0.7))
          } else {
            train_split <- training
            test_split <- NULL
          }
          
          classifier_base <- switch(input$classifier_type,
                                    "cart" = ee$Classifier$smileCart(),
                                    "svm"  = ee$Classifier$libsvm(kernelType = "RBF", gamma = 0.5, cost = 10),
                                    ee$Classifier$smileRandomForest(input$rf_trees) # default / "rf"
          )
          classifier <- classifier_base$train(train_split, t_prop, bands)
          
          rv$trained_classifier_prob <- if (input$classifier_type != "svm") {
            prob_base <- switch(input$classifier_type,
                                "cart" = ee$Classifier$smileCart()$setOutputMode("MULTIPROBABILITY"),
                                ee$Classifier$smileRandomForest(input$rf_trees)$setOutputMode("MULTIPROBABILITY")
            )
            tryCatch(prob_base$train(train_split, t_prop, bands), error = function(e) NULL)
          } else {
            NULL
          }
          
          rv$trained_classifier <- classifier
          rv$classifier_type_used <- input$classifier_type
          rv$rf_trees_used <- if (input$classifier_type == "rf") input$rf_trees else NA
          rv$training_indices_used <- input$calc_indices
          rv$training_points_count <- nrow(rv$train_vect)
          rv$training_bands <- bands
          rv$norm_mins <- if (!is.null(band_stats)) b_mins else NULL
          rv$norm_maxs <- if (!is.null(band_stats)) b_maxs else NULL
          rv$classification_ready_img <- img
          rv$lulc_boundary_used <- rv$mask_vect
          
          classified <- img$select(bands)$classify(classifier)
          
          if (use_holdout) {
            test_classified <- test_split$classify(classifier)
            conf_matrix <- test_classified$errorMatrix(t_prop, 'classification')
            validation_note <- sprintf("Held-out test set (%d points, ~30%% of training data, NOT seen during training)", tryCatch(test_split$size()$getInfo(), error = function(e) NA))
          } else {
            conf_matrix <- classifier$confusionMatrix()
            validation_note <- sprintf("\u26a0 Training data only (resubstitution) — only %d training points available (need 20+ for a genuine held-out test set). This accuracy is likely OPTIMISTIC and does not reflect real-world performance.", n_pts)
          }
          accuracy <- conf_matrix$accuracy()$getInfo()
          kappa <- conf_matrix$kappa()$getInfo()
          
          rv$model_accuracy <- accuracy
          rv$model_kappa <- kappa                                   # persisted for the Insights tab
          rv$model_validation_note <- validation_note               # held-out vs resubstitution
          rv$conf_matrix_array <- tryCatch(conf_matrix$array()$getInfo(), error = function(e) NULL)  # for area CI / F1
          rv$conf_matrix_order <- tryCatch(conf_matrix$order()$getInfo(), error = function(e) NULL)

          pa <- tryCatch(conf_matrix$producersAccuracy()$getInfo(), error=function(e) NULL)
          ua <- tryCatch(conf_matrix$consumersAccuracy()$getInfo(), error=function(e) NULL)
          
          classifier_label <- switch(input$classifier_type, "cart" = "CART (DECISION TREE)", "svm" = "SUPPORT VECTOR MACHINE", "RANDOM FOREST")
          acc_text <- sprintf("========================================================\n        %s VALIDATION REPORT\n========================================================\n\nValidation Method : %s\nOverall Accuracy : %.2f %%\nKappa Coefficient: %.4f\n", classifier_label, validation_note, accuracy * 100, kappa)
          
          acc_df_local <- rv$class_labels[, c("Class_ID", "Class_Name")]
          acc_df_local$Producer_Accuracy <- NA
          acc_df_local$User_Accuracy <- NA
          
          if(!is.null(pa) && !is.null(ua)) {
            pa_mat <- do.call(rbind, pa)
            ua_mat <- do.call(rbind, ua)
            found_classes <- as.integer(rownames(pa_mat))
            if(is.null(found_classes) || length(found_classes) == 0) {
              n_classes <- min(length(pa), nrow(acc_df_local))
              acc_df_local$Producer_Accuracy[1:n_classes] <- round(unlist(pa)[1:n_classes] * 100, 2)
              acc_df_local$User_Accuracy[1:n_classes] <- round(unlist(ua)[1:n_classes] * 100, 2)
            } else {
              n_classes <- min(length(pa), nrow(acc_df_local))
              acc_df_local$Producer_Accuracy[1:n_classes] <- round(unlist(pa)[1:n_classes] * 100, 2)
              acc_df_local$User_Accuracy[1:n_classes] <- round(unlist(ua)[1:n_classes] * 100, 2)
            }
          }
          rv$acc_df <- acc_df_local
          acc_text <- paste0(acc_text, "\nNote: Metrics are based on Out-Of-Bag (OOB) training estimates from Google Earth Engine.")
          rv$acc_text <- acc_text
          
          vis <- get_classification_vis(classified, rv$class_labels)
          
          # 🚀 FIX: "Image.visualize: Cannot provide a palette when visualizing more than one
          # band" — vis$vis_img is ALREADY a fully-visualized RGB (3-band) image, produced by
          # .visualize() inside get_classification_vis(). Passing palette/min/max to getMapId()
          # AGAIN here was applying a palette to that already-multi-band image, which is exactly
          # what this error message describes. getMapId() needs no visualization arguments at all
          # once the image has already been visualized.
          map_info <- vis$vis_img$getMapId()
          bbox <- st_bbox(st_transform(rv$mask_vect, 4326))
          
          leafletProxy("lulc_pred_map", session = session) %>% 
            clearGroup("LULC Map") %>% 
            clearGroup("Boundary") %>% 
            clearControls() %>%
            addTiles(urlTemplate=map_info$tile_fetcher$url_format, group="LULC Map") %>%
            addPolygons(data=st_transform(rv$mask_vect, 4326), fill=FALSE, color="#8b3a2b", weight=3, group="Boundary", options = pathOptions(clickable = FALSE)) %>%
            fitBounds(as.numeric(bbox["xmin"]), as.numeric(bbox["ymin"]), as.numeric(bbox["xmax"]), as.numeric(bbox["ymax"])) %>%
            addLegend("bottomright", colors=unname(trimws(vis$sorted_labels$Class_Color)), labels=vis$sorted_labels$Class_Name, title="Legend", opacity=1) %>% 
            addLayersControl(baseGroups = "Satellite", overlayGroups = c("Boundary", "LULC Map"), position = "bottomleft", options = layersControlOptions(collapsed = TRUE)) %>%
            inject_map_elements("Legend") 
          
          area_groups <- get_area_by_class_groups(classified, ee_roi, dyn_scale)
          
          if (!is.null(area_groups)) {
            total_area <- sum(area_groups$Area_sqm)
            rv$stats_df <- area_groups %>%
              mutate(Area_km2 = round(Area_sqm/1e6, 3), Area_Hectares = round(Area_sqm/10000, 2), Percentage = round((Area_sqm/total_area)*100, 2)) %>%
              left_join(rv$class_labels, by="Class_ID") %>%
              select(Class_Name, Area_km2, Area_Hectares, Percentage, Class_Color) %>%
              arrange(desc(Area_km2))
          }

          # Landscape configuration (patch structure) — best-effort, coarsened grid (EE memory).
          rv$lulc_patch_metrics <- tryCatch({
            total_m2 <- if (!is.null(area_groups)) sum(area_groups$Area_sqm, na.rm = TRUE) else NA_real_
            pscale <- max(as.numeric(dyn_scale) * 3, 30); pxa <- pscale * pscale
            geom <- ee_roi$geometry()
            cimg <- tryCatch(classified$reproject(classified$projection()$atScale(pscale)), error = function(e) classified)
            rrl <- function(img, reducer) tryCatch(as.numeric(img$reduceRegion(reducer = reducer, geometry = geom, scale = pscale, maxPixels = 1e12, bestEffort = TRUE, tileScale = 16)$getInfo()[[1]]), error = function(e) NA_real_)
            psize <- tryCatch(cimg$connectedPixelCount(maxSize = 256, eightConnected = TRUE), error = function(e) NULL)
            maxpix  <- if (!is.null(psize)) rrl(psize, ee$Reducer$max())  else NA_real_
            meanpix <- if (!is.null(psize)) rrl(psize, ee$Reducer$mean()) else NA_real_
            np <- tryCatch(as.numeric(cimg$connectedComponents(connectedness = ee$Kernel$plus(1), maxSize = 256)$
                     select("labels")$reduceRegion(reducer = ee$Reducer$countDistinctNonNull(), geometry = geom,
                     scale = pscale, maxPixels = 1e12, bestEffort = TRUE, tileScale = 16)$get("labels")$getInfo()), error = function(e) NA_real_)
            edge_len <- tryCatch({
              nd <- cimg$reduceNeighborhood(reducer = ee$Reducer$countDistinctNonNull(), kernel = ee$Kernel$square(1))
              as.numeric(nd$gt(1)$selfMask()$reduceRegion(reducer = ee$Reducer$count(), geometry = geom,
                scale = pscale, maxPixels = 1e12, bestEffort = TRUE, tileScale = 16)$getInfo()[[1]]) * pscale
            }, error = function(e) NA_real_)
            landscape_patch_metrics(total_m2, np = np,
              largest_patch_m2 = if (isTRUE(is.finite(maxpix)))  maxpix  * pxa else NA_real_,
              aw_mean_patch_m2 = if (isTRUE(is.finite(meanpix))) meanpix * pxa else NA_real_,
              edge_len_m = edge_len)
          }, error = function(e) NULL)

          rv$available_maps <- unique(c(rv$available_maps, "LULC Classification Map"))
          updateTabsetPanel(session, "main_tabs", selected="stats_tab")
          log_event("INFO", "mod_lulc", "classifier_trained", session_id = session$token,
                    classifier_type = input$classifier_type, n_training_points = n_pts,
                    n_classes = nrow(rv$class_labels), accuracy = round(accuracy, 4), kappa = round(kappa, 4),
                    used_holdout = use_holdout)
          showNotification("Classification Complete!", type="message")
        }, error = function(e) {
          log_event("ERROR", "mod_lulc", "classifier_training_failed", session_id = session$token,
                    classifier_type = input$classifier_type, error = conditionMessage(e))
          showNotification(paste("Processing Error:", e$message), type="error", duration=10)
        })
      })
    })
    
    observeEvent(input$show_confidence_map, {
      if (is.null(rv$trained_classifier_prob)) {
        return(showNotification("Confidence map isn't available for the SVM classifier (probability output isn't reliably supported). Try Random Forest or CART.", type = "warning", duration = 8))
      }
      if (is.null(rv$classification_ready_img) || is.null(rv$mask_vect)) {
        return(showNotification("Run a classification first (Step 4).", type = "error"))
      }
      
      tryCatch({
        boundary_for_render <- rv$lulc_boundary_used %||% rv$mask_vect
        ee_roi <- sf_as_ee(boundary_for_render)
        prob_img <- rv$classification_ready_img$select(rv$training_bands)$classify(rv$trained_classifier_prob)
        confidence_img <- prob_img$arrayReduce(ee$Reducer$max(), list(0L))$arrayFlatten(list(list("confidence")))$clip(ee_roi)
        
        map_info <- confidence_img$getMapId(list(min = 0, max = 1, palette = c('#d73027', '#fee08b', '#1a9850')))
        bbox <- st_bbox(st_transform(boundary_for_render, 4326))
        
        leafletProxy("lulc_pred_map", session = session) %>%
          clearGroup("LULC Map") %>% clearGroup("Boundary") %>% clearControls() %>%
          addTiles(urlTemplate = map_info$tile_fetcher$url_format, group = "LULC Map") %>%
          addPolygons(data = st_transform(boundary_for_render, 4326), fill = FALSE, color = "#8b3a2b", weight = 3, group = "Boundary", options = pathOptions(clickable = FALSE)) %>%
          fitBounds(as.numeric(bbox["xmin"]), as.numeric(bbox["ymin"]), as.numeric(bbox["xmax"]), as.numeric(bbox["ymax"])) %>%
          addLegend("bottomright", colors = c('#d73027', '#fee08b', '#1a9850'), labels = c("Low confidence", "Medium", "High confidence"), title = "Classifier Confidence", opacity = 1) %>%
          inject_map_elements("Confidence")
        
        showNotification("Confidence map generated — red areas are where the classifier is least certain.", type = "message", duration = 6)
        rv$available_maps <- unique(c(rv$available_maps, "LULC Classification Confidence Map"))
      }, error = function(e) {
        showNotification(paste("Confidence Map Error:", e$message), type = "error", duration = 8)
      })
    })
    
    observeEvent(input$show_classes_map, {
      if (is.null(rv$trained_classifier) || is.null(rv$classification_ready_img)) {
        return(showNotification("Run a classification first (Step 4).", type = "error"))
      }
      tryCatch({
        boundary_for_render <- rv$lulc_boundary_used %||% rv$mask_vect
        ee_roi <- sf_as_ee(boundary_for_render)
        classified <- rv$classification_ready_img$select(rv$training_bands)$classify(rv$trained_classifier)
        
        vis <- get_classification_vis(classified, rv$class_labels)
        
        # 🚀 FIX: same double-visualize bug as run_model — vis$vis_img is already a visualized
        # RGB image, so getMapId() needs no arguments here either.
        map_info <- vis$vis_img$getMapId()
        bbox <- st_bbox(st_transform(boundary_for_render, 4326))
        
        leafletProxy("lulc_pred_map", session = session) %>%
          clearGroup("LULC Map") %>% clearGroup("Boundary") %>% clearControls() %>%
          addTiles(urlTemplate = map_info$tile_fetcher$url_format, group = "LULC Map") %>%
          addPolygons(data = st_transform(boundary_for_render, 4326), fill = FALSE, color = "#8b3a2b", weight = 3, group = "Boundary", options = pathOptions(clickable = FALSE)) %>%
          fitBounds(as.numeric(bbox["xmin"]), as.numeric(bbox["ymin"]), as.numeric(bbox["xmax"]), as.numeric(bbox["ymax"])) %>%
          addLegend("bottomright", colors = unname(trimws(vis$sorted_labels$Class_Color)), labels = vis$sorted_labels$Class_Name, title = "Legend", opacity = 1) %>%
          inject_map_elements("Legend")
      }, error = function(e) {
        showNotification(paste("Error:", e$message), type = "error", duration = 8)
      })
    })
    
    # =========================================================================
    # LULC TEMPORAL GRID (ANTI-CRASH FIXES ADDED)
    # =========================================================================
    observeEvent(input$run_lulc_temporal, {
      if(is.null(rv$trained_classifier)) return(log_err("Model Not Trained", "Run Step 4 first to train the ML algorithm."))
      if(is.null(rv$mask_vect)) return(log_err("Missing Boundary", "Step 1"))
      if(input$lulc_temp_start > input$lulc_temp_end) return(log_err("Invalid Year Range", "Start Year must be less than or equal to End Year."))
      if(!check_rate_limit(rv, "run_lulc_temporal", cooldown_seconds = 15)) return()
      
      boundary_for_render <- rv$lulc_boundary_used %||% rv$mask_vect
      ee_roi <- sf_as_ee(boundary_for_render)
      
      rv$temporal_combined_plot <- NULL
      rv$sankey_data <- NULL
      rv$transition_analysis <- NULL
      rv$temporal_stats_df <- NULL
      rv$change_significance <- NULL
      # 🚀 FIX (Save Temporal Grid false error + non-functional export): rv$lulc_grid_urls
      # was READ by the "Save Temporal Grid" guard and the ZIP export, but never WRITTEN —
      # so the button always errored "Generate Temporal Grid first" and the export shipped
      # no tiles. We now capture each year's thumbnail URL below and store them here.
      rv$lulc_grid_urls <- NULL

      raw_years <- seq(input$lulc_temp_start, input$lulc_temp_end, by=as.numeric(input$lulc_temp_gap))
      if(length(raw_years) > 10) return(showNotification("Too many intervals! Keep it under 10 maps.", type="error"))
      
      dyn_scale <- get_dynamic_scale(boundary_for_render)
      
      old_to <- getOption("timeout")
      options(timeout = 3600)
      on.exit(options(timeout = old_to))
      
      
      withProgress(message="Generating Grid & Stabilizing Terrain Features...", value=0, {
        temp_stats_list <- list(); classified_imgs <- list(); plot_list <- list()
        grid_urls_acc <- list()  # 🚀 year -> thumbnail URL, for Save-to-Workspace + ZIP export
        mo <- as.integer(input$lulc_temp_month)
        
        b <- sf::st_bbox(sf::st_transform(boundary_for_render, 4326))
        actual_years <- c() 
        
        for(i in seq_along(raw_years)) {
          yr <- raw_years[i]
          incProgress(0.8 * (i/length(raw_years)), detail=paste("Normalizing Phenology:", yr))
          
          tryCatch({
            img_ready <- fetch_year_landsat_image(yr, mo, ee_roi, input$calc_indices, rv$training_bands, rv$norm_mins, rv$norm_maxs)
            classified <- img_ready$classify(rv$trained_classifier)
            bands_info <- classified$bandNames()$getInfo()
            
            if(length(bands_info) > 0) {
              classified_imgs[[as.character(yr)]] <- classified
              actual_years <- c(actual_years, yr)
              
              vis <- get_classification_vis(classified, rv$class_labels)
              url <- vis$vis_img$getThumbURL(list(region = ee_roi$geometry(), dimensions = 400, format = "png"))
              grid_urls_acc[[as.character(yr)]] <- url  # 🚀 capture for Save-to-Workspace + export
              tmp_png <- tempfile(fileext = ".png")
              download.file(url, tmp_png, mode = "wb", quiet = TRUE)
              img_arr <- png::readPNG(tmp_png)
              unlink(tmp_png)
              
              p <- build_categorical_map_plot(img_arr, b, rv$mask_vect, rv$class_labels, legend_title = "LULC Classes") +
                ggspatial::annotation_scale(location = "bl", style = "bar", text_col="black") +
                ggspatial::annotation_north_arrow(location = "tr", which_north = "true", style = ggspatial::north_arrow_fancy_orienteering()) +
                theme_bw() +
                theme(axis.text.x = element_text(size = 8, color="black", angle = 45, hjust=1), axis.text.y = element_text(size = 8, color="black"), axis.title = element_blank(), plot.margin = margin(2, 2, 2, 2), plot.title = element_text(face="bold", hjust=0.5), legend.position = "right", legend.background = element_rect(fill = alpha("white", 0.8), color = "gray80", linewidth = 0.5), legend.title = element_text(face="bold", size=10), legend.text = element_text(size=9)) +
                coord_sf(xlim = c(as.numeric(b["xmin"]), as.numeric(b["xmax"])), ylim = c(as.numeric(b["ymin"]), as.numeric(b["ymax"])), expand = FALSE, crs = 4326, default_crs = 4326) +
                ggtitle(paste("LULC Classification -", month.abb[mo], yr))
              
              plot_list[[as.character(yr)]] <- p
              map_name <- paste("LULC -", month.abb[mo], yr)
              rv$available_maps <- unique(c(rv$available_maps, map_name))
              
              area_groups <- get_area_by_class_groups(classified, ee_roi, dyn_scale)
              if (!is.null(area_groups)) {
                area_groups$Year <- yr
                temp_stats_list[[as.character(yr)]] <- area_groups
              }
            }
          }, error=function(e){
            log_event("WARN", "mod_lulc", "temporal_grid_year_skipped", session_id = session$token,
                      year = yr, error = conditionMessage(e))
          })
          
          if (i %% 3 == 0) gc(verbose = FALSE)
        }
        
        incProgress(0.9, detail="Computing Transition Matrix...")
        
        if(length(actual_years) >= 2) {
          y1_str <- as.character(actual_years[1])
          y2_str <- as.character(actual_years[length(actual_years)])
          
          if(!is.null(classified_imgs[[y1_str]]) && !is.null(classified_imgs[[y2_str]])) {
            img1 <- classified_imgs[[y1_str]]
            img2 <- classified_imgs[[y2_str]]
            
            transition_img <- img1$multiply(100)$add(img2)$rename('transition')$toInt()
            area_img <- ee$Image$pixelArea()$addBands(transition_img)
            
            tryCatch({
              change_mask <- compute_class_change_mask(img1, img2, min_patch_pixels = 9)
              
              change_vis <- change_mask$visualize(palette = c('#e74c3c'))
              map_info_change <- change_vis$getMapId()
              bbox_change <- st_bbox(st_transform(boundary_for_render, 4326))
              
              leafletProxy("change_map", session = session) %>%
                clearTiles() %>% clearGroup("Boundary") %>% clearControls() %>%
                addProviderTiles(providers$Esri.WorldImagery) %>%
                addTiles(urlTemplate = map_info_change$tile_fetcher$url_format, group = "Change") %>%
                addPolygons(data = st_transform(boundary_for_render, 4326), fill = FALSE, color = "#26333e", weight = 3, group = "Boundary", options = pathOptions(clickable = FALSE)) %>%
                fitBounds(as.numeric(bbox_change["xmin"]), as.numeric(bbox_change["ymin"]), as.numeric(bbox_change["xmax"]), as.numeric(bbox_change["ymax"])) %>%
                addLegend("bottomright", colors = "#e74c3c", labels = sprintf("Changed class (%s \u2192 %s), min. patch ~0.8 ha", y1_str, y2_str), title = "Change Detection", opacity = 1)
              
              rv$available_maps <- unique(c(rv$available_maps, "LULC Change Detection Map"))
              rv$change_detection_years <- c(y1_str, y2_str)
              
              change_binary <- img1$neq(img2)
              change_stats <- tryCatch(
                change_binary$reduceRegion(reducer = ee$Reducer$mean(), geometry = ee_roi, scale = dyn_scale, maxPixels = 1e13, bestEffort = TRUE, tileScale = 16)$getInfo(),
                error = function(e) NULL
              )
              observed_change_pct <- if (!is.null(change_stats) && length(change_stats) > 0) round(as.numeric(change_stats[[1]]) * 100, 1) else NA_real_
              
              noise_floor_pct <- if (!is.null(rv$model_accuracy) && !is.na(rv$model_accuracy)) round((1 - rv$model_accuracy^2) * 100, 1) else NA_real_
              
              rv$change_significance <- list(
                observed_pct = observed_change_pct,
                noise_floor_pct = noise_floor_pct,
                likely_real = if (!is.na(observed_change_pct) && !is.na(noise_floor_pct)) observed_change_pct > noise_floor_pct else NA
              )
            }, error = function(e) {
              showNotification(paste("Change map rendering skipped:", e$message), type = "warning", duration = 6)
            })
            
            stats <- tryCatch({
              area_img$reduceRegion(
                reducer = ee$Reducer$sum()$group(groupField=1, groupName="code"),
                geometry = ee_roi, scale = dyn_scale, maxPixels = 1e13, bestEffort = TRUE, tileScale = 16
              )$getInfo()
            }, error=function(e) NULL)
            
            if(!is.null(stats) && length(stats$groups) > 0) {
              trans_df <- bind_rows(lapply(stats$groups, function(g) data.frame(code=g$code, area_sqm=g$sum)))
              
              trans_df <- trans_df %>%
                mutate(Base_ID = as.integer(floor(code / 100)), Target_ID = as.integer(code %% 100), Area_km2 = area_sqm / 1e6) %>%
                filter(Area_km2 > 0.01)
              
              trans_df <- trans_df %>%
                left_join(rv$class_labels %>% select(Class_ID, Base_Name = Class_Name), by=c("Base_ID"="Class_ID")) %>%
                left_join(rv$class_labels %>% select(Class_ID, Target_Name = Class_Name), by=c("Target_ID"="Class_ID"))
              
              trans_df$Base_Name <- ifelse(is.na(trans_df$Base_Name), paste("Class", trans_df$Base_ID), trans_df$Base_Name)
              trans_df$Target_Name <- ifelse(is.na(trans_df$Target_Name), paste("Class", trans_df$Target_ID), trans_df$Target_Name)
              
              trans_df$Base_Node <- paste0(trans_df$Base_Name, " (", y1_str, ")")
              trans_df$Target_Node <- paste0(trans_df$Target_Name, " (", y2_str, ")")
              
              rv$sankey_data <- trans_df
              rv$transition_analysis <- tryCatch(
                lulc_transition_analysis(trans_df$Base_Name, trans_df$Target_Name, trans_df$Area_km2),
                error = function(e) NULL)
            }
          }
        }

        if(length(plot_list) > 0) {
          rv$temporal_combined_plot <- cowplot::plot_grid(plotlist = plot_list, ncol = 2, align = "hv")
          rv$temporal_grid_years <- actual_years
          rv$lulc_grid_urls <- grid_urls_acc  # 🚀 now the Save button guard + ZIP export both work
          rv$lulc_saved_month <- mo
          
          if(length(temp_stats_list) > 0) {
            all_temp_stats <- bind_rows(temp_stats_list)
            total_areas <- all_temp_stats %>% group_by(Year) %>% summarize(Total_Area = sum(Area_sqm))
            
            rv$temporal_stats_df <- all_temp_stats %>%
              left_join(total_areas, by="Year") %>%
              mutate(Area_km2 = round(Area_sqm/1e6, 3), Area_Hectares = round(Area_sqm/10000, 2), Percentage = round((Area_sqm/Total_Area)*100, 2)) %>%
              left_join(rv$class_labels, by="Class_ID") %>%
              select(Year, Class_Name, Area_km2, Area_Hectares, Percentage, Class_Color) %>%
              arrange(Year, desc(Area_km2))

            # ---- Publish this run into the cross-dataset registry --------------
            # The Statistical Analysis module reads analysis_registry$sets so LULC
            # Temporal runs can be Pearson-correlated on Year with no CSV round-trip.
            if (!is.null(analysis_registry)) {
              yr_rng <- tryCatch(paste(range(all_temp_stats$Year, na.rm = TRUE), collapse = "-"),
                                 error = function(e) "")
              stamp  <- format(Sys.time(), "%H:%M:%S")
              label  <- sprintf("LULC Temporal - %s (%s)", yr_rng, stamp)
              sets <- analysis_registry$sets
              sets[[label]] <- rv$temporal_stats_df
              analysis_registry$sets <- sets
            }
          }
          
          updateTabsetPanel(session, "main_tabs", selected="sankey_tab")
          showNotification("Gallery & Change Matrix Generated!", type="message")
        } else {
          rv$temporal_combined_plot <- NULL 
          showNotification("No data found for selected years. Try a different month.", type="error")
        }
      })
    })
    
    output$transition_plot <- renderPlot({
      shiny::validate(shiny::need(rv$sankey_data, "Please generate the temporal grid & matrix first."))
      df <- as.data.frame(rv$sankey_data)
      
      ggplot(df, aes(x = Target_Name, y = Base_Name, fill = Area_km2)) +
        geom_tile(color = "white", linewidth = 1) +
        geom_text(aes(label = sprintf("%.2f", Area_km2)), color = ifelse(df$Area_km2 > max(df$Area_km2)/2, "white", "black"), fontface = "bold", size = 5) +
        scale_fill_gradient(low = "#e5eef7", high = "#3a6aa0", name = "Area (Sq.Km)") +
        theme_minimal(base_family = "sans") +
        labs(
          x = "Target Year Classification", 
          y = "Base Year Classification", 
          title = "LULC Transition Matrix Heatmap (Sq.Km)"
        ) +
        theme(
          plot.title = element_text(face = "bold", size = 16, color = "#26333e", hjust = 0.5),
          axis.text.x = element_text(size = 12, face = "bold", angle = 45, hjust = 1),
          axis.text.y = element_text(size = 12, face = "bold"),
          axis.title = element_text(size = 14, face = "bold"),
          panel.grid = element_blank()
        )
    })
    
    output$sankey_table <- renderDT({
      shiny::validate(shiny::need(rv$sankey_data, "Please generate the temporal matrix first."))
      disp <- rv$sankey_data %>% 
        select(`Base Year Class`=Base_Name, `Target Year Class`=Target_Name, `Transition Area (Sq.Km)`=Area_km2) %>% 
        arrange(desc(`Transition Area (Sq.Km)`)) %>% 
        mutate(`Transition Area (Sq.Km)` = round(`Transition Area (Sq.Km)`, 3))
      datatable(disp, options=list(dom='t', paging=FALSE), rownames=FALSE)
    }, server = FALSE)

    output$transition_analysis_ui <- renderUI({
      a <- rv$transition_analysis
      if (is.null(a)) return(p("Generate the temporal grid & change matrix (two years) to compute the change budget and intensity analysis.", style="font-size:12px;color:#8b3a2b;"))
      h <- gf_transition_html(a)
      if (!nzchar(h)) return(NULL)
      HTML(h)
    })

    output$lulc_temporal_grid_ui_wrapper <- renderUI({
      shiny::validate(shiny::need(rv$temporal_combined_plot, "Please generate the temporal grid to view the map gallery."))
      n_years <- length(rv$temporal_grid_years)
      plot_h <- max(500, ceiling(n_years / 2) * 450)
      plotOutput(ns("lulc_temporal_grid_plot"), height = paste0(plot_h, "px"))
    })
    
    output$lulc_temporal_grid_plot <- renderPlot({
      shiny::validate(shiny::need(rv$temporal_combined_plot, "Please generate the temporal grid to view the map gallery."))
      rv$temporal_combined_plot
    })
    
    output$temporal_area_plot <- renderPlot({
      shiny::validate(shiny::need(rv$temporal_stats_df, "Please generate the temporal grid first."))
      df <- rv$temporal_stats_df
      pal <- setNames(trimws(rv$class_labels$Class_Color), rv$class_labels$Class_Name)
      ggplot(df, aes(x=factor(Year), y=Area_km2, fill=Class_Name)) +
        geom_bar(stat="identity", position="stack", color="black", linewidth=0.3) +
        scale_fill_manual(values=pal) +
        theme_minimal(base_family="sans") + 
        labs(x="Year", y="Total Area (Sq.Km)", title="Spatiotemporal LULC Transitions Over Time") +
        theme(legend.position="right", plot.title=element_text(face="bold", size=16, color="#26333e"), axis.text=element_text(size=12, face="bold"), axis.title=element_text(size=14, face="bold"))
    })
    
    output$temporal_area_tbl <- renderDT({
      shiny::validate(shiny::need(rv$temporal_stats_df, "Please generate the temporal grid first."))
      # 🚀 (temporal metrics): show BOTH the affected area and its class-wise share (%)
      # of that year's total in each cell, so change is legible in relative as well as
      # absolute terms — e.g. "12.34 km² (18.5%)".
      cellval <- rv$temporal_stats_df %>%
        dplyr::mutate(Cell = sprintf("%.2f km² (%.1f%%)", Area_km2, Percentage))
      wide_df <- cellval %>%
        dplyr::select(Class_Name, Class_Color, Year, Cell) %>%
        tidyr::pivot_wider(names_from = Year, values_from = Cell, names_prefix = "Year_")
      display_df <- wide_df %>% dplyr::select(-Class_Color)
      datatable(display_df, rownames = FALSE, options = list(dom = 't', paging = FALSE, scrollX = TRUE),
                caption = htmltools::tags$caption(style = "caption-side: bottom; text-align: left; font-size: 11px; color: #5c6b73; padding-top: 8px;",
                                                  "Each cell shows the class's area that year and its percentage of the total classified area for that year.")) %>%
        formatStyle('Class_Name', target = 'cell', borderLeft = styleEqual(wide_df$Class_Name, paste('15px solid', wide_df$Class_Color)), fontWeight = 'bold', color = 'black')
    }, server = FALSE)
    
    output$stats_table <- renderDT({ 
      shiny::validate(shiny::need(rv$stats_df, "Run classification to view statistics."))
      datatable(rv$stats_df[, c("Class_Name", "Area_km2", "Area_Hectares", "Percentage")], options=list(dom='t', paging=FALSE)) %>% 
        formatStyle('Class_Name', target='cell', borderLeft=styleEqual(rv$stats_df$Class_Name, paste('15px solid', rv$stats_df$Class_Color)), fontWeight='bold', color='black') 
    }, server = FALSE)
    
    output$stats_bar_plot <- renderPlot({ 
      shiny::validate(shiny::need(rv$stats_df, "Run classification to view the area chart."))
      if("Class_Color" %in% names(rv$stats_df) && nrow(rv$stats_df) > 0) { 
        clean_colors <- trimws(rv$stats_df$Class_Color); names(clean_colors) <- rv$stats_df$Class_Name; 
        ggplot(rv$stats_df, aes(x = reorder(Class_Name, Area_km2), y = Area_km2, fill = Class_Name)) + 
          geom_bar(stat = "identity", color = "black", linewidth = 0.5) + 
          geom_text(aes(label = round(Area_km2, 2)), hjust = -0.2, size = 4, fontface="bold") +
          scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
          scale_fill_manual(values = clean_colors) + 
          coord_flip() + theme_minimal(base_family = "sans") + 
          labs(x = "LULC Classification Categories", y = "Total Area (in Square Kilometers)", title = "Total Area Coverage by Class") + 
          theme(legend.position = "none", plot.title = element_text(face = "bold", size = 16, color = "#26333e"), axis.text = element_text(size = 12, face = "bold"), axis.title = element_text(size = 14, face = "bold")) 
      } 
    })
    
    output$acc_table <- renderDT({
      shiny::validate(shiny::need(rv$acc_df, "Run classification to view accuracy."))
      datatable(rv$acc_df %>% select(-Class_ID), options=list(dom='t', paging=FALSE), rownames=FALSE) %>% 
        formatStyle('Producer_Accuracy', color='#45936f', fontWeight='bold') %>% 
        formatStyle('User_Accuracy', color='#3a6aa0', fontWeight='bold')
    }, server = FALSE)
    
    output$acc_log <- renderText({ paste0(rv$acc_text) })
    output$logs <- renderText({ rv$logs })

    # =====================================================================
    # INSIGHTS & ANALYTICS — passive research-metrics dashboard. Reads ONLY
    # values the classifier/temporal grid already produced (rv$stats_df,
    # rv$temporal_stats_df, rv$acc_df, rv$model_accuracy / model_kappa,
    # rv$conf_matrix_array). No new Earth Engine calls; core logic untouched.
    # =====================================================================
    insights_total_area <- reactive({
      if (!is.null(rv$stats_df) && "Area_km2" %in% names(rv$stats_df)) sum(rv$stats_df$Area_km2, na.rm = TRUE) else NA_real_
    })
    insights_oa_ci <- reactive({
      oa <- rv$model_accuracy
      n  <- tryCatch(sum(unlist(rv$conf_matrix_array), na.rm = TRUE), error = function(e) NA_real_)
      if (is.null(oa) || is.na(oa) || is.na(n) || n <= 0) return(NULL)
      list(oa = oa, n = n, me = 1.96 * sqrt(max(oa * (1 - oa), 0) / n))
    })

    output$insights_headline <- renderUI({
      box <- function(val, lab, col) div(style = sprintf("flex:1; min-width:120px; background:#f7f6f2; border-radius:6px; padding:12px; border-left:4px solid %s;", col),
        div(val, style = "font-size:20px; font-weight:700; color:#26333e;"),
        div(lab, style = "font-size:11px; color:#5c6b73;"))
      ta <- insights_total_area(); nc <- if (!is.null(rv$class_labels)) nrow(rv$class_labels) else 0
      oa <- rv$model_accuracy; kp <- rv$model_kappa
      div(style = "display:flex; gap:12px; flex-wrap:wrap;",
        box(if (is.na(ta)) "—" else sprintf("%.1f", ta), "Study area (km²)", "#2c7a6b"),
        box(if (nc > 0) nc else "—", "Classes", "#6b4c7a"),
        box(if (is.null(oa)) "—" else sprintf("%.1f%%", oa * 100), "Overall accuracy", "#45936f"),
        box(if (is.null(kp)) "—" else sprintf("%.3f", kp), "Kappa", "#3a6aa0"))
    })

    output$insights_area_note <- renderUI({
      if (is.null(rv$stats_df)) return(p("Run a classification (Step 4) to populate area metrics.", style="font-size:12px;color:#8b3a2b;"))
      p(HTML(sprintf("Per-class area over the study region (total <b>%.2f km²</b>). Share = each class's percentage of the mapped area.", insights_total_area())), style="font-size:12px;color:#5c6b73;")
    })
    output$insights_area_table <- renderDT({
      shiny::validate(shiny::need(rv$stats_df, "No classification area data yet."))
      datatable(rv$stats_df %>% select(Class_Name, Area_km2, Area_Hectares, Percentage),
                options = list(dom = 't', paging = FALSE), rownames = FALSE,
                colnames = c("Class", "Area (km²)", "Area (ha)", "Share (%)")) %>%
        formatStyle('Percentage', color = '#26333e', fontWeight = 'bold')
    }, server = FALSE)

    output$insights_diversity <- renderUI({
      if (is.null(rv$stats_df) || !("Area_km2" %in% names(rv$stats_df))) return(NULL)
      h  <- gf_diversity_html(landscape_diversity(rv$stats_df$Area_km2), scope = "land-cover classes")
      ph <- gf_patch_html(rv$lulc_patch_metrics)
      if (!nzchar(h) && !nzchar(ph)) return(NULL)
      HTML(paste0(h, ph))
    })

    output$insights_rate <- renderUI({
      df <- rv$temporal_stats_df
      if (is.null(df) || !all(c("Year","Class_Name","Area_km2") %in% names(df)))
        return(div(style="font-size:12px;color:#8b3a2b;", "Run the LULC Temporal Grid (Step 5) to compute annual rate-of-change."))
      yrs <- sort(unique(df$Year)); if (length(yrs) < 2) return(div(style="font-size:12px;color:#8b3a2b;", "Need at least 2 years in the temporal grid for a rate."))
      rates <- lapply(sort(unique(df$Class_Name)), function(cl) {
        d <- aggregate(Area_km2 ~ Year, df[df$Class_Name == cl, c("Year","Area_km2")], sum)
        if (nrow(d) < 2) return(NULL)
        slope <- tryCatch(as.numeric(coef(stats::lm(Area_km2 ~ Year, d))[2]), error = function(e) NA_real_)
        data.frame(Class = cl, Rate = round(slope, 3), stringsAsFactors = FALSE)
      })
      rd <- do.call(rbind, Filter(Negate(is.null), rates))
      if (is.null(rd) || !nrow(rd)) return(div(style="font-size:12px;color:#8b3a2b;", "Not enough temporal data."))
      items <- vapply(seq_len(nrow(rd)), function(i) {
        rate <- rd$Rate[i]; dir <- if (isTRUE(rate > 0)) "gained" else "lost"; col <- if (isTRUE(rate > 0)) "#45936f" else "#8b3a2b"
        sprintf("<li><b>%s</b>: <span style='color:%s;font-weight:600;'>%s %.3f km²/year</span> (%d–%d)</li>", rd$Class[i], col, dir, abs(rate), min(yrs), max(yrs))
      }, character(1))
      HTML(sprintf("<ul style='margin:4px 0 0 18px;font-size:12px;color:#3a454d;line-height:1.6;'>%s</ul>", paste(items, collapse="")))
    })

    output$insights_validation_note <- renderUI({
      if (is.null(rv$model_accuracy)) return(p("Run a classification (Step 4) to populate validation metrics.", style="font-size:12px;color:#8b3a2b;"))
      ci <- insights_oa_ci()
      extra <- if (!is.null(ci)) sprintf(" Overall-accuracy 95%% margin: <b>±%.1f%%</b> (n = %d validation samples).", ci$me * 100, ci$n) else ""
      div(style="font-size:12px;color:#5c6b73;margin-bottom:8px;",
          HTML(sprintf("Validation: %s.%s <b>F1 / Dice</b> = 2×(Precision×Recall)/(Precision+Recall) — the best score for small or rare classes.", rv$model_validation_note %||% "training estimate", extra)))
    })
    output$insights_validation_table <- renderDT({
      shiny::validate(shiny::need(rv$acc_df, "Run classification to view validation metrics."))
      d <- rv$acc_df
      pa <- as.numeric(d$Producer_Accuracy); ua <- as.numeric(d$User_Accuracy)
      f1 <- round(2 * (ua * pa) / (ua + pa), 2)
      out <- data.frame(Class = d$Class_Name, `Producer's (Recall)` = round(pa, 2),
                        `User's (Precision)` = round(ua, 2), `F1 / Dice` = f1,
                        check.names = FALSE, stringsAsFactors = FALSE)
      datatable(out, options = list(dom = 't', paging = FALSE), rownames = FALSE) %>%
        formatStyle("Producer's (Recall)", color = '#45936f', fontWeight = 'bold') %>%
        formatStyle("User's (Precision)", color = '#3a6aa0', fontWeight = 'bold') %>%
        formatStyle("F1 / Dice", color = '#6b4c7a', fontWeight = 'bold')
    }, server = FALSE)

    # Rigorous accuracy & area (Olofsson 2014 + Pontius & Millones 2011).
    # Computed once from the stored confusion matrix + per-class mapped areas.
    insights_rigorous <- reactive({
      if (is.null(rv$conf_matrix_array) || is.null(rv$conf_matrix_order) ||
          is.null(rv$stats_df) || is.null(rv$class_labels)) return(NULL)
      lab <- rv$class_labels[, c("Class_ID", "Class_Name")]
      ab  <- merge(rv$stats_df[, c("Class_Name", "Area_km2")], lab, by = "Class_Name", all.x = TRUE)
      area_by_id <- stats::setNames(suppressWarnings(as.numeric(ab$Area_km2)), as.character(ab$Class_ID))
      tryCatch(lulc_rigorous_accuracy(rv$conf_matrix_array, rv$conf_matrix_order, area_by_id),
               error = function(e) NULL)
    })

    output$insights_disagreement <- renderUI({
      r <- insights_rigorous()
      if (is.null(r)) return(p(HTML("Run a classification (Step 4) with a held-out test set to compute error-adjusted area and disagreement metrics."), style="font-size:12px;color:#8b3a2b;"))
      chip <- function(val, lab, col) div(style = sprintf("flex:1;min-width:120px;background:#f7f6f2;border-radius:6px;padding:10px;border-left:4px solid %s;", col),
        div(val, style="font-size:18px;font-weight:700;color:#26333e;"), div(lab, style="font-size:11px;color:#5c6b73;"))
      tagList(
        div(style="display:flex;gap:10px;flex-wrap:wrap;margin:6px 0 8px;",
          chip(sprintf("%.1f%%", r$oa_aw * 100), "Area-weighted overall accuracy", "#45936f"),
          chip(sprintf("%.1f%%", r$quantity * 100), "Quantity disagreement", "#c98a3a"),
          chip(sprintf("%.1f%%", r$allocation * 100), "Allocation disagreement", "#8b5a3a"),
          chip(sprintf("%.1f%%", r$total_disagreement * 100), "Total disagreement", "#8b3a2b")),
        div(style="font-size:11px;color:#5c6b73;line-height:1.55;margin-bottom:4px;",
          HTML(paste0(
            "<b>Quantity + Allocation disagreement</b> (Pontius &amp; Millones, 2011) replaces the Kappa coefficient, which is now discouraged in the remote-sensing literature. ",
            "<i>Quantity</i> = disagreement from the wrong <i>total amount</i> of a class; <i>Allocation</i> = right amount but wrong <i>location</i>. ",
            "Total disagreement = 100%% &minus; area-weighted overall accuracy.",
            if (isTRUE(r$unsampled_strata > 0)) sprintf(" <span style='color:#8b3a2b;'>Note: %d mapped class(es) had no validation samples, so their area could not be error-adjusted.</span>", r$unsampled_strata) else "")))
      )
    })

    output$insights_area_ci <- renderUI({
      r <- insights_rigorous()
      if (is.null(r)) return(NULL)
      lab <- rv$class_labels[, c("Class_ID", "Class_Name")]
      pc  <- merge(r$per_class, lab, by = "Class_ID", all.x = TRUE)
      pc  <- pc[order(-pc$Adj_Area), ]
      rows <- vapply(seq_len(nrow(pc)), function(i) {
        nm  <- pc$Class_Name[i] %||% as.character(pc$Class_ID[i])
        ci  <- if (is.finite(pc$CI95[i])) sprintf("&plusmn; %.2f", pc$CI95[i]) else "—"
        lo  <- if (is.finite(pc$CI95[i])) max(0, pc$Adj_Area[i] - pc$CI95[i]) else NA_real_
        hi  <- if (is.finite(pc$CI95[i])) pc$Adj_Area[i] + pc$CI95[i] else NA_real_
        rng <- if (is.finite(lo)) sprintf("%.2f – %.2f", lo, hi) else "—"
        sprintf("<tr><td>%s</td><td style='text-align:right;'>%.2f</td><td style='text-align:right;'>%.2f</td><td style='text-align:right;color:#5c6b73;'>%s</td><td style='text-align:right;color:#5c6b73;'>%s</td></tr>",
                nm, pc$Mapped_Area[i], pc$Adj_Area[i], ci, rng)
      }, character(1))
      div(style="margin-top:10px;",
        HTML(sprintf(paste0(
          "<p style='font-size:12px;color:#5c6b73;margin:6px 0;'><b>Error-adjusted area</b> (Olofsson et al., 2014) corrects the mapped area for classification errors using the validation confusion matrix, and reports a <b>95%% confidence interval</b> — so an area estimate can be quoted honestly as a range, not a single deceptive number.</p>",
          "<table class='table table-sm' style='font-size:12px;'><thead><tr><th>Class</th><th style='text-align:right;'>Mapped (km²)</th><th style='text-align:right;'>Adjusted (km²)</th><th style='text-align:right;'>95%% CI</th><th style='text-align:right;'>Adjusted range (km²)</th></tr></thead><tbody>%s</tbody></table>",
          "<p style='font-size:11px;color:#8a97a0;margin:4px 0 0;'>Total sampled area %.1f km² from %d validation points. CIs assume the validation sample is representative; adjacent pixels are spatially correlated, so treat these as good-practice estimates, not exact bounds.</p>"),
          paste(rows, collapse=""), r$A_total, r$n_total)))
    })

    # ---- Publication exports (HTML report + multi-sheet Excel), mirroring the GEE pipeline. ----
    # Builds a self-contained report from the LULC results already in rv (class areas, standard +
    # rigorous accuracy, landscape metrics, change transition/intensity), reusing the shared pure
    # assemblers. Every rv read is guarded so a partial run still produces a valid file.
    lulc_class_area_df <- reactive({
      sdf <- rv$stats_df; if (is.null(sdf) || !("Area_km2" %in% names(sdf))) return(NULL)
      tryCatch({
        km2 <- suppressWarnings(as.numeric(sdf$Area_km2)); tot <- sum(km2, na.rm = TRUE)
        d <- data.frame(Class = sdf$Class_Name %||% as.character(seq_along(km2)),
                        `Area (km2)` = round(km2, 3), `Area (ha)` = round(km2 * 100, 1),
                        check.names = FALSE, stringsAsFactors = FALSE)
        if (isTRUE(tot > 0)) d[["Share (%)"]] <- round(100 * km2 / tot, 1)
        d
      }, error = function(e) NULL)
    })
    lulc_rigor_named <- reactive({
      rig <- insights_rigorous(); if (is.null(rig)) return(NULL)
      if (!is.null(rig$per_class) && !is.null(rv$class_labels))
        rig$per_class <- tryCatch(merge(rig$per_class, rv$class_labels[, c("Class_ID", "Class_Name")],
                                        by = "Class_ID", all.x = TRUE), error = function(e) rig$per_class)
      rig
    })
    lulc_provenance_df <- reactive({
      km2 <- tryCatch(sum(as.numeric(sf::st_area(rv$mask_vect)), na.rm = TRUE) / 1e6, error = function(e) NA_real_)
      s <- function(x) if (is.null(x) || length(x) != 1 || is.na(x)) "" else as.character(x)
      clf <- if (identical(rv$classifier_type_used, "rf")) "Random Forest" else (rv$classifier_type_used %||% "")
      rows <- list(
        c("Boundary area (km²)", if (isTRUE(is.finite(km2))) sprintf("%.3f", km2) else ""),
        c("Classes", s(if (!is.null(rv$class_labels)) nrow(rv$class_labels) else NULL)),
        c("Classifier", clf),
        c("Trees (Random Forest)", s(rv$rf_trees_used)),
        c("Spectral indices used", if (isTRUE(rv$training_indices_used)) "yes" else if (identical(rv$training_indices_used, FALSE)) "no" else ""),
        c("Training samples", s(rv$training_points_count)),
        c("Overall accuracy", if (isTRUE(is.finite(rv$model_accuracy))) sprintf("%.1f%%", rv$model_accuracy * 100) else ""),
        c("Imagery", "USGS Landsat Collection 2 Level 2 (surface reflectance)"),
        c("Platform", "Google Earth Engine (rgee) — Spatial Research Suite"),
        c("Generated", format(Sys.time(), "%Y-%m-%d %H:%M %Z")))
      rows <- Filter(function(r) nzchar(r[[2]]), rows)
      data.frame(Parameter = vapply(rows, function(r) r[[1]], character(1)),
                 Value = vapply(rows, function(r) r[[2]], character(1)),
                 check.names = FALSE, stringsAsFactors = FALSE)
    })

    output$lulc_dl_report <- downloadHandler(
      filename = function() paste0("LULC_Report_", Sys.Date(), ".html"),
      content = function(file) {
        rig <- lulc_rigor_named()
        accuracy_html <- gf_lulc_accuracy_html(
          oa = tryCatch(as.numeric(rv$model_accuracy), error = function(e) NA_real_),
          kappa = tryCatch(as.numeric(rv$model_kappa), error = function(e) NA_real_), rigor = rig)
        landscape_html <- if (!is.null(rv$stats_df) && "Area_km2" %in% names(rv$stats_df))
          paste0(gf_diversity_html(landscape_diversity(rv$stats_df$Area_km2), scope = "land-cover classes"),
                 gf_patch_html(rv$lulc_patch_metrics)) else ""
        change_html <- if (!is.null(rv$transition_analysis)) gf_transition_html(rv$transition_analysis) else ""
        methods_html <- paste0(
          "<p>Land cover was classified from USGS Landsat Collection 2 Level-2 surface reflectance on Google Earth Engine",
          if (!is.null(rv$classifier_type_used)) paste0(" using a <b>", gf_report_escape(if (identical(rv$classifier_type_used, "rf")) "Random Forest" else rv$classifier_type_used), "</b> classifier") else "",
          if (isTRUE(is.finite(rv$training_points_count))) paste0(" trained on <b>", rv$training_points_count, "</b> labelled samples") else "",
          ". Accuracy was assessed on a held-out validation set; area was error-adjusted following Olofsson et al. (2014) and map agreement summarised with Pontius &amp; Millones (2011) quantity/allocation disagreement instead of Kappa.</p>")
        km2 <- tryCatch(sum(as.numeric(sf::st_area(rv$mask_vect)), na.rm = TRUE) / 1e6, error = function(e) NA_real_)
        payload <- list(
          title = "Land-Cover Classification Report",
          generated = format(Sys.time(), "%Y-%m-%d %H:%M"),
          area_km2 = if (isTRUE(is.finite(km2))) round(km2, 1) else NULL,
          n_classes = if (!is.null(rv$class_labels)) nrow(rv$class_labels) else NULL,
          class_area = lulc_class_area_df(),
          accuracy_html = accuracy_html, landscape_html = landscape_html, change_html = change_html,
          methods_html = methods_html, provenance = lulc_provenance_df(),
          citations = c("USGS Landsat Collection 2 Level 2 Science Products",
                        "Breiman, L. (2001). Random Forests. Machine Learning, 45(1), 5-32.",
                        "Olofsson, P. et al. (2014). Good practices for estimating area and assessing accuracy of land change. Remote Sensing of Environment, 148, 42-57.",
                        "Pontius, R.G. & Millones, M. (2011). Death to Kappa. International Journal of Remote Sensing, 32(15), 4407-4429."),
          map_note = "The classification map is in the LULC Classification Map tab of the app.")
        writeLines(gf_lulc_report_html(payload), file)
      }
    )

    output$lulc_dl_data <- downloadHandler(
      filename = function() paste0("LULC_Data_", Sys.Date(), ".xlsx"),
      content = function(file) {
        sheets <- list()
        ca <- lulc_class_area_df(); if (!is.null(ca)) sheets[["Class_Areas"]] <- ca
        oa <- tryCatch(as.numeric(rv$model_accuracy), error = function(e) NA_real_)
        kp <- tryCatch(as.numeric(rv$model_kappa), error = function(e) NA_real_)
        sheets[["Accuracy"]] <- data.frame(
          Metric = c("Overall accuracy", "Cohen's kappa"),
          Value  = c(if (isTRUE(is.finite(oa))) round(oa, 4) else NA, if (isTRUE(is.finite(kp))) round(kp, 4) else NA),
          stringsAsFactors = FALSE)
        rig <- lulc_rigor_named()
        if (!is.null(rig)) {
          sheets[["Rigorous_Accuracy"]] <- data.frame(
            Metric = c("Area-weighted overall accuracy", "Quantity disagreement", "Allocation disagreement", "Total disagreement"),
            Value  = round(c(rig$oa_aw, rig$quantity, rig$allocation, rig$total_disagreement), 4),
            stringsAsFactors = FALSE)
          if (!is.null(rig$per_class) && nrow(rig$per_class) > 0) {
            pc <- rig$per_class
            sheets[["Error_Adjusted_Area"]] <- data.frame(
              Class = pc$Class_Name %||% as.character(pc$Class_ID),
              Mapped_km2 = round(pc$Mapped_Area, 3), Adjusted_km2 = round(pc$Adj_Area, 3),
              CI95_km2 = round(pc$CI95, 3), stringsAsFactors = FALSE)
          }
        }
        if (!is.null(rv$acc_df)) sheets[["Per_Class_Accuracy"]] <- rv$acc_df
        cm <- rv$conf_matrix_array
        if (!is.null(cm) && !is.null(rv$conf_matrix_order)) sheets[["Confusion_Matrix"]] <- tryCatch({
          m <- as.data.frame(cm); nmc <- as.character(rv$conf_matrix_order)
          if (ncol(m) == length(nmc)) names(m) <- nmc
          cbind(Reference = if (nrow(m) == length(nmc)) nmc else seq_len(nrow(m)), m)
        }, error = function(e) NULL)
        d <- if (!is.null(rv$stats_df) && "Area_km2" %in% names(rv$stats_df)) landscape_diversity(rv$stats_df$Area_km2) else NULL
        if (!is.null(d)) sheets[["Landscape_Diversity"]] <- data.frame(
          Metric = c("Richness (classes)", "Shannon diversity (SHDI)", "Shannon evenness (SHEI)", "Simpson diversity", "Dominant class share (%)"),
          Value  = c(d$richness, round(d$shdi, 3), round(d$shei, 3), round(d$simpson, 3), round(d$dominance * 100, 1)),
          stringsAsFactors = FALSE)
        pm <- rv$lulc_patch_metrics
        if (!is.null(pm)) sheets[["Landscape_Patch"]] <- data.frame(
          Metric = c("Number of patches", "Patch density (/100 ha)", "Mean patch size (ha)", "Area-weighted MPS (ha)", "Largest Patch Index (%)", "Edge density (m/ha)"),
          Value  = c(pm$np, round(pm$pd, 2), round(pm$mps_ha, 2), round(pm$awmps_ha, 2), round(pm$lpi, 1), round(pm$ed, 1)),
          stringsAsFactors = FALSE)
        ta <- rv$transition_analysis
        if (!is.null(ta) && !is.null(ta$per_class)) sheets[["Change_Budget"]] <- tryCatch(as.data.frame(ta$per_class), error = function(e) NULL)
        sheets[["Reproducibility"]] <- lulc_provenance_df()
        sheets <- Filter(Negate(is.null), sheets)
        if (!length(sheets)) sheets[["Info"]] <- data.frame(Note = "Run a classification first, then download.", stringsAsFactors = FALSE)
        writexl::write_xlsx(sheets, path = file)
      }
    )

    observeEvent(input$send_basket_pts, {
      if(is.null(rv$train_vect) || nrow(rv$train_vect) == 0) return(showNotification("No training data drawn.", type="error"))
      curr <- floating_rv$files; new_id <- paste0("file_", as.integer(Sys.time()), "_", sample(1:1000, 1))
      curr[[new_id]] <- list(id = new_id, name = "Custom Training Data (Pts/Polys)", data = rv$train_vect, type = "Shapefile (.shp, .shx, .dbf, .prj)")
      floating_rv$files <- curr
      showNotification("Training Data sent to Clipboard!", type="message")
    })
    
    observeEvent(input$import_basket_train, {
      valid_files <- Filter(function(x) any(sapply(c("polygon", "points", "shapefile"), function(rt) grepl(rt, x$type, ignore.case=TRUE))), floating_rv$files)
      if(length(valid_files) == 0) return(showNotification("No valid data found in Clipboard!", type="error"))
      sync_training_data(st_zm(valid_files[[length(valid_files)]]$data))
    })
    
    observeEvent(input$add_cart_sankey, { if(!is.null(rv$sankey_data)) add_to_workspace("sankey_csv", "LULC Change Matrix (.csv)", 0.50) })
    observeEvent(input$add_cart_csv, { if(!is.null(rv$stats_df)) add_to_workspace("lulc_csv", "LULC Tabular Zonal Metrics (.csv)", 0.50) })
    observeEvent(input$add_cart_plot, { if(!is.null(rv$stats_df)) add_to_workspace("lulc_plot", "LULC Area Distribution Chart (.png)", 0.50) })
    observeEvent(input$add_cart_report, { if(rv$acc_text != "Run classification to view detailed accuracy metrics.") add_to_workspace("acc_report", "Validation Accuracy Report (.txt)", 0.50) })
    
    observeEvent(input$add_cart_temporal_grid_btn, { 
      if(length(rv$lulc_grid_urls) > 0) {
        add_to_workspace(paste0("lulc_gallery_", as.integer(Sys.time())), "Temporal LULC Gallery Images (.png)", 0.50) 
      } else {
        showNotification("Generate Temporal Grid first.", type="error")
      }
    })
    
    observeEvent(input$add_cart_temporal_csv, { if(!is.null(rv$temporal_stats_df)) add_to_workspace("lulc_temp_csv", "LULC Temporal Tabular Metrics (.csv)", 0.50) })
    observeEvent(input$add_cart_temporal_plot, { if(!is.null(rv$temporal_stats_df)) add_to_workspace("lulc_temp_plot", "LULC Temporal Area Chart (.png)", 0.50) })
    
    output$change_significance_ui <- renderUI({
      req(rv$change_significance)
      sig <- rv$change_significance
      if (is.na(sig$likely_real)) return(NULL)
      
      if (isTRUE(sig$likely_real)) {
        div(style = "margin-top:12px; padding:12px 16px; background:#e8f0ea; border-left:4px solid #45936f; border-radius:0 6px 6px 0; font-size:13px;",
            tags$b(style = "color:#26333e;", sprintf("Observed change: %.1f%% \u2014 above the estimated classification-noise floor (%.1f%%).", sig$observed_pct, sig$noise_floor_pct)),
            tags$p(style = "margin:4px 0 0; color:#3d4f5c;", "This suggests the detected change is more likely to reflect genuine land-cover change than classification error alone \u2014 though this is an estimate, not a formal significance test.")
        )
      } else {
        div(style = "margin-top:12px; padding:12px 16px; background:#f5e6e3; border-left:4px solid #8b3a2b; border-radius:0 6px 6px 0; font-size:13px;",
            tags$b(style = "color:#26333e;", sprintf("Observed change: %.1f%% \u2014 within the estimated classification-noise floor (%.1f%%).", sig$observed_pct, sig$noise_floor_pct)),
            tags$p(style = "margin:4px 0 0; color:#3d4f5c;", "This means the detected change could plausibly be explained by classification uncertainty alone (based on your model's own held-out accuracy), not necessarily real land-cover change. Consider this map indicative rather than confirmed.")
        )
      }
    })
    
    # 🚀 INSIGHTS DASHBOARD: analysis-specific summary (Sq Km / Hectares), a donut of
    # class shares, and spatial context (AOI size + engine, read from processing_router.R).
    # Reads the SAME rv$stats_df the Area-Statistics tab uses — one source of truth.
    lulc_insights_payload <- reactive({
      insights_payload_categorical(rv$stats_df, aoi_sf = rv$mask_vect, title = "LULC Classification")
    })
    insights_drawer_server("lulc_insights", lulc_insights_payload)

    outputOptions(output, "interactive_map", suspendWhenHidden = FALSE)
    outputOptions(output, "lulc_pred_map", suspendWhenHidden = FALSE)
    outputOptions(output, "change_map", suspendWhenHidden = FALSE)
    outputOptions(output, "transition_plot", suspendWhenHidden = FALSE)
    outputOptions(output, "temporal_area_plot", suspendWhenHidden = FALSE)
    outputOptions(output, "stats_bar_plot", suspendWhenHidden = FALSE)
  })
}