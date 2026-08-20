# =========================================================================
# MOD_INSIGHTS.R  —  Dynamic Insights Dashboard (floating drawer over the map)
# =========================================================================
# A reusable, self-contained "Research-Ready Insights" panel that floats over
# any map pane and turns a raw map into a decision surface. Host modules build
# a normalized payload (via the insights_payload_* helpers) from their EXISTING
# result reactives and hand it to insights_drawer_server(). The drawer renders
# three cards, all analysis-aware:
#   1. Summary Statistics — analysis-specific units. Sq Km / Hectares for LULC
#      (categorical); Elevation (m) / Slope (degrees) / etc. for continuous
#      rasters, with min / mean / median / max / sd.
#   2. Data Distribution  — a donut of class shares for categorical data; a
#      histogram (with a mean line) for continuous data. ggplot2 only.
#   3. Spatial Context    — AOI size + which engine processed it, read DIRECTLY
#      from processing_router.R (aoi_area_sqkm() / route_engine()).
#
# Deliberately dependency-free: collapse is a pure-CSS checkbox hack (no JS, no
# server round-trip), charts are ggplot2 (already a dependency), and every hook
# into the router or GEE helpers is existence-guarded so this module can never
# crash a host pane even if a helper is missing.
# =========================================================================

# ---- brand palette (kept local so this file is portable) ----
.GF_INK    <- "#26333e"
.GF_FOREST <- "#45936f"
.GF_BLUE   <- "#4a83c4"
.GF_SLATE  <- "#5c6b73"
.GF_CLAY   <- "#c1683b"

# -------------------------------------------------------------------------
# UI — the floating drawer. Place it INSIDE a `.gf-map-shell` (position:relative)
# wrapper that also holds the map, so it anchors to the map, not the page.
# -------------------------------------------------------------------------
insights_drawer_ui <- function(id, title = "Research Insights") {
  ns  <- NS(id)
  chk <- ns("open")
  div(class = "gf-insights",
      # pure-CSS collapse: checked = open (body shown), unchecked = collapsed (tab shown).
      # BUG 4: collapsed by DEFAULT on initial render, without exception (no `checked`),
      # on desktop and mobile alike. The user taps the 'Insights' tab to open it.
      tags$input(type = "checkbox", id = chk, class = "gf-ins-toggle"),
      tags$label(`for` = chk, class = "gf-ins-tab",
                 tags$span(class = "gf-ins-tab-ico", HTML("&#9673;")),
                 tags$span(class = "gf-ins-tab-txt", "Insights")),
      div(class = "gf-ins-body",
          div(class = "gf-ins-head",
              tags$span(class = "gf-ins-title", title),
              uiOutput(ns("engine_badge"), inline = TRUE),
              tags$label(`for` = chk, class = "gf-ins-close", HTML("&times;"))),
          div(class = "gf-ins-scroll",
              div(class = "gf-ins-card",
                  div(class = "gf-ins-card-h", "Summary Statistics"),
                  uiOutput(ns("summary"))),
              uiOutput(ns("dist_card")),
              div(class = "gf-ins-card",
                  div(class = "gf-ins-card-h", "Spatial Context"),
                  uiOutput(ns("context"))))))
}

# -------------------------------------------------------------------------
# SERVER — `payload` is a reactive returning a normalized list (or NULL).
# -------------------------------------------------------------------------
insights_drawer_server <- function(id, payload) {
  moduleServer(id, function(input, output, session) {

    output$engine_badge <- renderUI({
      p <- payload()
      if (is.null(p) || is.null(p$engine)) return(NULL)
      is_local <- identical(p$engine, "local")
      tags$span(class = "gf-ins-eng",
                style = sprintf("background:%s;", if (is_local) .GF_FOREST else .GF_BLUE),
                if (is_local) "Local R" else "Earth Engine")
    })

    output$summary <- renderUI({
      p <- payload()
      if (is.null(p) || is.null(p$kind))
        return(div(class = "gf-ins-empty", "Run an analysis and the live metrics for your area appear here."))
      if (identical(p$kind, "categorical")) .gf_summary_categorical(p) else .gf_summary_continuous(p)
    })

    # Only CONTINUOUS analyses (DEM/Slope/LST/indices) get a distribution chart. For categorical
    # LULC the class-wise area breakdown in Summary Statistics already conveys the distribution, so
    # a donut here is redundant clutter — the whole card is omitted.
    output$dist_card <- renderUI({
      p <- payload()
      if (is.null(p) || !identical(p$kind, "continuous")) return(NULL)
      div(class = "gf-ins-card",
          div(class = "gf-ins-card-h", "Data Distribution"),
          plotOutput(session$ns("dist"), height = "175px"))
    })
    output$dist <- renderPlot({
      p <- payload()
      shiny::validate(shiny::need(!is.null(p) && identical(p$kind, "continuous"), "Awaiting analysis output."))
      insights_histogram(p)
    }, bg = "transparent")

    output$context <- renderUI({ .gf_context_ui(payload()) })
  })
}

# =========================================================================
# PAYLOAD BUILDERS — host modules call these from their result reactives.
# =========================================================================

# Categorical (LULC): expects a stats_df with Class_Name, Area_km2 and
# (optionally) Area_Hectares, Percentage, Class_Color.
insights_payload_categorical <- function(stats_df, aoi_sf = NULL, title = "Classification") {
  if (is.null(stats_df) || !is.data.frame(stats_df) || !nrow(stats_df)) return(NULL)
  km2 <- suppressWarnings(as.numeric(stats_df$Area_km2))
  classes <- data.frame(
    name     = as.character(stats_df$Class_Name),
    area_km2 = km2,
    area_ha  = if ("Area_Hectares" %in% names(stats_df)) suppressWarnings(as.numeric(stats_df$Area_Hectares)) else km2 * 100,
    pct      = if ("Percentage" %in% names(stats_df)) suppressWarnings(as.numeric(stats_df$Percentage)) else NA_real_,
    color    = if ("Class_Color" %in% names(stats_df)) trimws(as.character(stats_df$Class_Color)) else NA_character_,
    stringsAsFactors = FALSE
  )
  list(
    kind = "categorical", title = title, classes = classes,
    total_km2 = sum(classes$area_km2, na.rm = TRUE),
    total_ha  = sum(classes$area_ha,  na.rm = TRUE),
    engine = .gf_engine(aoi_sf), spatial = .gf_spatial(aoi_sf)
  )
}

# Continuous (DEM / Slope / LST / indices): reads the Metric/Value zonal table
# (build_zonal_stats_df output) and the Bin/Count histogram frame.
insights_payload_continuous <- function(zonal_df, hist_df, feature, units = NULL, aoi_sf = NULL) {
  if (is.null(zonal_df) && is.null(hist_df)) return(NULL)
  gv <- function(metric) {
    if (is.null(zonal_df) || !("Metric" %in% names(zonal_df))) return(NA_real_)
    v <- suppressWarnings(as.numeric(zonal_df$Value[zonal_df$Metric == metric]))
    if (length(v)) v[1] else NA_real_
  }
  if (is.null(units)) units <- tryCatch(get_feature_units(feature), error = function(e) "")
  hist_norm <- NULL
  if (!is.null(hist_df) && is.data.frame(hist_df) && "Bin" %in% names(hist_df)) {
    yv <- if ("Count" %in% names(hist_df)) hist_df$Count
          else if ("Area_SqKm" %in% names(hist_df)) hist_df$Area_SqKm
          else hist_df[[ncol(hist_df)]]
    hist_norm <- data.frame(bin_mid = suppressWarnings(as.numeric(hist_df$Bin)),
                            value   = suppressWarnings(as.numeric(yv)))
    hist_norm <- hist_norm[is.finite(hist_norm$bin_mid) & is.finite(hist_norm$value), , drop = FALSE]
    if (!nrow(hist_norm)) hist_norm <- NULL
  }
  list(
    kind = "continuous", title = feature, feature = feature,
    units = if (is.null(units)) "" else units,
    min = gv("Minimum"), mean = gv("Mean"), median = gv("Median"),
    max = gv("Maximum"), sd = gv("Std. Deviation"),
    hist = hist_norm,
    engine = .gf_engine(aoi_sf), spatial = .gf_spatial(aoi_sf)
  )
}

# =========================================================================
# INTERNAL RENDERERS
# =========================================================================

# ---- read the local processing logic (existence-guarded) ----
.gf_aoi_km2 <- function(aoi_sf) {
  if (is.null(aoi_sf) || !exists("aoi_area_sqkm")) return(NA_real_)
  tryCatch(aoi_area_sqkm(aoi_sf), error = function(e) NA_real_)
}
.gf_engine <- function(aoi_sf) {
  if (is.null(aoi_sf) || !exists("route_engine")) return(NULL)
  tryCatch(route_engine(aoi_sf), error = function(e) NULL)
}
# Real spatial context computed from the AOI sf object: feature count, area, centroid, and the
# exact bounding box — genuinely useful metadata, not basemap attribution.
.gf_spatial <- function(aoi_sf) {
  if (is.null(aoi_sf)) return(NULL)
  bb  <- tryCatch(as.numeric(sf::st_bbox(aoi_sf)), error = function(e) NULL)          # xmin,ymin,xmax,ymax
  ctr <- tryCatch(as.numeric(sf::st_coordinates(
           sf::st_centroid(sf::st_union(sf::st_geometry(aoi_sf))))[1, 1:2]),          # lon, lat
           error = function(e) NULL)
  list(
    km2      = .gf_aoi_km2(aoi_sf),
    bbox     = bb,
    centroid = ctr,
    nfeat    = tryCatch(nrow(aoi_sf), error = function(e) NA_integer_)
  )
}

.gf_num <- function(val, unit = NULL, digits = 2) {
  if (is.null(val) || length(val) != 1 || is.na(val)) return("—")
  out <- formatC(val, format = "f", big.mark = ",", digits = digits)
  if (!is.null(unit) && nzchar(unit)) paste0(out, " ", unit) else out
}

.gf_summary_categorical <- function(p) {
  cls <- p$classes[order(-p$classes$area_km2), , drop = FALSE]
  top <- utils::head(cls, 6)
  rows <- lapply(seq_len(nrow(top)), function(i) {
    col <- if (is.na(top$color[i])) .GF_SLATE else top$color[i]
    pct <- if (is.na(top$pct[i])) "" else sprintf(" · %s%%", .gf_num(top$pct[i], digits = 1))
    div(class = "gf-ins-row",
        tags$span(class = "gf-ins-chip", style = sprintf("background:%s;", col)),
        tags$span(class = "gf-ins-row-name", top$name[i]),
        tags$span(class = "gf-ins-row-val", HTML(sprintf("%s km<sup>2</sup>%s", .gf_num(top$area_km2[i], digits = 1), pct))))
  })
  tagList(
    div(class = "gf-ins-hero",
        div(class = "gf-ins-hero-num", .gf_num(p$total_km2, digits = 1)),
        div(class = "gf-ins-hero-lab", HTML("sq km · total mapped area"))),
    div(class = "gf-ins-sub", sprintf("%s hectares · %d classes",
                                      .gf_num(p$total_ha, digits = 0), nrow(p$classes))),
    div(class = "gf-ins-rows", rows)
  )
}

# Auto-interpretation: one plain-language sentence read off the summary stats
# (spread relative to the mean + mean-vs-median skew). Returns NULL when the
# numbers aren't available, so nothing renders rather than a broken sentence.
.gf_continuous_narrative <- function(p) {
  feat <- if (is.null(p$feature)) "This feature" else p$feature
  u <- if (is.null(p$units) || !nzchar(p$units)) "" else paste0(" ", p$units)
  m <- p$mean; md <- p$median; sd <- p$sd; mn <- p$min; mx <- p$max
  if (is.null(m) || is.na(m) || is.null(mx) || is.na(mx) || is.null(mn) || is.na(mn)) return(NULL)
  rng <- mx - mn
  cv <- if (!is.null(sd) && !is.na(sd) && abs(m) > 1e-9) abs(sd / m) else NA_real_
  spread <- if (is.na(cv)) "varies" else if (cv < 0.15) "is fairly uniform" else if (cv < 0.5) "shows moderate variation" else "varies widely"
  skew <- ""
  if (!is.null(md) && !is.na(md) && !is.null(sd) && !is.na(sd) && sd > 1e-9) {
    d <- (m - md) / sd
    if (d > 0.2) skew <- " Most of the area sits below the mean, with a smaller number of high values pulling the average up."
    else if (d < -0.2) skew <- " Most of the area sits above the mean, with a few low values pulling the average down."
  }
  sprintf("%s averages %s%s across your area and %s (from %s to %s%s).%s",
          feat, .gf_num(m), u, spread, .gf_num(mn), .gf_num(mx), u, skew)
}

.gf_summary_continuous <- function(p) {
  u <- if (is.null(p$units)) "" else p$units
  feat <- if (is.null(p$feature)) "value" else p$feature
  cell <- function(lab, val) div(class = "gf-ins-cell",
                                 div(class = "gf-ins-cell-v", .gf_num(val)),
                                 div(class = "gf-ins-cell-l", lab))
  narrative <- .gf_continuous_narrative(p)
  tagList(
    div(class = "gf-ins-hero",
        div(class = "gf-ins-hero-num", .gf_num(p$mean)),
        div(class = "gf-ins-hero-lab",
            sprintf("mean %s%s", tolower(feat), if (nzchar(u)) sprintf(" (%s)", u) else ""))),
    div(class = "gf-ins-grid",
        cell("Min", p$min), cell("Median", p$median),
        cell("Max", p$max), cell("Std dev", p$sd)),
    if (!is.null(narrative))
      div(style = "margin-top:10px; padding:9px 11px; background:rgba(74,131,196,0.10); border-left:3px solid #4a83c4; border-radius:4px; font-size:12px; line-height:1.5; color:#26333e;",
          tags$b("Interpretation: "), narrative)
  )
}

.gf_ctx_row <- function(lab, val) {
  div(class = "gf-ins-ctx-row",
      tags$span(class = "gf-ins-ctx-lab", lab),
      tags$span(class = "gf-ins-ctx-val", val))
}
.gf_bbox_cell <- function(dir, deg) {
  div(class = "gf-ins-bbox-cell",
      tags$span(class = "gf-ins-bbox-dir", dir),
      tags$span(class = "gf-ins-bbox-deg", sprintf("%.3f°", deg)))
}
.gf_context_ui <- function(p) {
  if (is.null(p))
    return(div(class = "gf-ins-empty", "Set an area of interest to see its spatial context."))
  sp <- p$spatial
  if (is.null(sp))
    return(div(class = "gf-ins-empty", "Spatial context appears once an area of interest is set."))

  rows <- list()
  if (!is.null(sp$nfeat) && !is.na(sp$nfeat))
    rows <- c(rows, list(.gf_ctx_row("Features", format(sp$nfeat, big.mark = ","))))
  if (!is.null(sp$km2) && !is.na(sp$km2))
    rows <- c(rows, list(.gf_ctx_row("Extent", sprintf("%s km²", .gf_num(sp$km2, digits = 1)))))
  if (!is.null(sp$centroid) && length(sp$centroid) == 2 && all(is.finite(sp$centroid)))
    rows <- c(rows, list(.gf_ctx_row("Center", sprintf("%.3f, %.3f", sp$centroid[2], sp$centroid[1]))))  # lat, lon
  if (!is.null(p$engine))
    rows <- c(rows, list(.gf_ctx_row("Engine",
             if (identical(p$engine, "local")) "Local R (sf/terra)" else "Earth Engine")))

  bbox_ui <- NULL
  if (!is.null(sp$bbox) && length(sp$bbox) == 4 && all(is.finite(sp$bbox))) {
    bbox_ui <- div(class = "gf-ins-bbox",
      div(class = "gf-ins-bbox-h", "Bounding box"),
      div(class = "gf-ins-bbox-grid",
          .gf_bbox_cell("N", sp$bbox[4]), .gf_bbox_cell("S", sp$bbox[2]),
          .gf_bbox_cell("E", sp$bbox[3]), .gf_bbox_cell("W", sp$bbox[1])))
  }
  tagList(div(class = "gf-ins-ctx-rows", rows), bbox_ui)
}

# =========================================================================
# CHART BUILDERS (ggplot2)
# =========================================================================
.gf_blank_plot <- function(msg) {
  ggplot2::ggplot() +
    ggplot2::annotate("text", x = 0, y = 0, label = msg, size = 3.1, colour = .GF_SLATE) +
    ggplot2::theme_void()
}

insights_donut <- function(p) {
  df <- p$classes
  df <- df[is.finite(df$area_km2) & df$area_km2 > 0, , drop = FALSE]
  if (!nrow(df)) return(.gf_blank_plot("No class areas yet"))
  pal <- df$color
  pal[is.na(pal) | !nzchar(pal)] <- .GF_SLATE
  names(pal) <- df$name
  df$name <- factor(df$name, levels = df$name)
  ggplot2::ggplot(df, ggplot2::aes(x = 2, y = area_km2, fill = name)) +
    ggplot2::geom_col(width = 1, colour = "white", linewidth = 0.4) +
    ggplot2::coord_polar(theta = "y") +
    ggplot2::xlim(0.5, 2.5) +
    ggplot2::scale_fill_manual(values = pal) +
    ggplot2::theme_void(base_family = "sans") +
    ggplot2::theme(
      legend.position   = "right",
      legend.text       = ggplot2::element_text(size = 8, colour = .GF_INK),
      legend.title      = ggplot2::element_blank(),
      legend.key.size   = ggplot2::unit(9, "pt"),
      legend.margin     = ggplot2::margin(0, 0, 0, 0),
      plot.margin       = ggplot2::margin(2, 2, 2, 2)
    )
}

insights_histogram <- function(p) {
  df <- p$hist
  if (is.null(df) || !nrow(df)) return(.gf_blank_plot("No distribution data yet"))
  n  <- max(length(df$bin_mid), 1)
  bw <- if (n > 1) (diff(range(df$bin_mid)) / n) * 0.95 else 1
  gg <- ggplot2::ggplot(df, ggplot2::aes(x = bin_mid, y = value)) +
    ggplot2::geom_col(fill = .GF_BLUE, colour = "white", linewidth = 0.15, width = bw) +
    ggplot2::theme_minimal(base_family = "sans") +
    ggplot2::labs(
      x = paste0(p$feature, if (nzchar(if (is.null(p$units)) "" else p$units)) sprintf(" (%s)", p$units) else ""),
      y = "Frequency") +
    ggplot2::theme(
      axis.text        = ggplot2::element_text(size = 7, colour = .GF_SLATE),
      axis.title       = ggplot2::element_text(size = 8, colour = .GF_INK),
      panel.grid.minor = ggplot2::element_blank(),
      plot.margin      = ggplot2::margin(2, 6, 2, 2)
    )
  if (!is.null(p$mean) && !is.na(p$mean))
    gg <- gg + ggplot2::geom_vline(xintercept = p$mean, linetype = "dashed",
                                   colour = .GF_CLAY, linewidth = 0.6)
  gg
}
