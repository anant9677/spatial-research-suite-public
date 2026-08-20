# =========================================================================
# EXPORT_ENGINE.R  —  Pure, dependency-injected export/bundling engine
# =========================================================================
# WHY THIS FILE EXISTS
# --------------------
# The ZIP + PDF export used to live as a single ~230-line downloadHandler
# inside app.R. That one function:
#   * did ~15 different jobs (SRP violation),
#   * read 26 different reactiveValues fields directly across rv / gee_rv /
#     carto_rv / stats_rv (hidden dependencies — a rename in ANY module could
#     silently break the export), and
#   * "knew" which module produced which piece of data (no black-box boundary).
#
# This file replaces that with three layers, none of which touch Shiny or the
# `rv` objects:
#   1. BUILDERS  — pure functions that turn plain data into a ggplot / table.
#   2. WRITERS   — pure functions that persist a plain object into a directory
#                  and return the file paths they created.
#   3. MANIFEST + BUNDLER — a declarative list of "what can be exported" plus a
#                  driver that, given a plain `state` list and the cart IDs,
#                  produces the ZIP. It has ZERO knowledge of where `state`
#                  came from — that is the dependency-injection / black-box seam.
#
# The ONLY code that still touches the reactive objects is the tiny adapter
# `collect_export_state()` in app.R (see the orchestration snippet). Swap that
# adapter for a fake list and the entire engine is unit-testable with no GEE,
# no Python, no Shiny — see tests/testthat/test-export-engine.R.
#
# BEHAVIOUR IS UNCHANGED: same filenames, folders, formats, DPIs, conditions
# and ordering as the original handler. This is a pure structural refactor.
# =========================================================================

# -------------------------------------------------------------------------
# 1. WRITERS  — persist a plain object, return the created file path(s).
#    Each does exactly one thing and never reads global/reactive state.
# -------------------------------------------------------------------------

# Writes a data.frame as BOTH .csv and .xlsx (the app's standard table export).
write_table_files <- function(df, out_dir, base_name) {
  csv_path  <- file.path(out_dir, paste0(base_name, ".csv"))
  xlsx_path <- file.path(out_dir, paste0(base_name, ".xlsx"))
  utils::write.csv(df, csv_path, row.names = FALSE)
  writexl::write_xlsx(df, xlsx_path)
  c(csv_path, xlsx_path)
}

# Writes a plain text file (e.g. the ML accuracy report).
write_text_file <- function(text, out_dir, filename) {
  path <- file.path(out_dir, filename)
  writeLines(text, path)
  path
}

# Saves a prebuilt ggplot. DPI defaults to 150 (the app's cost-optimised value).
save_ggplot <- function(plot_obj, out_dir, filename, width, height, dpi = 150) {
  path <- file.path(out_dir, filename)
  ggplot2::ggsave(path, plot = plot_obj, width = width, height = height, dpi = dpi)
  path
}

# Writes an sf object as an ESRI Shapefile inside its own sub-folder and returns
# every sidecar file (.shp/.shx/.dbf/.prj) the driver produced.
write_vector_shapefile <- function(sf_obj, out_dir, subdir, layer_basename) {
  d <- file.path(out_dir, subdir)
  dir.create(d, showWarnings = FALSE)
  sf::st_write(sf_obj, file.path(d, paste0(layer_basename, ".shp")),
               delete_layer = TRUE, quiet = TRUE)
  list.files(d, full.names = TRUE)
}

# Writes a terra raster as a GeoTIFF.
write_raster_tif <- function(rast, out_dir, filename) {
  path <- file.path(out_dir, filename)
  terra::writeRaster(rast, path, filetype = "GTiff", overwrite = TRUE)
  path
}

# Downloads a named list of image URLs into a sub-folder. Names become the
# <prefix>_<name>.png filenames. Individual failures are skipped (as before).
download_url_gallery <- function(url_list, out_dir, subdir, prefix) {
  d <- file.path(out_dir, subdir)
  dir.create(d, showWarnings = FALSE)
  for (nm in names(url_list)) {
    try(
      utils::download.file(url_list[[nm]],
                           file.path(d, paste0(prefix, "_", nm, ".png")),
                           mode = "wb", quiet = TRUE),
      silent = TRUE
    )
  }
  list.files(d, full.names = TRUE)
}

# -------------------------------------------------------------------------
# 2. BUILDERS  — turn plain data into a ggplot / reshaped table. No I/O.
#    (The chart specs are copied verbatim from the original handler so the
#     rendered output is byte-for-byte the same.)
# -------------------------------------------------------------------------

build_lulc_area_chart <- function(stats_df) {
  clean_colors <- trimws(stats_df$Class_Color)
  names(clean_colors) <- stats_df$Class_Name
  ggplot2::ggplot(stats_df, ggplot2::aes(x = stats::reorder(Class_Name, Area_km2),
                                         y = Area_km2, fill = Class_Name)) +
    ggplot2::geom_bar(stat = "identity", color = "black") +
    ggplot2::scale_fill_manual(values = clean_colors) +
    ggplot2::coord_flip() +
    ggplot2::theme_minimal(base_family = "sans") +
    ggplot2::labs(x = "LULC Classification Categories",
                  y = "Total Area (in Square Kilometers)",
                  title = "Total Area Coverage by Class") +
    ggplot2::theme(legend.position = "none",
                   plot.title = ggplot2::element_text(face = "bold", size = 16, color = "#26333e"),
                   axis.text = ggplot2::element_text(size = 12, face = "bold"),
                   axis.title = ggplot2::element_text(size = 14, face = "bold"))
}

build_lulc_temporal_chart <- function(temporal_stats_df, class_labels) {
  pal <- stats::setNames(trimws(class_labels$Class_Color), class_labels$Class_Name)
  ggplot2::ggplot(temporal_stats_df,
                  ggplot2::aes(x = factor(Year), y = Area_km2, fill = Class_Name)) +
    ggplot2::geom_bar(stat = "identity", position = "stack", color = "black", linewidth = 0.3) +
    ggplot2::scale_fill_manual(values = pal) +
    ggplot2::theme_minimal(base_family = "sans") +
    ggplot2::labs(x = "Year", y = "Total Area (Sq.Km)",
                  title = "Spatiotemporal LULC Transitions Over Time") +
    ggplot2::theme(legend.position = "right",
                   plot.title = ggplot2::element_text(face = "bold", size = 16, color = "#26333e"),
                   axis.text = ggplot2::element_text(size = 12, face = "bold"),
                   axis.title = ggplot2::element_text(size = 14, face = "bold"))
}

# Reshapes the temporal stats into the wide (one column per year) CSV layout.
build_temporal_wide <- function(temporal_stats_df) {
  temporal_stats_df |>
    dplyr::select(Class_Name, Year, Area_km2) |>
    tidyr::pivot_wider(names_from = Year, values_from = Area_km2, names_prefix = "Year_")
}

build_timeseries_chart <- function(ts_data, feature) {
  df_plot <- ts_data[!is.na(ts_data$Value), ]
  p <- ggplot2::ggplot(df_plot, ggplot2::aes(x = Month, y = Value, group = 1)) +
    ggplot2::geom_line(color = "#8e44ad", linewidth = 1.5) +
    ggplot2::geom_point(size = 4, color = "#26333e") +
    ggplot2::scale_x_discrete(drop = FALSE) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0.1, 0.2))) +
    ggplot2::theme_minimal(base_family = "sans") +
    ggplot2::labs(title = paste("Temporal Trend:", feature),
                  subtitle = "Real 12-month variation.", y = feature, x = "Month") +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", size = 16, color = "#26333e"),
                   plot.subtitle = ggplot2::element_text(color = "#5c6b73", face = "italic", size = 12),
                   axis.text = ggplot2::element_text(size = 10, face = "bold", angle = 45, hjust = 1),
                   axis.title = ggplot2::element_text(size = 14, face = "bold"))
  if (nrow(df_plot) > 2 && !feature %in% c("Elevation (DEM)", "Terrain Slope")) {
    p <- p + ggplot2::geom_smooth(method = "loess", se = FALSE,
                                  color = "gray50", linetype = "dashed", linewidth = 1)
  }
  p
}

build_histogram_chart <- function(hist_data, feature, mean_val, med_val) {
  p <- ggplot2::ggplot(hist_data, ggplot2::aes(x = Bin, y = Count)) +
    ggplot2::geom_col(fill = "#4a83c4", color = "black", alpha = 0.8) +
    ggplot2::theme_minimal(base_family = "sans") +
    ggplot2::labs(title = paste("Pixel Distribution:", feature),
                  x = "Value Range", y = "Pixel Count") +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", size = 16, color = "#26333e"),
                   plot.subtitle = ggplot2::element_text(color = "#5c6b73", face = "italic", size = 12),
                   axis.text = ggplot2::element_text(size = 12, face = "bold"),
                   axis.title = ggplot2::element_text(size = 14, face = "bold"),
                   legend.position = "bottom")
  if (!is.null(mean_val) && !is.na(mean_val)) {
    p <- p + ggplot2::geom_vline(ggplot2::aes(xintercept = mean_val, color = "Mean"),
                                 linetype = "dashed", linewidth = 1)
  }
  if (!is.null(med_val) && !is.na(med_val)) {
    p <- p + ggplot2::geom_vline(ggplot2::aes(xintercept = med_val, color = "Median"),
                                 linetype = "solid", linewidth = 1)
  }
  p + ggplot2::scale_color_manual(name = "Statistics",
                                  values = c("Mean" = "#e74c3c", "Median" = "#f1c40f"))
}

# -------------------------------------------------------------------------
# 3. MANIFEST  — the single source of truth for "what is exportable".
#    Each entry is a black box: given the cart IDs it decides if it applies,
#    given the state list it decides if data is present, and given an output
#    directory it writes its files and returns their paths.
#
#    Adding a new export = add one list entry here. Nothing else changes,
#    and the bundler/tests need no edits (open/closed principle).
# -------------------------------------------------------------------------

# Small helper: does the cart contain an exact ID or any ID with a prefix?
.cart_has <- function(cart_ids, exact = character(0), prefix = character(0)) {
  any(exact %in% cart_ids) ||
    any(vapply(prefix, function(p) any(grepl(p, cart_ids)), logical(1)))
}

default_export_manifest <- function() {
  list(
    list(
      key = "extracted_boundary",
      applies = function(ids) .cart_has(ids, exact = "ext_shp"),
      available = function(s) !is.null(s$ext_shp_export),
      write = function(s, dir) write_vector_shapefile(
        s$ext_shp_export, dir, "Extracted_Boundary",
        paste0(s$ext_shp_name, "_Extracted_Boundary"))
    ),
    list(
      key = "custom_roi",
      applies = function(ids) .cart_has(ids, exact = "drawn_roi"),
      available = function(s) !is.null(s$drawn_roi_export),
      write = function(s, dir) write_vector_shapefile(
        s$drawn_roi_export, dir, "Custom_ROI",
        paste0(s$drawn_roi_name, "_Custom_ROI"))
    ),
    list(
      key = "lulc_change_matrix",
      applies = function(ids) .cart_has(ids, exact = "sankey_csv"),
      available = function(s) !is.null(s$sankey_data),
      write = function(s, dir) write_table_files(s$sankey_data, dir, "LULC_Change_Matrix")
    ),
    list(
      key = "lulc_area_stats",
      applies = function(ids) .cart_has(ids, exact = "lulc_csv"),
      available = function(s) !is.null(s$stats_df),
      write = function(s, dir) write_table_files(s$stats_df, dir, "LULC_Area_Statistics")
    ),
    list(
      key = "lulc_area_chart",
      applies = function(ids) .cart_has(ids, exact = "lulc_plot"),
      available = function(s) !is.null(s$stats_df),
      write = function(s, dir) save_ggplot(build_lulc_area_chart(s$stats_df),
                                           dir, "LULC_Area_Chart.png", 8, 6)
    ),
    list(
      key = "accuracy_report",
      applies = function(ids) .cart_has(ids, exact = "acc_report"),
      available = function(s) !is.null(s$acc_text) && s$acc_text != "",
      write = function(s, dir) write_text_file(s$acc_text, dir, "ML_Accuracy_Report.txt")
    ),
    list(
      key = "lulc_temporal_stats",
      applies = function(ids) .cart_has(ids, exact = "lulc_temp_csv"),
      available = function(s) !is.null(s$temporal_stats_df),
      write = function(s, dir) write_table_files(build_temporal_wide(s$temporal_stats_df),
                                                 dir, "LULC_Temporal_Statistics")
    ),
    list(
      key = "lulc_temporal_chart",
      applies = function(ids) .cart_has(ids, exact = "lulc_temp_plot"),
      available = function(s) !is.null(s$temporal_stats_df),
      write = function(s, dir) save_ggplot(
        build_lulc_temporal_chart(s$temporal_stats_df, s$class_labels),
        dir, "LULC_Temporal_Area_Chart.png", 8, 6)
    ),
    list(
      key = "lulc_gallery",
      applies = function(ids) .cart_has(ids, prefix = "^lulc_gallery_"),
      available = function(s) length(s$lulc_grid_urls) > 0,
      write = function(s, dir) download_url_gallery(
        s$lulc_grid_urls, dir, "LULC_Temporal_Gallery", "LULC_Map")
    ),
    list(
      key = "publication_map_png",
      applies = function(ids) .cart_has(ids, prefix = "^carto_png_"),
      available = function(s) !is.null(s$carto_plot_clean),
      write = function(s, dir) save_ggplot(s$carto_plot_clean, dir, "Publication_Map.png", 10, 8)
    ),
    list(
      key = "publication_raster_tif",
      applies = function(ids) .cart_has(ids, prefix = "^carto_tif_"),
      available = function(s) !is.null(s$carto_raster),
      write = function(s, dir) write_raster_tif(s$carto_raster, dir, "Publication_Raster.tif")
    ),
    list(
      key = "gee_zonal_stats",
      applies = function(ids) .cart_has(ids, prefix = "^gee_zonal_"),
      available = function(s) !is.null(s$gee_zonal_data),
      write = function(s, dir) write_table_files(s$gee_zonal_data, dir, "Zonal_Statistics")
    ),
    list(
      key = "correlation_chart",
      applies = function(ids) .cart_has(ids, prefix = "^stats_corr_"),
      available = function(s) !is.null(s$stats_corr_plot),
      write = function(s, dir) save_ggplot(s$stats_corr_plot, dir,
                                           "Correlation_Analysis_Chart.png", 8, 6)
    ),
    list(
      key = "confidence_interval",
      applies = function(ids) .cart_has(ids, prefix = "^stats_ci_"),
      available = function(s) !is.null(s$stats_ci_df),
      write = function(s, dir) write_table_files(s$stats_ci_df, dir, "Confidence_Interval")
    ),
    list(
      key = "stats_by_class",
      applies = function(ids) .cart_has(ids, prefix = "^stats_lulc_"),
      available = function(s) !is.null(s$stats_lulc_class_stats),
      write = function(s, dir) write_table_files(s$stats_lulc_class_stats, dir,
                                                 "Statistics_by_Land_Cover_Class")
    ),
    list(
      key = "timeseries_chart",
      applies = function(ids) .cart_has(ids, prefix = "^gee_ts_"),
      available = function(s) !is.null(s$gee_ts_data),
      write = function(s, dir) save_ggplot(
        build_timeseries_chart(s$gee_ts_data, s$gee_current_feature),
        dir, "Time_Series_Trend.png", 8, 5)
    ),
    list(
      key = "histogram_chart",
      applies = function(ids) .cart_has(ids, prefix = "^gee_hist_"),
      available = function(s) !is.null(s$gee_hist_data),
      write = function(s, dir) save_ggplot(
        build_histogram_chart(s$gee_hist_data, s$gee_current_feature,
                              s$gee_mean_val, s$gee_med_val),
        dir, "Area_Distribution_Histogram.png", 8, 5)
    ),
    list(
      key = "gee_gallery",
      applies = function(ids) .cart_has(ids, prefix = "^gee_gallery_"),
      available = function(s) length(s$gee_grid_urls) > 0,
      write = function(s, dir) download_url_gallery(
        s$gee_grid_urls, dir, "Temporal_Gallery", "Grid_Map")
    )
  )
}

# -------------------------------------------------------------------------
# 4. BUNDLER  — pure driver. Given the cart IDs, a plain state list, a target
#    zip path and a manifest, it writes the ZIP. Knows nothing about Shiny/rv.
# -------------------------------------------------------------------------
bundle_workspace_zip <- function(zip_file, cart_ids, state,
                                 manifest = default_export_manifest(),
                                 out_dir = tempdir()) {
  files_to_zip <- character(0)

  # The methodology/citations file is always included.
  if (!is.null(state$methodology_text)) {
    files_to_zip <- c(files_to_zip,
                      write_text_file(state$methodology_text, out_dir,
                                      "Methodology_and_Citations.md"))
  }

  for (spec in manifest) {
    if (spec$applies(cart_ids) && isTRUE(spec$available(state))) {
      files_to_zip <- c(files_to_zip, spec$write(state, out_dir))
    }
  }

  zip::zipr(zipfile = zip_file, files = files_to_zip)
  invisible(files_to_zip)
}

# -------------------------------------------------------------------------
# 5. PDF REPORT  — pure builder. Takes the already-generated methodology text,
#    a region label, and a NAMED LIST of result tables (title -> data.frame).
#    Writes the PDF to `file`. No premium/subscription check here — that policy
#    decision stays in the orchestration layer (app.R), by design.
# -------------------------------------------------------------------------

# Renders one data.frame as monospace table page(s) onto the current pdf device.
.render_df_as_pdf_page <- function(df, title) {
  if (is.null(df) || nrow(df) == 0) return(invisible(NULL))
  df_fmt <- as.data.frame(lapply(df, function(col) {
    if (is.numeric(col)) format(round(col, 3), nsmall = 0, trim = TRUE) else as.character(col)
  }), stringsAsFactors = FALSE)
  col_widths <- pmax(nchar(names(df_fmt)),
                     sapply(df_fmt, function(col) max(nchar(col), na.rm = TRUE))) + 2
  header_line <- paste(mapply(function(nm, w) formatC(nm, width = -w),
                              names(df_fmt), col_widths), collapse = "")
  sep_line <- paste(rep("-", sum(col_widths)), collapse = "")
  body_lines <- apply(df_fmt, 1, function(row)
    paste(mapply(function(val, w) formatC(val, width = -w), row, col_widths), collapse = ""))
  rows_per_page <- 50
  n_sub_pages <- max(1, ceiling(length(body_lines) / rows_per_page))
  for (sp in seq_len(n_sub_pages)) {
    graphics::plot.new()
    graphics::text(0.02, 0.97, title, adj = c(0, 1), cex = 0.95, font = 2, col = "#26333e")
    idx_s <- (sp - 1) * rows_per_page + 1
    idx_e <- min(sp * rows_per_page, length(body_lines))
    table_text <- paste(c(header_line, sep_line, body_lines[idx_s:idx_e]), collapse = "\n")
    graphics::text(0.02, 0.90, table_text, adj = c(0, 1), cex = 0.55, family = "mono")
  }
}

build_pdf_report <- function(file, report_text, region_label, tables = list()) {
  grDevices::pdf(file, width = 8.5, height = 11)
  on.exit(grDevices::dev.off(), add = TRUE)

  # --- Title page ---
  graphics::plot.new()
  graphics::text(0.5, 0.82, "Spatial Research Suite", cex = 2.2, font = 2)
  graphics::text(0.5, 0.75, "Methodology & Analysis Summary Report", cex = 1.3)
  graphics::text(0.5, 0.65, format(Sys.Date(), "%B %d, %Y"), cex = 1)
  graphics::text(0.5, 0.58, paste("Study Area:", region_label), cex = 1.1, font = 3, col = "#26333e")
  graphics::text(0.5, 0.15,
                 "Generated by Spatial Research Suite - Google Earth Engine Cloud Analytics",
                 cex = 0.7, col = "#5c6b73")

  # --- Methodology text pages (same pagination as before) ---
  lines_vec <- strsplit(report_text, "\n")[[1]]
  lines_per_page <- 58
  n_pages <- max(1, ceiling(length(lines_vec) / lines_per_page))
  for (p in seq_len(n_pages)) {
    graphics::plot.new()
    idx_start <- (p - 1) * lines_per_page + 1
    idx_end <- min(p * lines_per_page, length(lines_vec))
    page_text <- paste(lines_vec[idx_start:idx_end], collapse = "\n")
    graphics::text(0.02, 0.98, page_text, adj = c(0, 1), cex = 0.62, family = "mono")
    graphics::mtext(sprintf("Page %d of %d", p + 1, n_pages + 1),
                    side = 1, line = -1, cex = 0.6, col = "#5c6b73")
  }

  # --- One page per available results table ---
  for (title in names(tables)) {
    .render_df_as_pdf_page(tables[[title]], title)
  }
}
