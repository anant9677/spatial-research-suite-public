# =========================================================================
# MODULE 1: GOOGLE EARTH ENGINE ANALYTICS (ANTI-CRASH FULL VERSION)
# =========================================================================

# -------------------------------------------------------------------------
# Non-parametric trend statistics (pure base R, no packages) for a regional
# time series (one value per year). Remote-sensing time series are short,
# noisy and rarely normal, so the field standard is:
#   * Mann-Kendall test  — is there a monotonic trend? (tau, S, tie-corrected
#     variance, normal-approx two-sided p-value). Robust to outliers, makes no
#     distributional assumption.
#   * Theil-Sen slope    — the median of all pairwise slopes: a breakdown-robust
#     estimate of the rate, reported alongside the ordinary least-squares slope.
# Returns NULL when there are fewer than 3 usable points (test not meaningful).
mann_kendall_sen <- function(x, y) {
  ok <- is.finite(x) & is.finite(y); x <- x[ok]; y <- y[ok]
  n <- length(y)
  if (n < 3) return(NULL)
  S <- 0
  for (k in seq_len(n - 1)) for (j in (k + 1):n) S <- S + sign(y[j] - y[k])
  tb  <- as.numeric(table(y))                     # tie groups in the values
  tie <- sum(tb * (tb - 1) * (2 * tb + 5))
  var_s <- (n * (n - 1) * (2 * n + 5) - tie) / 18
  z <- if (var_s <= 0) 0 else if (S > 0) (S - 1) / sqrt(var_s) else if (S < 0) (S + 1) / sqrt(var_s) else 0
  p <- 2 * (1 - stats::pnorm(abs(z)))
  tau <- S / (0.5 * n * (n - 1))
  slopes <- numeric(0)
  for (k in seq_len(n - 1)) for (j in (k + 1):n) if (x[j] != x[k]) slopes <- c(slopes, (y[j] - y[k]) / (x[j] - x[k]))
  sen <- if (length(slopes)) stats::median(slopes) else NA_real_
  intercept <- if (is.finite(sen)) stats::median(y - sen * x) else NA_real_
  list(n = n, S = S, tau = tau, var_s = var_s, z = z, p_value = p,
       significant = isTRUE(p < 0.05), sen_slope = sen, sen_intercept = intercept)
}

# -------------------------------------------------------------------------
# Publication report (pure base R, no packages). Assembles a SELF-CONTAINED
# HTML document from an already-computed pipeline run: the composite map
# (embedded as a base64 data-URI so the file needs no internet), every stats
# table, an auto-written Methods paragraph, citations, and an explicit
# "no independent field validation" limitations block. Returns one HTML
# string; the download handler just writes it to disk. Kept pure so it is
# unit-testable without Shiny/EE. `payload` fields are all optional except
# `title`; missing sections are silently skipped.
gf_report_escape <- function(x) {
  x <- as.character(x); x[is.na(x)] <- ""
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  gsub(">", "&gt;", x, fixed = TRUE)
}
gf_report_table <- function(df, headers = NULL) {
  if (is.null(df) || !NROW(df)) return("")
  hd <- if (!is.null(headers)) headers else names(df)
  th <- paste0("<th>", gf_report_escape(hd), "</th>", collapse = "")
  rows <- vapply(seq_len(nrow(df)), function(i) {
    tds <- vapply(seq_along(df), function(j) paste0("<td>", gf_report_escape(df[i, j]), "</td>"), character(1))
    paste0("<tr>", paste0(tds, collapse = ""), "</tr>")
  }, character(1))
  paste0("<table><thead><tr>", th, "</tr></thead><tbody>", paste0(rows, collapse = ""), "</tbody></table>")
}
# Shared print stylesheet for the self-contained HTML reports (pipeline + LULC), so the two
# deliverables look identical. Pure string.
gf_report_css <- function() paste0(
  "*{box-sizing:border-box;} ",
  "body{font-family:Segoe UI,Helvetica,Arial,sans-serif;color:#26333e;max-width:920px;",
  "margin:0 auto;padding:0 22px 42px;line-height:1.58;counter-reset:sec;background:#fff;} ",
  ".hdr{background:linear-gradient(135deg,#234a3d,#2c5a4a 58%,#37796a);color:#fff;margin:0 -22px 22px;",
  "padding:26px 30px 18px;border-bottom:4px solid #1c3a30;} ",
  ".hdr h1{font-size:23px;margin:0 0 6px;color:#fff;letter-spacing:.2px;font-weight:700;} ",
  ".hdr .sub{font-size:11px;color:#cfe3db;text-transform:uppercase;letter-spacing:.15em;margin-bottom:12px;} ",
  ".hdr .meta{color:#e7f1ed;font-size:12px;border-top:1px solid rgba(255,255,255,.25);padding-top:9px;} ",
  "h1{font-size:22px;margin:0 0 4px;} ",
  "h2{font-size:15px;color:#2c5a4a;margin-top:30px;padding-bottom:5px;border-bottom:2px solid #eef4f1;",
  "counter-increment:sec;font-weight:700;letter-spacing:.02em;} ",
  "h2::before{content:counter(sec);display:inline-block;min-width:22px;height:22px;line-height:22px;",
  "text-align:center;background:#2c5a4a;color:#fff;border-radius:5px;font-size:12px;margin-right:9px;vertical-align:2px;} ",
  "h3{font-size:13px;color:#3d4f5c;margin:14px 0 5px;font-weight:600;} .meta{color:#5c6b73;font-size:12px;} ",
  "table{border-collapse:collapse;width:100%;font-size:12px;margin:8px 0 4px;} ",
  "th,td{border:1px solid #d8dde1;padding:6px 9px;text-align:left;vertical-align:top;} ",
  "th{background:#2c5a4a;color:#fff;font-weight:600;font-size:11.5px;letter-spacing:.02em;} ",
  "tbody tr:nth-child(even){background:#f6f9f7;} ",
  ".fig{margin:14px 0;} .fig img{max-width:100%;border:1px solid #d8dde1;border-radius:6px;box-shadow:0 1px 3px rgba(0,0,0,.06);} ",
  ".cap{font-size:11px;color:#5c6b73;margin-top:5px;font-style:italic;} ",
  ".note{font-size:11px;color:#5c6b73;background:#f2f5f7;padding:7px 11px;border-radius:5px;margin:6px 0;} ",
  ".limit{margin-top:26px;background:#fbf4ef;border-left:4px solid #8b3a2b;border-radius:6px;padding:10px 16px;} ",
  ".limit h2{border:none;color:#8b3a2b;counter-increment:none;} .limit h2::before{content:none;} ",
  ".cite li{font-size:12px;margin-bottom:3px;} ",
  ".summary{margin:0 0 6px;background:#eef4f1;border-left:4px solid #2c5a4a;border-radius:6px;padding:12px 18px;} ",
  ".summary-t{font-size:12px;font-weight:700;letter-spacing:.08em;text-transform:uppercase;color:#2c5a4a;margin-bottom:5px;} ",
  ".summary p{margin:6px 0;font-size:13.5px;} .summary b{color:#26333e;} ",
  ".gallery{display:flex;flex-wrap:wrap;gap:12px;margin:10px 0;} ",
  ".gcard{margin:0;width:180px;} .gcard img{width:100%;height:130px;object-fit:cover;border:1px solid #d8dde1;border-radius:6px;background:#eaeef1;} ",
  ".gcard figcaption{font-size:10.5px;color:#3d4f5c;margin-top:4px;text-align:center;line-height:1.3;} ",
  "@media print{body{max-width:none;} .hdr{margin:0 0 18px;}}")

# Self-contained HTML report for the LULC (land-cover classification) module. Mirrors
# gf_pipeline_report_html's look (shared CSS + gf_report_table/escape), but with the sections a
# classification deserves: per-class area, formal accuracy (standard + Olofsson error-adjusted
# area + Pontius disagreement), landscape metrics, change transition/intensity, methods,
# reproducibility, limitations. Trusted section HTML (accuracy/landscape/change) is inserted raw;
# data tables go through gf_report_table (escaped). Always carries the field-validation caveat.
gf_lulc_report_html <- function(payload) {
  p <- payload; if (is.null(p)) p <- list()
  sec <- function(title, body) if (is.null(body) || !nzchar(body)) "" else
    paste0("<h2>", gf_report_escape(title), "</h2>", body)
  parts <- character(0)
  parts <- c(parts, paste0(
    "<div class='hdr'><h1>", gf_report_escape(p$title %||% "Land-Cover Classification Report"), "</h1>",
    "<p class='meta'>",
    paste(Filter(nzchar, c(
      if (!is.null(p$study_area) && nzchar(p$study_area)) paste0("Location: ", gf_report_escape(p$study_area)) else "",
      if (!is.null(p$generated)) paste0("Generated: ", gf_report_escape(p$generated)) else "",
      if (!is.null(p$area_km2)) paste0("Study area: ", gf_report_escape(p$area_km2), " km²") else "",
      if (!is.null(p$n_classes)) paste0("Classes: ", gf_report_escape(p$n_classes)) else "")),
      collapse = " &nbsp;|&nbsp; "),
    "</p></div>"))
  if (!is.null(p$exec_summary) && nzchar(p$exec_summary))
    parts <- c(parts, paste0("<div class='summary'><div class='summary-t'>Summary</div>", p$exec_summary, "</div>"))
  if (!is.null(p$map_datauri) && nzchar(p$map_datauri))
    parts <- c(parts, paste0("<div class='fig'><img src='", p$map_datauri, "' alt='classification map'/>",
                             if (!is.null(p$map_caption)) paste0("<div class='cap'>", gf_report_escape(p$map_caption), "</div>") else "",
                             "</div>"))
  else if (!is.null(p$map_note) && nzchar(p$map_note))
    parts <- c(parts, paste0("<p class='note'>", gf_report_escape(p$map_note), "</p>"))
  parts <- c(parts, sec("Class areas", gf_report_table(p$class_area)))
  parts <- c(parts, sec("Accuracy assessment", p$accuracy_html))
  parts <- c(parts, sec("Landscape metrics", p$landscape_html))
  parts <- c(parts, sec("Change analysis", p$change_html))
  parts <- c(parts, sec("Methods", p$methods_html))
  parts <- c(parts, sec("Reproducibility", gf_report_table(p$provenance)))
  if (!is.null(p$citations) && length(p$citations))
    parts <- c(parts, sec("Data sources & references",
      paste0("<ol class='cite'>", paste0("<li>", gf_report_escape(p$citations), "</li>", collapse = ""), "</ol>")))
  lim <- p$limitations; if (is.null(lim) || !length(lim)) lim <- character(0)
  lim <- c(lim, "Accuracy and error-adjusted areas assume the validation sample is representative and independent; adjacent pixels are spatially correlated, so confidence intervals are good-practice estimates, not exact bounds. Class identities should be confirmed against ground reference where decisions depend on them.")
  parts <- c(parts, paste0("<div class='limit'><h2>Limitations</h2><ul>",
             paste0("<li>", gf_report_escape(lim), "</li>", collapse = ""), "</ul></div>"))
  paste0("<!DOCTYPE html><html lang='en'><head><meta charset='utf-8'>",
         "<meta name='viewport' content='width=device-width,initial-scale=1'>",
         "<title>", gf_report_escape(p$title %||% "Land-Cover Classification Report"), "</title><style>", gf_report_css(), "</style></head><body>",
         paste0(parts, collapse = "\n"),
         "<div class='meta' style='margin-top:26px;border-top:1px solid #e2e6ea;padding-top:8px;'>",
         "Generated by the Spatial Research Suite. Figures and statistics are reproducible from the Methods above.</div>",
         "</body></html>")
}

# Executive summary (plain-language abstract) for the pipeline — shared by the HTML report AND
# the in-app Insights dashboard so the two never drift. Pure: composes <p> sentences from the
# actual results. Inputs: headline layer label; study-area km²; date-range string; aca summary
# (list with coral_km2/coral_pct or NULL) and whether coral ran over it; the trend list
# (interpretation/summary) and the correlation result (corr$r/$p, response).
gf_pipeline_exec_summary <- function(headline, area_km2 = NA, date_range = "",
                                     aca = NULL, aca_over_coral = FALSE, trend = NULL, correl = NULL) {
  sents <- character(0)
  if (is.null(headline) || !nzchar(headline)) headline <- "the mapped indicator"
  area_txt <- if (isTRUE(is.finite(area_km2))) sprintf(" across a %.1f km² study area", area_km2) else ""
  sents <- c(sents, sprintf(
    "This report screens <b>%s</b>%s from satellite imagery composited over <b>%s</b>.",
    gf_report_escape(headline), area_txt, gf_report_escape(date_range)))
  if (!is.null(aca) && isTRUE(is.finite(aca$coral_km2)))
    sents <- c(sents, sprintf(
      "The Allen Coral Atlas maps <b>%.3f km² (%.1f%%)</b> of the area as coral/algae habitat%s.",
      aca$coral_km2, aca$coral_pct,
      if (isTRUE(aca_over_coral)) ", over which the coral, landscape and trend results were computed" else ""))
  if (!is.null(trend)) {
    lead <- ""
    if (!is.null(trend$interpretation) && nzchar(trend$interpretation))
      lead <- trimws(sub("\\s*[\u2014\u2013].*$", "",
                strsplit(trend$interpretation, "(?<=[.])\\s+", perl = TRUE)[[1]][1]))
    if (!nzchar(lead) && !is.null(trend$summary)) lead <- sub("\\.\\s*$", "", trend$summary)
    if (nzchar(lead)) {
      m <- regmatches(trend$summary %||% "", regexpr("changed by [^ ]+ per year", trend$summary %||% ""))
      rate_txt <- if (length(m) && nzchar(m)) paste0(" (", m, ")") else ""
      sents <- c(sents, paste0(gf_report_escape(lead), gf_report_escape(rate_txt), "."))
    }
  }
  if (!is.null(correl) && !is.null(correl$r) && !is.null(correl$response) && correl$response %in% rownames(correl$r)) {
    resp <- correl$response; row <- correl$r[resp, ]; row <- row[names(row) != resp]; row <- row[is.finite(row)]
    if (length(row)) {
      j <- which.max(abs(row)); drv <- names(row)[j]; rr <- as.numeric(row[[j]])
      pv  <- tryCatch(as.numeric(correl$p[resp, drv]), error = function(e) NA_real_)
      sig <- if (isTRUE(is.finite(pv) && pv < 0.05)) "statistically significant"
             else "not yet statistically significant over this short series"
      sents <- c(sents, sprintf("The strongest association with %s is <b>%s</b> (r = %.2f, %s; %s).",
                 gf_report_escape(resp), gf_report_escape(drv), rr, if (rr < 0) "inverse" else "direct", sig))
    }
  }
  sents <- c(sents, "<i>These are satellite-derived screening results, not field-validated; see Limitations.</i>")
  paste0(paste0("<p>", sents, "</p>"), collapse = "")
}

# Auto-written Methods paragraph — shared by report + dashboard. Generated only from what actually
# ran (marine/trend/correlation flags), so it never claims a step that wasn't used.
gf_pipeline_methods_html <- function(date_range = "", agg = "Median", chain = "",
                                     marine_used = FALSE, trend_used = FALSE, correl_used = FALSE) {
  paste0(
    "<p>All layers were computed on Google Earth Engine over the study boundary. ",
    "Point-in-time layers were composited over <b>", gf_report_escape(date_range), "</b> using the <b>", gf_report_escape(agg),
    "</b> reducer at an adaptive spatial scale matched to the boundary size (Sentinel-2 SR Harmonized with per-pixel SCL cloud masking; Landsat Collection-2 for land indices / pre-2015). ",
    if (isTRUE(marine_used)) paste0("Marine optical layers used a blue-green bottom index, ln(green)\u2212ln(blue) (ln B3 \u2212 ln B2), over optically-shallow reef (deep water excluded), oriented so higher values indicate darker, light-absorbing bottoms (coral / algae / seagrass) and lower values bright sand / rubble; it reflects benthic cover/brightness and cannot on its own distinguish live coral from macroalgae. The Sentinel-2 index applies sun-glint correction (Hedley et al., 2005)",
      if (grepl("Landsat", chain) && grepl("Coral", chain)) "; the Landsat long-record index (30 m, 1984+) is not sun-glint corrected and carries small cross-sensor (TM/ETM+/OLI) offsets, so it is treated as a screening long-record index" else "", ". ") else "",
    if (grepl("Heat Stress|SST", chain)) "Marine heat stress used the NOAA OISST v2.1 sea-surface-temperature anomaly (~25 km, daily, referenced to the 1971-2000 climatology) as a coral-bleaching stress proxy \u2014 this is the SST anomaly, not the NOAA Coral Reef Watch Degree-Heating-Weeks product. " else "",
    if (isTRUE(trend_used)) "Trends used the regional annual mean fitted with ordinary least squares (reported with a 95% confidence interval on the slope) plus the non-parametric Mann-Kendall test and a Theil-Sen slope estimator. " else "",
    if (isTRUE(correl_used)) "Indicator series were cross-correlated (Pearson &amp; Spearman) over their shared years. " else "",
    if (nzchar(chain)) paste0("Processing chain: <b>", gf_report_escape(chain), "</b>.") else "", "</p>")
}

gf_pipeline_report_html <- function(payload) {
  p <- payload; if (is.null(p)) p <- list()
  sec <- function(title, body) if (is.null(body) || !nzchar(body)) "" else
    paste0("<h2>", gf_report_escape(title), "</h2>", body)
  parts <- character(0)
  # Header
  parts <- c(parts, paste0(
    "<div class='hdr'><h1>", gf_report_escape(p$title %||% "Spatial Analysis Report"), "</h1>",
    "<div class='sub'>Satellite Remote-Sensing Analysis &middot; Spatial Research Suite</div>",
    "<p class='meta'>",
    paste(Filter(nzchar, c(
      if (!is.null(p$study_area) && nzchar(p$study_area)) paste0("Location: ", gf_report_escape(p$study_area)) else "",
      if (!is.null(p$generated)) paste0("Generated: ", gf_report_escape(p$generated)) else "",
      if (!is.null(p$area_km2)) paste0("Study area: ", gf_report_escape(p$area_km2), " km²") else "",
      if (!is.null(p$date_range)) paste0("Date range: ", gf_report_escape(p$date_range)) else "",
      if (!is.null(p$agg)) paste0("Compositing: ", gf_report_escape(p$agg)) else "")),
      collapse = " &nbsp;|&nbsp; "),
    "</p></div>"))
  # Executive summary — plain-language abstract auto-composed by the handler
  # from the actual results (headline layer, coral extent, trend, correlation).
  # Trusted HTML built by us; inserted raw.
  if (!is.null(p$exec_summary) && nzchar(p$exec_summary))
    parts <- c(parts, paste0("<div class='summary'><div class='summary-t'>Summary</div>",
                             p$exec_summary, "</div>"))
  # Honest completeness banner: any step that did not compute is excluded from all results below.
  if (!is.null(p$steps) && is.data.frame(p$steps) && "Status" %in% names(p$steps)) {
    .fail <- p$steps[tolower(as.character(p$steps$Status)) %in% c("failed", "skipped"), , drop = FALSE]
    if (nrow(.fail))
      parts <- c(parts, paste0("<div style='margin:8px 0;background:#fbeee9;border:1px solid #e2b8a8;border-left:4px solid #8b3a2b;border-radius:6px;padding:9px 14px;font-size:12px;color:#7a3322;'><b>",
        nrow(.fail), " step(s) did not compute</b> and are excluded from the results below (and from any correlation): ",
        gf_report_escape(paste(.fail$Layer, collapse = ", ")), ". See the Processing chain for the reason.</div>"))
  }
  # Map figure
  if (!is.null(p$map_datauri) && nzchar(p$map_datauri))
    parts <- c(parts, paste0("<div class='fig'><img src='", p$map_datauri, "' alt='composite map'/>",
                             if (!is.null(p$map_caption)) paste0("<div class='cap'>", gf_report_escape(p$map_caption), "</div>") else "",
                             "</div>"))
  # No map rendered — state why, rather than leaving a silent gap.
  else if (!is.null(p$map_note) && nzchar(p$map_note))
    parts <- c(parts, paste0("<p class='note'>", gf_report_escape(p$map_note), "</p>"))
  # What the map shows (our own trusted HTML — inserted raw)
  if (!is.null(p$explain_html) && nzchar(p$explain_html))
    parts <- c(parts, sec("What the map shows", p$explain_html))
  # All-indicators gallery — every layer the pipeline built, thumbnail + label
  if (!is.null(p$gallery) && length(p$gallery)) {
    items <- vapply(p$gallery, function(g) paste0(
      "<figure class='gcard'><img src='", g$uri, "' alt='layer thumbnail'/>",
      "<figcaption>", gf_report_escape(g$label), "</figcaption></figure>"), character(1))
    miss <- if (!is.null(p$gallery_missing) && isTRUE(p$gallery_missing > 0))
      paste0("<div class='note'>", p$gallery_missing, " layer thumbnail(s) could not be fetched and were omitted.</div>") else ""
    parts <- c(parts, sec("Layer gallery", paste0(
      "<div class='note'>Every layer the pipeline computed, in sequence. Colours are each layer's own display stretch.</div>",
      "<div class='gallery'>", paste0(items, collapse = ""), "</div>", miss)))
  }
  # Descriptive stats
  parts <- c(parts, sec("Descriptive statistics", gf_report_table(p$descriptive)))
  # Class-based area
  parts <- c(parts, sec("Area by class", gf_report_table(p$class_area)))
  # Landscape metrics (diversity + patch structure) — pre-rendered HTML (trusted)
  parts <- c(parts, sec("Landscape metrics", p$landscape_html))
  # Coral habitat reference (Allen Coral Atlas)
  parts <- c(parts, sec("Coral habitat (Allen Coral Atlas)",
             paste0(gf_report_table(p$aca_area),
                    if (!is.null(p$aca_note) && nzchar(p$aca_note)) paste0("<p class='note'>", gf_report_escape(p$aca_note), "</p>") else "")))
  # Rate of change
  if (!is.null(p$trend)) {
    tr <- p$trend
    body <- paste0("<p>", gf_report_escape(tr$summary %||% ""), "</p>")
    if (!is.null(tr$interpretation) && nzchar(tr$interpretation))
      body <- paste0(body, "<p><b>What it means.</b> ", gf_report_escape(tr$interpretation), "</p>")
    if (!is.null(p$trend_chart) && nzchar(p$trend_chart))
      body <- paste0(body, "<div class='fig'><img src='", p$trend_chart, "' alt='trend chart'/></div>")
    if (!is.null(tr$stats)) body <- paste0(body, gf_report_table(tr$stats))
    if (!is.null(tr$series)) body <- paste0(body, "<h3>Yearly regional values</h3>", gf_report_table(tr$series))
    if (!is.null(tr$cloud) && nzchar(tr$cloud)) body <- paste0(body, "<p class='note'>", gf_report_escape(tr$cloud), "</p>")
    parts <- c(parts, sec("Rate of change (temporal dynamics)", body))
  }
  # Cross-indicator correlation — pre-rendered trusted HTML + optional scatter chart
  parts <- c(parts, sec("Cross-indicator correlation",
    paste0(p$correl_html %||% "",
           if (!is.null(p$correl_chart) && nzchar(p$correl_chart))
             paste0("<div class='fig'><img src='", p$correl_chart, "' alt='correlation scatter'/></div>") else "",
           if (!is.null(p$correl_html) && nzchar(p$correl_html)) gf_correlation_caveats_html() else "")))
  # Pipeline steps
  parts <- c(parts, sec("Processing chain", gf_report_table(p$steps)))
  # Methods (trusted HTML)
  parts <- c(parts, sec("Methods", p$methods_html))
  # Sensors & datasets actually used in this run (platform, EE ID, coverage, bands, role)
  if (!is.null(p$sensors) && nrow(p$sensors)) parts <- c(parts, sec("Sensors & datasets", gf_report_table(p$sensors)))
  # Reproducibility — exact parameters to re-run the analysis
  parts <- c(parts, sec("Reproducibility", gf_report_table(p$provenance)))
  # Citations
  if (!is.null(p$citations) && length(p$citations))
    parts <- c(parts, sec("Data sources & references",
      paste0("<ol class='cite'>", paste0("<li>", gf_report_escape(p$citations), "</li>", collapse = ""), "</ol>")))
  # Limitations — always present, and always states no field validation.
  lim <- p$limitations
  if (is.null(lim) || !length(lim)) lim <- character(0)
  lim <- c(lim, "This analysis was NOT validated against independent field/ground-truth data. Satellite-derived indices and classifications are screening tools: class identities and thresholds are indicative and should be confirmed on the ground before firm conclusions are drawn.")
  parts <- c(parts, paste0("<div class='limit'><h2>Limitations</h2><ul>",
             paste0("<li>", gf_report_escape(lim), "</li>", collapse = ""), "</ul></div>"))
  css <- gf_report_css()
  paste0("<!DOCTYPE html><html lang='en'><head><meta charset='utf-8'>",
         "<meta name='viewport' content='width=device-width,initial-scale=1'>",
         "<title>", gf_report_escape(p$title %||% "Spatial Analysis Report"), "</title><style>", css, "</style></head><body>",
         paste0(parts, collapse = "\n"),
         "<div class='meta' style='margin-top:26px;border-top:1px solid #e2e6ea;padding-top:8px;'>",
         "Generated by the Spatial Research Suite. Figures and statistics are reproducible from the Methods above.</div>",
         "</body></html>")
}

# =========================================================================
# SEQUENTIAL PIPELINE  —  unified tool catalog + Smart Sequence Assistant
#
# ADDITIVE ONLY. Everything below is consumed exclusively by the new
# "Sequential Pipeline" tab and its isolated server block. The existing
# GEE tabs and their independent-analysis observers are NOT touched, and
# the universal state bus (pipeline_state$current_roi_mask) only ever
# mutates inside observeEvent(input$run_pipeline). No bslib layout_sidebar
# / accordion is involved here (GEE uses tabsetPanel); no websocket logic.
#
# Each catalog entry:
#   label   : human name shown in the builder
#   cat     : category grouping for the catalog chips
#   role    : "mask"  -> produces an ee$Image mask, published to the bus
#             "index" -> a continuous layer; clipped to the upstream mask
#             "static"-> terrestrial/date-independent layer (pop, hillshade)
#             "trend" -> multi-year per-pixel slope (needs a base index)
#   feature : the GEE_FEATURE_CHOICES name to hand to get_feature_img(),
#             or a "__special__" token handled directly by the runner
#   hint    : one-line scientific guidance shown under the step
# =========================================================================
# Unified tool catalog. Built LAZILY (a function, not a source-time constant)
# so it can pull EVERY analysis from GEE_FEATURE_CHOICES (defined in global.R)
# regardless of file source order. Three engine-backed "special" steps
# (Benthic, Coral, Trend) sit on top of the full GEE index list.
#
# Entry fields: label, cat (category), role ("mask"|"index"|"static"|"trend"),
# dry (TRUE = terrestrial layer a water mask would blank), feature (the
# GEE_FEATURE_CHOICES name for get_feature_img(), or a "__token__"), hint.
gf_pipe_catalog <- function() {
  cat <- list(
    benthic = list(label = "Benthic Mapping", cat = "Marine", role = "mask", dry = FALSE, feature = "__benthic__",
                   hint = "Sentinel-2 shallow-water stack (NDWI mask + Hedley deglint + Lyzenga DII). Publishes the reef/water mask downstream marine steps clip to."),
    coral   = list(label = "Coral Health", cat = "Marine", role = "index", dry = FALSE, feature = "__coral__",
                   hint = "Live-coral separability (Blue-Green depth-invariant index) on masked reef pixels. Needs a benthic/water mask upstream."),
    coral_ls = list(label = "Coral Health (Landsat, long-record)", cat = "Marine", role = "index", dry = FALSE, feature = "__coral_ls__",
                   hint = "Same blue-green bottom index as Coral Health, but on Landsat 5/7/8/9 (30 m, 1984+) so the coral record spans the FULL timeline incl. pre-2018. Coarser than the Sentinel-2 version and not sun-glint corrected — use for the long-term / pre-closure baseline. Put an ACA / benthic mask BEFORE it."),
    coralalgae = list(label = "Coral vs Algae (heuristic)", cat = "Marine", role = "index", dry = FALSE, feature = "__coralalgae__",
                   hint = "SCREENING heuristic (not ground truth): separates temporally STABLE bottom (coral-likely) from VARIABLE bottom (algae-likely) using Sentinel-2 time-series variability, over the upstream coral mask. Put it AFTER Coral Habitat (ACA) / Coral Health. Needs several S2 scenes across the date range."),
    trend   = list(label = "Trend Analysis", cat = "Temporal", role = "trend", dry = NA, feature = "__trend__",
                   hint = "Per-pixel multi-year slope. Belongs after a base index step (NDVI, LST, ...)."),
    aca     = list(label = "Coral Habitat (Allen Coral Atlas)", cat = "Marine", role = "mask", dry = FALSE, feature = "__aca__",
                   hint = "Published, expert- & field-validated benthic habitat map (Coral/Algae, Sand, Rubble, Rock, Seagrass, Microalgal Mats). Identifies WHERE coral is AND publishes a Coral/Algae mask — put it BEFORE Coral Health / Trend to run those over ACA-confirmed coral only. (Static ~2020 baseline.)"),
    landmask = list(label = "Land Mask (exclude ocean)", cat = "Masks", role = "mask", dry = FALSE, feature = "__landmask__",
                   hint = "Publishes a LAND-only mask (ocean/water removed via MNDWI) to the bus. Put it BEFORE dry-land indicators (Nighttime Lights, Urban Sprawl, NDVI, Built-up…) so they are computed over land only, not sea."),
    heatstress = list(label = "Marine Heat Stress (SST anomaly)", cat = "Marine", role = "index", dry = FALSE, feature = "__heatstress__",
                   hint = "NOAA OISST sea-surface-temperature anomaly (~25 km, 1981+): the regional coral-bleaching HEAT-STRESS driver. Not reef-pixel resolution — its value is the yearly series the Correlation step pairs against Coral Health. Deliberately ignores the reef/land mask (a 25 km cell never aligns with reef pixels)."),
    dhw = list(label = "Marine Heat Stress (Degree Heating Weeks)", cat = "Marine", role = "index", dry = FALSE, feature = "__dhw__",
                   hint = "NOAA Coral Reef Watch-style bleaching metric from OISST: heat accumulated above the 1985-2012 max-monthly-mean. >4 degC-weeks = significant bleaching risk, >8 = severe. Coarse ~25 km regional; the field-standard upgrade over raw SST anomaly. Mask-exempt."),
    turbidity = list(label = "Turbidity (Nechad, calibrated)", cat = "Marine", role = "index", dry = FALSE, feature = "__turbidity__",
                   hint = "Physically-calibrated turbidity in FNU (Nechad et al.) from the Sentinel-2 red band \u2014 better than the NDTI ratio for water clarity. Clipped to any upstream mask."),
    blackmarble = list(label = "Nighttime Lights (Black Marble)", cat = "Economy & Population", role = "static", dry = TRUE, feature = "__blackmarble__",
                   hint = "NASA Black Marble VNP46A2 (500 m, gap-filled, BRDF- & stray-light-corrected) nighttime lights \u2014 a cleaner tourism/activity proxy than the raw VIIRS DNB monthly. Put it after Land Mask."),
    correl  = list(label = "Cross-Indicator Correlation", cat = "Temporal", role = "static", dry = NA, feature = "__correl__",
                   hint = "Correlates the yearly series of EVERY indicator you added to the pipeline before it (Coral Health, any index — NDVI, LST, Turbidity, Nighttime Lights, Built-up…). Generic: whatever you build, it correlates. Uses the active mask. Put your indicator steps first, then this. Heavy: years × #indicators.")
  )
  mask_feats <- c("Water Body Mapping (NDWI)", "Modified NDWI (MNDWI)", "Auto Water Extraction (AWEI)")
  slug <- function(x) paste0("f_", gsub("^_|_$", "", tolower(gsub("[^A-Za-z0-9]+", "_", x))))
  if (exists("GEE_FEATURE_CHOICES")) {
    for (grp in names(GEE_FEATURE_CHOICES)) for (f in GEE_FEATURE_CHOICES[[grp]]) {
      role <- if (f %in% mask_feats) "mask" else if (grp %in% c("Terrain & Topography", "Economy & Population")) "static" else "index"
      cat[[slug(f)]] <- list(label = f, cat = grp, role = role, dry = !(grp == "Water & Moisture"), feature = f,
        hint = sprintf("%s. %s", grp, if (role == "mask") "Can publish a water mask to the bus." else if (role == "static") "Terrestrial / static layer." else "Continuous index; clipped to any upstream mask."))
    }
  }
  cat
}

# Smart Sequence Assistant — a PURE function of the ordered key vector -> list
# of {type = "ok"|"recommend"|"warn", text}. Role/flag-driven so it scales to
# the whole catalog. No Earth Engine, no reactivity — safe at boot.
gf_pipe_assistant <- function(seq_keys) {
  cat   <- gf_pipe_catalog()
  items <- lapply(seq_keys, function(k) cat[[k]])
  items <- items[!vapply(items, is.null, logical(1))]
  msgs  <- list()
  add   <- function(type, text) msgs[[length(msgs) + 1]] <<- list(type = type, text = text)
  if (!length(items)) { add("ok", "Pick tools from the catalog on the left to start a sequence. I'll check the scientific order as you build it."); return(msgs) }

  roles <- vapply(items, function(x) x$role, character(1))
  feats <- vapply(items, function(x) x$feature, character(1))
  drys  <- vapply(items, function(x) isTRUE(x$dry), logical(1))
  labs  <- vapply(items, function(x) x$label, character(1))
  fpos  <- function(tok) { w <- which(feats == tok); if (length(w)) w[1] else NA_integer_ }
  first_mask <- if (any(roles == "mask")) min(which(roles == "mask")) else NA_integer_

  # Coral Health needs a water/reef mask before it.
  if ("__coral__" %in% feats) {
    cp <- fpos("__coral__")
    if (cp == 1L || !any(roles[seq_len(cp - 1)] == "mask"))
      add("recommend", "Add <b>Benthic Mapping</b> (or a water index such as NDWI) <b>before</b> Coral Health to isolate reef pixels from sand &amp; rock.")
  }
  # Trend Analysis needs a base index earlier in the sequence.
  if ("__trend__" %in% feats) {
    tp <- fpos("__trend__")
    if (tp == 1L || !any(roles[seq_len(tp - 1)] == "index"))
      add("warn", "<b>Trend Analysis</b> needs a base index (e.g. NDVI, LST, Coral Health) to run first. Put an index step earlier, then reorder Trend after it.")
  }
  # Dry-land layers placed after a water mask get blanked.
  if (!is.na(first_mask)) {
    for (i in which(drys & roles %in% c("index", "static"))) if (i > first_mask)
      add("warn", sprintf("<b>%s</b> is a dry-land layer sitting <b>after</b> a water mask — the mask will blank it. Move it <b>before</b> the mask, or drop it.", labs[i]))
  }
  # Benthic present but a land index runs before it (reef workflows).
  if ("__benthic__" %in% feats) {
    bp <- fpos("__benthic__")
    if (bp > 1L && any(drys[seq_len(bp - 1)] & roles[seq_len(bp - 1)] == "index"))
      add("recommend", "A land index runs <b>before</b> Benthic Mapping. In a reef study, run Benthic first so the water mask is established early.")
  }
  # Coral with no mask anywhere in the sequence.
  if ("__coral__" %in% feats && is.na(first_mask))
    add("recommend", "Coral Health has no water mask in the sequence. Add <b>Benthic Mapping</b> so coral is measured only on reef pixels.")

  if (!length(msgs))
    add("ok", "This sequence looks scientifically sound. Set a boundary in Step 1, then <b>Run Sequence</b> — each step passes its region / mask to the next via the state bus.")
  msgs
}

# ---------------------------------------------------------------------------
# CONTINUOUS -> DISCRETE class insights. Turn a continuous index into 5
# equal-interval classes (Very Low .. Very High) and report per-class AREA +
# share — the LULC-style per-class insight, for GEE's continuous layers.
#   * gf_classify_hist(): PASSIVE — from the already-computed histogram (which
#     carries per-bin area), no Earth Engine call. Used for Cloud Analysis.
#   * gf_class_labels(): the 5 class names.
# (The pipeline path reclassifies the real ee$Image via get_area_by_class_groups
#  in the runner; both produce the same data.frame shape below.)
# ---------------------------------------------------------------------------
gf_class_labels <- function(n = 5) c("Very Low", "Low", "Medium", "High", "Very High")[seq_len(n)]

gf_classify_hist <- function(hist_df, vmin, vmax, nclass = 5) {
  if (is.null(hist_df) || !("Bin" %in% names(hist_df))) return(NULL)
  bins <- suppressWarnings(as.numeric(hist_df$Bin))
  if (!isTRUE(is.finite(vmin)) || !isTRUE(is.finite(vmax)) || vmax <= vmin) {
    vmin <- suppressWarnings(min(bins, na.rm = TRUE)); vmax <- suppressWarnings(max(bins, na.rm = TRUE))
  }
  if (!isTRUE(is.finite(vmin)) || !isTRUE(is.finite(vmax)) || vmax <= vmin) return(NULL)
  brks <- seq(vmin, vmax, length.out = nclass + 1)
  lab  <- gf_class_labels(nclass)
  cls  <- cut(bins, breaks = brks, include.lowest = TRUE, labels = lab)
  area_v <- if ("Area_SqKm" %in% names(hist_df)) suppressWarnings(as.numeric(hist_df$Area_SqKm)) else NULL
  cnt_v  <- if ("Count" %in% names(hist_df)) suppressWarnings(as.numeric(hist_df$Count)) else NULL
  if (is.null(area_v) && is.null(cnt_v)) return(NULL)
  agg <- function(v) { r <- tapply(v, cls, sum, na.rm = TRUE); r <- as.numeric(r[lab]); r[is.na(r)] <- 0; r }
  km2 <- if (!is.null(area_v)) agg(area_v) else NULL
  cnt <- if (!is.null(cnt_v)) agg(cnt_v) else NULL
  base <- if (!is.null(km2)) km2 else cnt
  tot  <- sum(base); if (!isTRUE(is.finite(tot)) || tot <= 0) return(NULL)
  data.frame(Class = lab,
             Range = vapply(seq_len(nclass), function(i) sprintf("%.2f to %.2f", brks[i], brks[i + 1]), character(1)),
             Area_m2  = if (!is.null(km2)) round(km2 * 1e6, 0) else NA_real_,
             Area_ha  = if (!is.null(km2)) round(km2 * 100, 1) else NA_real_,
             Area_km2 = if (!is.null(km2)) round(km2, 3) else NA_real_,
             Pixels   = if (!is.null(cnt)) round(cnt, 0) else NA_real_,
             Pct = round(100 * base / tot, 1), stringsAsFactors = FALSE)
}

# From get_area_by_class_groups() output (Class_ID 0..n-1, Area_sqm) -> same shape.
gf_classify_area_groups <- function(area_groups, vmin, vmax, nclass = 5) {
  if (is.null(area_groups) || !all(c("Class_ID", "Area_sqm") %in% names(area_groups))) return(NULL)
  if (!isTRUE(is.finite(vmin)) || !isTRUE(is.finite(vmax)) || vmax <= vmin) return(NULL)
  brks <- seq(vmin, vmax, length.out = nclass + 1); lab <- gf_class_labels(nclass)
  km2 <- setNames(rep(0, nclass), as.character(0:(nclass - 1)))
  for (i in seq_len(nrow(area_groups))) {
    cid <- suppressWarnings(as.integer(area_groups$Class_ID[i]))
    if (!is.na(cid) && cid >= 0 && cid < nclass) km2[as.character(cid)] <- km2[as.character(cid)] + area_groups$Area_sqm[i] / 1e6
  }
  km2 <- as.numeric(km2); tot <- sum(km2); if (tot <= 0) return(NULL)
  data.frame(Class = lab,
             Range = vapply(seq_len(nclass), function(i) sprintf("%.2f to %.2f", brks[i], brks[i + 1]), character(1)),
             Area_m2 = round(km2 * 1e6, 0), Area_ha = round(km2 * 100, 1), Area_km2 = round(km2, 3),
             Pixels = NA_real_, Pct = round(100 * km2 / tot, 1), stringsAsFactors = FALSE)
}

# Trend slope classes: Class_ID 0=Decreasing, 1=Stable, 2=Increasing -> area table.
gf_trend_area_classes <- function(area_groups) {
  if (is.null(area_groups) || !all(c("Class_ID", "Area_sqm") %in% names(area_groups))) return(NULL)
  km2 <- setNames(rep(0, 3), c("0", "1", "2"))
  for (i in seq_len(nrow(area_groups))) {
    cid <- suppressWarnings(as.integer(area_groups$Class_ID[i]))
    if (!is.na(cid) && cid >= 0 && cid < 3) km2[as.character(cid)] <- km2[as.character(cid)] + area_groups$Area_sqm[i] / 1e6
  }
  km2 <- as.numeric(km2); tot <- sum(km2); if (tot <= 0) return(NULL)
  data.frame(Class = c("Decreasing", "Stable", "Increasing"),
             Range = c("slope < 0", "slope ~ 0", "slope > 0"),
             Area_m2 = round(km2 * 1e6, 0), Area_ha = round(km2 * 100, 1), Area_km2 = round(km2, 3),
             Pixels = NA_real_, Pct = round(100 * km2 / tot, 1), stringsAsFactors = FALSE)
}

mod_gee_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(width = 4, class = "sidebar-panel-custom",
             div(class = "step-card", style = "border-left: 3px solid #6b4c7a;",
                 h4("1. Data Source", info_tooltip("Set the area you want to analyze. Upload a shapefile (.shp+.shx+.dbf+.prj together) or fetch a boundary you previously sent to the Data Clipboard from Shapefile Extractor."), class = "step-title", style="border-bottom-color: #6b4c7a;"),
                 actionButton(ns("import_basket_gee"), "Fetch Boundary from Clipboard", class="btn-custom", style="border: 1px dashed #6b4c7a; background-color: #f0e8f2; color: #6b4c7a; margin-bottom:10px;"),
                 p("— OR UPLOAD FILE —", style="text-align: center; color: #5c6b73; font-weight: 600; margin: 5px 0 8px 0; font-size: 11px;"),
                 fileInput(ns("gee_mask_file"), "Upload Target Area (.shp, .shx, .dbf, .prj)", multiple = TRUE, accept = c(".shp", ".shx", ".dbf", ".prj")), 
                 actionButton(ns("process_gee_mask_btn"), "Set Uploaded Boundary", class = "btn-primary btn-custom", style="margin-bottom:10px; border:none;"),
                 uiOutput(ns("gee_mask_status2")),
                 hr(style="margin: 15px 0; border-top: 1px solid #eee;"),
                 
                 selectInput(ns("gee_feature"), "Select Cloud Analysis:", choices = GEE_FEATURE_CHOICES),
                 selectInput(ns("gee_agg"), "Temporal Aggregation:", choices = c("Median", "Mean", "Max", "Min"), selected = "Median"),
                 dateRangeInput(ns("gee_date"), "Date Range:", start = Sys.Date() - 180, end = Sys.Date())
             ),
             div(class = "step-card", style = "border-left: 3px solid #3a6aa0;", 
                 h4("2. Run Analytics", info_tooltip("Fetches satellite imagery for your selected feature (NDVI, LST, etc.) and date range, computes statistics (mean, median, histogram), and renders it on the map. Cached for 15 minutes if you repeat the exact same request."), class="step-title"), 
                 actionButton(ns("run_gee_analytics"), "Generate Map & Statistics", class="btn-primary btn-custom", style="padding:12px; font-size:14px; border:none;")
             ),
             div(class = "step-card", style = "border-left: 3px solid #8b3a2b; background:#f5e6e3;", 
                 h4("3. Spatiotemporal Grid", info_tooltip("Generates a grid of maps — one panel per time interval — so you can visually track how your selected feature changed over multiple years. Automatically widens the search window for years with sparse cloud-free data."), class="step-title"), 
                 fluidRow(
                   column(6, numericInput(ns("temp_start"), "Start Year:", value=2015, min=1984, max=as.numeric(format(Sys.Date(), "%Y")))), 
                   column(6, numericInput(ns("temp_end"), "End Year:", value=as.numeric(format(Sys.Date(), "%Y")), min=1984, max=as.numeric(format(Sys.Date(), "%Y"))))
                 ), 
                 fluidRow(
                   column(6, selectInput(ns("temp_gap"), "Time Interval:", choices=c("1 Year"=1, "2 Years"=2, "3 Years"=3, "4 Years"=4, "5 Years"=5, "10 Years"=10), selected=3)),
                   column(6, selectInput(ns("temp_month"), "Select Month:", choices = setNames(1:12, month.name), selected = 1))
                 ),
                 actionButton(ns("run_temporal"), "Generate Temporal Gallery", class="btn-primary btn-custom", style="border:none; margin-top:10px;"),
                 actionButton(ns("add_cart_gallery"), "Save Gallery to Workspace", class="btn-success btn-custom", style="border:none;")
             )
      ),
      column(width = 8, class = "main-panel-custom",
             tabsetPanel(
               tabPanel("Live Map Render", div(style="margin-top:15px;", class="gf-map-shell", withSpinner(leafletOutput(ns("gee_live_map"), height="70vh"), type=8, color="#26333e"), insights_drawer_ui(ns("gee_insights"), title = "Analytics Insights"), uiOutput(ns("gee_live_map_citation")))),
               tabPanel("Zonal Statistics", div(style="padding:20px; background:#fff; margin-top:15px; border-radius:6px; border:1px solid #d8d4c8;", 
                                                div(style="display: flex; justify-content: space-between; align-items: center; margin-bottom: 15px;",
                                                    h4("Advanced Zonal Metrics", style="font-size:16px; font-weight:600; color:#26333e; margin:0;"),
                                                    actionButton(ns("add_cart_zonal"), "Save to Export Manager", class="btn-sm btn-success", style="font-weight:500; border:none;")
                                                ),
                                                div(class="secure-content", style="border: 1px solid #eee; padding: 10px; border-radius: 4px; background: #f7f6f2; overflow-x: auto;", withSpinner(DTOutput(ns("zonal_stats_tbl")), type=8, color="#26333e"))
               )),
               tabPanel("Zonal Profiling", div(style="padding:20px; background:#fff; margin-top:15px; border-radius:6px; border:1px solid #d8d4c8;", 
                                               div(style="display: flex; justify-content: space-between; align-items: center; margin-bottom: 15px;",
                                                   h4("Index Area Distribution", style="font-size:16px; font-weight:600; color:#26333e; margin:0;"),
                                                   actionButton(ns("add_cart_ts"), "Save to Export Manager", class="btn-sm btn-success", style="font-weight:500; border:none;")
                                               ),
                                               p("This chart profiles the landscape by grouping continuous index values into distinct ranges and calculating the total geographic area covered by each range.", style="font-size:12px; color:#5c6b73; margin-bottom:15px;"),
                                               div(class="protect-wrap", oncontextmenu="return false;",
                                                   withSpinner(plotOutput(ns("zonal_profile_plot"), height="350px"), type=8, color="#26333e")
                                               )
               )),
               tabPanel("Pixel Distribution", div(style="padding:20px; background:#fff; margin-top:15px; border-radius:6px; border:1px solid #d8d4c8;", 
                                                  div(style="display: flex; justify-content: space-between; align-items: center; margin-bottom: 15px;",
                                                      h4("Area Distribution Histogram", style="font-size:16px; font-weight:600; color:#26333e; margin:0;"),
                                                      actionButton(ns("add_cart_hist"), "Save Histogram", class="btn-sm btn-success", style="font-weight:500; border:none;")
                                                  ),
                                                  div(class="protect-wrap", oncontextmenu="return false;",
                                                      withSpinner(plotOutput(ns("histogram_plot"), height="350px"), type=8, color="#26333e")
                                                  )
               )),
               tabPanel("Temporal Grid", div(style="padding:20px; background:#fff; margin-top:15px; border-radius:6px; border:1px solid #d8d4c8;", h4("Spatiotemporal Scientific Grid", style="font-size:16px; font-weight:600; color:#26333e; margin-bottom:15px;"), withSpinner(uiOutput(ns("temporal_grid_ui")), type=8, color="#26333e"))),
               tabPanel("Trend Analysis", div(style="padding:20px; background:#fff; margin-top:15px; border-radius:6px; border:1px solid #d8d4c8;",
                                              h4("Multi-Year Per-Pixel Trend", info_tooltip("Fits a linear regression per pixel across the years you select, showing whether your feature is increasing (red) or decreasing (blue) over time. Includes a Mann-Kendall test to flag which areas show a STATISTICALLY significant trend (p<0.05), not just noise."), style="font-size:16px; font-weight:600; color:#26333e; margin-bottom:5px;"),
                                              p("Computes a linear regression slope per pixel across years for the currently selected feature (e.g. NDVI change/year, LST warming/cooling rate), with a Mann-Kendall test for statistical significance.", style="color:#5c6b73; font-size:12px; margin-bottom:15px;"),
                                              fluidRow(
                                                column(3, numericInput(ns("trend_start_year"), "Start Year", value = 2018, min = 1984, max = 2026)),
                                                column(3, numericInput(ns("trend_end_year"), "End Year", value = 2024, min = 1984, max = 2026)),
                                                column(3, selectInput(ns("trend_month"), "Month", choices = setNames(1:12, month.name), selected = 3)),
                                                column(3, style="padding-top:25px;", actionButton(ns("run_trend_btn"), "Compute Trend", class="btn-primary btn-custom", style="border:none; width:100%;"))
                                              ),
                                              checkboxInput(ns("trend_sig_only"), "Show only statistically significant trends (Mann-Kendall, p < 0.05)", value = FALSE),
                                              withSpinner(leafletOutput(ns("trend_map")), type=8, color="#26333e"),
                                              uiOutput(ns("trend_map_citation"))
               )),
               
               tabPanel("Batch Processing", div(style="padding:20px; background:#fff; margin-top:15px; border-radius:6px; border:1px solid #d8d4c8;",
                                                h4("Batch Zonal Statistics — Multiple Boundaries at Once", info_tooltip("Runs one Earth Engine server-side call across ALL boundaries in your current multi-feature selection (e.g. every district in a state), instead of repeating the analysis one boundary at a time. Requires selecting multiple features WITHOUT dissolving them in Shapefile Extractor first."), style="font-size:16px; font-weight:600; color:#26333e; margin-bottom:5px;"),
                                                p("Runs the feature/date range/aggregation currently selected in Step 2 across EVERY boundary in your current selection in one go (e.g. all districts in a state) — much faster than repeating Step 2 one boundary at a time.", style="color:#5c6b73; font-size:12px; margin-bottom:5px;"),
                                                p(HTML("<b>Setup:</b> In Shapefile Extractor, select multiple features <b>without</b> checking 'Merge Selected Boundaries (Dissolve)', send to Clipboard, then import that as your boundary here (Step 1)."), style="color:#5c6b73; font-size:11px; margin-bottom:15px; font-style:italic;"),
                                                actionButton(ns("run_batch_stats"), "Run Batch Zonal Stats", class="btn-primary btn-custom", style="border:none;"),
                                                hr(),
                                                withSpinner(DTOutput(ns("batch_results_table")), type=8, color="#26333e"),
                                                downloadButton(ns("download_batch_csv"), "Download Batch CSV", class="btn-success", style="margin-top:15px; border:none;"),
                                                downloadButton(ns("download_batch_xlsx"), "Download Batch Excel", class="btn-success", style="margin-top:15px; margin-left:8px; border:none;")
               )),

               # ============================================================
               # NEW TAB — Intelligent Sequential Pipeline (additive; the
               # tabs above remain the default "Independent Analysis" mode).
               # ============================================================
               tabPanel("Sequential Pipeline",
                 div(style = "padding:18px; background:#fff; margin-top:15px; border-radius:6px; border:1px solid #d8d4c8;",
                   # SortableJS (CDN, same version used by the Publication Maps canvas).
                   # Loading twice is idempotent; the init guards on window.Sortable.
                   tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/Sortable/1.15.2/Sortable.min.js"),
                   tags$script(HTML(sprintf("
                     (function(){
                       function report(el){
                         try{
                           var ids = Array.prototype.slice.call(el.querySelectorAll('.gf-pipe-step'))
                                       .map(function(c){return c.getAttribute('data-tool');}).filter(Boolean);
                           if(window.Shiny && Shiny.setInputValue) Shiny.setInputValue('%s', ids, {priority:'event'});
                         }catch(e){}
                       }
                       function init(){
                         if(!window.Sortable) return;
                         document.querySelectorAll('.gf-pipe-seq').forEach(function(el){
                           if(el._gfPipeSort) return;
                           el._gfPipeSort = Sortable.create(el, {handle:'.gf-pipe-handle', animation:150,
                             ghostClass:'gf-pipe-ghost', onEnd:function(){report(el);}});
                         });
                       }
                       document.addEventListener('shiny:connected', function(){ setTimeout(init, 600); });
                       setInterval(init, 1200);
                     })();
                   ", ns("pipeline_order")))),
                   tags$style(HTML("
                     .gf-pipe-chip{display:inline-block;margin:3px;padding:6px 11px;border-radius:16px;border:1px solid #c9d3da;
                       background:#f3f6f8;color:#26333e;font-size:12px;font-weight:600;cursor:pointer;transition:all .12s;}
                     .gf-pipe-chip:hover{background:#26333e;color:#fff;border-color:#26333e;}
                     .gf-pipe-seq{min-height:60px;}
                     .gf-pipe-step{display:flex;align-items:center;gap:10px;background:#fbfaf7;border:1px solid #d8d4c8;
                       border-left:4px solid #3a6aa0;border-radius:6px;padding:9px 11px;margin-bottom:7px;}
                     .gf-pipe-step[data-role='mask']{border-left-color:#2b7a8b;}
                     .gf-pipe-step[data-role='static']{border-left-color:#8a6d3b;}
                     .gf-pipe-step[data-role='trend']{border-left-color:#6b4c7a;}
                     .gf-pipe-handle{cursor:grab;color:#a9b4bb;font-weight:700;letter-spacing:-2px;user-select:none;}
                     .gf-pipe-num{display:inline-flex;align-items:center;justify-content:center;width:22px;height:22px;border-radius:50%;
                       background:#26333e;color:#fff;font-size:11px;font-weight:700;flex:0 0 auto;}
                     .gf-pipe-role{font-size:10px;text-transform:uppercase;letter-spacing:.5px;color:#8a97a0;font-weight:700;}
                     .gf-pipe-rm{border:none;background:transparent;color:#b06a5b;font-size:17px;line-height:1;cursor:pointer;padding:0 4px;}
                     .gf-pipe-rm:hover{color:#8b3a2b;}
                     .gf-pipe-ghost{opacity:.5;background:#e8eff6;}
                     .gf-pipe-empty{color:#8a97a0;font-style:italic;font-size:12px;border:1px dashed #cdd6dc;border-radius:6px;padding:18px;text-align:center;}
                   ")),
                   div(style = "display:flex;justify-content:space-between;align-items:center;flex-wrap:wrap;gap:8px;border-bottom:1px solid #eee;padding-bottom:10px;margin-bottom:14px;",
                       h4(HTML("Intelligent Workflow Builder &nbsp;<span style='font-size:11px;font-weight:600;color:#45936f;background:#e6f3ec;padding:2px 8px;border-radius:10px;'>Guided Mode</span>"),
                          style = "margin:0;color:#26333e;font-weight:600;font-size:16px;"),
                       span("The tabs above stay independent. This tab chains tools in a guided order.", style = "font-size:11px;color:#5c6b73;")
                   ),
                   # --- Collapsible "How it works" help (native <details>, no dependency) ---
                   tags$details(style = "margin-bottom:14px;border:1px solid #e2e6ea;border-radius:6px;background:#fbfcfd;",
                     tags$summary(style = "cursor:pointer;padding:9px 12px;font-weight:600;font-size:12px;color:#26333e;",
                                  HTML("&#9432; How the Sequential Pipeline works &mdash; read me")),
                     div(style = "padding:4px 14px 14px 14px;font-size:12px;color:#3a454d;line-height:1.55;",
                       HTML("
                       <p style='margin:6px 0;'>The pipeline runs a list of analyses <b>in order, top to bottom, in one click</b>. Each step can hand a <b>region mask</b> to the next through a shared <i>state bus</i> — so a step doesn't just run in isolation, it can constrain everything after it.</p>
                       <p style='margin:8px 0 4px;'><b>Four steps to use it:</b></p>
                       <ol style='margin:0 0 8px 18px;padding:0;'>
                         <li><b>Set a boundary</b> in Step 1 (Data Source) in the sidebar. Then set the pipeline's <b>own date range &amp; compositing</b> in the box below the sequence — it is fully independent of the sidebar and applies to every step.</li>
                         <li><b>Add tools</b> from the catalog (click a chip). Every GEE analysis in the app is here, plus Benthic Mapping, Coral Health and Trend.</li>
                         <li><b>Order them</b> by dragging. The <b>Smart Sequence Assistant</b> checks the order live and flags problems.</li>
                         <li><b>Run Sequence.</b> You get a status row per step (computed / skipped / failed) and a thumbnail of each computed layer.</li>
                       </ol>
                       <p style='margin:8px 0 4px;'><b>What a step passes on (its role):</b></p>
                       <ul style='margin:0 0 8px 18px;padding:0;'>
                         <li><b>mask</b> (Benthic Mapping, NDWI, MNDWI, AWEI) &mdash; publishes a <b>water mask</b> to the bus. Every later step is clipped to it.</li>
                         <li><b>index</b> (NDVI, NDBI, LST, coral, &hellip;) &mdash; a continuous layer; if a mask is active upstream, it is clipped to that mask.</li>
                         <li><b>static</b> (Hillshade, Population, terrain) &mdash; date-independent layers; a water mask upstream would blank them.</li>
                         <li><b>trend</b> &mdash; multi-year slope; belongs after a base index.</li>
                       </ul>
                       <p style='margin:8px 0 4px;'><b>Worked examples:</b></p>
                       <ul style='margin:0 0 4px 18px;padding:0;'>
                         <li><b>Reef / coral study:</b> <code>Benthic Mapping &rarr; Coral Health</code>. Benthic isolates the reef/water pixels and publishes that mask; Coral Health is then measured <i>only</i> on reef pixels, not surrounding sand or rock.</li>
                         <li><b>Water-quality:</b> <code>NDWI &rarr; Moisture Index (NDMI)</code>. NDWI publishes a water mask; the moisture index is computed only inside the water body.</li>
                         <li><b>Terrestrial batch:</b> <code>NDVI &rarr; Built-up (NDBI) &rarr; Surface Temp (LST)</code>. No mask step, so all three run over the full boundary &mdash; a one-click way to generate several indices for the same area and date.</li>
                         <li><b>What the assistant catches:</b> putting <code>Population Density</code> <i>after</i> <code>Benthic Mapping</code> triggers a warning &mdash; the water mask would blank a land-only layer, so it tells you to move it before the mask or drop it.</li>
                       </ul>
                       <p style='margin:8px 0 0;color:#5c6b73;'><i>The other GEE tabs are unaffected &mdash; use them exactly as before for one-off, independent analyses. The pipeline only runs when you press Run Sequence here.</i></p>
                       ")
                     )
                   ),
                   fluidRow(
                     column(5,
                       h5("1 · Tool Catalog", style = "font-weight:600;color:#26333e;font-size:13px;"),
                       p("Click a tool to add it to the sequence. Drag steps to reorder.", style = "font-size:11px;color:#5c6b73;margin-bottom:8px;"),
                       uiOutput(ns("pipeline_catalog")),
                       hr(style = "margin:14px 0;"),
                       h5("Smart Sequence Assistant", style = "font-weight:600;color:#26333e;font-size:13px;"),
                       div(style = "max-height:280px;overflow-y:auto;", uiOutput(ns("pipeline_assistant")))
                     ),
                     column(7,
                       div(style = "display:flex;justify-content:space-between;align-items:center;",
                           h5("2 · Your Sequence", style = "font-weight:600;color:#26333e;font-size:13px;margin:0;"),
                           actionButton(ns("pipe_clear"), "Clear", class = "btn-sm", style = "font-size:11px;padding:2px 9px;border:1px solid #cdd6dc;background:#f3f6f8;color:#26333e;font-weight:600;")
                       ),
                       p("Runs top-to-bottom. Each step passes its region / mask to the next.", style = "font-size:11px;color:#5c6b73;margin:4px 0 8px 0;"),
                       uiOutput(ns("pipeline_builder")),
                       div(style = "background:#eef3f6;border-radius:6px;padding:9px 11px;margin-top:10px;border:1px solid #cfdbe3;",
                           div(HTML("Pipeline date range &amp; compositing &nbsp;<span style='color:#8a97a0;font-weight:400;'>(one control for the WHOLE pipeline — steps AND trend; independent of the sidebar)</span>"),
                               style = "font-size:11px;font-weight:700;color:#1f2c35;margin-bottom:2px;"),
                           div(HTML("Point-in-time steps (Coral Health, Benthic Mapping, all indices) composite over this <b>whole range</b>. If <b>Trend Analysis</b> is in the sequence, it spans this range's <b>start year → end year</b>. The sidebar date/aggregation are ignored while the pipeline runs."),
                               style = "font-size:10px;color:#5c6b73;margin-bottom:7px;"),
                           fluidRow(
                             column(7, dateRangeInput(ns("pipe_date"), NULL,
                                                      start = as.Date(sprintf("%d-01-01", as.numeric(format(Sys.Date(), "%Y")) - 4)),
                                                      end = Sys.Date(), min = "1984-01-01", max = Sys.Date(), width = "100%")),
                             column(5, selectInput(ns("pipe_agg"), NULL, choices = c("Median", "Mean", "Max", "Min"), selected = "Median", width = "100%"))
                           ),
                           div(HTML("<b>Tip:</b> pick a range with several years of clear imagery (e.g. 2016-01-01 → 2019-12-31). One common range now drives every step, including Trend."),
                               style = "font-size:10px;color:#6a7780;margin-top:2px;")),
                       textInput(ns("pipe_area_name"), NULL, value = "",
                                 placeholder = "Study-area name / location (optional — shown on the report)", width = "100%"),
                       fluidRow(
                         column(6, selectInput(ns("pipe_headline"), "Composite / headline layer",
                                               choices = c("Auto (recommended)" = ""), width = "100%")),
                         column(6, selectInput(ns("pipe_response"), "Correlation response variable",
                                               choices = c("Auto (recommended)" = ""), width = "100%"))
                       ),
                       div(HTML("<b>Auto</b> uses Coral Health if present, else the first layer. Override to make any layer the report's composite / the variable others are correlated against — for non-coral studies (urban, population, etc.)."),
                           style = "font-size:10px;color:#8a97a0;margin-top:-4px;margin-bottom:2px;"),
                       actionButton(ns("run_pipeline"), "Run Sequence", class = "btn-primary btn-custom",
                                    style = "border:none;margin-top:12px;width:100%;padding:11px;font-size:14px;"),
                       p(HTML("Uses the boundary from <b>Step 1 · Data Source</b> in the sidebar. Every date, the compositing, AND the Trend span all come from the single <b>Pipeline date range</b> above — nothing here depends on the sidebar."),
                         style = "font-size:10px;color:#8a97a0;margin-top:6px;text-align:center;")
                     )
                   ),
                   uiOutput(ns("pipeline_results")),
                   # ---- Final composite map (the end product of the whole chain) ----
                   uiOutput(ns("pipeline_final_header")),
                   div(class = "protect-wrap", oncontextmenu = "return false;",
                       withSpinner(plotOutput(ns("pipeline_final_map"), height = "520px"), type = 8, color = "#26333e")),
                   uiOutput(ns("pipeline_final_caption")),
                   uiOutput(ns("pipeline_final_explain"))
                 )
               ),
               # NEW TAB — Insights & Analytics (passive numeric summary of the last run).
               tabPanel("Insights & Analytics",
                 div(style = "padding:18px; background:#fff; margin-top:15px; border-radius:6px; border:1px solid #d8d4c8;",
                   div(style = "display:flex; justify-content:space-between; align-items:center; border-bottom:1px solid #eee; padding-bottom:10px; margin-bottom:14px;",
                       h4(HTML("Insights &amp; Analytics &nbsp;<span style='font-size:11px;font-weight:600;color:#45936f;background:#e6f3ec;padding:2px 8px;border-radius:10px;'>Research Metrics</span>"),
                          style = "margin:0; color:#26333e; font-weight:600; font-size:16px;"),
                       div(style = "display:flex; align-items:center; gap:8px;",
                           downloadButton(ns("pipe_dl_report"), "Report (HTML)", class = "btn-sm",
                                          style = "font-size:11px; padding:5px 10px; background:#2c5a4a; border:none; color:#fff;"),
                           downloadButton(ns("pipe_dl_data"), "Data (Excel)", class = "btn-sm",
                                          style = "font-size:11px; padding:5px 10px; background:#3a6aa0; border:none; color:#fff;"))),
                   div("Publication outputs bundle the composite map, every stats table, an auto-written Methods section, citations, and a no-field-validation limitations note.",
                       style = "font-size:10px; color:#8a97a0; margin:-6px 0 10px;"),
                   uiOutput(ns("gee_insights_dash"))
                 )
               )
             )
      )
    )
  )
}
mod_gee_server <- function(id, rv, gee_rv, floating_rv, cart_rv, add_to_workspace) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    log_err <- function(msg, fix) { showModal(modalDialog(title = HTML("<b style='color:#8b3a2b;'>Action Required</b>"), HTML(paste0("<div style='font-size: 14px;'><p><b>Error:</b> ", msg, "</p><hr><p style='color:#45936f;'><b>Suggestion:</b> ", fix, "</p></div>")), size = "m", easyClose = TRUE, footer = modalButton("Okay"))) }
    
    # Safe Leaflet Init
    output$gee_live_map <- renderLeaflet({
      tryCatch({
        leaflet(options = leafletOptions(maxZoom = 24)) %>% 
          addProviderTiles(providers$CartoDB.Positron) %>% 
          setView(78.96, 20.59, 5) %>% 
          htmlwidgets::onRender(js_coords) %>% 
          inject_map_elements("Cloud Analytics")
      }, error = function(e) { leaflet() %>% addProviderTiles(providers$CartoDB.Positron) })
    })
    
    output$gee_live_map_citation <- renderUI({
      req(gee_rv$current_feature)
      p(sprintf("Data source: %s", get_feature_citation(gee_rv$current_feature)), style = "font-size:11px; color:#5c6b73; font-style:italic; margin-top:6px; margin-bottom:0;")
    })
    
    output$trend_map_citation <- renderUI({
      req(gee_rv$trend_feature)
      p(sprintf("Data source: %s", get_feature_citation(gee_rv$trend_feature)), style = "font-size:11px; color:#5c6b73; font-style:italic; margin-top:6px; margin-bottom:0;")
    })
    
    observeEvent(input$process_gee_mask_btn, { 
      req(input$gee_mask_file)
      if(!any(grepl("\\.shp$", tolower(input$gee_mask_file$name)))) return(log_err("Missing .shp file", "Select .shp, .shx, .dbf, and .prj together."))
      
      tryCatch({
        td <- tempdir(); for(i in 1:nrow(input$gee_mask_file)) file.copy(input$gee_mask_file$datapath[i], file.path(td, input$gee_mask_file$name[i]), overwrite=TRUE); 
        p <- file.path(td, input$gee_mask_file$name[grep("\\.shp$", tolower(input$gee_mask_file$name))][1]); 
        
        v <- st_read(p, quiet=TRUE) %>% st_make_valid() %>% st_zm()
        # 🚀 REFACTOR: uses shared ensure_crs_4326() (global.R) instead of inline if/else-if
        v <- ensure_crs_4326(v)
        if (!validate_roi_size(v)) return()
        
        dyn_scale <- get_dynamic_scale(v)
        rv$mask_vect <- v
        zoom_to_boundary("gee_live_map", v)
        rm(v); gc(); 
        
        output$gee_mask_status2 <- renderUI({ HTML(sprintf("<div style='color:#45936f; font-weight:600; font-size:12px; text-align:center; margin-bottom:10px; background: #e8f0ea; padding: 6px; border-radius: 4px; border: 1px solid #9bc4ab;'>Boundary Uploaded. Scale set to %dm.</div>", dyn_scale)) }); 
        showNotification("Boundary Set for Analytics!", type="message") 
      }, error = function(e) log_err("Corrupt Shapefile", e$message))
    })
    
    # 🚀 FIX: "always grab the last clipboard item" was fragile and confusing when multiple items
    # existed. Now shows a modal listing every valid boundary (with its feature count) so the user
    # explicitly picks the correct one instead of relying on send-order.
    observeEvent(input$import_basket_gee, {
      req_types <- c("polygon", "shapefile")
      valid_files <- Filter(function(x) any(sapply(req_types, function(rt) grepl(rt, x$type, ignore.case=TRUE))), floating_rv$files)
      if(length(valid_files) == 0) { showNotification("No valid boundaries found in Clipboard!", type="error"); return() }
      
      choices <- setNames(names(valid_files), sapply(valid_files, function(x) sprintf("%s (%d feature%s)", x$name, nrow(x$data), if(nrow(x$data) == 1) "" else "s")))
      
      showModal(modalDialog(
        title = "Select Boundary to Import",
        selectInput(ns("selected_clipboard_item"), "Choose which Clipboard item to use as your boundary:", choices = choices),
        footer = tagList(modalButton("Cancel"), actionButton(ns("confirm_import_boundary"), "Import This Boundary", class = "btn-primary"))
      ))
    })
    
    observeEvent(input$confirm_import_boundary, {
      req(input$selected_clipboard_item)
      req_types <- c("polygon", "shapefile")
      valid_files <- Filter(function(x) any(sapply(req_types, function(rt) grepl(rt, x$type, ignore.case=TRUE))), floating_rv$files)
      chosen <- valid_files[[input$selected_clipboard_item]]
      if (is.null(chosen)) { showNotification("Selected item no longer exists in Clipboard.", type = "error"); return() }
      
      data <- st_zm(chosen$data)
      if (!validate_roi_size(data)) return()
      rv$mask_vect <- data
      zoom_to_boundary("gee_live_map", data)
      removeModal()
      output$gee_mask_status2 <- renderUI({ HTML(sprintf("<div style='color:#45936f; font-weight:600; font-size:12px; text-align:center; margin-bottom:10px; background: #e8f0ea; padding: 6px; border-radius: 4px; border: 1px solid #9bc4ab;'>Boundary Linked: %s (%d feature%s)</div>", chosen$name, nrow(data), if(nrow(data)==1) "" else "s")) })
      showNotification(sprintf("Boundary linked: '%s' with %d feature(s).", chosen$name, nrow(data)), type="message", duration=5)
    })
    
    observeEvent(input$run_gee_analytics, {
      if(is.null(rv$mask_vect)) return(showNotification("Please set a Boundary first!", type="error"))
      if(!check_rate_limit(rv, "run_gee_analytics")) return()
      
      gee_rv$zonal_data <- NULL
      gee_rv$hist_data <- NULL
      
      feature_name <- input$gee_feature
      start_d <- as.character(input$gee_date[1]); end_d <- as.character(input$gee_date[2])
      agg <- input$gee_agg
      dyn_scale <- get_dynamic_scale(rv$mask_vect)
      roi_wkt <- sf::st_as_text(sf::st_union(rv$mask_vect))
      mask_vect_snapshot <- rv$mask_vect # plain sf object — safe to keep for main-session boundary rendering
      
      # 🚀 COST FIX: cache results per-session for identical repeat requests — avoids re-calling
      # the compute service (and re-running expensive GEE reduceRegion calls) if the user re-clicks
      # or revisits a tab within a short window.
      cache_key <- digest::digest(list(feature_name, start_d, end_d, agg, dyn_scale, roi_wkt))
      if (is.null(gee_rv$analysis_cache)) gee_rv$analysis_cache <- list()
      cached <- gee_rv$analysis_cache[[cache_key]]
      
      set_busy(session, paste0("Analyzing ", feature_name, "... (app stays usable — feel free to keep working)"))
      # 🚀 FIX: withProgress() only stays open while its enclosing code block runs synchronously —
      # since this handler dispatches an ASYNC promise and returns immediately, a withProgress()
      # wrapper here would close (and the bottom-right progress bar would vanish) long before the
      # promise actually resolves. Progress$new() is the lower-level API that isn't tied to a
      # synchronous scope: it stays open until something explicitly calls $close(), which lets us
      # close it from inside the promise's success/error callbacks instead.
      progress <- shiny::Progress$new(session)
      progress$set(message = paste0("Analyzing ", feature_name, "..."), value = NULL)
      
      # Renders a fully-resolved result (a plain list — same shape whether it came from the
      # compute service's JSON response or from this session's cache). Called either immediately
      # (cache hit) or from the async request's .then() callback (cache miss).
      render_result <- function(result) {
        gee_rv$current_feature <- feature_name
        rv$available_maps <- unique(c(rv$available_maps, feature_name))
        gee_rv$saved_images[[feature_name]] <- list(type = "simple", feature = feature_name, start_d = start_d, end_d = end_d, agg = agg, scale = dyn_scale, roi_wkt = roi_wkt)
        gee_rv$saved_vis[[feature_name]] <- list(pal = result$palette, min = result$min, max = result$max)
        gee_rv$sensor_used[[feature_name]] <- result$sensor %||% "Unknown"
        gee_rv$mean_val <- result$stats$mean
        gee_rv$med_val <- result$stats$median
        
        # 🚀 REFACTOR (Option B): the 95% CI calculation and zonal-stats table-building previously
        # lived entirely inline here — now extracted to build_zonal_stats_df() (global.R) so it's
        # independently unit-testable. Pixels are spatially autocorrelated (not independent
        # samples), so this CI is an optimistic lower bound on true uncertainty, not a rigorous
        # geostatistical interval — noted in the caption below the table.
        gee_rv$zonal_data <- build_zonal_stats_df(result$stats)
        
        if (!is.null(result$histogram)) {
          hist_df <- data.frame(Bin = unlist(result$histogram$bins), Count = unlist(result$histogram$counts))
          pixel_area_sqm <- dyn_scale * dyn_scale
          hist_df$Area_SqKm <- (hist_df$Count * pixel_area_sqm) / 1e6
          gee_rv$hist_data <- hist_df
        }
        # Continuous -> discrete class insights (passive, from the histogram's per-bin area).
        gee_rv$gee_class_insights <- tryCatch({
          df <- gf_classify_hist(gee_rv$hist_data, result$min, result$max)
          if (!is.null(df)) list(feature = feature_name, source = "Cloud Analysis", classes = df) else NULL
        }, error = function(e) NULL)
        
        bbox <- st_bbox(st_transform(mask_vect_snapshot, 4326))
        leafletProxy("gee_live_map") %>% 
          clearTiles() %>% clearGroup("Boundary") %>% clearGroup("Analytics Layer") %>% clearControls() %>%
          addProviderTiles(providers$CartoDB.Positron) %>% 
          addTiles(urlTemplate = result$tile_url, group="Analytics Layer") %>% 
          addPolygons(data=st_transform(mask_vect_snapshot, 4326), fill=F, color="#8b3a2b", weight=3, group="Boundary") %>%
          fitBounds(as.numeric(bbox["xmin"]), as.numeric(bbox["ymin"]), as.numeric(bbox["xmax"]), as.numeric(bbox["ymax"])) %>%
          addLegend("bottomright", pal = colorNumeric(unlist(result$palette), domain = c(result$min, result$max)), values = c(result$min, result$max), title = feature_name, opacity = 1) %>%
          addLayersControl(baseGroups = "Satellite", overlayGroups = c("Boundary", "Analytics Layer"), position = "bottomleft", options = layersControlOptions(collapsed = TRUE))
        
        showNotification(paste0("Analytics Ready! (Data source: ", result$sensor %||% "Unknown", ")"), type="message", duration = 6)
        clear_busy(session)
        progress$close()
      }
      
      if (!is.null(cached) && as.numeric(difftime(Sys.time(), cached$time, units = "mins")) < 15) {
        # Fast path: cache hit — render immediately, no network call needed at all
        log_event("INFO", "mod_gee", "analytics_cache_hit", session_id = session$token, feature = feature_name)
        render_result(cached)
        return()
      }

      # 🚀 EXPANDED CATALOGUE: rgee-native analyses (spectral indices + terrain, see
      # R/gee_indices.R) are computed HERE in the Shiny server via Earth Engine rather than the
      # external compute service. EE calls are synchronous (same pattern the Temporal Grid already
      # uses); compute_rgee_analytics() returns the exact compute-service response shape, so
      # render_result() below is reused verbatim and the Insights Dashboard works automatically.
      if (is_rgee_native_feature(feature_name)) {
        result <- tryCatch({
          ee_roi_local <- sf_as_ee(rv$mask_vect)
          r <- compute_rgee_analytics(feature_name, start_d, end_d, agg, ee_roi_local, dyn_scale)
          r$time <- Sys.time()
          r
        }, error = function(e) {
          log_event("ERROR", "mod_gee", "analytics_rgee_failed", session_id = session$token, feature = feature_name, error = conditionMessage(e))
          showNotification(paste("Analytics Error:", conditionMessage(e)), type = "error", duration = 9)
          NULL
        })
        if (!is.null(result)) {
          log_event("INFO", "mod_gee", "analytics_rgee_succeeded", session_id = session$token, feature = feature_name, sensor = result$sensor %||% "Unknown")
          gee_rv$analysis_cache[[cache_key]] <- result
          render_result(result)
        }
        clear_busy(session); progress$close()
        return()
      }

      log_event("INFO", "mod_gee", "analytics_requested", session_id = session$token,
                feature = feature_name, agg = agg, scale_m = dyn_scale, start_d = start_d, end_d = end_d)
      
      # 🚀 REFACTOR: request-construction now goes through the shared build_compute_service_request()
      # (global.R) — this exact boilerplate (URL path, form-body, timeout, error-passthrough, auth
      # token) was previously repeated at 3 call sites (analytics, trend, batch).
      req <- build_compute_service_request(
        "run-analytics",
        list(feature = feature_name, start_date = start_d, end_date = end_d, agg = agg, roi_wkt = roi_wkt, scale = as.character(dyn_scale))
      )
      
      prom <- httr2::req_perform_promise(req)
      
      prom %...>% (function(resp) {
        # 🚀 REFACTOR: uses shared parse_compute_service_response() (global.R) — the JSON/status/
        # error-checking block was word-for-word identical across all 3 compute-service call sites.
        result <- parse_compute_service_response(resp)
        result$time <- Sys.time()
        gee_rv$analysis_cache[[cache_key]] <- result
        log_event("INFO", "mod_gee", "analytics_succeeded", session_id = session$token,
                  feature = feature_name, sensor = result$sensor %||% "Unknown")
        render_result(result)
      }) %...!% (function(e) {
        msg <- conditionMessage(e)
        log_event("ERROR", "mod_gee", "analytics_failed", session_id = session$token, feature = feature_name, error = msg)
        if (grepl("no bands", msg, ignore.case = TRUE) || grepl("ST_B10", msg)) {
          msg <- "No clear satellite imagery found. Please expand the Date Range or increase Cloud Tolerance."
        }
        if (grepl("Timeout|timed out", msg, ignore.case = TRUE)) {
          msg <- "The compute service took too long to respond. Try a smaller boundary or narrower date range."
        }
        showNotification(paste("Analytics Error:", msg), type = "error", duration = 8)
        clear_busy(session)
        progress$close()
      })
    })
    
    output$trend_map <- renderLeaflet({
      leaflet(options = leafletOptions(maxZoom = 24)) %>% 
        addProviderTiles(providers$CartoDB.Positron) %>% 
        setView(78.96, 20.59, 5)
    })
    
    observeEvent(input$run_trend_btn, {
      if(is.null(rv$mask_vect)) return(showNotification("Please set a Boundary first!", type="error"))
      if(input$trend_end_year - input$trend_start_year < 1) return(showNotification("Select at least 2 different years.", type="error"))
      if(!check_rate_limit(rv, "run_trend_btn", cooldown_seconds = 15)) return()
      
      set_busy(session, "Computing multi-year trend... (app stays usable — feel free to keep working)")
      progress <- shiny::Progress$new(session)
      progress$set(message = "Computing multi-year trend...", value = NULL)
      
      feature_name <- input$gee_feature
      # 🚀 FIX (trend "not enough year data"): coerce the year inputs to clean integers so the
      # compute service receives a proper numeric year SEQUENCE, never a stray float/factor that
      # could collapse to a single value. (numericInput can yield e.g. 2018.0; send 2018.)
      trend_start_year <- as.integer(round(input$trend_start_year)); trend_end_year <- as.integer(round(input$trend_end_year))
      mo <- as.integer(input$trend_month); agg <- input$gee_agg
      sig_only <- isTRUE(input$trend_sig_only)
      dyn_scale <- get_dynamic_scale(rv$mask_vect)
      roi_wkt <- sf::st_as_text(sf::st_union(rv$mask_vect))
      mask_vect_snapshot <- rv$mask_vect
      
      # 🚀 REFACTOR: uses shared build_compute_service_request() (global.R)
      req <- build_compute_service_request(
        "run-trend",
        list(feature = feature_name, start_year = trend_start_year, end_year = trend_end_year,
             month = mo, agg = agg, roi_wkt = roi_wkt, scale = as.character(dyn_scale),
             sig_only = tolower(as.character(sig_only)))
      )
      
      prom <- httr2::req_perform_promise(req)
      
      prom %...>% (function(resp) {
        # 🚀 REFACTOR: uses shared parse_compute_service_response() (global.R)
        result <- parse_compute_service_response(resp)
        
        bbox <- sf::st_bbox(sf::st_transform(mask_vect_snapshot, 4326))
        abs_max <- result$max
        
        leafletProxy("trend_map") %>%
          clearTiles() %>% clearShapes() %>% clearControls() %>%
          addProviderTiles(providers$CartoDB.Positron) %>%
          addTiles(urlTemplate = result$tile_url, group = "Trend") %>%
          addPolygons(data = sf::st_transform(mask_vect_snapshot, 4326), fill = FALSE, color = "#26333e", weight = 2, group = "Boundary") %>%
          addLegend("bottomright", colors = unlist(result$palette), labels = c(sprintf("%.4f /yr", -abs_max), "0", sprintf("+%.4f /yr", abs_max)), title = paste(feature_name, "Trend", if (sig_only) "(p<0.05 only)" else "")) %>%
          fitBounds(lng1 = as.numeric(bbox["xmin"]), lat1 = as.numeric(bbox["ymin"]), lng2 = as.numeric(bbox["xmax"]), lat2 = as.numeric(bbox["ymax"]))
        
        trend_map_name <- paste(feature_name, "- Trend", trend_start_year, "to", trend_end_year)
        gee_rv$saved_images[[trend_map_name]] <- list(type = "trend", feature = feature_name, start_year = trend_start_year, end_year = trend_end_year, month = mo, agg = agg, scale = dyn_scale, roi_wkt = roi_wkt, sig_only = sig_only)
        gee_rv$saved_vis[[trend_map_name]] <- list(pal = unlist(result$palette), min = result$min, max = result$max)
        rv$available_maps <- unique(c(rv$available_maps, trend_map_name))
        gee_rv$trend_feature <- feature_name
        
        sig_pct <- result$sig_pct
        # Persist for the Insights → Rate of Change section.
        gee_rv$trend_summary <- list(feature = feature_name, start_year = trend_start_year,
                                     end_year = trend_end_year, rate = result$max, sig_pct = sig_pct,
                                     units = tryCatch(get_feature_units(feature_name), error = function(e) ""))
        sig_msg <- if (!is.null(sig_pct) && !is.na(sig_pct)) sprintf(" %.1f%% of the region shows a statistically significant trend (Mann-Kendall, p<0.05).", sig_pct) else ""
        log_event("INFO", "mod_gee", "trend_analysis_succeeded", session_id = session$token,
                  feature = feature_name, start_year = trend_start_year, end_year = trend_end_year, sig_pct = sig_pct)
        showNotification(paste0("Trend map generated! Blue = decreasing, Red = increasing.", sig_msg, " (Also added to Publication Map list)"), type = "message", duration = 10)
        clear_busy(session)
        progress$close()
      }) %...!% (function(e) {
        log_event("ERROR", "mod_gee", "trend_analysis_failed", session_id = session$token,
                  feature = feature_name, error = conditionMessage(e))
        showNotification(paste("Trend Analysis Error:", conditionMessage(e)), type = "error", duration = 10)
        clear_busy(session)
        progress$close()
      })
    })
    
    # =========================================================================
    # 🚀 FEATURE: BATCH ZONAL STATISTICS — one server-side reduceRegions() call computes
    # per-feature stats across ALL boundaries in the current multi-feature selection at once,
    # instead of repeating "Run Analytics" one boundary at a time.
    # =========================================================================
    observeEvent(input$run_batch_stats, {
      # 🚀 FIX (batch multi-feature miscount): count features robustly. A boundary that arrives
      # as a SINGLE multi-part feature (one MULTIPOLYGON row) reads as nrow()==1 even though it
      # holds several distinct polygons — st_cast() splits it into its constituent polygons so
      # each is batched separately. This makes "multiple unmerged features" work whether they
      # arrive as multiple sf rows OR as one multi-part geometry, with no merge/dissolve involved.
      boundary <- rv$mask_vect
      n_feat <- if (is.null(boundary)) 0L else nrow(boundary)
      if (n_feat == 1) {
        split_try <- tryCatch(sf::st_cast(boundary, "POLYGON", warn = FALSE), error = function(e) NULL)
        if (!is.null(split_try) && nrow(split_try) >= 2) {
          boundary <- split_try
          n_feat <- nrow(boundary)
          showNotification(sprintf("Detected a single multi-part boundary — split into %d polygons for batch processing.", n_feat), type = "message", duration = 8)
        }
      }
      if (is.null(boundary) || n_feat < 2) {
        return(showNotification(sprintf("Batch processing needs multiple boundaries — your current boundary has %d feature(s). In Shapefile Extractor, select multiple features WITHOUT checking 'Merge/Dissolve' (confirm the count shown after selecting), send to Clipboard, then re-import here in Step 1 (make sure it's the LAST thing you sent to the Clipboard).", n_feat), type = "error", duration = 12))
      }
      if (!check_rate_limit(rv, "run_batch_stats", cooldown_seconds = 15)) return()

      set_busy(session, "Running batch zonal statistics across all selected boundaries... (app stays usable — feel free to keep working)")
      progress <- shiny::Progress$new(session)
      progress$set(message = "Running batch zonal statistics...", value = NULL)

      feature_name <- input$gee_feature
      dyn_scale <- get_dynamic_scale(boundary)
      start_d <- as.character(input$gee_date[1]); end_d <- as.character(input$gee_date[2])
      agg <- input$gee_agg

      name_col <- if ("shapeName" %in% names(boundary)) "shapeName" else NULL
      # make.unique() guards against duplicate names (e.g. split multi-part polygons all inheriting
      # the parent's shapeName) so every boundary is a distinct, correctly-keyed batch row.
      name_keys <- make.unique(if (!is.null(name_col)) as.character(boundary[[name_col]]) else paste("Boundary", seq_len(nrow(boundary))))
      area_lookup <- setNames(round(as.numeric(sf::st_area(boundary)) / 1e6, 2), name_keys)

      # Each row becomes its own {name, wkt} entry — sent as JSON since a variable-length list
      # of boundaries doesn't fit cleanly into form-encoding.
      boundaries_list <- lapply(seq_len(nrow(boundary)), function(i) {
        list(name = as.character(name_keys[i]), wkt = sf::st_as_text(sf::st_geometry(boundary)[i]))
      })
      
      # 🚀 REFACTOR: uses shared build_compute_service_request() (global.R), with is_json=TRUE
      # since a variable-length boundary list doesn't fit cleanly into form-encoding.
      req <- build_compute_service_request(
        "run-batch",
        list(feature = feature_name, start_date = start_d, end_date = end_d, agg = agg, scale = dyn_scale, boundaries = boundaries_list),
        is_json = TRUE
      )
      
      prom <- httr2::req_perform_promise(req)
      
      prom %...>% (function(resp) {
        # 🚀 REFACTOR: uses shared parse_compute_service_response() (global.R)
        result <- parse_compute_service_response(resp)
        
        # 🚀 REFACTOR (Option B): the per-row building logic (including Category-B extra columns:
        # Total Population, % Vegetated Area, UHI Intensity) previously lived entirely inline here
        # — now extracted to build_batch_results_row() (global.R) so it's independently unit-tested.
        df <- do.call(rbind, lapply(result$results, build_batch_results_row, area_lookup = area_lookup))
        
        rv$batch_results_df <- df
        rv$batch_results_meta <- list(feature = feature_name, units = get_feature_units(feature_name), start_d = start_d, end_d = end_d, agg = agg)
        log_event("INFO", "mod_gee", "batch_stats_succeeded", session_id = session$token,
                  feature = feature_name, n_boundaries = nrow(df))
        showNotification(sprintf("Batch analysis complete for %d boundaries!", nrow(df)), type = "message")
        clear_busy(session)
        progress$close()
      }) %...!% (function(e) {
        log_event("ERROR", "mod_gee", "batch_stats_failed", session_id = session$token,
                  feature = feature_name, error = conditionMessage(e))
        showNotification(paste("Batch Analysis Error:", conditionMessage(e)), type = "error", duration = 10)
        clear_busy(session)
        progress$close()
      })
    })
    
    output$batch_results_table <- renderDT({
      req(rv$batch_results_df)
      meta <- rv$batch_results_meta
      
      # 🚀 REFACTOR: uses shared rename_batch_results_columns() (global.R) — this exact
      # base-columns + extra-columns renaming logic was previously duplicated 3 times (here, CSV
      # download, XLSX download), and had a bug where a fixed-length rename vector silently
      # mismatched once Category B metrics added extra columns.
      display_df <- rename_batch_results_columns(rv$batch_results_df, if (!is.null(meta)) meta$units else "")
      
      dt <- datatable(
        display_df,
        options = list(pageLength = 15, scrollX = TRUE, dom = 'tip'),
        rownames = FALSE,
        caption = if (!is.null(meta)) htmltools::tags$caption(
          style = "caption-side: top; text-align: left; font-size: 13px; color: #26333e; padding-bottom: 8px;",
          htmltools::HTML(sprintf(
            "<b>%s</b> &nbsp;|&nbsp; %s to %s &nbsp;|&nbsp; Aggregation: %s &nbsp;|&nbsp; <i style='color:#5c6b73;'>Std. Deviation shows how much the value varies WITHIN each boundary \u2014 a bigger number means more spread (e.g. hot cities + cool forests mixed together), a smaller number means more uniform. 95%% CI is based on pixel-level standard error, which treats pixels as independent \u2014 an optimistic (narrower) estimate since adjacent pixels are spatially correlated.</i>",
            meta$feature, meta$start_d, meta$end_d, meta$agg
          ))
        ) else NULL
      ) %>% formatStyle(names(display_df)[3], background = styleColorBar(range(display_df[[3]], na.rm = TRUE), '#f3e6de'), backgroundSize = '90% 70%', backgroundRepeat = 'no-repeat', backgroundPosition = 'center')
      
      dt
    }, server = FALSE)
    
    output$download_batch_csv <- downloadHandler(
      filename = function() paste0("Batch_Zonal_Stats_", gsub("[^A-Za-z0-9]", "_", input$gee_feature), "_", Sys.Date(), ".csv"),
      content = function(file) { 
        meta <- rv$batch_results_meta
        # 🚀 REFACTOR: uses shared rename_batch_results_columns() (global.R)
        out_df <- rename_batch_results_columns(rv$batch_results_df, if (!is.null(meta)) meta$units else "")
        write.csv(out_df, file, row.names = FALSE) 
      }
    )
    
    # 🚀 FEATURE: Excel export alongside CSV — same data, formatted as a proper .xlsx with a
    # methodology sheet included, since Excel is often preferred for client-facing deliverables.
    output$download_batch_xlsx <- downloadHandler(
      filename = function() paste0("Batch_Zonal_Stats_", gsub("[^A-Za-z0-9]", "_", input$gee_feature), "_", Sys.Date(), ".xlsx"),
      content = function(file) {
        meta <- rv$batch_results_meta
        # 🚀 REFACTOR: uses shared rename_batch_results_columns() (global.R)
        out_df <- rename_batch_results_columns(rv$batch_results_df, if (!is.null(meta)) meta$units else "")
        
        info_df <- data.frame(
          Field = c("Feature", "Date Range", "Aggregation", "Boundaries", "Generated"),
          Value = c(
            if (!is.null(meta)) meta$feature else input$gee_feature,
            if (!is.null(meta)) paste(meta$start_d, "to", meta$end_d) else "",
            if (!is.null(meta)) meta$agg else "",
            as.character(nrow(out_df)),
            as.character(Sys.Date())
          )
        )
        
        writexl::write_xlsx(list("Zonal Statistics" = out_df, "Analysis Info" = info_df), path = file)
      }
    )
    
    observeEvent(input$add_cart_zonal, {
      if(!is.null(gee_rv$zonal_data)) add_to_workspace(paste0("gee_zonal_", as.integer(Sys.time())), paste("Zonal Stats:", gee_rv$current_feature), 5)
      else showNotification("Run Analytics first.", type="error")
    })
    
    observeEvent(input$add_cart_ts, {
      if(!is.null(gee_rv$hist_data)) add_to_workspace(paste0("gee_ts_", as.integer(Sys.time())), paste("Trend Plot:", gee_rv$current_feature), 5)
      else showNotification("Run Analytics first.", type="error")
    })
    
    observeEvent(input$add_cart_hist, {
      if(!is.null(gee_rv$hist_data)) add_to_workspace(paste0("gee_hist_", as.integer(Sys.time())), paste("Histogram:", gee_rv$current_feature), 5)
      else showNotification("Run Analytics first.", type="error")
    })
    
    output$zonal_stats_tbl <- renderDT({ 
      shiny::validate(shiny::need(gee_rv$zonal_data, "Please run analytics to view the statistics."))
      datatable(
        gee_rv$zonal_data, options=list(dom='t', paging=FALSE), rownames=FALSE,
        caption = htmltools::tags$caption(
          style = "caption-side: bottom; text-align: left; font-size: 11px; color: #5c6b73; padding-top: 8px;",
          "95% CI is based on pixel-level standard error \u2014 since adjacent pixels are spatially correlated rather than independent samples, this is an optimistic (narrower) estimate, not a rigorous geostatistical interval."
        )
      ) %>% formatStyle('Value', color='#6b4c7a', fontWeight='bold') 
    }, server = FALSE)
    
    output$zonal_profile_plot <- renderPlot({ 
      shiny::validate(shiny::need(gee_rv$hist_data, "Please run analytics to generate the profile chart."))
      df <- gee_rv$hist_data
      df <- df[df$Area_SqKm > 0, ] 
      
      ggplot(df, aes(x=as.factor(round(Bin, 2)), y=Area_SqKm)) + 
        geom_bar(stat="identity", fill="#3a6aa0", color="black", width=0.8) + 
        geom_text(aes(label=round(Area_SqKm, 1)), vjust=-0.5, size=3.5, fontface="bold") +
        theme_minimal(base_family="sans") + 
        labs(x = paste(gee_rv$current_feature, "Ranges"), y = "Area Coverage (Sq.Km)") + 
        theme(axis.text.x = element_text(size=10, face="bold", angle=45, hjust=1), axis.title = element_text(size=12, face="bold"), panel.grid.major.x = element_blank()) 
    })
    
    output$histogram_plot <- renderPlot({ 
      shiny::validate(shiny::need(gee_rv$hist_data, "Please run analytics to view the histogram."))
      p <- ggplot(gee_rv$hist_data, aes(x=Bin, y=Count)) + geom_col(fill="#4a83c4", color="black", alpha=0.8) + theme_minimal(base_family="sans") + labs(title=paste("Area Distribution Histogram:", gee_rv$current_feature), subtitle="Identifies the dominant pixel ranges and data skewness across your study area.", x=paste(gee_rv$current_feature, "Range"), y="Number of Pixels") + theme(plot.title=element_text(face="bold", size=16, color="#26333e"), plot.subtitle=element_text(color="#5c6b73", face="italic", size=12), axis.text=element_text(size=12, face="bold"), axis.title=element_text(size=14, face="bold"), legend.position="bottom") 
      if(!is.null(gee_rv$mean_val) && !is.na(gee_rv$mean_val)) { p <- p + geom_vline(aes(xintercept=gee_rv$mean_val, color="Mean"), linetype="dashed", linewidth=1) }
      if(!is.null(gee_rv$med_val) && !is.na(gee_rv$med_val)) { p <- p + geom_vline(aes(xintercept=gee_rv$med_val, color="Median"), linetype="solid", linewidth=1) }
      p <- p + scale_color_manual(name="Statistics", values=c("Mean"="#e74c3c", "Median"="#f1c40f"))
      p
    })
    
    # 🚀 INSIGHTS DASHBOARD: continuous-analysis summary in the feature's own units
    # (Elevation m, Slope degrees, LST °C, ...) with min/mean/median/max/sd, a
    # distribution histogram, and spatial context. Reads the SAME zonal_data /
    # hist_data the Zonal-Statistics and Pixel-Distribution tabs use.
    gee_insights_payload <- reactive({
      insights_payload_continuous(
        gee_rv$zonal_data, gee_rv$hist_data,
        feature = gee_rv$current_feature,
        units   = tryCatch(get_feature_units(gee_rv$current_feature), error = function(e) NULL),
        aoi_sf  = rv$mask_vect)
    })
    insights_drawer_server("gee_insights", gee_insights_payload)

    observeEvent(input$run_temporal, {
      if(is.null(rv$mask_vect)) return(showNotification("Please Fetch a Boundary from the Basket first!", type="error"))
      if(!check_rate_limit(rv, "run_temporal_gee", cooldown_seconds = 15)) return()
      ee_roi <- sf_as_ee(rv$mask_vect)
      
      # 🚀 FIX 1: Immediately reset variables to kill old spinners and force re-validation
      gee_rv$temporal_combined_plot <- NULL
      gee_rv$temporal_grid_years <- NULL
      # 🚀 FIX (Save Gallery to Workspace was dead): gee_rv$grid_urls is read by the ZIP
      # export but was never written, and add_cart_gallery had NO observer at all. Reset here,
      # capture each tile's thumbnail URL during the loop, and wire the button below.
      gee_rv$grid_urls <- NULL
      
      years <- seq(input$temp_start, input$temp_end, by=as.numeric(input$temp_gap))
      if(length(years) > 10) return(showNotification("Too many intervals! Keep it under 10 maps.", type="error"))
      
      old_to <- getOption("timeout")
      options(timeout = 3600) 
      on.exit(options(timeout = old_to)) 
      
      withProgress(message="Generating Scientific Spatiotemporal Grid...", value=0, {
        plot_list <- list()
        grid_urls_acc <- list()  # 🚀 year -> thumbnail URL, for Save-to-Workspace + ZIP export
        mo <- as.integer(input$temp_month)
        dyn_scale <- get_dynamic_scale(rv$mask_vect)
        b <- sf::st_bbox(sf::st_transform(rv$mask_vect, 4326))
        
        vis_cache <- gee_rv$saved_vis[[input$gee_feature]]
        if(is.null(vis_cache)) {
          return(showNotification("Please run 'Generate Map & Statistics' in Step 2 first to configure the color scaling.", type="warning"))
        }
        
        grid_pal <- vis_cache$pal
        fallback_min <- vis_cache$min
        fallback_max <- vis_cache$max
        
        for(i in seq_along(years)) {
          yr <- years[i]
          incProgress(i/length(years), detail=paste("Rendering", month.abb[mo], yr))
          
          s_d <- sprintf("%04d-%02d-01", yr, mo)
          e_d <- as.character(seq(as.Date(s_d), length=2, by="3 months")[2] - 1)
          window_label <- "3-month"
          
          tryCatch({
            cache_key <- digest::digest(list("temporal_grid", input$gee_feature, yr, mo, input$gee_agg, dyn_scale, sf::st_as_text(sf::st_union(rv$mask_vect))))
            if (is.null(gee_rv$temporal_cache)) gee_rv$temporal_cache <- list()
            cached_yr <- gee_rv$temporal_cache[[cache_key]]
            
            if (!is.null(cached_yr) && as.numeric(difftime(Sys.time(), cached_yr$time, units = "mins")) < 15) {
              f_data <- cached_yr$f_data; coverage_pct <- cached_yr$coverage_pct; window_label <- cached_yr$window_label
            } else {
              
              f_data <- get_feature_img(input$gee_feature, s_d, e_d, input$gee_agg, ee_roi, dyn_scale)
              
              get_coverage <- function(img) {
                tryCatch({
                  v <- img$mask()$reduceRegion(reducer = ee$Reducer$mean(), geometry = ee_roi$geometry(), scale = dyn_scale * 2, maxPixels = 1e13, bestEffort = TRUE, tileScale = 16)$getInfo()
                  round(as.numeric(v[[1]]) * 100, 1)
                }, error = function(e) NA_real_)
              }
              
              coverage_pct <- if (!is.null(f_data) && !is.null(f_data$img)) get_coverage(f_data$img) else NA_real_
              if (is.na(coverage_pct)) coverage_pct <- 0
              
              # 🚀 FIX: Auto-widen the window if data is too sparse — common for pre-2020 years
              # where satellite revisit frequency (esp. before Sentinel-2B, launched 2017) was lower.
              if (coverage_pct < 5) {
                e_d_wide <- as.character(seq(as.Date(s_d), length=2, by="6 months")[2] - 1)
                f_data_wide <- tryCatch(get_feature_img(input$gee_feature, s_d, e_d_wide, input$gee_agg, ee_roi, dyn_scale), error = function(e) NULL)
                if (!is.null(f_data_wide) && !is.null(f_data_wide$img)) {
                  coverage_wide <- get_coverage(f_data_wide$img)
                  if (!is.na(coverage_wide) && coverage_wide > coverage_pct) {
                    f_data <- f_data_wide; coverage_pct <- coverage_wide; e_d <- e_d_wide; window_label <- "6-month, widened"
                  }
                }
              }
              
              # 🚀 FIX: Final fallback — try the full calendar year before giving up. Some years/regions
              # (e.g. persistent monsoon cloud cover, or a sparse pre-2000 Landsat-5-only period) genuinely
              # have no clear data in a 6-month window either.
              if (coverage_pct < 5) {
                s_d_full <- sprintf("%04d-01-01", yr)
                e_d_full <- sprintf("%04d-12-31", yr)
                f_data_full <- tryCatch(get_feature_img(input$gee_feature, s_d_full, e_d_full, input$gee_agg, ee_roi, dyn_scale), error = function(e) NULL)
                if (!is.null(f_data_full) && !is.null(f_data_full$img)) {
                  coverage_full <- get_coverage(f_data_full$img)
                  if (!is.na(coverage_full) && coverage_full > coverage_pct) {
                    f_data <- f_data_full; coverage_pct <- coverage_full; s_d <- s_d_full; e_d <- e_d_full; window_label <- "full year, widened"
                  }
                }
              }
              
              gee_rv$temporal_cache[[cache_key]] <- list(f_data = f_data, coverage_pct = coverage_pct, window_label = window_label, time = Sys.time())
            } # end cache-miss else block
            
            if(!is.null(f_data) && !is.null(f_data$img) && coverage_pct >= 5) {
              img <- f_data$img
              
              # DYNAMIC MIN/MAX EXTRACTION USING bestEffort TO PREVENT TIMEOUTS
              range_stats <- tryCatch({
                img$reduceRegion(
                  reducer   = ee$Reducer$percentile(c(2, 50, 98)),
                  geometry  = ee_roi$geometry(),
                  scale     = dyn_scale * 2, 
                  maxPixels = 1e13,
                  bestEffort = TRUE,
                  tileScale = 16
                )$getInfo()
              }, error = function(e) NULL)
              
              get_pctl <- function(sfx) {
                if (is.null(range_stats) || length(range_stats) == 0) return(NA_real_)
                k <- names(range_stats)[grepl(paste0(sfx, "$"), names(range_stats))][1]
                if (is.na(k) || is.null(range_stats[[k]])) NA_real_ else as.numeric(range_stats[[k]])
              }
              
              p2_val  <- get_pctl("p2")
              p98_val <- get_pctl("p98")
              
              year_min <- if (!is.na(p2_val)) as.numeric(p2_val) else as.numeric(fallback_min)
              year_max <- if (!is.na(p98_val)) as.numeric(p98_val) else as.numeric(fallback_max)
              
              if(is.na(year_min)) year_min <- 0
              if(is.na(year_max)) year_max <- 1
              if (year_min >= year_max) year_max <- year_min + 0.1
              
              range_txt <- if (!is.na(p2_val) && !is.na(p98_val)) {
                sprintf("Auto-scaled %.1f to %.1f (%s window)", year_min, year_max, window_label)
              } else {
                "Data range unavailable"
              }
              
              vis_img <- img$visualize(min=year_min, max=year_max, palette=grid_pal)
              thumb_dim <- get_thumb_dimensions(rv$mask_vect)
              
              url <- tryCatch({
                vis_img$getThumbURL(list(region = ee_roi$geometry(), dimensions = thumb_dim, format = "png"))
              }, error = function(e) {
                vis_img$getThumbURL(list(region = ee_roi$geometry(), dimensions = max(256, thumb_dim / 2), format = "png"))
              })
              
              grid_urls_acc[[as.character(yr)]] <- url  # 🚀 capture for Save-to-Workspace + export
              tmp_png <- tempfile(fileext = ".png")
              download.file(url, tmp_png, mode = "wb", quiet = TRUE)
              img_arr <- png::readPNG(tmp_png)
              
              # 🚀 REFACTOR: uses shared build_continuous_map_plot() (global.R) as the base plot
              # (raster overlay + boundary outline + dummy-points colorbar trick), matching the same
              # pattern used in Publication Map — the additional scale-bar/north-arrow/title-with-
              # subtitle styling below is specific to this gallery-tile use-case (small multi-panel
              # tiles, not a single publication map) so stays inline rather than going through
              # apply_cartography_styling(), which doesn't support a subtitle.
              p <- build_continuous_map_plot(img_arr, b, rv$mask_vect, year_min, year_max, grid_pal, legend_title = gsub("\\(.*\\)", "", input$gee_feature), linewidth = 0.8) +
                coord_sf(xlim = c(as.numeric(b["xmin"]), as.numeric(b["xmax"])), ylim = c(as.numeric(b["ymin"]), as.numeric(b["ymax"])), expand = FALSE, crs = 4326, default_crs = 4326) +
                ggspatial::annotation_scale(location = "bl", style = "bar", text_col="black") +
                ggspatial::annotation_north_arrow(location = "tr", which_north = "true", style = ggspatial::north_arrow_fancy_orienteering()) +
                theme_bw() +
                theme(
                  axis.text.x = element_text(size = 8, color="black", angle = 45, hjust=1), 
                  axis.text.y = element_text(size = 8, color="black"),
                  axis.title = element_blank(), 
                  plot.margin = margin(2, 2, 2, 2),
                  plot.title = element_text(face="bold", hjust=0.5),
                  plot.subtitle = element_text(size=8, color="#5c6b73", hjust=0.5),
                  legend.position = "right",
                  legend.title = element_text(size=9, face="bold"),
                  legend.text = element_text(size=8),
                  legend.background = element_rect(fill = alpha("white", 0.8), color = "gray80", linewidth = 0.5)
                ) +
                ggtitle(
                  paste(gsub("\\(.*\\)", "", input$gee_feature), "-", month.abb[mo], yr),
                  subtitle = paste0(coverage_pct, "% clear pixels | ", range_txt)
                )
              
              plot_list[[as.character(yr)]] <- p
              map_name <- paste(input$gee_feature, "-", month.abb[mo], yr)
              gee_rv$saved_images[[map_name]] <- list(type = "simple", feature = input$gee_feature, start_d = s_d, end_d = e_d, agg = input$gee_agg, scale = dyn_scale, roi_wkt = sf::st_as_text(sf::st_union(rv$mask_vect)))
              rv$available_maps <- unique(c(rv$available_maps, map_name))
            } else {
              # 🚀 FIX: Show a VISIBLE placeholder instead of silently vanishing, so missing years are explained, not just absent
              p <- ggplot() +
                annotate("text", x = 0.5, y = 0.5, label = paste0("No clear imagery available\nfor ", month.abb[mo], " ", yr, "\n(only ", coverage_pct, "% cloud-free coverage)"), 
                         size = 3.8, color = "#5c6b73", hjust = 0.5, vjust = 0.5) +
                xlim(0, 1) + ylim(0, 1) +
                theme_void() +
                theme(plot.background = element_rect(fill = "#f7f6f2", color = "#d8d4c8"), plot.margin = margin(2,2,2,2)) +
                ggtitle(paste(gsub("\\(.*\\)", "", input$gee_feature), "-", month.abb[mo], yr))
              plot_list[[as.character(yr)]] <- p
            }
          }, error=function(e){ 
            # Skip empty images silently so grid continues for available years
          })
        }
        
        if(length(plot_list) > 0) { 
          gee_rv$temporal_combined_plot <- cowplot::plot_grid(plotlist = plot_list, ncol = 2, align = "hv")
          gee_rv$temporal_grid_years <- names(plot_list)
          gee_rv$grid_urls <- grid_urls_acc  # 🚀 now Save Gallery to Workspace + ZIP export work
          gee_rv$temporal_feature <- input$gee_feature
          gee_rv$temporal_month <- mo
          showNotification("Scientific Grid Generated Successfully!", type="message") 
        } else { 
          gee_rv$temporal_combined_plot <- NULL
          showNotification("No data found for any of the selected years.", type="error") 
        }
      })
    })

    # 🚀 FIX: "Save Gallery to Workspace" (add_cart_gallery) previously had NO observer,
    # so the button did nothing. Wire it now — guard on the actual generated-grid state,
    # and register a `gee_gallery_` item so the ZIP export (which downloads gee_rv$grid_urls)
    # picks it up.
    observeEvent(input$add_cart_gallery, {
      if (is.null(gee_rv$temporal_combined_plot) || length(gee_rv$grid_urls) == 0) {
        return(showNotification("Generate the Spatiotemporal Grid first.", type = "error"))
      }
      add_to_workspace(paste0("gee_gallery_", as.integer(Sys.time())), "GEE Spatiotemporal Gallery Images (.png)", 0.50)
    })

    output$temporal_grid_ui <- renderUI({
      shiny::validate(shiny::need(gee_rv$temporal_combined_plot, "Please select parameters and generate the temporal gallery."))
      n_years <- length(gee_rv$temporal_grid_years)
      plot_h <- max(500, ceiling(n_years / 2) * 450)
      plotOutput(ns("gee_temporal_grid_plot"), height = paste0(plot_h, "px"))
    })
    
    output$gee_temporal_grid_plot <- renderPlot({
      shiny::validate(shiny::need(gee_rv$temporal_combined_plot, "Please select parameters and generate the temporal gallery."))
      gee_rv$temporal_combined_plot
    })

    # =====================================================================
    # SEQUENTIAL PIPELINE  (isolated — nothing below is reached unless the
    # user interacts with the "Sequential Pipeline" tab. The existing tabs
    # and their observers above are completely independent of this block.)
    # =====================================================================
    pipe_steps     <- reactiveVal(character(0))                 # ordered tool keys
    pipeline_state <- reactiveValues(current_roi_mask = NULL,   # <-- the universal state bus
                                     status = NULL, steps = list(), ran_at = NULL,
                                     final_plot = NULL, final_caption = NULL, final_explain = NULL,
                                     dyn_scale = NULL, thumb_dim = NULL, area_km2 = NULL)   # captured run parameters (for the provenance table)

    # Keep the headline-layer / correlation-response selectors in sync with the sequence being
    # built. Choices = the current sequence's layers (excluding the Trend & Correlation special
    # steps, which are never a headline or a response). "Auto" (empty value) preserves the default
    # Coral > Benthic > first behaviour, so existing runs are unaffected.
    observeEvent(pipe_steps(), {
      cat0 <- tryCatch(gf_pipe_catalog(), error = function(e) list())
      keys <- Filter(function(k) { e <- cat0[[k]]
        !is.null(e) && !identical(e$role, "trend") && !identical(e$feature, "__correl__") }, pipe_steps())
      labs <- unique(vapply(keys, function(k) cat0[[k]]$label %||% k, character(1)))
      ch   <- c("Auto (recommended)" = "", stats::setNames(labs, labs))
      updateSelectInput(session, "pipe_headline", choices = ch, selected = isolate(input$pipe_headline) %||% "")
      updateSelectInput(session, "pipe_response", choices = ch, selected = isolate(input$pipe_response) %||% "")
    }, ignoreNULL = FALSE)

    # ---- Catalog chips (click to add) ----
    output$pipeline_catalog <- renderUI({
      catalog <- gf_pipe_catalog()
      keys <- names(catalog)
      cats <- unique(vapply(keys, function(k) catalog[[k]]$cat, character(1)))
      tagList(lapply(cats, function(grp) {
        kk <- keys[vapply(keys, function(k) catalog[[k]]$cat == grp, logical(1))]
        div(style = "margin-bottom:9px;",
            span(grp, style = "font-size:10px;text-transform:uppercase;letter-spacing:.5px;color:#8a97a0;font-weight:700;display:block;margin-bottom:2px;"),
            lapply(kk, function(k) tags$span(class = "gf-pipe-chip", title = catalog[[k]]$hint, catalog[[k]]$label,
                   onclick = sprintf("Shiny.setInputValue('%s','%s',{priority:'event'})", ns("pipe_add"), k)))
        )
      }))
    })

    observeEvent(input$pipe_add, {
      k <- input$pipe_add
      catalog <- gf_pipe_catalog()
      if (is.null(k) || !(k %in% names(catalog))) return()
      cur <- pipe_steps()
      if (k %in% cur) { showNotification(sprintf("%s is already in the sequence.", catalog[[k]]$label), type = "warning", duration = 3); return() }
      pipe_steps(c(cur, k))
    })
    observeEvent(input$pipe_remove, { pipe_steps(setdiff(pipe_steps(), input$pipe_remove)) })
    observeEvent(input$pipe_clear,  { pipe_steps(character(0)); pipeline_state$status <- NULL; pipeline_state$steps <- list(); pipeline_state$final_plot <- NULL; pipeline_state$final_caption <- NULL; pipeline_state$final_explain <- NULL })

    # Drag-reorder: SortableJS reports the DOM order; sync it back (guarded so
    # the render->reorder->render cycle can't loop).
    observeEvent(input$pipeline_order, {
      ord <- input$pipeline_order; cur <- pipe_steps()
      ord <- ord[ord %in% cur]
      if (length(ord) == length(cur) && !identical(ord, cur)) pipe_steps(ord)
    }, ignoreInit = TRUE)

    # ---- The ordered sequence (draggable) ----
    output$pipeline_builder <- renderUI({
      ks <- pipe_steps()
      if (!length(ks)) return(div(class = "gf-pipe-empty", "No steps yet — click tools from the catalog to add them here."))
      catalog <- gf_pipe_catalog()
      div(class = "gf-pipe-seq",
        lapply(seq_along(ks), function(i) {
          k <- ks[i]; it <- catalog[[k]]
          div(class = "gf-pipe-step", `data-tool` = k, `data-role` = it$role,
              span(class = "gf-pipe-handle", HTML("&#8942;&#8942;")),
              span(class = "gf-pipe-num", i),
              div(style = "flex:1;min-width:0;",
                  div(strong(it$label), span(it$role, class = "gf-pipe-role", style = "margin-left:6px;")),
                  div(it$hint, style = "font-size:11px;color:#5c6b73;line-height:1.3;")),
              tags$button(HTML("&times;"), class = "gf-pipe-rm", title = "Remove",
                          onclick = sprintf("Shiny.setInputValue('%s','%s',{priority:'event'})", ns("pipe_remove"), k))
          )
        })
      )
    })

    # ---- Smart Sequence Assistant ----
    output$pipeline_assistant <- renderUI({
      msgs <- gf_pipe_assistant(pipe_steps())
      tagList(lapply(msgs, function(m) {
        cfg <- switch(m$type,
          warn      = list(bd = "#8b3a2b", bg = "#f5e6e3", ic = "&#9888;"),
          recommend = list(bd = "#3a6aa0", bg = "#e8eff6", ic = "&#128161;"),
                      list(bd = "#45936f", bg = "#e6f3ec", ic = "&#10003;"))
        div(style = sprintf("border-left:3px solid %s;background:%s;padding:8px 10px;border-radius:4px;margin-bottom:6px;font-size:12px;color:#26333e;line-height:1.4;", cfg$bd, cfg$bg),
            HTML(sprintf("%s&nbsp; %s", cfg$ic, m$text)))
      }))
    })

    # ---- Runner: execute the sequence, threading the state bus ----
    observeEvent(input$run_pipeline, {
      ks <- pipe_steps()
      if (!length(ks)) return(showNotification("Add at least one tool to the sequence first.", type = "error"))
      if (is.null(rv$mask_vect)) return(log_err("No boundary is set.", "Set a target area in Step 1 (Data Source) in the sidebar, then run the sequence."))

      ee_roi <- tryCatch(sf_as_ee(rv$mask_vect), error = function(e) NULL)
      if (is.null(ee_roi)) return(log_err("Could not build the analysis region from your boundary.", "Re-set the boundary in Step 1 and try again."))

      dyn_scale <- tryCatch(get_dynamic_scale(rv$mask_vect), error = function(e) 30)
      # Pipeline has its OWN date range + compositing (input$pipe_date / input$pipe_agg),
      # fully independent of the sidebar. Fall back to the sidebar only if the pipeline
      # inputs somehow aren't rendered yet, so an old session can't hard-error.
      p_dates <- input$pipe_date %||% input$gee_date
      s_d <- as.character(p_dates[1]); e_d <- as.character(p_dates[2])
      if (is.na(s_d) || is.na(e_d) || !nzchar(s_d) || !nzchar(e_d) || as.Date(e_d) <= as.Date(s_d))
        return(log_err("Pick a valid pipeline date range.", "In the 'Pipeline date range & compositing' box, set a start date that is earlier than the end date."))
      agg <- input$pipe_agg %||% input$gee_agg %||% "Median"
      thumb_dim <- tryCatch(get_thumb_dimensions(rv$mask_vect), error = function(e) 512)
      pipeline_state$dyn_scale <- dyn_scale; pipeline_state$thumb_dim <- thumb_dim   # for the provenance table
      # Boundary area for the header + provenance (the single-analysis payload isn't populated
      # on a pipeline run, so compute it straight from the boundary geometry).
      pipeline_state$area_km2 <- tryCatch(sum(as.numeric(sf::st_area(rv$mask_vect)), na.rm = TRUE) / 1e6,
                                          error = function(e) NA_real_)

      pipeline_state$current_roi_mask <- NULL   # fresh run resets the bus
      gee_rv$gee_class_insights <- NULL          # fresh run resets class insights
      gee_rv$trend_summary <- NULL               # fresh run resets trend summary
      gee_rv$aca_summary <- NULL                 # fresh run resets Allen Coral Atlas summary
      gee_rv$correl_series <- NULL               # fresh run resets cross-indicator correlation
      steps <- list(); last_single <- NULL      # last_single = the terminal single-band layer -> final composite
      # last_trendable = how to REBUILD the most recent analysis for any year, so
      # Trend analyses THAT layer (e.g. Coral Health), not the sidebar feature.
      last_trendable <- NULL
      trendables <- list()   # ALL indicators added to the pipeline (label -> rebuild-per-year), for the
                             # generic Cross-Indicator Correlation step. Order preserved = pipeline order.
      singles <- list()      # every mappable index layer produced (label -> last_single), so the report's
                             # HEADLINE layer is a principled pick, not just whatever ran last.
      catalog <- gf_pipe_catalog()

      thumb_of <- function(img, pal, mn, mx) tryCatch({
        vis <- img$visualize(min = mn, max = mx, palette = pal)
        if (!is.null(pipeline_state$current_roi_mask)) vis <- vis$updateMask(pipeline_state$current_roi_mask)
        vis$getThumbURL(list(region = ee_roi$geometry(), dimensions = thumb_dim, format = "png"))
      }, error = function(e) NULL)
      # Insight: mean of a layer over the region (respecting the active bus mask).
      mean_of <- function(img) tryCatch({
        im <- if (!is.null(pipeline_state$current_roi_mask)) img$updateMask(pipeline_state$current_roi_mask) else img
        v  <- im$reduceRegion(reducer = ee$Reducer$mean(), geometry = ee_roi$geometry(),
                              scale = dyn_scale * 2, maxPixels = 1e12, bestEffort = TRUE, tileScale = 16)$getInfo()
        if (length(v) && !is.null(v[[1]])) round(as.numeric(v[[1]]), 3) else NA_real_
      }, error = function(e) NA_real_)
      cite_of <- function(feat) tryCatch(get_feature_citation(feat), error = function(e) NULL)
      # Bug-3 fix: render any generated mask as a HIGH-CONTRAST neon-cyan overlay
      # (selfMask drops the 0s so only the mask area glows; rest stays transparent).
      mask_thumb <- function(mask_img) tryCatch(
        mask_img$selfMask()$visualize(palette = c("#00e5ff"), min = 0, max = 1)$
          getThumbURL(list(region = ee_roi$geometry(), dimensions = thumb_dim, format = "png")),
        error = function(e) NULL)

      withProgress(message = "Running sequential pipeline…", value = 0, {
        for (i in seq_along(ks)) {
          k <- ks[i]; it <- catalog[[k]]
          if (is.null(it)) {
            steps[[i]] <- list(step = i, label = k, role = "?", status = "skipped",
                               note = "Unknown tool key.", url = NULL, legend = NULL, citation = NULL, mean = NA_real_)
            next
          }
          incProgress(1 / length(ks), detail = sprintf("Step %d/%d — %s", i, length(ks), it$label))
          st <- "computed"; note <- ""; url <- NULL; legend <- NULL; citation <- NULL; mean_val <- NA_real_; mask_url <- NULL
          tryCatch({
            if (identical(it$feature, "__benthic__")) {
              bs  <- build_benthic_stack(s_d, e_d, ee_roi, max_cloud = 20, agg = agg)
              s2  <- get_s2_sr_collection(s_d, e_d, ee_roi, 20)
              wm  <- s2$median()$normalizedDifference(c("B3", "B8"))$gt(0)   # NDWI>0 water
              pipeline_state$current_roi_mask <- wm
              mask_url <- mask_thumb(wm)
              note <- "Benthic stack built; water/reef mask published to the state bus."
              url  <- tryCatch(bs$img$select(c("B4", "B3", "B2"))$visualize(min = 0, max = 0.15)$updateMask(wm)$
                                 getThumbURL(list(region = ee_roi$geometry(), dimensions = thumb_dim, format = "png")),
                               error = function(e) NULL)
              legend   <- list(type = "composite", text = "True-colour composite (B4/B3/B2) over reef/water pixels only")
              citation <- "Sentinel-2 SR Harmonized (COPERNICUS/S2_SR_HARMONIZED)"
              last_single <- list(img = bs$img$select("DII_B2_B3"), pal = c("#3b0f0f", "#8b3a2b", "#d9a441", "#45936f"), mn = -2, mx = 2, label = "Benthic DII (B2-B3)")
              singles[["Benthic DII (B2-B3)"]] <- last_single
              last_trendable <- list(label = "Benthic (DII)", units = "DII", build = function(y) {
                b2 <- tryCatch(build_benthic_stack(sprintf("%04d-01-01", y), sprintf("%04d-12-31", y), ee_roi, max_cloud = 90, agg = "Median"), error = function(e) NULL)
                if (!is.null(b2)) b2$img$select("DII_B2_B3") else NULL })
              trendables[["Benthic (DII)"]] <- c(last_trendable, list(mask = pipeline_state$current_roi_mask))
            } else if (identical(it$feature, "__aca__")) {
              # Allen Coral Atlas — the authoritative, expert- & field-validated benthic
              # habitat map. Answers "which pixels are actually Coral/Algae" (vs our own
              # index which only measures bottom brightness). Categorical, so it does NOT
              # feed the continuous composite; it renders its own map + a per-class area
              # table (incl. Coral/Algae km2) shown in Insights.
              aca_b <- tryCatch(ee$Image("ACA/reef_habitat/v2_0")$select("benthic")$clip(ee_roi), error = function(e) NULL)
              if (is.null(aca_b)) stop("Could not load the Allen Coral Atlas (ACA/reef_habitat/v2_0).")
              meta <- tryCatch(list(
                vals = suppressWarnings(as.integer(unlist(aca_b$get("benthic_class_values")$getInfo()))),
                nms  = unlist(aca_b$get("benthic_class_names")$getInfo()),
                pal  = unlist(aca_b$get("benthic_class_palette")$getInfo())), error = function(e) NULL)
              if (is.null(meta) || !length(meta$vals) || !length(meta$nms)) {
                # documented ACA v2.0 benthic scheme (used only if band metadata can't be read)
                meta <- list(vals = c(11L, 12L, 13L, 14L, 15L, 18L),
                             nms  = c("Sand", "Rubble", "Rock", "Microalgal Mats", "Seagrass", "Coral/Algae"),
                             pal  = c("#ffffbe", "#e0d05e", "#b19c3a", "#668438", "#00a884", "#ff6161"))
              }
              short_nm <- function(x) { x <- trimws(as.character(x)); ifelse(is.na(x), NA_character_, trimws(sub(" - .*$", "", x))) }
              url <- tryCatch(aca_b$visualize(min = min(meta$vals), max = max(meta$vals), palette = meta$pal)$
                        getThumbURL(list(region = ee_roi$geometry(), dimensions = thumb_dim, format = "png")),
                        error = function(e) NULL)
              # Publish the Coral/Algae mask to the state bus so DOWNSTREAM steps (Coral Health,
              # Trend, indices) run over ACA-confirmed coral ONLY. ACA is a static ~2020 baseline,
              # so this restricts your time-specific analysis to pixels ACA mapped as coral.
              coral_vals <- meta$vals[grepl("coral", meta$nms, ignore.case = TRUE)]
              if (length(coral_vals)) {
                cmask <- aca_b$remap(as.list(coral_vals), as.list(rep(1L, length(coral_vals))), 0L)$rename("coral")
                pipeline_state$current_roi_mask <- cmask$selfMask()
                mask_url <- mask_thumb(cmask)
              }
              ag <- tryCatch(get_area_by_class_groups(aca_b$rename("classification"), ee_roi, max(10, dyn_scale)), error = function(e) NULL)
              aca_df <- NULL; coral_km2 <- NA_real_; coral_pct <- NA_real_
              if (!is.null(ag) && nrow(ag) > 0) {
                tot <- sum(ag$Area_sqm, na.rm = TRUE)
                nm  <- short_nm(meta$nms[match(ag$Class_ID, meta$vals)])
                nm[is.na(nm)] <- paste0("Class ", ag$Class_ID[is.na(nm)])
                col <- meta$pal[match(ag$Class_ID, meta$vals)]; col[is.na(col)] <- "#9aa7ae"
                aca_df <- data.frame(Class = nm, Area_km2 = round(ag$Area_sqm / 1e6, 3),
                                     Area_ha = round(ag$Area_sqm / 1e4, 1),
                                     Pct = round(100 * ag$Area_sqm / tot, 1), Color = col,
                                     stringsAsFactors = FALSE)
                aca_df <- aca_df[order(-aca_df$Area_km2), ]
                ci <- grep("coral", aca_df$Class, ignore.case = TRUE)
                if (length(ci)) { coral_km2 <- sum(aca_df$Area_km2[ci]); coral_pct <- sum(aca_df$Pct[ci]) }
              }
              gee_rv$aca_summary <- list(classes = aca_df, coral_km2 = coral_km2, coral_pct = coral_pct)
              note <- if (isTRUE(is.finite(coral_km2)))
                        sprintf("Allen Coral Atlas: Coral/Algae covers %.3f km2 (%.1f%%). Its coral mask is now published — any Coral Health / Trend / index step AFTER this runs over ACA coral only.", coral_km2, coral_pct)
                      else "Allen Coral Atlas loaded, but no mapped benthic habitat here (ACA maps shallow tropical reefs only)."
              legend   <- list(type = "composite", text = "Allen Coral Atlas benthic habitat (Coral/Algae, Sand, Rubble, Rock, Seagrass, Microalgal Mats) — legend & areas in Insights.")
              citation <- "Allen Coral Atlas (Earth Engine: ACA/reef_habitat/v2_0)."
            } else if (identical(it$feature, "__coral__")) {
              # Coral Health = the blue-green bottom index (ln B2 - ln B3) over optically-shallow
              # reef (deep water excluded). This is the SAME index the Trend step analyses, so the
              # map, the Area-by-class table and the trend are ONE consistent quantity (matching
              # range & sign). PERF: build ONE benthic stack and derive BOTH the shallow-reef mask
              # and the index from it (previously coral_health_layer built a second stack — ~2x slower).
              bs_c   <- build_benthic_stack(s_d, e_d, ee_roi, max_cloud = 90, agg = agg)
              greenc <- bs_c$img$select("B3")
              shallow <- greenc$gt(0.015)                       # deep clear water -> ~0, so this drops it
              reef   <- if (!is.null(pipeline_state$current_roi_mask)) shallow$And(pipeline_state$current_roi_mask) else shallow
              pipeline_state$current_roi_mask <- reef           # downstream = shallow reef (∩ any upstream ACA/water mask)
              mask_url <- mask_thumb(reef)
              b2c  <- bs_c$img$select("B2"); b3c <- bs_c$img$select("B3")
              posc <- b2c$gt(0)$And(b3c$gt(0))
              # Coral Health = ln(B3) - ln(B2): oriented so HIGHER = darker, light-absorbing
              # bottom (coral / algae / seagrass) and LOWER = bright sand/rubble. i.e. a rising
              # index = more coral-like cover, so "Coral Health" reads the intuitive way.
              bgi  <- b3c$updateMask(posc)$log()$subtract(b2c$updateMask(posc)$log())$rename("BGI")$updateMask(reef)
              # data-driven 2/98 percentile stretch so the map shows real contrast.
              pct  <- tryCatch(bgi$reduceRegion(reducer = ee$Reducer$percentile(list(2, 98)), geometry = ee_roi$geometry(),
                        scale = max(10, dyn_scale), maxPixels = 1e12, bestEffort = TRUE, tileScale = 16)$getInfo(), error = function(e) NULL)
              vv   <- tryCatch(sort(as.numeric(unlist(pct))), error = function(e) numeric(0))
              if (length(vv) >= 2 && is.finite(vv[1]) && is.finite(vv[length(vv)]) && vv[1] < vv[length(vv)]) {
                cmn <- vv[1]; cmx <- vv[length(vv)]
              } else { cmn <- -0.5; cmx <- 0.5 }
              pal <- c("#f4ecd0", "#d9b382", "#c1553b", "#8c2d19")  # low = bright sand -> high = dark coral/algae bottom
              note <- sprintf("Coral Health index ln(B3) − ln(B2) on optically-shallow reef only (deep water excluded); oriented so HIGHER = more coral/algae cover (darker bottom), LOWER = bright sand/rubble; stretched %.2f to %.2f. This is the same index the Trend step analyses.", cmn, cmx)
              url  <- tryCatch(bgi$visualize(min = cmn, max = cmx, palette = pal)$
                                 getThumbURL(list(region = ee_roi$geometry(), dimensions = thumb_dim, format = "png")),
                               error = function(e) NULL)
              mean_val <- mean_of(bgi)
              legend   <- list(type = "gradient", pal = pal, min = round(cmn, 2), max = round(cmx, 2),
                               unit = "Low = bright sand / rubble  →  High = darker coral / algae / seagrass")
              citation <- "Sentinel-2 SR Harmonized (COPERNICUS/S2_SR_HARMONIZED)"
              last_single <- list(img = bgi, pal = pal, mn = cmn, mx = cmx, label = "Coral Health (blue-green bottom index)")
              singles[["Coral Health (blue-green bottom index)"]] <- last_single
              last_trendable <- list(label = "Coral Health", units = "bottom index", build = function(y) {
                # Trend a ROBUST blue-green bottom index over water, NOT the Lyzenga DII.
                # Why not DII: its attenuation ratio (ki/kj) is estimated from a covariance
                # reduceRegion inside depth_invariant_index(); when that covariance is degenerate
                # for the ROI the ratio goes non-finite and masks the ENTIRE DII band -> every
                # year reports "no valid pixels" (while the RGB benthic map still renders on
                # B2/B3/B4, so benthic mapping looks fine). Instead we take the deglinted,
                # water-masked Blue and Green bands and form ln(B2)-ln(B3): the same bottom-type
                # contrast (bright sand high, dark coral/algae low), but with NO fragile covariance
                # step, so it has valid pixels wherever there is shallow water.
                # Relaxed image-level cloud filter (max_cloud=90): keep almost all scenes and let
                # per-pixel SCL masking + median compositing remove clouds instead of dropping years.
                bs <- tryCatch(build_benthic_stack(sprintf("%04d-01-01", y), sprintf("%04d-12-31", y),
                                                   ee_roi, max_cloud = 90, agg = "Median"),
                               error = function(e) NULL)
                if (is.null(bs)) return(NULL)
                b2 <- bs$img$select("B2"); b3 <- bs$img$select("B3")
                # keep only positive reflectance (log domain) before the ratio
                pos <- b2$gt(0)$And(b3$gt(0))
                # ln(B3)-ln(B2): SAME orientation as the display index above (higher = coral/algae).
                b3$updateMask(pos)$log()$subtract(b2$updateMask(pos)$log())$rename("BGI") })
              trendables[["Coral Health"]] <- c(last_trendable, list(mask = pipeline_state$current_roi_mask))
            } else if (identical(it$feature, "__coral_ls__")) {
              # Coral Health on LANDSAT (30 m, 1984+) — SAME ln(green)-ln(blue) bottom index as the
              # Sentinel-2 Coral Health step, but from the harmonized Landsat 5/7/8/9 archive so the
              # coral record spans the FULL timeline (incl. the pre-2018 closure baseline S2 can't reach).
              # Landsat C2 L2 SR are scaled ints -> real reflectance = DN*0.0000275 - 0.2 (applied below).
              lcol <- get_harmonized_landsat_collection(s_d, e_d, ee_roi)
              if (is.null(lcol) || as.integer(tryCatch(lcol$size()$getInfo(), error = function(e) 0L)) == 0L)
                stop("No Landsat scenes for this area / date range.")
              lcomp <- switch(agg, "Mean" = lcol$mean(), "Max" = lcol$max(), "Min" = lcol$min(), lcol$median())$clip(ee_roi)
              lref  <- lcomp$select(c("SR_B2", "SR_B3"))$multiply(0.0000275)$add(-0.2)   # -> reflectance (B2 blue, B3 green)
              lblue <- lref$select("SR_B2"); lgreen <- lref$select("SR_B3")
              shallow <- lgreen$gt(0.015)                                # deep clear water -> ~0, drops it
              reef   <- if (!is.null(pipeline_state$current_roi_mask)) shallow$And(pipeline_state$current_roi_mask) else shallow
              pipeline_state$current_roi_mask <- reef                    # downstream = shallow reef (n any upstream ACA/water mask)
              mask_url <- mask_thumb(reef)
              posc <- lblue$gt(0)$And(lgreen$gt(0))
              bgi  <- lgreen$updateMask(posc)$log()$subtract(lblue$updateMask(posc)$log())$rename("BGI")$updateMask(reef)
              pct  <- tryCatch(bgi$reduceRegion(reducer = ee$Reducer$percentile(list(2, 98)), geometry = ee_roi$geometry(),
                        scale = max(30, dyn_scale), maxPixels = 1e12, bestEffort = TRUE, tileScale = 16)$getInfo(), error = function(e) NULL)
              vv   <- tryCatch(sort(as.numeric(unlist(pct))), error = function(e) numeric(0))
              if (length(vv) >= 2 && is.finite(vv[1]) && is.finite(vv[length(vv)]) && vv[1] < vv[length(vv)]) { cmn <- vv[1]; cmx <- vv[length(vv)] } else { cmn <- -0.5; cmx <- 0.5 }
              pal  <- c("#f4ecd0", "#d9b382", "#c1553b", "#8c2d19")       # low = bright sand -> high = dark coral/algae
              note <- sprintf("Coral Health on Landsat (30 m): ln(green) - ln(blue) on optically-shallow reef; oriented so HIGHER = more coral/algae cover (darker bottom), LOWER = bright sand/rubble; stretched %.2f to %.2f. Built from the harmonized Landsat 5/7/8/9 archive (1984+) so the record spans the FULL timeline, incl. pre-2018. Coarser than the Sentinel-2 version and NOT sun-glint corrected -> treat as a screening long-record index, and note small cross-sensor (TM/ETM+/OLI) offsets.", cmn, cmx)
              url  <- tryCatch(bgi$visualize(min = cmn, max = cmx, palette = pal)$
                                 getThumbURL(list(region = ee_roi$geometry(), dimensions = thumb_dim, format = "png")),
                               error = function(e) NULL)
              mean_val <- mean_of(bgi)
              legend   <- list(type = "gradient", pal = pal, min = round(cmn, 2), max = round(cmx, 2),
                               unit = "Low = bright sand / rubble  ->  High = darker coral / algae / seagrass (Landsat 30 m)")
              citation <- "USGS Landsat Collection 2 Level-2 Science Products (LC08 / LC09 / LE07 / LT05)"
              last_single <- list(img = bgi, pal = pal, mn = cmn, mx = cmx, label = "Coral Health (Landsat, long-record)")
              singles[["Coral Health (Landsat, long-record)"]] <- last_single
              last_trendable <- list(label = "Coral Health (Landsat, long-record)", units = "bottom index", build = function(y) {
                lc <- get_harmonized_landsat_collection(sprintf("%04d-01-01", y), sprintf("%04d-12-31", y), ee_roi)
                if (is.null(lc) || as.integer(tryCatch(lc$size()$getInfo(), error = function(e) 0L)) == 0L) return(NULL)
                lm <- lc$median()$clip(ee_roi)$select(c("SR_B2", "SR_B3"))$multiply(0.0000275)$add(-0.2)
                bl <- lm$select("SR_B2"); gr <- lm$select("SR_B3")
                pp <- bl$gt(0)$And(gr$gt(0))
                gr$updateMask(pp)$log()$subtract(bl$updateMask(pp)$log())$rename("BGI") })
              trendables[["Coral Health (Landsat, long-record)"]] <- c(last_trendable, list(mask = pipeline_state$current_roi_mask))
            } else if (identical(it$feature, "__coralalgae__")) {
              # Heuristic Coral-vs-Algae discriminator (SCREENING, not ground truth): live coral is
              # temporally STABLE while macroalgae cover fluctuates seasonally / inter-annually, so a LOW
              # temporal std-dev of the blue-green bottom index -> coral-likely, HIGH -> algae-likely.
              # Computed over the upstream coral/reef mask only, from the Sentinel-2 time series.
              caCol <- ee$ImageCollection("COPERNICUS/S2_SR_HARMONIZED")$filterBounds(ee_roi)$filterDate(s_d, e_d)$map(mask_s2_clouds)
              if (as.integer(tryCatch(caCol$size()$getInfo(), error = function(e) 0L)) < 4L)
                stop("Coral-vs-Algae heuristic needs several Sentinel-2 scenes across the date range (too few here).")
              caBgi <- caCol$map(function(im) { b2 <- im$select("B2"); b3 <- im$select("B3"); pos <- b2$gt(0)$And(b3$gt(0)); b3$updateMask(pos)$log()$subtract(b2$updateMask(pos)$log())$rename("BGI") })
              sdimg <- caBgi$reduce(ee$Reducer$stdDev())$rename("BGI_sd")
              if (!is.null(pipeline_state$current_roi_mask)) sdimg <- sdimg$updateMask(pipeline_state$current_roi_mask)
              medv <- tryCatch(sdimg$reduceRegion(reducer = ee$Reducer$median(), geometry = ee_roi$geometry(), scale = max(10, dyn_scale), maxPixels = 1e12, bestEffort = TRUE, tileScale = 16)$getInfo(), error = function(e) NULL)
              thr  <- suppressWarnings(as.numeric(unlist(medv)[1])); if (!isTRUE(is.finite(thr))) thr <- 0.1
              # CONTINUOUS variability index (NOT a forced binary split, which at the median would always
              # read ~50/50): higher temporal std-dev of the bottom index = more seasonal change = algae-
              # likely; lower = stable = coral-likely.
              caStr <- tryCatch(sort(as.numeric(unlist(sdimg$reduceRegion(reducer = ee$Reducer$percentile(list(95)), geometry = ee_roi$geometry(), scale = max(10, dyn_scale), maxPixels = 1e12, bestEffort = TRUE, tileScale = 16)$getInfo()))), error = function(e) numeric(0))
              caMx <- if (length(caStr) >= 1 && is.finite(caStr[length(caStr)]) && caStr[length(caStr)] > 0) caStr[length(caStr)] else max(thr * 2, 0.2)
              caPal <- c("#1b7837", "#a6dba0", "#f7f7f7", "#e08214", "#b35806")   # low = stable (green) -> high = variable (orange)
              url  <- tryCatch(sdimg$visualize(min = 0, max = caMx, palette = caPal)$getThumbURL(list(region = ee_roi$geometry(), dimensions = thumb_dim, format = "png")), error = function(e) NULL)
              q75  <- tryCatch(as.numeric(unlist(sdimg$reduceRegion(reducer = ee$Reducer$percentile(list(75)), geometry = ee_roi$geometry(), scale = max(10, dyn_scale), maxPixels = 1e12, bestEffort = TRUE, tileScale = 16)$getInfo())[1]), error = function(e) NA_real_)
              gee_rv$coralalgae_summary <- list(median_sd = thr, q75_sd = q75)
              note <- sprintf("Heuristic coral-vs-algae from Sentinel-2 temporal variability (std-dev) of the blue-green bottom index over the coral mask, shown as a CONTINUOUS index: LOW variability (green) = temporally stable = coral-likely, HIGH (orange) = fluctuating = algae-likely. Median pixel std-dev = %.3f (a higher reef-wide value = more algae-like). SCREENING ONLY — NOT field-validated: a stable bottom signal is consistent with persistent coral, a fluctuating one with seasonal macroalgae. Confirm with in-situ / hyperspectral data.", thr)
              legend   <- list(type = "gradient", pal = caPal, min = 0, max = round(caMx, 3), unit = "Bottom-index temporal std-dev: green = stable (coral-likely), orange = variable (algae-likely)")
              citation <- "Sentinel-2 SR Harmonized — temporal-variability coral/algae heuristic (screening; not validated)."
              mean_val <- round(thr, 3)
            } else if (identical(it$feature, "__dhw__")) {
              # Degree Heating Weeks (NOAA Coral Reef Watch-style bleaching metric) from NOAA OISST v2.1.
              # MMM = maximum monthly-mean SST over the 1985-2012 climatology; HotSpot = SST-MMM (zeroed
              # below 1 degC); annual DHW = sum(daily HotSpots)/7 (degC-weeks). For a single tropical warm
              # season this approximates CRW's rolling 12-week accumulation. Coarse (~25 km) regional metric.
              oiC   <- ee$ImageCollection("NOAA/CDR/OISST/V2_1")$filterBounds(ee_roi)
              climC <- oiC$filterDate("1985-01-01", "2012-12-31")$select("sst")
              mMeans <- lapply(1:12, function(mo) climC$filter(ee$Filter$calendarRange(mo, mo, "month"))$mean())
              MMM   <- ee$ImageCollection$fromImages(mMeans)$max()$multiply(0.01)$rename("MMM")
              dhwYear <- function(y) {
                yc <- oiC$filterDate(sprintf("%04d-01-01", y), sprintf("%04d-12-31", y))$select("sst")
                hs <- yc$map(function(im) { d <- im$multiply(0.01)$subtract(MMM); d$where(d$lt(1), 0)$rename("HS") })
                hs$sum()$divide(7)$rename("DHW")
              }
              dyr0 <- suppressWarnings(as.integer(format(as.Date(s_d), "%Y"))); dyr1 <- suppressWarnings(as.integer(format(as.Date(e_d), "%Y")))
              if (is.na(dyr0) || is.na(dyr1)) { dyr1 <- as.integer(format(Sys.Date(), "%Y")); dyr0 <- dyr1 }
              maxDHW <- ee$ImageCollection$fromImages(lapply(dyr0:dyr1, dhwYear))$max()$clip(ee_roi)
              dhwPal <- c("#2c7bb6", "#ffffbf", "#fdae61", "#d7191c", "#7a0177")
              url  <- NULL   # coarse ~25 km DHW is a flat block over a small ROI and its thumbnail is very heavy to render; its value is the yearly series + correlation, not a fine map
              mean_val <- tryCatch({ v <- maxDHW$reduceRegion(reducer = ee$Reducer$mean(), geometry = ee_roi$geometry(), scale = 25000, maxPixels = 1e12, bestEffort = TRUE, tileScale = 16)$getInfo(); if (length(v) && !is.null(v[[1]])) round(as.numeric(v[[1]]), 2) else NA_real_ }, error = function(e) NA_real_)
              note <- sprintf("Degree Heating Weeks (peak annual, %d-%d) from NOAA OISST v2.1: heat accumulated above the 1985-2012 maximum-monthly-mean (HotSpots >=1 degC, summed over the year / 7). Peak regional DHW here: %s degC-weeks (>4 = significant bleaching risk, >8 = severe). Coarse ~25 km \u2014 read as a regional bleaching-stress series; single-warm-season approximation of the CRW 12-week metric.", dyr0, dyr1, if (isTRUE(is.finite(mean_val))) sprintf("%.1f", mean_val) else "n/a")
              legend   <- list(type = "gradient", pal = dhwPal, min = 0, max = 12, unit = "Degree Heating Weeks (degC-weeks): >4 bleaching risk, >8 severe")
              citation <- "NOAA OISST v2.1 (NOAA/CDR/OISST/V2_1); Degree Heating Weeks after Liu et al. (2014) / NOAA Coral Reef Watch."
              last_trendable <- list(label = "Marine Heat Stress (DHW)", units = "degC-weeks", build = function(y) dhwYear(y))
              trendables[["Marine Heat Stress (DHW)"]] <- c(last_trendable, list(mask = NULL))
            } else if (identical(it$feature, "__turbidity__")) {
              # Calibrated turbidity (Nechad et al. 2009/2016) from Sentinel-2 red band (B4, 665 nm):
              # T[FNU] = A*rho / (1 - rho/C), A=610.94, C=0.2324. Physically grounded vs the NDTI ratio.
              tA <- 610.94; tC <- 0.2324
              turbImg <- function(sd2, ed2) {
                tcol <- ee$ImageCollection("COPERNICUS/S2_SR_HARMONIZED")$filterBounds(ee_roi)$filterDate(sd2, ed2)$map(mask_s2_clouds)
                if (as.integer(tryCatch(tcol$size()$getInfo(), error = function(e) 0L)) == 0L) return(NULL)
                comp <- switch(agg, "Mean" = tcol$mean(), "Max" = tcol$max(), "Min" = tcol$min(), tcol$median())
                rho  <- comp$select("B4")$multiply(0.0001)
                tt   <- rho$multiply(tA)$divide(ee$Image$constant(1)$subtract(rho$divide(tC)))$rename("Turbidity_FNU")
                tt$updateMask(rho$gt(0)$And(rho$lt(tC)))
              }
              turb <- turbImg(s_d, e_d)
              if (is.null(turb)) stop("No Sentinel-2 scenes for turbidity in this date range.")
              if (!is.null(pipeline_state$current_roi_mask)) turb <- turb$updateMask(pipeline_state$current_roi_mask)
              tpct <- tryCatch(turb$reduceRegion(reducer = ee$Reducer$percentile(list(2, 98)), geometry = ee_roi$geometry(), scale = max(10, dyn_scale), maxPixels = 1e12, bestEffort = TRUE, tileScale = 16)$getInfo(), error = function(e) NULL)
              tvv  <- tryCatch(sort(as.numeric(unlist(tpct))), error = function(e) numeric(0))
              tmx  <- if (length(tvv) >= 1 && is.finite(tvv[length(tvv)]) && tvv[length(tvv)] > 0) tvv[length(tvv)] else 20
              tPal <- c("#08306b", "#4292c6", "#d9c8a5", "#8c6d31")
              url  <- tryCatch(turb$visualize(min = 0, max = tmx, palette = tPal)$getThumbURL(list(region = ee_roi$geometry(), dimensions = thumb_dim, format = "png")), error = function(e) NULL)
              mean_val <- mean_of(turb)
              note <- sprintf("Calibrated turbidity (Nechad et al.) from Sentinel-2 red band: T[FNU] = A*rho/(1-rho/C), A=%.2f, C=%.4f; stretched 0 to %.1f FNU. Physically grounded vs the NDTI ratio; clipped to the upstream mask.", tA, tC, tmx)
              legend   <- list(type = "gradient", pal = tPal, min = 0, max = round(tmx, 1), unit = "Turbidity (FNU) \u2014 Nechad et al.")
              citation <- "Sentinel-2 SR Harmonized; Nechad, B. et al. (2009/2016) turbidity algorithm."
              last_single <- list(img = turb, pal = tPal, mn = 0, mx = tmx, label = "Turbidity (Nechad, FNU)")
              singles[["Turbidity (Nechad, FNU)"]] <- last_single
              last_trendable <- list(label = "Turbidity (Nechad)", units = "FNU", build = function(y) { ti <- turbImg(sprintf("%04d-01-01", y), sprintf("%04d-12-31", y)); if (is.null(ti)) NULL else ti$select(0L) })
              trendables[["Turbidity (Nechad)"]] <- c(last_trendable, list(mask = pipeline_state$current_roi_mask))
            } else if (identical(it$feature, "__blackmarble__")) {
              # NASA Black Marble VNP46A2 (500 m, gap-filled, BRDF- & stray-light-corrected) nighttime
              # lights \u2014 a cleaner tourism/activity proxy than the raw VIIRS DNB monthly product.
              bmImg <- function(sd2, ed2) {
                bm <- ee$ImageCollection("NASA/VIIRS/002/VNP46A2")$filterBounds(ee_roi)$filterDate(sd2, ed2)$select("Gap_Filled_DNB_BRDF_Corrected_NTL")
                if (as.integer(tryCatch(bm$size()$getInfo(), error = function(e) 0L)) == 0L) return(NULL)
                switch(agg, "Mean" = bm$mean(), "Max" = bm$max(), "Min" = bm$min(), bm$median())$rename("NTL")
              }
              ntl <- bmImg(s_d, e_d)
              if (is.null(ntl)) stop("No Black Marble scenes for this area / date range (VNP46A2 starts 2012).")
              ntl <- ntl$clip(ee_roi)
              if (!is.null(pipeline_state$current_roi_mask)) ntl <- ntl$updateMask(pipeline_state$current_roi_mask)
              bPal <- c("#000004", "#3b0f70", "#8c2981", "#de4968", "#fe9f6d", "#fcfdbf")
              url  <- tryCatch(ntl$visualize(min = 0, max = 60, palette = bPal)$getThumbURL(list(region = ee_roi$geometry(), dimensions = thumb_dim, format = "png")), error = function(e) NULL)
              mean_val <- mean_of(ntl)
              note <- if (!is.null(pipeline_state$current_roi_mask)) "NASA Black Marble VNP46A2 nighttime lights (500 m, gap-filled BRDF/stray-light corrected). Clipped to the upstream mask." else "NASA Black Marble VNP46A2 nighttime lights (500 m, gap-filled BRDF/stray-light corrected). Computed over the full boundary."
              legend   <- list(type = "gradient", pal = bPal, min = 0, max = 60, unit = "Nighttime radiance (nW/cm2/sr) \u2014 Black Marble VNP46A2")
              citation <- "NASA Black Marble VIIRS/NPP VNP46A2 (NASA/VIIRS/002/VNP46A2)."
              last_single <- list(img = ntl, pal = bPal, mn = 0, mx = 60, label = "Nighttime Lights (Black Marble)")
              singles[["Nighttime Lights (Black Marble)"]] <- last_single
              last_trendable <- list(label = "Nighttime Lights (Black Marble)", units = "nW/cm2/sr", build = function(y) { bi <- bmImg(sprintf("%04d-01-01", y), sprintf("%04d-12-31", y)); if (is.null(bi)) NULL else bi$select(0L) })
              trendables[["Nighttime Lights (Black Marble)"]] <- c(last_trendable, list(mask = pipeline_state$current_roi_mask))
            } else if (identical(it$feature, "__trend__")) {
              # ADDITIVE trend: analyses the SAME layer that came before it (e.g. Coral
              # Health), NOT the sidebar feature. It does NOT replace the pipeline's final
              # map/class-area — its result lives in the Rate of Change insight + this
              # step's own slope map. Local rgee: yearly rebuilds -> linearFit -> slope/yr.
              tb <- last_trendable
              if (is.null(tb)) stop("Add an index or Coral Health step BEFORE Trend — Trend analyses whatever ran just before it.")
              # Trend span comes from the SAME common Pipeline date range as every other
              # step: start year -> end year of pipe_date (s_d/e_d already resolved above).
              ty0 <- suppressWarnings(as.integer(format(as.Date(s_d), "%Y")))
              ty1 <- suppressWarnings(as.integer(format(as.Date(e_d), "%Y")))
              if (is.na(ty0) || is.na(ty1)) { ty1 <- as.integer(format(Sys.Date(), "%Y")); ty0 <- ty1 - 4 }
              if (ty1 - ty0 < 1) stop("Trend needs at least 2 different years — widen the Pipeline date range so its start and end fall in different years (e.g. 2016-01-01 to 2019-12-31).")
              mask_exempt <- grepl("Heat Stress|SST anomaly", tb$label %||% "", ignore.case = TRUE)   # coarse regional layers (e.g. 25 km SST) must NOT be reef-masked
              # PRIMARY (robust): per-year REGIONAL MEAN -> straight-line fit. Always works with
              # 2+ years of data, even when per-pixel coverage doesn't overlap across years.
              yr_diag <- character(0)   # per-year outcome, for a useful failure message
              per_year <- Filter(Negate(is.null), lapply(ty0:ty1, function(y) {
                im <- tryCatch(tb$build(y), error = function(e) NULL)
                if (is.null(im)) { yr_diag[[length(yr_diag) + 1]] <<- sprintf("%d: no imagery", y); return(NULL) }
                im1 <- im$select(0L)
                # Reef/analysis-mask the trend to the SAME pixels the composite map & the
                # cross-indicator correlation use, so "Coral Health" is ONE consistent quantity
                # (same magnitude) across the map, the trend and the correlation. Without this the
                # trend reduces over every water pixel (incl. deep water) and drifts to a different
                # scale than the reef-masked map.
                if (!mask_exempt && !is.null(pipeline_state$current_roi_mask)) im1 <- im1$updateMask(pipeline_state$current_roi_mask)
                mv  <- tryCatch(im1$reduceRegion(reducer = ee$Reducer$mean(), geometry = ee_roi$geometry(),
                                 scale = dyn_scale * 2, maxPixels = 1e12, bestEffort = TRUE, tileScale = 16)$getInfo(), error = function(e) NULL)
                val <- suppressWarnings(as.numeric(unlist(mv)[1]))
                if (!isTRUE(is.finite(val))) { yr_diag[[length(yr_diag) + 1]] <<- sprintf("%d: no valid pixels", y); return(NULL) }
                yr_diag[[length(yr_diag) + 1]] <<- sprintf("%d: OK (%.3f)", y, val)
                list(year = y, img = im1, value = round(val, 4))
              }))
              if (length(per_year) < 2)
                stop(sprintf("Could not get %s values for enough years in %d–%d (need 2+ years of data). Per-year status: %s. Widen the Pipeline date range to span more clear-imagery years.",
                             tb$label, ty0, ty1, paste(yr_diag, collapse = "; ")))
              series <- do.call(rbind, lapply(per_year, function(p) data.frame(Year = p$year, Value = p$value)))
              lmfit  <- stats::lm(Value ~ Year, data = series)
              rate   <- as.numeric(coef(lmfit)[2]); r2 <- summary(lmfit)$r.squared
              rate_ci <- tryCatch(as.numeric(stats::confint(lmfit)["Year", ]), error = function(e) c(NA_real_, NA_real_))
              # Non-parametric trend on the regional series (field standard for short EO series).
              mk <- tryCatch(mann_kendall_sen(series$Year, series$Value), error = function(e) NULL)
              # How cloudy was the raw data (before we masked clouds per-pixel)?
              cloud_info <- if (grepl("Landsat|Heat Stress|SST", tb$label %||% "", ignore.case = TRUE)) NULL else tryCatch({
                s2raw <- ee$ImageCollection("COPERNICUS/S2_SR_HARMONIZED")$filterBounds(ee_roi)$
                  filterDate(sprintf("%04d-01-01", ty0), sprintf("%04d-12-31", ty1))
                list(n = as.integer(s2raw$size()$getInfo()),
                     cloud = suppressWarnings(as.numeric(s2raw$aggregate_mean("CLOUDY_PIXEL_PERCENTAGE")$getInfo())))
              }, error = function(e) NULL)
              # BONUS (best-effort): per-pixel slope map + area of increase/decrease. Wrapped in
              # try() so a sparse-overlap failure never kills the regional trend above.
              t_thumb <- NULL; t_area <- NULL
              # A coarse regional indicator (e.g. 25 km SST) has no meaningful per-pixel slope over a
              # small ROI — it renders as a flat colour block — so skip the slope map for mask-exempt layers.
              if (!mask_exempt) try({
                imgs2 <- lapply(per_year, function(p) { base <- p$img$rename("v"); base$addBands(base$multiply(0)$add(p$year)$toFloat()$rename("t")) })
                slope <- ee$ImageCollection$fromImages(imgs2)$select(c("t", "v"))$reduce(ee$Reducer$linearFit())$select("scale")$rename("slope")
                # Restrict the per-pixel trend to the ANALYSIS pixels only (reef/water mask from the
                # step before Trend). Without this, land + deep-ocean pixels flood the "stable" class
                # and make the increasing/stable/decreasing table meaningless.
                if (!mask_exempt && !is.null(pipeline_state$current_roi_mask)) slope <- slope$updateMask(pipeline_state$current_roi_mask)
                mm <- slope$reduceRegion(reducer = ee$Reducer$minMax(), geometry = ee_roi$geometry(), scale = dyn_scale * 2, maxPixels = 1e12, bestEffort = TRUE, tileScale = 16)$getInfo()
                smin <- as.numeric(mm[["slope_min"]]); smax <- as.numeric(mm[["slope_max"]])
                if (isTRUE(is.finite(smin)) && isTRUE(is.finite(smax))) {
                  absmax <- max(abs(smin), abs(smax), 1e-6); tpal <- c("#2c7bb6", "#f7f7f7", "#d7191c")
                  t_thumb <- slope$visualize(min = -absmax, max = absmax, palette = tpal)$getThumbURL(list(region = ee_roi$geometry(), dimensions = thumb_dim, format = "png"))
                  thr <- absmax * 0.10
                  classed <- slope$multiply(0)$add(1)$where(slope$lt(-thr), 0)$where(slope$gt(thr), 2)$rename("classification")$toInt()
                  if (!mask_exempt && !is.null(pipeline_state$current_roi_mask)) classed <- classed$updateMask(pipeline_state$current_roi_mask)
                  t_area <- gf_trend_area_classes(get_area_by_class_groups(classed, ee_roi, dyn_scale))
                }
              }, silent = TRUE)
              url <- t_thumb; mean_val <- round(rate, 4)
              note <- sprintf("Regional trend of %s, %d–%d: %+.4f per year (R2 %.2f over %d years).%s", tb$label, ty0, ty1, rate, r2, nrow(series), if (is.null(t_thumb)) " Per-pixel map unavailable (sparse cloud-free overlap)." else "")
              legend <- if (!is.null(t_thumb)) list(type = "composite", text = "Per-pixel slope/yr — blue = decreasing, red = increasing") else NULL
              # NOTE: intentionally does NOT set last_single or gee_rv$gee_class_insights (additive).
              gee_rv$trend_summary <- list(feature = tb$label, start_year = ty0, end_year = ty1, rate = rate, r2 = r2,
                                           rate_lo = rate_ci[[1]], rate_hi = rate_ci[[2]],
                                           series = series, sig_pct = NA, units = "/yr", area_df = t_area, thumb = t_thumb,
                                           cloud_info = cloud_info, mk = mk)
            } else if (identical(it$feature, "__correl__")) {
              # GENERIC cross-indicator correlation. Correlates the yearly regional-mean series of
              # EVERY indicator already added to the pipeline before this step (Coral Health, any
              # index — NDVI, LST, Turbidity, Nighttime Lights, Built-up, …). Not hard-wired to any
              # topic: whatever indicators you build, it correlates them. Each series uses the mask
              # that is active on the state bus (so add a mask/ACA step first to restrict, or none
              # to use the full boundary). Every indicator is best-effort; a failing one is dropped.
              cy0 <- suppressWarnings(as.integer(format(as.Date(s_d), "%Y")))
              cy1 <- suppressWarnings(as.integer(format(as.Date(e_d), "%Y")))
              if (is.na(cy0) || is.na(cy1) || cy1 - cy0 < 2)
                stop("Correlation needs at least 3 years — widen the Pipeline date range.")
              if (length(trendables) < 2)
                stop("Add at least TWO indicator steps (e.g. Coral Health, an index) BEFORE Cross-Indicator Correlation — it correlates whatever indicators the pipeline built.")
              cyears <- cy0:cy1
              # Each indicator uses the mask that was ACTIVE WHEN ITS STEP RAN (captured at
              # registration) — so a land indicator placed BEFORE the reef/ACA mask stays full-ROI,
              # while coral/turbidity placed after stay reef-masked. This is what makes the
              # Assistant's "move land layers before the mask" advice actually work here.
              yr_mean <- function(tb, y) {
                img <- tryCatch(tb$build(y), error = function(e) NULL)
                if (is.null(img)) return(NA_real_)
                im <- img$select(0L); if (!is.null(tb$mask)) im <- im$updateMask(tb$mask)
                v <- tryCatch(im$reduceRegion(reducer = ee$Reducer$mean(), geometry = ee_roi$geometry(), scale = dyn_scale * 2,
                              maxPixels = 1e12, bestEffort = TRUE, tileScale = 16)$getInfo(), error = function(e) NULL)
                # unlist(NULL)/unlist(list(band=NULL)) -> NULL, and as.numeric(NULL) is length 0,
                # which makes the vapply below error ("result is length 0"). Coerce FIRST, index
                # AFTER, so a fully-masked / empty region for one year is a clean NA, not a crash.
                x <- suppressWarnings(as.numeric(unlist(v)))
                if (length(x) == 0) NA_real_ else x[[1]]
              }
              series <- list(); tnames <- names(trendables)
              withProgress(message = "Correlating indicators…", value = 0, {
                for (nm in tnames) {
                  incProgress(1 / length(tnames), detail = nm)
                  vals <- vapply(cyears, function(y) yr_mean(trendables[[nm]], y), numeric(1))
                  if (sum(is.finite(vals)) >= 3) series[[nm]] <- vals
                }
              })
              # User's explicit choice wins (input$pipe_response); else Coral Health, else first.
              resp <- gf_pick_layer(names(series), selected = input$pipe_response %||% "",
                                    prefer = c("Coral Health"))
              corr_p <- gf_correlation_analysis(series, method = "pearson")
              corr_s <- gf_correlation_analysis(series, method = "spearman")
              lag_tbl <- NULL
              if (!is.null(resp) && !is.null(series[[resp]]))
                lag_tbl <- lapply(setdiff(names(series), resp), function(dn)
                  list(driver = dn, lag = gf_lag_correlation(series[[resp]], series[[dn]], max_lag = 2)))
              gee_rv$correl_series <- list(years = cyears, series = series, response = resp,
                                           corr = corr_p, corr_sp = corr_s, lags = lag_tbl)
              note <- if (!is.null(corr_p)) sprintf("Correlated %d pipeline indicators over %d-%d — matrix, series & lags in Insights.", length(series), cy0, cy1)
                      else "Not enough overlapping indicator-years to correlate (need 3+ years with data)."
              legend   <- list(type = "composite", text = "Cross-indicator correlation — see the Insights tab for the matrix and driver relationships.")
              citation <- NULL   # data sources are already cited by the individual indicator steps
            } else if (identical(it$feature, "__landmask__")) {
              # LAND-only mask: exclude OCEAN so dry-land indicators (Nighttime Lights, Urban
              # Sprawl, NDVI…) placed AFTER it are computed over land only. Uses SRTM's data
              # footprint — ocean is NoData in SRTM, so its mask IS a land mask. This is a STATIC,
              # cheap image: crucially it does NOT add a per-year composite to the correlation.
              # (An MNDWI median over the whole date range would be re-evaluated on every yearly
              # getInfo and could stall the correlation on the first land indicator.)
              landm <- tryCatch(ee$Image("USGS/SRTMGL1_003")$mask()$gt(0), error = function(e) NULL)
              if (is.null(landm)) stop("Could not build a land mask (SRTM unavailable for this area).")
              landm <- if (!is.null(pipeline_state$current_roi_mask)) landm$And(pipeline_state$current_roi_mask) else landm
              pipeline_state$current_roi_mask <- landm$selfMask()
              mask_url <- mask_thumb(pipeline_state$current_roi_mask)
              note <- "Land-only mask published (ocean excluded via the SRTM land footprint). Dry-land indicators placed after this are now restricted to land."
              legend   <- list(type = "composite", text = "Land mask — ocean excluded (SRTM).")
              citation <- "NASA SRTM Digital Elevation 30m (USGS/SRTMGL1_003)"
            } else if (identical(it$feature, "__heatstress__")) {
              # Marine Heat Stress (SST anomaly) — NOAA OISST v2.1, ~25 km daily SST.
              # This is a REGIONAL ocean-heat / coral-bleaching indicator, NOT a per-reef-pixel
              # layer: it is deliberately NOT clipped to the fine reef/land bus mask (a 25 km SST
              # cell never aligns with reef pixels, so masking would blank it). Its main value is
              # the yearly series the Cross-Indicator Correlation pairs against Coral Health.
              oisst <- ee$ImageCollection("NOAA/CDR/OISST/V2_1")$filterBounds(ee_roi)$filterDate(s_d, e_d)
              if (as.integer(tryCatch(oisst$size()$getInfo(), error = function(e) 0L)) == 0L)
                stop("No NOAA OISST SST scenes for this area / date range (OISST starts 1981-09).")
              anom <- oisst$select("anom")$mean()$multiply(0.01)$rename("SST_anom")$clip(ee_roi)   # raw x0.01 -> deg C
              hs_pal <- c("#2166ac", "#67a9cf", "#f7f7f7", "#ef8a62", "#b2182b")                   # cool(blue) -> warm(red)
              hs_mean <- tryCatch({
                v <- anom$reduceRegion(reducer = ee$Reducer$mean(), geometry = ee_roi$geometry(),
                                       scale = 25000, maxPixels = 1e12, bestEffort = TRUE, tileScale = 16)$getInfo()
                if (length(v) && !is.null(v[[1]])) round(as.numeric(v[[1]]), 3) else NA_real_
              }, error = function(e) NA_real_)
              mean_val <- hs_mean
              url  <- tryCatch(anom$visualize(min = -3, max = 3, palette = hs_pal)$
                                 getThumbURL(list(region = ee_roi$geometry(), dimensions = thumb_dim, format = "png")),
                               error = function(e) NULL)
              note <- sprintf("Sea-surface-temperature anomaly from NOAA OISST v2.1 (~25 km, daily), averaged over %s to %s; anomalies are vs the 1971-2000 climatology. Regional ocean-heat / coral-bleaching-stress indicator (positive = warmer than normal) — NOT reef-pixel resolution, so read it as the yearly series correlated with Coral Health rather than a fine map. Mean anomaly here: %s degC.",
                              s_d, e_d, if (isTRUE(is.finite(hs_mean))) sprintf("%+.2f", hs_mean) else "n/a")
              legend   <- list(type = "gradient", pal = hs_pal, min = -3, max = 3, unit = "SST anomaly (degC): blue = cooler, red = warmer than 1971-2000 normal")
              citation <- "NOAA Optimum Interpolation Sea Surface Temperature (OISST) v2.1 (NOAA/CDR/OISST/V2_1)"
              last_single <- list(img = anom, pal = hs_pal, mn = -3, mx = 3, label = "Marine Heat Stress (SST anomaly)")
              singles[["Marine Heat Stress (SST anomaly)"]] <- last_single
              last_trendable <- list(label = "Marine Heat Stress (SST anomaly)", units = "degC anomaly", build = function(y) {
                c2 <- ee$ImageCollection("NOAA/CDR/OISST/V2_1")$filterBounds(ee_roi)$filterDate(sprintf("%04d-01-01", y), sprintf("%04d-12-31", y))
                if (as.integer(tryCatch(c2$size()$getInfo(), error = function(e) 0L)) == 0L) return(NULL)
                c2$select("anom")$mean()$multiply(0.01)$rename("SST_anom") })
              # mask = NULL on purpose: correlation must reduce SST over the whole ROI ocean, never
              # the reef mask (a 25 km SST cell would otherwise be masked to nothing).
              trendables[["Marine Heat Stress (SST anomaly)"]] <- c(last_trendable, list(mask = NULL))
            } else if (identical(it$role, "mask")) {
              fd <- get_feature_img(it$feature, s_d, e_d, agg, ee_roi, dyn_scale)
              if (is.null(fd) || is.null(fd$img)) stop(sprintf("%s returned no imagery for this area / date range.", it$label))
              pipeline_state$current_roi_mask <- fd$img$gt(0)
              mask_url <- mask_thumb(pipeline_state$current_roi_mask)
              note <- sprintf("Water mask published to the state bus (%s > 0).", it$label)
              url  <- thumb_of(fd$img, fd$pal, fd$min, fd$max); mean_val <- mean_of(fd$img)
              legend   <- list(type = "gradient", pal = fd$pal, min = fd$min, max = fd$max, unit = it$label)
              citation <- cite_of(it$feature)
              last_trendable <- local({ f <- it$feature; l <- it$label; list(label = l, units = "", build = function(y) {
                fd2 <- tryCatch(get_feature_img(f, sprintf("%04d-01-01", y), sprintf("%04d-12-31", y), agg, ee_roi, dyn_scale, stretch = FALSE), error = function(e) NULL)
                if (!is.null(fd2) && !is.null(fd2$img)) fd2$img$select(0L) else NULL }) })
              trendables[[it$label]] <- c(last_trendable, list(mask = pipeline_state$current_roi_mask))
              last_single <- list(img = fd$img, pal = fd$pal, mn = fd$min, mx = fd$max, label = it$label)
              singles[[it$label]] <- last_single
            } else {
              fd <- get_feature_img(it$feature, s_d, e_d, agg, ee_roi, dyn_scale)
              if (is.null(fd) || is.null(fd$img)) stop(sprintf("%s returned no imagery for this area / date range.", it$label))
              # Actually clip the layer to the active bus mask (e.g. a Land mask before Nighttime
              # Lights / Urban Sprawl), so the displayed map, its regional mean and the composite
              # all honour the mask — not just the correlation. Makes the note below literally true.
              if (!is.null(pipeline_state$current_roi_mask)) fd$img <- fd$img$updateMask(pipeline_state$current_roi_mask)
              note <- if (!is.null(pipeline_state$current_roi_mask)) "Clipped to the upstream mask from the state bus." else "Computed over the full boundary (no upstream mask)."
              url  <- thumb_of(fd$img, fd$pal, fd$min, fd$max); mean_val <- mean_of(fd$img)
              legend   <- list(type = "gradient", pal = fd$pal, min = fd$min, max = fd$max, unit = it$label)
              citation <- cite_of(it$feature)
              last_single <- list(img = fd$img, pal = fd$pal, mn = fd$min, mx = fd$max, label = it$label)
              singles[[it$label]] <- last_single
              last_trendable <- local({ f <- it$feature; l <- it$label; list(label = l, units = "", build = function(y) {
                fd2 <- tryCatch(get_feature_img(f, sprintf("%04d-01-01", y), sprintf("%04d-12-31", y), agg, ee_roi, dyn_scale, stretch = FALSE), error = function(e) NULL)
                if (!is.null(fd2) && !is.null(fd2$img)) fd2$img$select(0L) else NULL }) })
              trendables[[it$label]] <- c(last_trendable, list(mask = pipeline_state$current_roi_mask))
            }
          }, error = function(e) { st <<- "failed"; note <<- conditionMessage(e) })
          steps[[i]] <- list(step = i, label = it$label, role = it$role, status = st,
                             note = note, url = url, legend = legend, citation = citation, mean = mean_val, mask_url = mask_url)
        }
      })

      # ---- Choose the report HEADLINE layer intentionally, not "whatever ran last". Prefer a
      #      Coral Health analysis, else Benthic, else the FIRST index the user added. The composite
      #      map, Area-by-class and landscape metrics all follow this one layer, so they stay
      #      consistent with the study's story instead of flipping to a trailing step (e.g. Turbidity).
      if (length(singles)) {
        # User's explicit choice wins (input$pipe_headline); else Coral Health > Benthic > first.
        hk <- gf_pick_layer(names(singles), selected = input$pipe_headline %||% "",
                            prefer = c("Coral Health", "Benthic"))
        if (!is.null(hk)) last_single <- singles[[hk]]
      }

      # ---- FINAL composite map + class insights: ONE shared discrete classification,
      #      so the map colours line up 1:1 with the Insights "Area by class" table.
      #      Masked to the pipeline's reef/analysis mask (deep water & land excluded).
      #      Robust thumbnail fetch (httr) + unmasked fallback + surfaced error reason. ----
      final_plot <- NULL; final_plot_err <- NULL; final_caption <- NULL; final_explain <- NULL
      final_url <- NULL   # visualized composite thumbnail URL — captured for the GeoTIFF export
      final_bbox <- tryCatch(sf::st_bbox(sf::st_transform(rv$mask_vect, 4326)), error = function(e) NULL)
      pal5 <- c("#2c7bb6", "#abd9e9", "#ffffbf", "#fdae61", "#d7191c")   # matches the Insights palette
      if (!is.null(last_single)) {
        mn <- last_single$mn; mx <- last_single$mx
        use_classes <- isTRUE(is.finite(mn)) && isTRUE(is.finite(mx)) && mx > mn
        chain_lbl <- paste(vapply(steps, function(s) s$label, character(1)), collapse = " -> ")

        # 1) Discrete 5-class classification (reef/analysis-masked) -> per-class AREA table.
        class_df <- NULL
        if (use_classes) {
          class_df <- withProgress(message = "Computing per-class areas…", value = 0.15, tryCatch({
            step5 <- (mx - mn) / 5
            classed <- last_single$img$subtract(mn)$divide(step5)$floor()$clamp(0, 4)$toInt()$rename("classification")
            if (!is.null(pipeline_state$current_roi_mask)) classed <- classed$updateMask(pipeline_state$current_roi_mask)
            gf_classify_area_groups(get_area_by_class_groups(classed, ee_roi, dyn_scale), mn, mx)
          }, error = function(e) NULL))
          if (!is.null(class_df))
            gee_rv$gee_class_insights <- list(feature = last_single$label, source = "Sequential Pipeline",
                                              classes = class_df, palette = pal5)
        }

        # 1b) Landscape CONFIGURATION (patch structure). Each EE metric is extracted with its OWN
        #     tryCatch so one failing call (e.g. a reducer the backend rejects) can't wipe out the
        #     whole block — as long as the total area is known, landscape_patch_metrics() returns a
        #     result and the block shows, with "—" for any piece that couldn't be computed. The
        #     first failure's message is kept in gee_rv$gee_patch_error so the UI can explain a gap.
        gee_rv$gee_patch_metrics <- NULL; gee_rv$gee_patch_error <- NULL
        if (use_classes) withProgress(message = "Computing landscape patch metrics…", value = 0.35, {
          step5 <- (mx - mn) / 5
          cimg <- last_single$img$subtract(mn)$divide(step5)$floor()$clamp(0, 4)$toInt()$rename("classification")
          if (!is.null(pipeline_state$current_roi_mask)) cimg <- cimg$updateMask(pipeline_state$current_roi_mask)
          geom <- ee_roi$geometry()
          # Connected-component ops (connectedPixelCount / connectedComponents) are memory-hungry
          # at native 10 m over a whole reef -> "User memory limit exceeded". Run them at a COARSER
          # analysis scale (fixed to the coarsened grid via reproject) so EE evaluates far fewer
          # pixels; the metrics stay valid, just at that coarser grain. Smaller maxSize also helps.
          patch_scale <- max(as.numeric(dyn_scale) * 3, 30); pxa <- patch_scale * patch_scale
          cimg_c <- tryCatch(cimg$reproject(cimg$projection()$atScale(patch_scale)), error = function(e) cimg)
          total_m2 <- if (!is.null(class_df)) sum(class_df$Area_km2, na.rm = TRUE) * 1e6 else NA_real_
          note_err <- function(e) { if (is.null(gee_rv$gee_patch_error)) gee_rv$gee_patch_error <- conditionMessage(e); NA_real_ }
          rr1 <- function(img, reducer) tryCatch(as.numeric(img$reduceRegion(reducer = reducer, geometry = geom,
                     scale = patch_scale, maxPixels = 1e12, bestEffort = TRUE, tileScale = 16)$getInfo()[[1]]), error = note_err)
          psize   <- tryCatch(cimg_c$connectedPixelCount(maxSize = 256, eightConnected = TRUE), error = function(e) { note_err(e); NULL })
          maxpix  <- if (!is.null(psize)) rr1(psize, ee$Reducer$max())  else NA_real_
          meanpix <- if (!is.null(psize)) rr1(psize, ee$Reducer$mean()) else NA_real_
          np <- tryCatch(as.numeric(cimg_c$connectedComponents(connectedness = ee$Kernel$plus(1), maxSize = 256)$
                           select("labels")$reduceRegion(reducer = ee$Reducer$countDistinctNonNull(),
                           geometry = geom, scale = patch_scale, maxPixels = 1e12, bestEffort = TRUE, tileScale = 16)$
                           get("labels")$getInfo()), error = note_err)
          edge_len <- tryCatch({
            nd <- cimg_c$reduceNeighborhood(reducer = ee$Reducer$countDistinctNonNull(), kernel = ee$Kernel$square(1))
            as.numeric(nd$gt(1)$selfMask()$reduceRegion(reducer = ee$Reducer$count(), geometry = geom,
              scale = patch_scale, maxPixels = 1e12, bestEffort = TRUE, tileScale = 16)$getInfo()[[1]]) * patch_scale
          }, error = note_err)
          gee_rv$gee_patch_metrics <- landscape_patch_metrics(total_m2, np = np,
                                  largest_patch_m2 = if (isTRUE(is.finite(maxpix)))  maxpix  * pxa else NA_real_,
                                  aw_mean_patch_m2 = if (isTRUE(is.finite(meanpix))) meanpix * pxa else NA_real_,
                                  edge_len_m = edge_len)
        })

        # 2) The composite MAP. Robust fetch helper: EE thumbnail via httr (clear error on
        #    failure) instead of download.file (which fails silently on some setups).
        # Heavy composites (e.g. a Median reducer over hundreds of scenes) can make EE's
        # on-the-fly thumbnail computation slow enough to blow a short timeout. Fetch defensively:
        #  - longer timeout (180s),
        #  - a few retries with linear backoff (transient 5xx / timeouts),
        #  - and a smaller-dimension fallback (fewer pixels = far less server compute),
        # so a genuine map still renders instead of the whole figure silently vanishing.
        fetch_png <- function(vimg) {
          dims <- unique(c(max(768L, as.integer(thumb_dim)), 512L))   # full res first, then a lighter fallback
          last_err <- "unknown reason"
          for (di in seq_along(dims)) {
            dim <- dims[di]
            for (attempt in 1:3) {
              out <- tryCatch({
                url_f <- vimg$getThumbURL(list(region = ee_roi$geometry(), dimensions = as.integer(dim), format = "png"))
                tmp <- tempfile(fileext = ".png")
                resp <- httr::GET(url_f, httr::timeout(180))
                if (httr::status_code(resp) != 200) stop(sprintf("EE thumbnail server returned HTTP %d", httr::status_code(resp)))
                writeBin(httr::content(resp, "raw"), tmp); png::readPNG(tmp)
              }, error = function(e) { last_err <<- conditionMessage(e); NULL })
              if (!is.null(out)) return(out)
              # don't sleep after the very last attempt of the last dimension
              if (!(di == length(dims) && attempt == 3)) Sys.sleep(2 * attempt)
            }
          }
          stop(last_err)
        }
        final_plot <- tryCatch(withProgress(message = "Compositing final map…", value = 0.5, {
          base_vimg <- if (use_classes) {
            step5 <- (mx - mn) / 5
            last_single$img$subtract(mn)$divide(step5)$floor()$clamp(0, 4)$toInt()$visualize(min = 0, max = 4, palette = pal5)
          } else last_single$img$visualize(min = mn, max = mx, palette = last_single$pal)
          vimg <- if (!is.null(pipeline_state$current_roi_mask)) base_vimg$updateMask(pipeline_state$current_roi_mask) else base_vimg
          # Capture the visualized thumbnail URL so the Export Manager can georeference it to GeoTIFF.
          final_url <<- tryCatch(vimg$getThumbURL(list(region = ee_roi$geometry(), dimensions = max(768L, as.integer(thumb_dim)), format = "png")), error = function(e) NULL)
          # If the masked thumbnail fails (e.g. an empty mask), retry unmasked so a map still shows.
          arr <- tryCatch(fetch_png(vimg), error = function(e) fetch_png(base_vimg))
          if (use_classes && !is.null(class_df) && nrow(class_df) > 0) {
            leg_labs <- sprintf("%s (%s)", class_df$Class, class_df$Range)
            build_classified_map_plot(arr, final_bbox, rv$mask_vect, pal5[seq_len(nrow(class_df))],
                                      leg_labs, legend_title = last_single$label)
          } else {
            build_continuous_map_plot(arr, final_bbox, rv$mask_vect, mn, mx, last_single$pal,
                                      legend_title = last_single$label, base_fill = "#eaeef1")
          }
        }), error = function(e) { final_plot_err <<- conditionMessage(e); NULL })

        if (is.null(final_plot)) {
          final_caption <- sprintf("All steps computed, but the composite map could not be rendered: %s. This is a map-rendering issue, not an analysis failure — the Insights numbers are still valid. Chain: %s.",
                                   final_plot_err %||% "unknown reason", chain_lbl)
        } else {
          used_aca <- any(vapply(steps, function(s) grepl("Allen Coral Atlas", s$label %||% ""), logical(1)))
          mask_phrase <- if (used_aca) "clipped to ACA-confirmed coral (deep water & land excluded)" else "clipped to the reef/analysis mask (deep water & land excluded)"
          final_caption <- sprintf("Final composite = %s, shown as %s, %s. Chain: %s.",
                                   last_single$label,
                                   if (use_classes) "5 equal-interval classes whose colours match the Area-by-class table" else "a continuous stretch",
                                   mask_phrase, chain_lbl)
        }

        # ---- Plain-language explanation: process + number ranges + meaning ----
        lab <- last_single$label; lo <- round(last_single$mn, 2); hi <- round(last_single$mx, 2)
        bands <- if (grepl("Trend|slope", lab, ignore.case = TRUE))
                   list(low = "areas where the value is <b>decreasing</b> over the years (blue)",
                        high = "areas where the value is <b>increasing</b> over the years (red)",
                        extra = "Slope is measured per year; near-zero (white) means stable / no real change.")
                 else if (grepl("Coral|benthic", lab, ignore.case = TRUE))
                   list(low = "brighter seabed — consistent with <b>sand or bare rubble</b> (light-reflecting bottoms)",
                        high = "darker seabed — consistent with <b>live coral, algae or seagrass</b> (light-absorbing bottoms)",
                        extra = "Higher = more coral-like cover, lower = more bare substrate. Note the index cannot separate live coral from macroalgae (both are dark) — a high value can mean healthy coral OR algal overgrowth, so pair it with turbidity/chlorophyll before concluding. Deep open water has been removed, so every coloured pixel is genuine shallow reef/seabed.")
                 else if (grepl("NDVI|Vegetation", lab, ignore.case = TRUE))
                   list(low = "little or no vegetation — bare soil, water or built-up ground", high = "dense, healthy green vegetation", extra = "")
                 else if (grepl("LST|Temp", lab, ignore.case = TRUE))
                   list(low = "cooler surfaces (water, shade, vegetation)", high = "hotter surfaces (bare ground, rooftops, pavement)", extra = "Values are in degrees Celsius.")
                 else if (grepl("NDWI|Water", lab, ignore.case = TRUE))
                   list(low = "dry land", high = "open water", extra = "")
                 else if (grepl("NDBI|Built", lab, ignore.case = TRUE))
                   list(low = "vegetation / soil / water", high = "built-up, impervious surfaces (concrete, rooftops)", extra = "")
                 else if (grepl("Population", lab, ignore.case = TRUE))
                   list(low = "sparsely populated", high = "densely populated", extra = "Values are people per pixel.")
                 else if (grepl("Turbidity|NDTI", lab, ignore.case = TRUE))
                   list(low = "clearer water (little suspended sediment)", high = "more turbid water (suspended sediment / runoff / resuspension)", extra = "Higher turbidity over a reef is a stress signal (light reduction, sedimentation).")
                 else if (grepl("Chlorophyll|NDCI", lab, ignore.case = TRUE))
                   list(low = "low chlorophyll-a (oligotrophic, clearer water)", high = "high chlorophyll-a (nutrient enrichment / algal biomass)", extra = "Elevated chlorophyll can indicate nutrient pollution.")
                 else if (grepl("Nighttime|VIIRS", lab, ignore.case = TRUE))
                   list(low = "dark / little human activity at night", high = "bright / intense night-time activity (settlement, tourism, industry)", extra = "A common proxy for economic / tourism activity.")
                 else list(low = sprintf("lower %s", lab), high = sprintf("higher %s", lab), extra = "")
        steps_html <- paste(vapply(steps, function(s)
          sprintf("<li><b>%s</b> — %s</li>", s$label, s$note %||% ""), character(1)), collapse = "")
        cite <- {
          cc <- Filter(function(s) !is.null(s$citation) && nzchar(s$citation), steps)
          if (length(cc)) cc[[length(cc)]]$citation else "Google Earth Engine"
        }
        final_explain <- sprintf(paste0(
          "<div style='font-size:12px;line-height:1.55;color:#3a454d;'>",
          "<p style='margin:4px 0;'><b>How this map was built.</b> The pipeline ran these steps in order, each handing its region/mask to the next:</p>",
          "<ol style='margin:2px 0 8px 18px;'>%s</ol>",
          "<p style='margin:4px 0;'><b>What you are looking at.</b> The colours show <b>%s</b>. On the legend, values run from about <b>%s</b> to <b>%s</b>:</p>",
          "<ul style='margin:2px 0 8px 18px;'>",
          "<li><b>Lower values (~%s):</b> %s.</li>",
          "<li><b>Higher values (~%s):</b> %s.</li>",
          "</ul>",
          "%s",
          "<p style='margin:6px 0 0;color:#5c6b73;'><b>Source &amp; caveat.</b> %s. This is a satellite-derived screening index — treat it as a guide and confirm on the ground before drawing firm conclusions.</p>",
          "</div>"),
          steps_html, lab, lo, hi, lo, bands$low, hi, bands$high,
          if (nzchar(bands$extra)) sprintf("<p style='margin:4px 0;'>%s</p>", bands$extra) else "",
          cite)
      }

      # (class insights are computed together with the composite map above, so the
      #  map's discrete colours and the Area-by-class table always share one classification)

      # Safety net: always give feedback about the composite, even if there was no mappable layer.
      if (is.null(final_caption) && length(steps))
        final_caption <- "No final composite map: this sequence ended without a mappable layer (e.g. only a water-mask or a Trend step). Add an index, Benthic Mapping, or Coral Health step to produce a composite. The per-step maps and Insights above still apply."

      pipeline_state$steps  <- steps
      pipeline_state$status <- do.call(rbind, lapply(steps, function(s)
        data.frame(Step = s$step, Tool = s$label, Role = s$role, Status = s$status, Detail = s$note, stringsAsFactors = FALSE)))
      pipeline_state$final_plot    <- final_plot
      pipeline_state$final_caption <- final_caption
      pipeline_state$final_explain <- final_explain
      pipeline_state$ran_at        <- Sys.time()
      gee_rv$pipeline_final_plot   <- final_plot   # <-- shared channels so Publication Maps can render it
      gee_rv$pipeline_final_bbox   <- final_bbox
      gee_rv$pipeline_final_url    <- final_url     # composite thumbnail URL (for GeoTIFF export)
      # Shared channels for the Export Manager (app.R) — it can only see gee_rv, not pipeline_state.
      gee_rv$pipeline_steps <- steps               # per-step (pre-composite) maps + gallery
      local({
        drx    <- tryCatch(paste(as.character(input$pipe_date[1]), "to", as.character(input$pipe_date[2])), error = function(e) "")
        aggx   <- input$pipe_agg %||% "Median"
        chainx <- if (length(steps)) paste(vapply(steps, function(s) s$label %||% "", character(1)), collapse = " → ") else ""
        gee_rv$pipeline_date_range <- drx; gee_rv$pipeline_agg <- aggx
        gee_rv$pipeline_provenance <- tryCatch(gf_provenance_df(
          area_name = tryCatch(trimws(input$pipe_area_name %||% ""), error = function(e) ""),
          km2 = pipeline_state$area_km2 %||% NA_real_, date_range = drx, agg = aggx,
          dyn_scale = pipeline_state$dyn_scale, thumb_dim = pipeline_state$thumb_dim, chain = chainx,
          datasets = unique(unlist(Filter(function(x) !is.null(x) && nzchar(x), lapply(steps, function(s) s$citation)))),
          generated = format(pipeline_state$ran_at %||% Sys.time(), "%Y-%m-%d %H:%M %Z")), error = function(e) NULL)
      })
      if (!is.null(final_plot)) rv$available_maps <- unique(c(rv$available_maps, "Sequential Pipeline (Final Map)"))

      showNotification(if (is.null(final_plot)) "Sequential pipeline finished — see Status & Results below."
                       else "Pipeline finished — final composite map ready, with a plain-language explanation. It's also selectable in Publication Maps.",
                       type = "message", duration = 8)
    })

    # ---- Individual "Add to Workspace" for every pipeline output. Each button registers a
    #      uniquely-prefixed cart id; the Export Manager (app.R) turns it into a file from the
    #      shared gee_rv channels. Observers are created ONCE here; the dashboard just renders
    #      buttons with these ids (clicks are caught regardless of re-render). ----
    ws_add <- function(id, name) add_to_workspace(paste0(id, "_", as.integer(Sys.time())), name)
    observeEvent(input$ws_cmap,         { if (!is.null(gee_rv$pipeline_final_plot)) ws_add("pipe_cmap", "Pipeline composite map (PNG)") }, ignoreInit = TRUE)
    observeEvent(input$ws_chart_trend,  { if (!is.null(gee_rv$trend_summary$series)) ws_add("pipe_chart_trend", "Pipeline trend chart (PNG)") }, ignoreInit = TRUE)
    observeEvent(input$ws_chart_correl, { if (!is.null(gee_rv$correl_series$series)) ws_add("pipe_chart_correl", "Pipeline correlation scatter (PNG)") }, ignoreInit = TRUE)
    observeEvent(input$ws_tbl_classarea,{ if (!is.null(gee_rv$gee_class_insights$classes)) ws_add("pipe_tbl_classarea", "Pipeline area-by-class (CSV)") }, ignoreInit = TRUE)
    observeEvent(input$ws_tbl_trend,    { if (!is.null(gee_rv$trend_summary$series)) ws_add("pipe_tbl_trend", "Pipeline trend table (CSV)") }, ignoreInit = TRUE)
    observeEvent(input$ws_tbl_correl,   { if (!is.null(gee_rv$correl_series$corr)) ws_add("pipe_tbl_correl", "Pipeline correlation matrix (CSV)") }, ignoreInit = TRUE)
    observeEvent(input$ws_tbl_aca,      { if (!is.null(gee_rv$aca_summary$classes)) ws_add("pipe_tbl_aca", "Pipeline ACA habitat (CSV)") }, ignoreInit = TRUE)
    observeEvent(input$ws_tbl_prov,     { if (!is.null(gee_rv$pipeline_provenance)) ws_add("pipe_tbl_prov", "Pipeline reproducibility (CSV)") }, ignoreInit = TRUE)
    # Per-step (pre-composite) map buttons: fixed pool of observers keyed by step index.
    for (i in seq_len(20)) local({ ii <- i
      observeEvent(input[[paste0("ws_smap_", ii)]], {
        st <- (gee_rv$pipeline_steps %||% list())[[ii]]
        if (!is.null(st) && !is.null(st$url) && nzchar(st$url))
          ws_add(paste0("pipe_smap_", ii), paste0("Pre-composite map: ", st$label %||% paste("Step", ii)))
      }, ignoreInit = TRUE)
    })

    # ---- Results: status table + per-step map cards (legend + insight each) ----
    output$pipeline_results <- renderUI({
      steps <- pipeline_state$steps
      if (!length(steps)) return(NULL)
      badge <- function(s) {
        col <- switch(s, computed = "#45936f", failed = "#8b3a2b", skipped = "#8a6d3b", queued = "#6b4c7a", "#5c6b73")
        sprintf("<span style='background:%s;color:#fff;font-size:10px;font-weight:700;padding:2px 7px;border-radius:9px;text-transform:uppercase;'>%s</span>", col, s)
      }
      # Per-map legend: gradient bar (index/mask) or a descriptive line (RGB composite).
      legend_html <- function(lg) {
        if (is.null(lg)) return("")
        if (identical(lg$type, "composite"))
          return(sprintf("<div style='font-size:10px;color:#5c6b73;margin-top:5px;'>%s</div>", lg$text))
        grad <- paste(lg$pal, collapse = ",")
        sprintf(paste0("<div style='margin-top:6px;'>",
                       "<div style='height:9px;border-radius:3px;border:1px solid #ccc;background:linear-gradient(to right,%s);'></div>",
                       "<div style='display:flex;justify-content:space-between;font-size:9px;color:#5c6b73;margin-top:1px;'><span>%s</span><span>%s</span></div>",
                       "<div style='font-size:9px;color:#8a97a0;text-align:center;'>%s</div></div>"),
                grad, lg$min, lg$max, lg$unit)
      }
      cards <- Filter(function(s) !is.null(s$url), steps)
      tagList(
        hr(style = "margin:16px 0;"),
        h5("Status & Results", style = "font-weight:600;color:#26333e;font-size:13px;"),
        div(style = "overflow-x:auto;",
          tags$table(class = "table table-sm", style = "font-size:12px;",
            tags$thead(tags$tr(tags$th("#"), tags$th("Tool"), tags$th("Status"), tags$th("Detail"))),
            tags$tbody(lapply(steps, function(s) tags$tr(
              tags$td(s$step),
              tags$td(HTML(sprintf("<b>%s</b>", s$label))),
              tags$td(HTML(badge(s$status))),
              tags$td(s$note)
            )))
          )
        ),
        if (length(cards)) tagList(
          h6("Per-step maps, legends & insights", style = "font-weight:600;color:#26333e;font-size:12px;margin-top:10px;"),
          div(style = "display:flex;flex-wrap:wrap;gap:14px;margin-top:6px;",
            lapply(cards, function(s) {
              ins <- character(0)
              if (!is.na(s$mean)) ins <- c(ins, sprintf("Mean = %s", s$mean))
              if (!is.null(s$citation) && nzchar(s$citation)) ins <- c(ins, s$citation)
              div(style = "width:236px;border:1px solid #e2e6ea;border-radius:6px;padding:8px;background:#fbfcfd;",
                  tags$img(src = s$url, style = "width:100%;border:1px solid #d8d4c8;border-radius:4px;"),
                  div(HTML(sprintf("<b>%d.</b> %s", s$step, s$label)), style = "font-size:12px;color:#26333e;margin-top:5px;"),
                  HTML(legend_html(s$legend)),
                  if (!is.null(s$mask_url)) div(style = "margin-top:6px;",
                      tags$img(src = s$mask_url, style = "width:100%;border:1px solid #d8d4c8;border-radius:4px;background:#26333e;"),
                      div(HTML("&#9632; Published mask (highlighted cyan)"), style = "font-size:9px;color:#00849b;text-align:center;margin-top:2px;")),
                  if (length(ins)) div(paste(ins, collapse = "  ·  "), style = "font-size:10px;color:#5c6b73;margin-top:5px;line-height:1.35;")
              )
            })
          )
        )
      )
    })

    # ---- Final composite map (end product of the chain) + Publication-Maps note ----
    output$pipeline_final_header <- renderUI({
      if (is.null(pipeline_state$status)) return(NULL)
      tagList(
        hr(style = "margin:18px 0 8px;"),
        h5("Final Composite Map", style = "font-weight:600;color:#26333e;font-size:13px;"),
        p(HTML("The end product of the whole chain — the last analysis, clipped to the pipeline's accumulated mask. Also available as a source in <b>Publication Maps</b> (“Sequential Pipeline (Final Map)”)."),
          style = "font-size:11px;color:#5c6b73;margin-bottom:6px;")
      )
    })
    output$pipeline_final_map <- renderPlot({
      shiny::validate(shiny::need(pipeline_state$final_plot,
        "Run a sequence to generate the final composite map (built from the last analysis, clipped to the pipeline's accumulated mask)."))
      pipeline_state$final_plot
    })
    output$pipeline_final_caption <- renderUI({
      req(pipeline_state$final_caption)
      p(pipeline_state$final_caption, style = "font-size:11px;color:#5c6b73;font-style:italic;margin-top:6px;")
    })
    output$pipeline_final_explain <- renderUI({
      req(pipeline_state$final_explain)
      div(style = "margin-top:10px;border:1px solid #e2e6ea;border-radius:6px;background:#fbfcfd;padding:12px 14px;",
          h6("Reading this map", style = "font-weight:700;color:#26333e;font-size:12px;margin:0 0 6px;"),
          HTML(pipeline_state$final_explain))
    })

    # =====================================================================
    # INSIGHTS & ANALYTICS — passive research-metrics dashboard for GEE. The
    # continuous-data analog of the LULC Insights tab: descriptive statistics,
    # area-by-value-band (proportional), spatial context, plus the pipeline
    # run summary. Reads ONLY already-computed values (gee_rv$zonal_data /
    # hist_data via gee_insights_payload, and pipeline_state$steps). No new EE.
    # =====================================================================
    output$gee_insights_dash <- renderUI({
      pl    <- tryCatch(gee_insights_payload(), error = function(e) NULL)
      steps <- pipeline_state$steps
      cins  <- gee_rv$gee_class_insights
      has_analysis <- !is.null(pl) && (isTRUE(is.finite(pl$mean)) || !is.null(pl$hist))
      if (!has_analysis && !length(steps) && is.null(cins))
        return(div(style = "font-size:12px;color:#8b3a2b;", "Run a Cloud Analysis (Step 2) or a Sequential Pipeline first — this tab then summarizes it automatically (nothing to click)."))

      box <- function(val, lab, col) div(style = sprintf("flex:1;min-width:130px;background:#f7f6f2;border-radius:6px;padding:12px;border-left:4px solid %s;", col),
        div(val, style = "font-size:19px;font-weight:700;color:#26333e;"), div(lab, style = "font-size:11px;color:#5c6b73;"))

      # ---- Continuous -> discrete class insights (the LULC-style per-class area) ----
      class_section <- NULL
      if (!is.null(cins) && !is.null(cins$classes) && nrow(cins$classes) > 0) {
        cd <- cins$classes; pal5 <- if (!is.null(cins$palette)) cins$palette else c("#2c7bb6", "#abd9e9", "#ffffbf", "#fdae61", "#d7191c")
        has_px <- "Pixels" %in% names(cd) && any(is.finite(cd$Pixels))
        crows <- vapply(seq_len(nrow(cd)), function(i) {
          m2  <- if (is.na(cd$Area_m2[i]))  "—" else formatC(cd$Area_m2[i],  format = "f", digits = 0, big.mark = ",")
          ha  <- if (is.na(cd$Area_ha[i]))  "—" else formatC(cd$Area_ha[i],  format = "f", digits = 1, big.mark = ",")
          km2 <- if (is.na(cd$Area_km2[i])) "—" else formatC(cd$Area_km2[i], format = "f", digits = 3, big.mark = ",")
          px  <- if (has_px) sprintf("<td>%s</td>", if (is.finite(cd$Pixels[i])) formatC(cd$Pixels[i], format = "f", digits = 0, big.mark = ",") else "—") else ""
          sprintf("<tr><td><span style='display:inline-block;width:11px;height:11px;border-radius:2px;background:%s;margin-right:6px;'></span><b>%s</b></td><td>%s</td><td>%s</td><td>%s</td><td>%s</td>%s<td>%.1f%%</td></tr>",
                  pal5[((i - 1) %% length(pal5)) + 1], cd$Class[i], cd$Range[i], m2, ha, km2, px, cd$Pct[i])
        }, character(1))
        dom <- cd$Class[which.max(cd$Pct)]
        px_head <- if (has_px) "<th>Pixels</th>" else ""
        div_html <- gf_diversity_html(landscape_diversity(cd$Area_km2), scope = "value classes")
        patch_html <- gf_patch_html(gee_rv$gee_patch_metrics)
        patch_err  <- if (nzchar(patch_html) && !is.null(gee_rv$gee_patch_error))
          sprintf("<div style='font-size:10px;color:#8b3a2b;margin-top:-4px;'>Some patch metrics show “—”: Earth Engine could not compute them (%s).</div>", gee_rv$gee_patch_error) else ""
        class_section <- tagList(
          h5(sprintf("Class-based Area — %s", cins$feature %||% "index"), style = "color:#3d4f5c;font-weight:600;margin-top:2px;"),
          p(HTML(sprintf("%s Dominant class: <b>%s</b> (%.1f%% of the area).",
                         cins$blurb %||% "The continuous layer was split into 5 equal-interval classes and the <b>area of each class</b> measured.",
                         dom, max(cd$Pct))), style = "font-size:12px;color:#5c6b73;"),
          div(style = "overflow-x:auto;",
            HTML(sprintf("<table class='table table-sm' style='font-size:12px;'><thead><tr><th>Class</th><th>Value range</th><th>Area (m²)</th><th>Area (ha)</th><th>Area (km²)</th>%s<th>Share</th></tr></thead><tbody>%s</tbody></table>", px_head, paste(crows, collapse = "")))),
          if (nzchar(div_html)) HTML(div_html),
          if (nzchar(patch_html)) HTML(patch_html),
          if (nzchar(patch_err)) HTML(patch_err))
      }
      fmt <- function(v, u = "") if (is.null(v) || length(v) != 1 || !is.finite(v)) "—" else paste0(formatC(v, format = "f", digits = 2, big.mark = ","), if (nzchar(u)) paste0(" ", u) else "")
      out <- list()
      if (!is.null(class_section)) out <- c(out, list(class_section, hr(style = "margin:14px 0;")))

      # ---- Coral Habitat (Allen Coral Atlas) — authoritative benthic reference ----
      aca <- gee_rv$aca_summary
      if (!is.null(aca)) {
        if (!is.null(aca$classes) && nrow(aca$classes) > 0) {
          ad <- aca$classes
          arows <- vapply(seq_len(nrow(ad)), function(i) sprintf(
            "<tr><td><span style='display:inline-block;width:11px;height:11px;border-radius:2px;background:%s;margin-right:6px;'></span><b>%s</b></td><td>%s</td><td>%s</td><td>%.1f%%</td></tr>",
            ad$Color[i], ad$Class[i], formatC(ad$Area_km2[i], format = "f", digits = 3, big.mark = ","),
            formatC(ad$Area_ha[i], format = "f", digits = 1, big.mark = ","), ad$Pct[i]), character(1))
          coral_line <- if (isTRUE(is.finite(aca$coral_km2)))
            sprintf("<b style='color:#c0392b;'>Coral/Algae: %.3f km² (%.1f%% of mapped habitat)</b>", aca$coral_km2, aca$coral_pct)
          else "No Coral/Algae class mapped here."
          out <- c(out, list(
            h5("Coral Habitat — Allen Coral Atlas", style = "color:#3d4f5c;font-weight:600;margin-top:2px;"),
            HTML(sprintf("<p style='font-size:12px;color:#5c6b73;margin:2px 0 6px;'>Authoritative, expert- &amp; field-validated benthic habitat map — this tells you <b>where coral actually is</b> (unlike our brightness index). %s</p>", coral_line)),
            div(style = "overflow-x:auto;", HTML(sprintf("<table class='table table-sm' style='font-size:12px;'><thead><tr><th>Benthic class</th><th>Area (km²)</th><th>Area (ha)</th><th>Share</th></tr></thead><tbody>%s</tbody></table>", paste(arows, collapse = "")))),
            HTML("<p style='font-size:10px;color:#8a97a0;'>Source: Allen Coral Atlas (ACA/reef_habitat/v2_0). “Coral/Algae” is a combined class — satellite mapping cannot reliably split live coral from algae. Use this as the habitat reference and our indices/patch metrics for condition &amp; change monitoring.</p>"),
            hr(style = "margin:14px 0;")))
        } else {
          out <- c(out, list(
            h5("Coral Habitat — Allen Coral Atlas", style = "color:#3d4f5c;font-weight:600;margin-top:2px;"),
            HTML("<p style='font-size:12px;color:#8b3a2b;'>Allen Coral Atlas has <b>no mapped benthic habitat</b> for this area — it only covers shallow tropical/sub-tropical reefs. Use the index-based Coral Health step instead here.</p>"),
            hr(style = "margin:14px 0;")))
        }
      }

      # ---- Tourism -> water quality -> coral correlation ----
      cs <- gee_rv$correl_series
      if (!is.null(cs) && !is.null(cs$corr)) {
        snames <- names(cs$series); yrs <- cs$years
        shead <- paste0("<th>", snames, "</th>", collapse = "")
        srows <- vapply(seq_along(yrs), function(i) {
          cells <- paste0(vapply(snames, function(nm) { v <- cs$series[[nm]][i]; sprintf("<td>%s</td>", if (isTRUE(is.finite(v))) formatC(v, format = "f", digits = 3) else "—") }, character(1)), collapse = "")
          sprintf("<tr><td><b>%d</b></td>%s</tr>", yrs[i], cells)
        }, character(1))
        lag_html <- ""
        if (!is.null(cs$lags) && length(cs$lags)) {
          lrows <- vapply(cs$lags, function(L) { b <- L$lag$best; if (is.null(b) || !is.finite(b$r)) return("")
            sprintf("<tr><td><b>%s</b></td><td>%d yr</td><td>%.2f</td><td>%.3f</td></tr>", L$driver, b$lag, b$r, b$p) }, character(1))
          lrows <- lrows[nzchar(lrows)]
          if (length(lrows)) lag_html <- paste0(
            "<h6 style='font-weight:600;color:#26333e;font-size:12px;margin:8px 0 2px;'>Best lead–lag (does the driver lead coral?)</h6>",
            "<table class='table table-sm' style='font-size:11px;'><thead><tr><th>Driver</th><th>Lead</th><th>r</th><th>p</th></tr></thead><tbody>", paste(lrows, collapse = ""), "</tbody></table>")
        }
        out <- c(out, list(
          hr(style = "margin:14px 0;"),
          h5("Cross-Indicator Correlation", style = "color:#3d4f5c;font-weight:600;"),
          HTML(gf_correlation_html(cs$corr, response = cs$response)),
          HTML(paste0("<h6 style='font-weight:600;color:#26333e;font-size:12px;margin:8px 0 2px;'>Annual indicator series</h6>",
                      "<div style='overflow-x:auto;'><table class='table table-sm' style='font-size:11px;'><thead><tr><th>Year</th>", shead, "</tr></thead><tbody>",
                      paste(srows, collapse = ""), "</tbody></table></div>")),
          HTML(lag_html)))
      }

      # ---- Continuous cloud-analysis metrics (independent runs) ----
      if (has_analysis) {
        u   <- pl$units %||% ""
        rng <- if (isTRUE(is.finite(pl$max)) && isTRUE(is.finite(pl$min))) pl$max - pl$min else NA_real_
        cv  <- if (isTRUE(is.finite(pl$sd)) && isTRUE(is.finite(pl$mean)) && pl$mean != 0) abs(pl$sd / pl$mean) * 100 else NA_real_
        km2 <- tryCatch(pl$spatial$km2, error = function(e) NA_real_)

        band_html <- tryCatch({
          if (!is.null(pl$hist) && nrow(pl$hist) > 0 && isTRUE(is.finite(pl$min)) && isTRUE(is.finite(pl$max)) && pl$max > pl$min) {
            h <- pl$hist; brks <- pl$min + c(0, 1/3, 2/3, 1) * (pl$max - pl$min)
            band <- cut(h$bin_mid, breaks = brks, include.lowest = TRUE, labels = c("Low", "Medium", "High"))
            agg  <- tapply(h$value, band, sum, na.rm = TRUE); tot <- sum(h$value, na.rm = TRUE)
            if (isTRUE(is.finite(tot)) && tot > 0) {
              rows <- vapply(c("Low", "Medium", "High"), function(b) {
                cnt <- agg[[b]]; if (is.null(cnt) || is.na(cnt)) cnt <- 0
                lo <- switch(b, Low = brks[1], Medium = brks[2], High = brks[3]); hi <- switch(b, Low = brks[2], Medium = brks[3], High = brks[4])
                sprintf("<tr><td><b>%s</b></td><td>%.2f to %.2f %s</td><td>%.1f%%</td></tr>", b, lo, hi, u, 100 * cnt / tot)
              }, character(1))
              sprintf("<table class='table table-sm' style='font-size:12px;'><thead><tr><th>Band</th><th>Value range</th><th>Share of area</th></tr></thead><tbody>%s</tbody></table>", paste(rows, collapse = ""))
            } else ""
          } else ""
        }, error = function(e) "")

        out <- c(out, list(
          h5(sprintf("Latest Cloud Analysis — %s", pl$feature %||% "index"), style = "color:#3d4f5c;font-weight:600;margin-top:2px;"),
          div(style = "display:flex;gap:12px;flex-wrap:wrap;margin-bottom:12px;",
            box(fmt(km2), "Study area (km²)", "#2c7a6b"),
            box(fmt(pl$mean, u), "Regional mean", "#45936f"),
            box(fmt(pl$sd, u), "Std dev (spread)", "#3a6aa0"),
            box(if (isTRUE(is.finite(cv))) sprintf("%.1f%%", cv) else "—", "Heterogeneity (CV)", "#6b4c7a")),
          h6("1 · Descriptive Statistics (hard numbers)", style = "font-weight:600;color:#26333e;font-size:12px;"),
          HTML(sprintf("<table class='table table-sm' style='font-size:12px;'><tbody><tr><td>Minimum</td><td><b>%s</b></td><td>Maximum</td><td><b>%s</b></td></tr><tr><td>Mean</td><td><b>%s</b></td><td>Median</td><td><b>%s</b></td></tr><tr><td>Std deviation</td><td><b>%s</b></td><td>Range</td><td><b>%s</b></td></tr></tbody></table>",
            fmt(pl$min, u), fmt(pl$max, u), fmt(pl$mean, u), fmt(pl$median, u), fmt(pl$sd, u), fmt(rng, u))),
          if (nzchar(band_html)) tagList(
            h6("2 · Area by Value Band (proportional)", style = "font-weight:600;color:#26333e;font-size:12px;margin-top:8px;"),
            HTML(band_html),
            p("Splits the study area into low / medium / high thirds of the value range — the continuous-data analog of per-class area.", style = "font-size:10px;color:#8a97a0;"))
        ))
      }

      # ---- Rate of Change (Temporal Dynamics) — from Trend Analysis ----
      ts <- gee_rv$trend_summary
      out <- c(out, list(hr(style = "margin:14px 0;"),
        h5("Rate of Change (Temporal Dynamics)", style = "color:#3d4f5c;font-weight:600;")))
      if (!is.null(ts)) {
        rt   <- ts$rate
        dir  <- if (isTRUE(rt > 0)) "increased" else if (isTRUE(rt < 0)) "decreased" else "stayed roughly flat"
        col  <- if (isTRUE(rt > 0)) "#45936f" else if (isTRUE(rt < 0)) "#8b3a2b" else "#5c6b73"
        rate <- if (isTRUE(is.finite(rt))) formatC(abs(rt), format = "f", digits = 4, big.mark = ",") else "—"
        r2t  <- if (!is.null(ts$r2) && isTRUE(is.finite(ts$r2))) sprintf(" (R² = %.2f — how straight-line the change was)", ts$r2) else ""
        out <- c(out, list(HTML(sprintf(
          "<p style='font-size:12px;color:#3a454d;line-height:1.5;'>Regional average of <b>%s</b> over <b>%d–%d</b> <b style='color:%s;'>%s by %s per year</b>%s.</p>",
          ts$feature, ts$start_year, ts$end_year, col, dir, rate, r2t))))
        # ---- Non-parametric significance: Mann-Kendall + Theil-Sen (research standard) ----
        mk <- ts$mk
        if (!is.null(mk)) {
          verdict <- if (isTRUE(mk$significant))
            sprintf("<b style='color:%s;'>statistically significant</b>", col)
          else "<b style='color:#8a97a0;'>not statistically significant</b>"
          sen_txt <- if (isTRUE(is.finite(mk$sen_slope))) formatC(mk$sen_slope, format = "f", digits = 4, big.mark = ",") else "—"
          out <- c(out, list(div(style = "background:#f2f5f7;border-radius:4px;padding:8px 10px;margin-bottom:6px;",
            HTML(sprintf(paste0(
              "<div style='font-size:12px;color:#3a454d;line-height:1.55;'>",
              "<b>Trend test (Mann-Kendall):</b> &tau; = <b>%.2f</b>, p = <b>%.3f</b> over %d years — the trend is %s (p %s 0.05).<br>",
              "<b>Theil-Sen slope:</b> <b>%s per year</b> (median of pairwise slopes — robust to outliers; compare with the ordinary least-squares %s/yr above).",
              "</div>"),
              mk$tau, mk$p_value, mk$n, verdict, if (isTRUE(mk$p_value < 0.05)) "&lt;" else "&ge;",
              sen_txt, rate)))))
        }
        # yearly regional values (the actual time series)
        if (!is.null(ts$series) && nrow(ts$series) > 0) {
          srows <- vapply(seq_len(nrow(ts$series)), function(i) sprintf("<tr><td>%d</td><td>%s</td></tr>", ts$series$Year[i], formatC(ts$series$Value[i], format = "f", digits = 4)), character(1))
          out <- c(out, list(div(style = "overflow-x:auto;margin-bottom:6px;",
            HTML(sprintf("<table class='table table-sm' style='font-size:12px;max-width:260px;'><thead><tr><th>Year</th><th>Regional mean</th></tr></thead><tbody>%s</tbody></table>", paste(srows, collapse = ""))))))
        }
        # Cloud transparency: how cloudy the raw scenes were (before per-pixel masking)
        ci <- ts$cloud_info
        if (!is.null(ci) && isTRUE(is.finite(ci$cloud)))
          out <- c(out, list(div(style = "font-size:11px;color:#5c6b73;background:#f2f5f7;border-radius:4px;padding:7px 9px;margin-bottom:6px;",
            HTML(sprintf("<b>Cloud handling:</b> built from <b>%s Sentinel-2 scenes</b> across %d–%d, whose average scene cloud cover was <b>%.0f%%</b>. Clouds were removed <b>per pixel</b> (SCL band) and the clear pixels median-composited each year — so cloudy scenes still contribute their clear parts (no year is thrown away).",
              if (!is.null(ci$n)) formatC(ci$n, format = "d", big.mark = ",") else "several", ts$start_year, ts$end_year, ci$cloud)))))
        if (!is.null(ts$thumb))
          out <- c(out, list(div(style = "margin:4px 0;", tags$img(src = ts$thumb, style = "max-width:280px;border:1px solid #d8d4c8;border-radius:4px;background:#eaeef1;"),
                                  div("Per-pixel trend map — slope/yr (blue = decreasing, red = increasing)", style = "font-size:10px;color:#5c6b73;"))))
        if (!is.null(ts$sig_pct) && !is.na(ts$sig_pct))
          out <- c(out, list(HTML(sprintf("<p style='font-size:12px;color:#3a454d;'><b>%.1f%%</b> of the region shows a <b>statistically significant</b> trend (Mann-Kendall, p&lt;0.05) — the rest is within noise.</p>", ts$sig_pct))))
        # Area where the index is decreasing / stable / increasing — reef pixels ONLY,
        # with a plain-language meaning column so the table is self-explanatory.
        adf <- ts$area_df
        if (!is.null(adf) && nrow(adf) > 0) {
          is_benthic <- grepl("Coral|benthic|bottom", ts$feature %||% "", ignore.case = TRUE)
          gloss <- function(cl) {
            if (identical(cl, "Decreasing")) { if (is_benthic) "Coral Health fell — seabed got brighter, shift toward sand / bare rubble (less coral/algae cover)" else "index values fell over the years" }
            else if (identical(cl, "Increasing")) { if (is_benthic) "Coral Health rose — seabed got darker, shift toward coral / algae / seagrass (more cover)" else "index values rose over the years" }
            else "little or no change (within noise)"
          }
          colr <- function(cl) if (identical(cl, "Decreasing")) "#2c7bb6" else if (identical(cl, "Increasing")) "#d7191c" else "#8a97a0"
          arows <- vapply(seq_len(nrow(adf)), function(i) sprintf(
            "<tr><td><span style='display:inline-block;width:10px;height:10px;border-radius:2px;background:%s;margin-right:6px;'></span><b>%s</b></td><td style='color:#5c6b73;'>%s</td><td>%s</td><td>%s</td><td><b>%.1f%%</b></td></tr>",
            colr(adf$Class[i]), adf$Class[i], gloss(adf$Class[i]),
            formatC(adf$Area_m2[i], format = "f", digits = 0, big.mark = ","),
            formatC(adf$Area_km2[i], format = "f", digits = 3, big.mark = ","), adf$Pct[i]), character(1))
          out <- c(out, list(
            HTML("<p style='font-size:11px;color:#5c6b73;margin:6px 0 3px;'>Measured over the <b>reef / analysis pixels only</b> (deep water &amp; land excluded). “Stable” = per-pixel slope within &plusmn;10% of the strongest slope in the area, i.e. no meaningful change. Shares add up across reef pixels only.</p>"),
            div(style = "overflow-x:auto;",
              HTML(sprintf("<table class='table table-sm' style='font-size:12px;'><thead><tr><th>Direction</th><th>What it means</th><th>Area (m²)</th><th>Area (km²)</th><th>Share of reef</th></tr></thead><tbody>%s</tbody></table>", paste(arows, collapse = ""))))))
        }
      } else {
        # If a Trend step ran but failed, its reason is in the status table — point there.
        tfail <- Filter(function(s) identical(s$role, "trend") && identical(s$status, "failed"), pipeline_state$steps %||% list())
        msg <- if (length(tfail))
          sprintf("The <b>Trend</b> step didn't complete: <i>%s</i> See the <b>Status &amp; Results</b> table above. Tip: widen the Trend years, or trend a fuller-coverage index (e.g. NDVI) to confirm.", tfail[[1]]$note %||% "no detail")
        else
          "No trend yet. Add <b>Trend Analysis</b> to a Sequential Pipeline (set the <b>Trend years</b> in the pipeline panel), or run the Trend Analysis tab. The Temporal Grid also visualizes change across years."
        out <- c(out, list(div(style = "font-size:12px;color:#8b3a2b;", HTML(msg))))
      }

      # ---- Validation & Robustness — statistical uncertainty of the estimate ----
      zget <- function(metric) {
        zd <- gee_rv$zonal_data
        if (is.null(zd) || !("Metric" %in% names(zd))) return(NA_real_)
        v <- suppressWarnings(as.numeric(zd$Value[zd$Metric == metric])); if (length(v)) v[1] else NA_real_
      }
      ci95 <- zget("95% CI (±)"); vpix <- zget("Valid Pixels"); sdv <- zget("Std. Deviation"); mnv <- zget("Mean")
      if (isTRUE(is.finite(ci95)) || isTRUE(is.finite(vpix))) {
        uu2 <- if (!is.null(pl)) (pl$units %||% "") else ""
        cv2 <- if (isTRUE(is.finite(sdv)) && isTRUE(is.finite(mnv)) && mnv != 0) sprintf("%.1f%%", abs(sdv / mnv) * 100) else "—"
        sensor <- tryCatch(gee_rv$sensor_used[[gee_rv$current_feature]], error = function(e) NULL)
        out <- c(out, list(
          hr(style = "margin:14px 0;"),
          h5("Validation & Robustness", style = "color:#3d4f5c;font-weight:600;"),
          HTML(sprintf("<ul style='margin:2px 0 4px 18px;font-size:12px;color:#3a454d;line-height:1.6;'><li>Regional mean estimate: <b>%s &plusmn; %s</b> (95%% confidence interval).</li><li>Computed from <b>%s</b> valid pixels; spatial spread (CV) <b>%s</b>.</li>%s</ul>",
            fmt(mnv, uu2), fmt(ci95, uu2),
            if (isTRUE(is.finite(vpix))) formatC(vpix, format = "d", big.mark = ",") else "—", cv2,
            if (!is.null(sensor) && nzchar(sensor)) sprintf("<li>Source imagery: <b>%s</b>.</li>", sensor) else "")),
          p("Continuous indices have no class ground-truth, so classification accuracy / Kappa / F1 don't apply — the honest robustness measure here is the statistical uncertainty of the estimate. (Pixels are spatially correlated, so this CI is an optimistic lower bound.) For classification accuracy, use LULC → Insights.",
            style = "font-size:10px;color:#8a97a0;")))
      }

      # ---- Sequential Pipeline run summary ----
      if (length(steps)) {
        comp <- sum(vapply(steps, function(s) identical(s$status, "computed"), logical(1)))
        prows <- vapply(steps, function(s) {
          rng2 <- if (!is.null(s$legend) && identical(s$legend$type, "gradient")) sprintf("%s to %s", s$legend$min, s$legend$max) else "—"
          mn2  <- if (!is.na(s$mean)) as.character(s$mean) else "—"
          msk  <- if (!is.null(s$mask_url)) "yes" else "—"
          sprintf("<tr><td>%d</td><td><b>%s</b></td><td>%s</td><td>%s</td><td>%s</td><td>%s</td></tr>", s$step, s$label, s$role, mn2, rng2, msk)
        }, character(1))
        out <- c(out, list(
          hr(style = "margin:14px 0;"),
          h5("Latest Sequential Pipeline Run", style = "color:#3d4f5c;font-weight:600;"),
          div(style = "font-size:11px;color:#5c6b73;margin-bottom:6px;", sprintf("%d steps · %d computed layers.", length(steps), comp)),
          div(style = "overflow-x:auto;", HTML(sprintf("<table class='table table-sm' style='font-size:12px;'><thead><tr><th>#</th><th>Layer</th><th>Role</th><th>Regional mean</th><th>Value range</th><th>Mask</th></tr></thead><tbody>%s</tbody></table>", paste(prows, collapse = ""))))
        ))
      }

      # ---- Report-matching sections: the dashboard now carries the SAME content as the
      #      downloadable HTML report (shared exec-summary/methods/provenance/chart builders),
      #      so the two never diverge. Composite map + charts are rasterised locally (no EE);
      #      gallery thumbnails load in the browser from each step's URL (no server fetch).
      uri_of <- function(pl, w = 6.6, h = 3.4) tryCatch({
        if (is.null(pl)) return(NULL)
        tf <- tempfile(fileext = ".png"); ggplot2::ggsave(tf, pl, width = w, height = h, dpi = 130, bg = "white")
        paste0("data:image/png;base64,", jsonlite::base64_enc(readBin(tf, "raw", file.info(tf)$size)))
      }, error = function(e) NULL)
      pp    <- tryCatch(pipe_report_payload(), error = function(e) NULL)
      aca_d <- gee_rv$aca_summary; cs <- gee_rv$correl_series
      dr    <- tryCatch(paste(as.character(input$pipe_date[1]), "to", as.character(input$pipe_date[2])), error = function(e) "")
      agg   <- input$pipe_agg %||% "Median"
      chain <- if (length(steps)) paste(vapply(steps, function(s) s$label %||% "", character(1)), collapse = " → ") else ""
      used_aca_coral  <- any(vapply(steps, function(s) grepl("Allen Coral Atlas", s$label %||% ""), logical(1)))
      used_coral_step <- any(vapply(steps, function(s) grepl("Coral Health", s$label %||% ""), logical(1)))
      marine_used <- any(vapply(steps, function(s) grepl("Coral Health|Benthic", s$label %||% ""), logical(1)))
      trend_used  <- !is.null(pp) && !is.null(pp$trend); correl_used <- !is.null(cs) && !is.null(cs$corr)

      es_head <- tryCatch(cins$feature, error = function(e) NULL)
      if (is.null(es_head) || !nzchar(es_head)) es_head <- pipeline_state$final_caption
      exec_html <- gf_pipeline_exec_summary(headline = es_head, area_km2 = pipeline_state$area_km2 %||% NA_real_,
        date_range = dr, aca = aca_d, aca_over_coral = isTRUE(used_aca_coral && used_coral_step),
        trend = if (!is.null(pp)) pp$trend else NULL,
        correl = if (correl_used) list(r = cs$corr$r, p = cs$corr$p, response = cs$response) else NULL)
      head_front <- list(HTML(paste0("<div style='background:#eef4f1;border-left:4px solid #2c5a4a;border-radius:4px;padding:10px 14px;margin-bottom:12px;'><div style='font-size:11px;font-weight:700;letter-spacing:.06em;text-transform:uppercase;color:#2c5a4a;margin-bottom:4px;'>Summary</div>", exec_html, "</div>")))
      # Individual "Add to Workspace" buttons — one per output (composite + each pre-composite
      # map, every table, each chart). Real actionButtons so Shiny binds the clicks on re-render.
      wsbtn <- function(bid, lbl) actionButton(ns(bid), lbl, class = "action-button",
        style = "font-size:10px;padding:3px 8px;margin:2px;background:#eef4f1;border:1px solid #cfe0d8;color:#2c5a4a;border-radius:4px;")
      btns <- list()
      if (!is.null(gee_rv$pipeline_final_plot)) btns <- c(btns, list(wsbtn("ws_cmap", "＋ Composite map")))
      for (i in seq_along(steps)) { s <- steps[[i]]
        if (!is.null(s$url) && nzchar(s$url)) btns <- c(btns, list(wsbtn(paste0("ws_smap_", i), paste0("＋ Map: ", s$label %||% paste("Step", i))))) }
      if (!is.null(cins) && !is.null(cins$classes)) btns <- c(btns, list(wsbtn("ws_tbl_classarea", "＋ Area-by-class")))
      if (trend_used && !is.null(pp$trend$series)) btns <- c(btns, list(wsbtn("ws_chart_trend", "＋ Trend chart"), wsbtn("ws_tbl_trend", "＋ Trend table")))
      if (correl_used) btns <- c(btns, list(wsbtn("ws_chart_correl", "＋ Correlation scatter"), wsbtn("ws_tbl_correl", "＋ Correlation matrix")))
      if (!is.null(aca_d) && !is.null(aca_d$classes)) btns <- c(btns, list(wsbtn("ws_tbl_aca", "＋ ACA habitat")))
      btns <- c(btns, list(wsbtn("ws_tbl_prov", "＋ Reproducibility")))
      head_front <- c(head_front, list(div(style = "background:#f7f6f2;border:1px dashed #cfe0d8;border-radius:6px;padding:8px 10px;margin-bottom:12px;",
        div("Add individual outputs to Workspace", style = "font-size:11px;font-weight:600;color:#2c5a4a;margin-bottom:4px;"),
        do.call(div, c(list(style = "display:flex;flex-wrap:wrap;"), btns)))))
      cmp_uri <- uri_of(gee_rv$pipeline_final_plot, 7, 6)
      if (!is.null(cmp_uri)) head_front <- c(head_front, list(HTML(paste0(
        "<div style='margin-bottom:10px;'><img src='", cmp_uri, "' style='max-width:100%;border:1px solid #d8d4c8;border-radius:4px;'/>",
        if (!is.null(pipeline_state$final_caption)) paste0("<div style='font-size:11px;color:#5c6b73;margin-top:4px;'>", gf_report_escape(pipeline_state$final_caption), "</div>") else "", "</div>"))))
      else if (!is.null(pipeline_state$final_caption)) head_front <- c(head_front, list(HTML(paste0("<p style='font-size:11px;color:#5c6b73;background:#f2f5f7;padding:6px 9px;border-radius:4px;'>", gf_report_escape(pipeline_state$final_caption), "</p>"))))
      if (!is.null(cmp_uri) && !is.null(pipeline_state$final_explain) && nzchar(pipeline_state$final_explain))
        head_front <- c(head_front, list(HTML(pipeline_state$final_explain)))
      gimgs <- Filter(nzchar, vapply(steps, function(s) if (!is.null(s$url) && nzchar(s$url))
        sprintf("<figure style='margin:0;width:150px;'><img src='%s' style='width:100%%;height:110px;object-fit:cover;border:1px solid #d8dde1;border-radius:4px;'/><figcaption style='font-size:10px;color:#3d4f5c;text-align:center;'>%s</figcaption></figure>", s$url, gf_report_escape(s$label %||% "Layer")) else "", character(1)))
      if (length(gimgs)) head_front <- c(head_front, list(HTML(paste0("<h5 style='color:#3d4f5c;font-weight:600;'>Layer gallery</h5><div style='display:flex;flex-wrap:wrap;gap:10px;margin-bottom:10px;'>", paste(gimgs, collapse = ""), "</div>"))))
      out <- c(head_front, out)

      if (trend_used && !is.null(pp$trend$series)) {
        tcu <- uri_of(gf_trend_line_plot(pp$trend$series, label = pp$trend$feature %||% "Value", units = ""))
        if (!is.null(tcu)) out <- c(out, list(HTML(paste0("<h5 style='color:#3d4f5c;font-weight:600;margin-top:14px;'>Trend chart</h5><img src='", tcu, "' style='max-width:100%;'/>"))))
      }
      if (correl_used && !is.null(cs$series)) {
        scu <- uri_of(gf_correlation_scatter_plot(cs$series, cs$years, cs$response), 6.8, 4.2)
        if (!is.null(scu)) out <- c(out, list(HTML(paste0("<h5 style='color:#3d4f5c;font-weight:600;margin-top:14px;'>Correlation scatter</h5><img src='", scu, "' style='max-width:100%;'/>"))))
      }
      if (correl_used) out <- c(out, list(HTML(gf_correlation_caveats_html())))
      out <- c(out, list(HTML(paste0("<h5 style='color:#3d4f5c;font-weight:600;margin-top:16px;'>Methods</h5>",
        gf_pipeline_methods_html(dr, agg, chain, marine_used, trend_used, correl_used)))))
      sens_df <- tryCatch(gf_sensors_used_df(unique(unlist(Filter(function(x) !is.null(x) && nzchar(x), lapply(steps, function(s) s$citation))))), error = function(e) NULL)
      if (!is.null(sens_df) && nrow(sens_df)) out <- c(out, list(HTML(paste0("<h5 style='color:#3d4f5c;font-weight:600;margin-top:16px;'>Sensors &amp; datasets</h5>", gf_report_table(sens_df)))))
      prov <- gf_provenance_df(area_name = tryCatch(trimws(input$pipe_area_name %||% ""), error = function(e) ""),
        km2 = pipeline_state$area_km2 %||% NA_real_, date_range = dr, agg = agg,
        dyn_scale = pipeline_state$dyn_scale, thumb_dim = pipeline_state$thumb_dim, chain = chain,
        datasets = unique(unlist(Filter(function(x) !is.null(x) && nzchar(x), lapply(steps, function(s) s$citation)))),
        bbox = gee_rv$pipeline_final_bbox, versions = gf_runtime_versions(),
        generated = format(pipeline_state$ran_at %||% Sys.time(), "%Y-%m-%d %H:%M %Z"))
      out <- c(out, list(HTML(paste0("<h5 style='color:#3d4f5c;font-weight:600;margin-top:16px;'>Reproducibility</h5>", gf_report_table(prov)))))
      cites <- unique(c(unlist(Filter(function(x) !is.null(x) && nzchar(x), lapply(steps, function(s) s$citation))),
        "Olofsson, P. et al. (2014). Remote Sensing of Environment, 148, 42-57.",
        "Pontius, R.G. & Millones, M. (2011). International Journal of Remote Sensing, 32(15), 4407-4429.",
        gf_method_references(marine = grepl("Coral|Benthic", chain), trend = grepl("Trend", chain), correl = grepl("Correlation", chain), heat = grepl("Heat Stress|SST", chain), aca = grepl("Allen Coral|Coral Habitat", chain))))
      out <- c(out, list(HTML(paste0("<h5 style='color:#3d4f5c;font-weight:600;margin-top:16px;'>Data sources &amp; references</h5><ol style='font-size:11px;color:#5c6b73;margin:0 0 0 16px;'>", paste0("<li>", gf_report_escape(cites), "</li>", collapse = ""), "</ol>"))))
      lims2 <- c("Continuous indices have no per-class ground reference; formal classification accuracy is reported only for the LULC module.",
        if (trend_used || correl_used) sprintf("The temporal analysis spans %s. Short series (few years) have low statistical power, so trends/correlations here may be non-significant; a longer record is needed for firm inference.", dr) else NULL,
        if (trend_used) "If the range includes the current, incomplete year, its value rests on partial-year imagery and can bias the end of the series." else NULL,
        "This analysis was NOT validated against independent field/ground-truth data. Satellite-derived indices and classifications are screening tools: class identities and thresholds are indicative and should be confirmed on the ground before firm conclusions are drawn.")
      out <- c(out, list(HTML(paste0("<div style='margin-top:16px;background:#fbf4ef;border-left:4px solid #8b3a2b;border-radius:4px;padding:8px 14px;'><h5 style='color:#8b3a2b;font-weight:600;margin:0 0 4px;'>Limitations</h5><ul style='font-size:11px;color:#5c6b73;margin:0 0 0 16px;'>", paste0("<li>", gf_report_escape(lims2), "</li>", collapse = ""), "</ul></div>"))))

      tagList(out)
    })

    # =====================================================================
    # PUBLICATION OUTPUTS (Phase 2) — assemble the last pipeline run into a
    # self-contained HTML report and a multi-sheet Excel workbook. Reads only
    # already-computed reactives; no new Earth Engine calls. Honest framing is
    # baked in: every export carries a "no independent field validation" note.
    # =====================================================================
    pipe_report_payload <- reactive({
      pl    <- tryCatch(gee_insights_payload(), error = function(e) NULL)
      cins  <- gee_rv$gee_class_insights
      ts    <- gee_rv$trend_summary
      steps <- pipeline_state$steps
      fnum  <- function(v) if (is.null(v) || length(v) != 1 || !is.finite(v)) NA_real_ else round(v, 4)
      desc <- NULL
      if (!is.null(pl) && isTRUE(is.finite(pl$mean))) {
        desc <- data.frame(
          Metric = c("Feature", "Units", "Minimum", "Maximum", "Mean", "Median", "Std deviation", "Study area (km2)"),
          Value  = c(pl$feature %||% "index", pl$units %||% "",
                     as.character(fnum(pl$min)), as.character(fnum(pl$max)), as.character(fnum(pl$mean)),
                     as.character(fnum(pl$median)), as.character(fnum(pl$sd)),
                     as.character(tryCatch(round(pl$spatial$km2, 2), error = function(e) NA_real_))),
          stringsAsFactors = FALSE)
      }
      carea <- NULL
      if (!is.null(cins) && !is.null(cins$classes) && nrow(cins$classes) > 0) {
        cd <- cins$classes
        carea <- data.frame(Class = cd$Class, `Value range` = cd$Range, `Area (km2)` = cd$Area_km2,
                            `Area (ha)` = cd$Area_ha, `Share (%)` = cd$Pct,
                            check.names = FALSE, stringsAsFactors = FALSE)
      }
      trend <- NULL
      if (!is.null(ts)) {
        mk <- ts$mk
        tstats <- data.frame(
          Statistic = c("OLS slope (per year)", "OLS slope 95% CI", "R-squared", "Mann-Kendall tau", "Mann-Kendall p-value",
                        "Significant (p<0.05)", "Theil-Sen slope (per year)", "Years used"),
          Value = c(as.character(round(ts$rate, 4)),
                    if (isTRUE(is.finite(ts$rate_lo)) && isTRUE(is.finite(ts$rate_hi))) sprintf("%+.4f to %+.4f", ts$rate_lo, ts$rate_hi) else "n/a",
                    as.character(if (!is.null(ts$r2)) round(ts$r2, 3) else NA),
                    as.character(if (!is.null(mk)) round(mk$tau, 3) else NA),
                    as.character(if (!is.null(mk)) round(mk$p_value, 4) else NA),
                    as.character(if (!is.null(mk)) mk$significant else NA),
                    as.character(if (!is.null(mk) && is.finite(mk$sen_slope)) round(mk$sen_slope, 4) else NA),
                    as.character(if (!is.null(mk)) mk$n else if (!is.null(ts$series)) nrow(ts$series) else NA)),
          stringsAsFactors = FALSE)
        cloudtxt <- if (!is.null(ts$cloud_info) && isTRUE(is.finite(ts$cloud_info$cloud)))
          sprintf("Built from %s Sentinel-2 scenes across %d-%d; mean scene cloud cover %.0f%% (removed per-pixel via SCL; clear pixels median-composited).",
                  if (!is.null(ts$cloud_info$n)) ts$cloud_info$n else "several", ts$start_year, ts$end_year, ts$cloud_info$cloud) else ""
        is_benthic <- grepl("Coral|benthic|bottom", ts$feature %||% "", ignore.case = TRUE)
        # Significance-calibrated plain-language interpretation (pure helper, unit-tested):
        # a near-zero (tau == 0) or non-significant slope is reported as flat / a slight
        # tendency, never as a confident decline/rise.
        interpretation <- gf_trend_narrative(
          ts$rate,
          tau         = if (!is.null(mk)) mk$tau else NA_real_,
          significant = if (!is.null(mk)) isTRUE(mk$significant) else FALSE,
          p_value     = if (!is.null(mk)) mk$p_value else NA_real_,
          n           = if (!is.null(mk)) mk$n else NA_integer_,
          is_benthic  = is_benthic)
        trend <- list(
          summary = sprintf("Regional average of %s over %d-%d changed by %+.4f per year%s (ordinary least squares; R-squared %.2f).",
                            ts$feature, ts$start_year, ts$end_year, ts$rate,
                            if (isTRUE(is.finite(ts$rate_lo)) && isTRUE(is.finite(ts$rate_hi))) sprintf(" (95%% CI %+.4f to %+.4f)", ts$rate_lo, ts$rate_hi) else "",
                            ts$r2 %||% NA_real_),
          interpretation = interpretation,
          stats = tstats, series = ts$series, cloud = cloudtxt,
          feature = ts$feature, units = ts$units %||% "")
      }
      stepdf <- NULL
      if (length(steps)) stepdf <- do.call(rbind, lapply(steps, function(s) data.frame(
        Step = s$step, Layer = s$label, Role = s$role, Status = s$status,
        Detail = s$note %||% "", check.names = FALSE, stringsAsFactors = FALSE)))
      list(pl = pl, desc = desc, carea = carea, trend = trend, stepdf = stepdf, steps = steps)
    })

    output$pipe_dl_report <- downloadHandler(
      filename = function() paste0("Pipeline_Report_", Sys.Date(), ".html"),
      content = function(file) {
        pp <- pipe_report_payload(); steps <- pp$steps
        # Rasterise a ggplot to an embeddable data URI. Pure/local (no EE), wrapped so a
        # chart that can't build simply drops out of the report instead of failing the export.
        plot_uri <- function(p, w = 6.6, h = 3.4) {
          if (is.null(p)) return(NULL)
          tryCatch({
            tf <- tempfile(fileext = ".png")
            ggplot2::ggsave(tf, p, width = w, height = h, dpi = 150, bg = "white")
            paste0("data:image/png;base64,", jsonlite::base64_enc(readBin(tf, "raw", file.info(tf)$size)))
          }, error = function(e) NULL)
        }
        map_uri <- tryCatch({
          gp <- gee_rv$pipeline_final_plot
          if (is.null(gp)) NULL else {
            tf <- tempfile(fileext = ".png")
            ggplot2::ggsave(tf, gp, width = 7, height = 6, dpi = 150, bg = "white")
            paste0("data:image/png;base64,", jsonlite::base64_enc(readBin(tf, "raw", file.info(tf)$size)))
          }
        }, error = function(e) NULL)
        # All-indicators gallery — one small thumbnail per step, so the report represents EVERY
        # layer the pipeline built, not just the headline composite. Fetched from each step's
        # already-computed thumbnail URL with a short timeout; any thumbnail that fails is simply
        # skipped (never blocks the export). A single-band visualize thumbnail is cheap to render
        # (unlike the percentile reduceRegion that used to stall the correlation), so this is safe.
        thumb_uri <- function(u) {
          if (is.null(u) || !nzchar(u)) return(NULL)
          for (attempt in 1:2) {                       # heavy S2 composites can need a 2nd try
            out <- tryCatch({
              resp <- httr::GET(u, httr::timeout(40))   # per-thumbnail cap
              if (httr::status_code(resp) != 200) NULL
              else paste0("data:image/png;base64,", jsonlite::base64_enc(httr::content(resp, "raw")))
            }, error = function(e) NULL)
            if (!is.null(out)) return(out)
          }
          NULL
        }
        # Hard total budget so the gallery can NEVER make the download hang: once ~45s of
        # wall-clock is spent fetching thumbnails, the rest are skipped (counted as missing) and
        # the report still writes immediately. The bulk of the report never waits on EE.
        gal_deadline <- Sys.time() + 200
        gallery <- Filter(Negate(is.null), lapply(steps, function(s) {
          if (Sys.time() > gal_deadline) return(NULL)
          uri <- thumb_uri(s$url %||% NULL)
          if (is.null(uri)) NULL else list(label = s$label %||% "Layer", uri = uri)
        }))
        gallery_missing <- sum(vapply(steps, function(s) isTRUE(!is.null(s$url) && nzchar(s$url)), logical(1))) - length(gallery)
        # Trend line chart (from the yearly series already in the trend payload).
        trend_chart_uri <- if (!is.null(pp$trend) && !is.null(pp$trend$series))
          # units="" on purpose: the y-axis carries per-year index VALUES, not the per-year slope,
          # so the trend's "/yr" rate unit does not belong on this axis.
          plot_uri(gf_trend_line_plot(pp$trend$series, label = pp$trend$feature %||% "Value",
                                      units = "")) else NULL
        cites <- unique(unlist(Filter(function(x) !is.null(x) && nzchar(x), lapply(steps, function(s) s$citation))))
        if (!length(cites)) cites <- "Google Earth Engine (https://earthengine.google.com)"
        cites <- c(cites,
                   "Olofsson, P. et al. (2014). Good practices for estimating area and assessing accuracy of land change. Remote Sensing of Environment, 148, 42-57.",
                   "Pontius, R.G. & Millones, M. (2011). Death to Kappa. International Journal of Remote Sensing, 32(15), 4407-4429.")
        dr  <- tryCatch(paste(as.character(input$pipe_date[1]), "to", as.character(input$pipe_date[2])), error = function(e) "")
        agg <- input$pipe_agg %||% "Median"
        chain <- paste(vapply(steps, function(s) s$label, character(1)), collapse = " → ")
        # Landscape metrics section (diversity + patch structure), reusing the Insights renderers.
        cins_r <- gee_rv$gee_class_insights
        landscape_html <- ""
        if (!is.null(cins_r) && !is.null(cins_r$classes) && "Area_km2" %in% names(cins_r$classes))
          landscape_html <- paste0(
            gf_diversity_html(landscape_diversity(cins_r$classes$Area_km2), scope = "value classes"),
            gf_patch_html(gee_rv$gee_patch_metrics))
        aca_r <- gee_rv$aca_summary
        aca_area <- if (!is.null(aca_r) && !is.null(aca_r$classes) && nrow(aca_r$classes) > 0)
          data.frame(`Benthic class` = aca_r$classes$Class, `Area (km2)` = aca_r$classes$Area_km2,
                     `Area (ha)` = aca_r$classes$Area_ha, `Share (%)` = aca_r$classes$Pct,
                     check.names = FALSE, stringsAsFactors = FALSE) else NULL
        cs_r <- gee_rv$correl_series
        correl_html <- if (!is.null(cs_r) && !is.null(cs_r$corr)) gf_correlation_html(cs_r$corr, response = cs_r$response) else ""
        correl_chart_uri <- if (!is.null(cs_r) && !is.null(cs_r$series) && !is.null(cs_r$response))
          plot_uri(gf_correlation_scatter_plot(cs_r$series, cs_r$years, cs_r$response), w = 6.8, h = 4.2) else NULL
        used_aca_coral  <- any(vapply(steps, function(s) grepl("Allen Coral Atlas", s$label %||% ""), logical(1)))
        used_coral_step <- any(vapply(steps, function(s) grepl("Coral Health", s$label %||% ""), logical(1)))
        aca_note <- if (!is.null(aca_r) && isTRUE(is.finite(aca_r$coral_km2)) && used_aca_coral && used_coral_step)
          sprintf("Allen Coral Atlas mapped %.3f km² (%.1f%%) of this area as Coral/Algae. Because the ACA step ran before Coral Health, the Coral Health index, landscape metrics and trend above were all computed over this ACA-confirmed coral only.", aca_r$coral_km2, aca_r$coral_pct) else NULL
        # Methods generated from what ACTUALLY ran (no boilerplate about steps that weren't used).
        marine_used <- any(vapply(steps, function(s) grepl("Coral Health|Benthic", s$label %||% ""), logical(1)))
        trend_used  <- !is.null(pp$trend); correl_used <- !is.null(cs_r) && !is.null(cs_r$corr)
        methods <- gf_pipeline_methods_html(date_range = dr, agg = agg, chain = chain,
                    marine_used = marine_used, trend_used = trend_used, correl_used = correl_used)
        lims <- Filter(Negate(is.null), list(
          "Continuous indices have no per-class ground reference; formal classification accuracy (overall accuracy, disagreement) is reported only for the LULC module.",
          if (trend_used || correl_used) sprintf("The temporal analysis spans %s. Short series (few years) have low statistical power, so trends/correlations here may be non-significant; a longer record is needed for firm inference.", gf_report_escape(dr)) else NULL,
          if (trend_used) "If the range includes the current, incomplete year, its value rests on partial-year imagery and can bias the end of the series." else NULL))
        # Executive summary — shared builder (identical to the Insights dashboard's).
        headline_lbl <- tryCatch(cins_r$feature, error = function(e) NULL)
        if (is.null(headline_lbl) || !nzchar(headline_lbl)) headline_lbl <- pipeline_state$final_caption
        exec_summary <- gf_pipeline_exec_summary(
          headline = headline_lbl,
          area_km2 = tryCatch(pipeline_state$area_km2 %||% pp$pl$spatial$km2, error = function(e) NA_real_),
          date_range = dr, aca = aca_r, aca_over_coral = isTRUE(used_aca_coral && used_coral_step),
          trend = pp$trend,
          correl = if (!is.null(cs_r) && !is.null(cs_r$corr)) list(r = cs_r$corr$r, p = cs_r$corr$p, response = cs_r$response) else NULL)
        area_name <- tryCatch(trimws(input$pipe_area_name %||% ""), error = function(e) "")
        # Reproducibility / provenance — the exact parameters needed to re-run this analysis.
        datasets <- unique(unlist(Filter(function(x) !is.null(x) && nzchar(x),
                                         lapply(steps, function(s) s$citation))))
        provenance <- gf_provenance_df(
          area_name = area_name,
          km2       = tryCatch(pipeline_state$area_km2 %||% pp$pl$spatial$km2, error = function(e) NA_real_),
          date_range = dr, agg = agg,
          dyn_scale = pipeline_state$dyn_scale, thumb_dim = pipeline_state$thumb_dim,
          chain = chain, datasets = datasets,
          bbox = gee_rv$pipeline_final_bbox, versions = gf_runtime_versions(),
          generated = format(pipeline_state$ran_at %||% Sys.time(), "%Y-%m-%d %H:%M %Z"))
        payload <- list(
          title = if (nzchar(area_name)) paste0(area_name, " — Spatial Analysis Report") else "Sequential Pipeline - Spatial Analysis Report",
          study_area = if (nzchar(area_name)) area_name else NULL,
          generated = format(pipeline_state$ran_at %||% Sys.time(), "%Y-%m-%d %H:%M"),
          area_km2 = tryCatch(round(pipeline_state$area_km2 %||% pp$pl$spatial$km2, 1), error = function(e) NULL),
          date_range = dr, agg = agg, map_datauri = map_uri,
          map_caption = pipeline_state$final_caption, exec_summary = exec_summary,
          # If the composite couldn't render, say so honestly instead of silently dropping the figure.
          map_note = if (is.null(map_uri)) pipeline_state$final_caption else NULL,
          explain_html = if (!is.null(map_uri)) pipeline_state$final_explain else NULL,   # don't describe a map that didn't render
          descriptive = pp$desc, class_area = pp$carea, trend = pp$trend, steps = pp$stepdf,
          trend_chart = trend_chart_uri, correl_chart = correl_chart_uri,
          gallery = gallery, gallery_missing = gallery_missing,
          landscape_html = landscape_html, aca_area = aca_area, aca_note = aca_note, correl_html = correl_html, methods_html = methods,
          citations = unique(c(cites, gf_method_references(marine = grepl("Coral|Benthic", chain), trend = grepl("Trend", chain), correl = grepl("Correlation", chain), heat = grepl("Heat Stress|SST", chain), aca = grepl("Allen Coral|Coral Habitat", chain)))),
          provenance = provenance, limitations = unlist(lims),
          sensors = tryCatch(gf_sensors_used_df(datasets), error = function(e) NULL))
        writeLines(gf_pipeline_report_html(payload), file)
      }
    )

    output$pipe_dl_data <- downloadHandler(
      filename = function() paste0("Pipeline_Data_", Sys.Date(), ".xlsx"),
      content = function(file) {
        pp <- pipe_report_payload(); sheets <- list()
        if (!is.null(pp$desc))  sheets[["Descriptive_Stats"]] <- pp$desc
        if (!is.null(pp$carea)) sheets[["Area_by_Class"]] <- pp$carea
        if (!is.null(pp$trend)) {
          sheets[["Rate_of_Change"]] <- pp$trend$stats
          if (!is.null(pp$trend$series)) sheets[["Trend_Series"]] <- pp$trend$series
        }
        cins_x <- gee_rv$gee_class_insights
        d <- if (!is.null(cins_x) && !is.null(cins_x$classes) && "Area_km2" %in% names(cins_x$classes)) landscape_diversity(cins_x$classes$Area_km2) else NULL
        if (!is.null(d)) sheets[["Landscape_Diversity"]] <- data.frame(
          Metric = c("Richness (classes)", "Shannon diversity (SHDI)", "Shannon evenness (SHEI)", "Simpson diversity", "Dominant class share (%)"),
          Value  = c(d$richness, round(d$shdi, 3), round(d$shei, 3), round(d$simpson, 3), round(d$dominance * 100, 1)),
          stringsAsFactors = FALSE)
        pm <- gee_rv$gee_patch_metrics
        if (!is.null(pm)) sheets[["Landscape_Patch"]] <- data.frame(
          Metric = c("Number of patches", "Patch density (/100 ha)", "Mean patch size (ha)", "Area-weighted MPS (ha)", "Largest Patch Index (%)", "Edge density (m/ha)"),
          Value  = c(pm$np, round(pm$pd, 2), round(pm$mps_ha, 2), round(pm$awmps_ha, 2), round(pm$lpi, 1), round(pm$ed, 1)),
          stringsAsFactors = FALSE)
        aca_x <- gee_rv$aca_summary
        if (!is.null(aca_x) && !is.null(aca_x$classes) && nrow(aca_x$classes) > 0)
          sheets[["Coral_Habitat_ACA"]] <- data.frame(
            Benthic_class = aca_x$classes$Class, Area_km2 = aca_x$classes$Area_km2,
            Area_ha = aca_x$classes$Area_ha, Share_pct = aca_x$classes$Pct, stringsAsFactors = FALSE)
        cs_x <- gee_rv$correl_series
        if (!is.null(cs_x) && !is.null(cs_x$series) && length(cs_x$series)) {
          sdf <- data.frame(Year = cs_x$years, stringsAsFactors = FALSE)
          for (nm in names(cs_x$series)) sdf[[nm]] <- round(cs_x$series[[nm]], 4)
          sheets[["Indicator_Series"]] <- sdf
          if (!is.null(cs_x$corr)) {
            rmat <- as.data.frame(round(cs_x$corr$r, 3)); rmat <- cbind(Indicator = rownames(rmat), rmat)
            sheets[["Correlation_Matrix"]] <- rmat
          }
        }
        if (!is.null(pp$stepdf)) sheets[["Pipeline_Steps"]] <- pp$stepdf
        # Reproducibility sheet — same provenance table as the HTML report (shared helper, no drift).
        datasets_x <- unique(unlist(Filter(function(x) !is.null(x) && nzchar(x),
                                           lapply(pp$steps, function(s) s$citation))))
        sheets[["Reproducibility"]] <- gf_provenance_df(
          area_name = tryCatch(trimws(input$pipe_area_name %||% ""), error = function(e) ""),
          km2 = tryCatch(pipeline_state$area_km2 %||% pp$pl$spatial$km2, error = function(e) NA_real_),
          date_range = tryCatch(paste(as.character(input$pipe_date[1]), "to", as.character(input$pipe_date[2])), error = function(e) ""),
          agg = input$pipe_agg %||% "Median",
          dyn_scale = pipeline_state$dyn_scale, thumb_dim = pipeline_state$thumb_dim,
          chain = if (!is.null(pp$steps)) paste(vapply(pp$steps, function(s) s$label, character(1)), collapse = " -> ") else "",
          datasets = datasets_x,
          generated = format(pipeline_state$ran_at %||% Sys.time(), "%Y-%m-%d %H:%M %Z"))
        sheets[["Notes"]] <- data.frame(
          Field = "Limitation",
          Value = "Not validated against independent field/ground-truth data; satellite indices are screening tools.",
          stringsAsFactors = FALSE)
        if (length(sheets) <= 1) sheets[["Info"]] <- data.frame(Note = "Run a Sequential Pipeline first, then download.", stringsAsFactors = FALSE)
        writexl::write_xlsx(sheets, path = file)
      }
    )
  })
}
