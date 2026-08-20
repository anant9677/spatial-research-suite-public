# =========================================================================
# MULTIVARIATE_ENGINE.R  —  N-variable spatial statistics via rgee
# =========================================================================
# Upgrades the Statistical Analysis module from bivariate (2-band correlation)
# to full multivariate (3+ bands): a correlation MATRIX and standardized PCA,
# computed from a single Earth-Engine covariance reduction over a stack of the
# user-selected indices.
#
# Pipeline:
#   1. Each selected feature is rendered to a single-band ee$Image via the
#      existing get_feature_img() engine, then stacked into one multi-band image.
#   2. stack$toArray()$reduceRegion(ee$Reducer$centeredCovariance()) returns the
#      NxN covariance matrix in ONE server call (the canonical GEE PCA pattern).
#   3. .mv_derive() (pure R, unit-tested) turns that covariance matrix into a
#      correlation matrix + a STANDARDIZED PCA (eigen-decomposition of the
#      correlation matrix — the right choice when variables live on wildly
#      different scales, e.g. NDVI in [-1,1] vs Elevation in metres).
#
# The heavy EE call runs synchronously in the Shiny server (same pattern as the
# per-class LULC statistics), so it must be validated on the TEST deployment
# before promoting to live. The pure-R derivation + plot builders below are
# CI-testable without Earth Engine.
# =========================================================================

# Short axis/heatmap label: prefer the code in parentheses ("Vegetation Health
# (NDVI)" -> "NDVI"), else a trimmed name ("Terrain Slope" -> "Terrain Slope").
mv_short_label <- function(name) {
  m <- regmatches(name, regexpr("\\(([^)]+)\\)", name))
  if (length(m) == 1 && nzchar(m)) return(gsub("[()]", "", m))
  if (nchar(name) > 16) paste0(substr(name, 1, 15), "…") else name
}

# ---- Pure derivation: covariance matrix -> correlation + standardized PCA ----
.mv_derive <- function(cov, labels) {
  n <- length(labels)
  cov <- matrix(as.numeric(cov), nrow = n, ncol = n)
  d <- sqrt(diag(cov))
  d[!is.finite(d) | d == 0] <- NA_real_
  cor <- cov / (d %o% d)
  cor[!is.finite(cor)] <- 0
  diag(cor) <- 1
  # Standardized PCA == eigen-decomposition of the correlation matrix.
  e <- eigen(cor, symmetric = TRUE)
  vals <- pmax(e$values, 0)                       # clamp tiny negative numerical noise
  var_explained <- if (sum(vals) > 0) vals / sum(vals) else rep(0, n)
  vecs <- e$vectors
  loadings <- vecs %*% diag(sqrt(vals), nrow = n) # correlation-scaled loadings (for the biplot)
  rownames(cor) <- labels; colnames(cor) <- labels
  list(labels = labels, cov = cov, cor = cor,
       eigenvalues = vals, var_explained = var_explained,
       eigenvectors = vecs, loadings = loadings)
}

# -------------------------------------------------------------------------
# compute_multivariate_stats(): the Earth-Engine half. Returns the .mv_derive
# result plus feature_names and pixel count. Synchronous (getInfo()).
# -------------------------------------------------------------------------
compute_multivariate_stats <- function(feature_names, start_date, end_date, agg, ee_roi, dyn_scale) {
  feature_names <- unique(feature_names)
  if (length(feature_names) < 3) stop("Select at least 3 variables for multivariate analysis.")
  if (length(feature_names) > 8)  stop("Please select at most 8 variables (keeps the matrix readable).")

  bands <- character(0); labels <- character(0); stacked <- NULL
  for (i in seq_along(feature_names)) {
    fd <- get_feature_img(feature_names[i], start_date, end_date, agg, ee_roi, dyn_scale)
    if (is.null(fd) || is.null(fd$img)) {
      stop(paste0("Could not compute '", feature_names[i], "' for this area / date range."))
    }
    b <- paste0("b", i)  # short, collision-free band name
    one <- fd$img$select(list(fd$img$bandNames()$get(0)))$rename(b)$toFloat()
    stacked <- if (is.null(stacked)) one else stacked$addBands(one)
    bands  <- c(bands, b)
    labels <- c(labels, mv_short_label(feature_names[i]))
  }

  # One server call: covariance matrix across all bands (canonical GEE pattern).
  covar <- stacked$toArray()$reduceRegion(
    reducer = ee$Reducer$centeredCovariance(),
    geometry = ee_roi, scale = dyn_scale, maxPixels = 1e13, bestEffort = TRUE, tileScale = 16
  )
  cov_list <- covar$get("array")$getInfo()
  if (is.null(cov_list) || length(cov_list) == 0) {
    stop("Not enough overlapping valid pixels to build a covariance matrix. Try a larger area or a wider date range.")
  }
  n <- length(bands)
  cov <- matrix(as.numeric(unlist(cov_list)), nrow = n, ncol = n, byrow = TRUE)

  npx <- tryCatch(
    stacked$select(list(bands[1]))$reduceRegion(
      reducer = ee$Reducer$count(), geometry = ee_roi, scale = dyn_scale,
      maxPixels = 1e13, bestEffort = TRUE, tileScale = 16)$get(bands[1])$getInfo(),
    error = function(e) NA_real_)

  res <- .mv_derive(cov, labels)
  res$feature_names <- feature_names
  res$n_pixels <- as.numeric(npx %||% NA_real_)
  res
}

# =========================================================================
# PC INTERPRETATION — which INPUT variables drive each component
# =========================================================================
# For component k, the driver is the input variable with the largest |loading|
# on that component (loadings are constant-scaled per column, so |loading| and
# |eigenvector| rank variables identically within a PC).
mv_pc_drivers <- function(res, n_pc = NULL, top = 2) {
  if (is.null(res) || is.null(res$loadings) || is.null(res$labels)) return(NULL)
  L <- res$loadings; labs <- res$labels
  npc <- if (is.null(n_pc)) ncol(L) else min(n_pc, ncol(L))
  lapply(seq_len(npc), function(k) {
    lk  <- L[, k]
    ord <- order(abs(lk), decreasing = TRUE)
    keep <- ord[seq_len(min(top, length(ord)))]
    list(pc = k,
         var_explained = res$var_explained[k],
         dominant = labs[ord[1]],
         dominant_sign = if (lk[ord[1]] >= 0) "positive" else "negative",
         top_vars = labs[keep],
         top_loadings = lk[keep])
  })
}

# Axis label like  "PC1 (42.3%) · Dominant: NDVI"
mv_pc_axis_label <- function(res, k) {
  d <- mv_pc_drivers(res, n_pc = k, top = 1)
  if (is.null(d) || length(d) < k) return(paste0("PC", k))
  sprintf("PC%d (%.1f%%) · Dominant: %s", k, d[[k]]$var_explained * 100, d[[k]]$dominant)
}

# HTML narrative: "PC1 accounts for X% ... driven by NDVI (with Elevation); PC2 ...".
mv_pc_narrative <- function(res, n_pc = 2) {
  drivers <- mv_pc_drivers(res, n_pc = n_pc, top = 2)
  if (is.null(drivers)) return("")
  parts <- vapply(drivers, function(d) {
    extra <- if (length(d$top_vars) > 1) sprintf(" (with %s)", d$top_vars[2]) else ""
    sprintf("<b>PC%d</b> accounts for <b>%.1f%%</b> of the variance and is primarily driven by <b>%s</b>%s",
            d$pc, d$var_explained * 100, d$dominant, extra)
  }, character(1))
  paste0(paste(parts, collapse = "; "), ".")
}

# =========================================================================
# PLOT BUILDERS (ggplot2, pure given a .mv_derive result)
# =========================================================================
.mv_blank <- function(msg) {
  ggplot2::ggplot() +
    ggplot2::annotate("text", x = 0, y = 0, label = msg, size = 4, colour = "#5c6b73") +
    ggplot2::theme_void()
}

# Correlation heatmap of the selected variables.
mv_corr_heatmap <- function(res) {
  if (is.null(res) || is.null(res$cor)) return(.mv_blank("Run a multivariate analysis"))
  labs <- res$labels; n <- length(labs)
  grid <- expand.grid(i = seq_len(n), j = seq_len(n))
  df <- data.frame(
    Xf = factor(labs[grid$j], levels = labs),
    Yf = factor(labs[grid$i], levels = rev(labs)),
    r  = mapply(function(i, j) res$cor[i, j], grid$i, grid$j)
  )
  ggplot2::ggplot(df, ggplot2::aes(Xf, Yf, fill = r)) +
    ggplot2::geom_tile(colour = "white", linewidth = 0.6) +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.2f", r)), size = 3.4,
                       colour = ifelse(abs(df$r) > 0.6, "white", "#26333e")) +
    ggplot2::scale_fill_gradient2(low = "#8b3a2b", mid = "#f7f6f2", high = "#26557f",
                                  midpoint = 0, limits = c(-1, 1), name = "r") +
    ggplot2::labs(title = "Correlation Matrix", x = NULL, y = NULL) +
    ggplot2::coord_equal() +
    ggplot2::theme_minimal(base_family = "sans") +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 15, colour = "#26333e"),
      axis.text = ggplot2::element_text(size = 11, face = "bold", colour = "#26333e"),
      axis.text.x = ggplot2::element_text(angle = 35, hjust = 1),
      panel.grid = ggplot2::element_blank())
}

# PCA scree / variance-explained chart (bars = per-PC, line = cumulative).
mv_pca_scree <- function(res) {
  if (is.null(res) || is.null(res$var_explained)) return(.mv_blank("Run a multivariate analysis"))
  ve <- res$var_explained; n <- length(ve)
  drivers <- mv_pc_drivers(res, top = 1)
  pc_lab <- if (is.null(drivers)) paste0("PC", seq_len(n))
            else vapply(seq_len(n), function(k) sprintf("PC%d\n%s", k, drivers[[k]]$dominant), character(1))
  df <- data.frame(
    PC  = factor(pc_lab, levels = pc_lab),
    var = ve * 100,
    cum = cumsum(ve) * 100)
  ggplot2::ggplot(df, ggplot2::aes(x = PC)) +
    ggplot2::geom_col(ggplot2::aes(y = var), fill = "#45936f", width = 0.68) +
    ggplot2::geom_text(ggplot2::aes(y = var, label = sprintf("%.1f%%", var)),
                       vjust = -0.4, size = 3.3, colour = "#26333e") +
    ggplot2::geom_line(ggplot2::aes(y = cum, group = 1), colour = "#8b3a2b", linewidth = 0.8) +
    ggplot2::geom_point(ggplot2::aes(y = cum), colour = "#8b3a2b", size = 2) +
    ggplot2::scale_y_continuous(limits = c(0, 105), expand = ggplot2::expansion(mult = c(0, 0.05))) +
    ggplot2::labs(title = "PCA — Variance Explained",
                  subtitle = "Bars: variance per component  ·  Line: cumulative",
                  x = NULL, y = "% of total variance") +
    ggplot2::theme_minimal(base_family = "sans") +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 15, colour = "#26333e"),
      plot.subtitle = ggplot2::element_text(size = 10, colour = "#5c6b73"),
      axis.text = ggplot2::element_text(size = 11, colour = "#26333e"))
}

# PCA biplot: variable loadings on PC1 vs PC2.
mv_pca_biplot <- function(res) {
  if (is.null(res) || is.null(res$loadings) || ncol(res$loadings) < 2) {
    return(.mv_blank("Need at least 2 components for a biplot"))
  }
  L <- res$loadings; ve <- res$var_explained; labs <- res$labels
  df <- data.frame(x = L[, 1], y = L[, 2], lab = labs)
  lim <- max(1, max(abs(c(df$x, df$y)), na.rm = TRUE)) * 1.15
  ggplot2::ggplot(df, ggplot2::aes(x, y)) +
    ggplot2::geom_hline(yintercept = 0, colour = "#d8d4c8") +
    ggplot2::geom_vline(xintercept = 0, colour = "#d8d4c8") +
    ggplot2::geom_segment(ggplot2::aes(x = 0, y = 0, xend = x, yend = y),
                          arrow = ggplot2::arrow(length = ggplot2::unit(0.18, "cm")),
                          colour = "#4a83c4", linewidth = 0.7) +
    ggplot2::geom_text(ggplot2::aes(label = lab), vjust = -0.5, size = 3.4,
                       fontface = "bold", colour = "#26333e") +
    ggplot2::coord_equal(xlim = c(-lim, lim), ylim = c(-lim, lim)) +
    ggplot2::labs(title = "PCA Biplot (PC1 vs PC2)",
                  x = mv_pc_axis_label(res, 1),
                  y = mv_pc_axis_label(res, 2)) +
    ggplot2::theme_minimal(base_family = "sans") +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 15, colour = "#26333e"),
      axis.title = ggplot2::element_text(size = 11, colour = "#5c6b73"))
}

# Tidy data frame of the correlation matrix for the results table / export.
mv_corr_table <- function(res) {
  if (is.null(res) || is.null(res$cor)) return(NULL)
  m <- round(res$cor, 3)
  out <- data.frame(Variable = res$labels, m, check.names = FALSE, stringsAsFactors = FALSE)
  colnames(out) <- c("Variable", res$labels)
  out
}
