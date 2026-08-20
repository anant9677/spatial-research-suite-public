# =========================================================================
# APP_CONTRACTS.R  —  Typed contract objects that dissolve the `rv` god-object
# =========================================================================
# THE PROBLEM THIS SOLVES
# -----------------------
# Today a single `rv` reactiveValues object is passed by reference into every
# module. mod_lulc writes ~33 fields into it; mod_carto and mod_stats reach
# directly into LULC's *private* fields (trained_classifier, training_bands,
# norm_mins, norm_maxs, class_labels, classification_ready_img, gee_landsat_img)
# to run prediction and statistics. LULC's internal field layout is therefore an
# UNDECLARED cross-module API: rename or restructure one field while fixing a
# LULC bug and you silently break Publication Maps and Statistical Analysis,
# with no error until a user hits it. That is the mechanism behind "fix A,
# break B."
#
# THE FIX
# -------
# Replace those bare, undeclared field reads with NAMED, VALIDATED contract
# objects. mod_lulc PRODUCES a `classification_result`; mod_carto/mod_stats
# CONSUME it through documented accessors. The cross-module API is now explicit,
# validated in ONE place, and cannot silently drift.
#
# These constructors/validators are pure (no Shiny, no reactiveValues), so they
# unit-test with plain data — see tests/testthat/test-app-contracts.R. The
# reactive wiring (storing the contract in a reactiveVal and reading it back in
# the consumer modules) is the adoption step, documented at the bottom.
#
# NOTE: GEE image objects (ready_img/source_img) are opaque Python handles; the
# contract just carries them unchanged. No GIS math is performed here.
# =========================================================================

# -------------------------------------------------------------------------
# 1. CLASSIFICATION RESULT  — the mod_lulc -> mod_carto / mod_stats hand-off.
#    Every field the downstream modules currently read off `rv` is a named,
#    documented slot here instead.
# -------------------------------------------------------------------------
new_classification_result <- function(model,
                                      training_bands,
                                      class_labels,
                                      ready_img,
                                      source_img,
                                      model_prob      = NULL,
                                      norm_mins       = NULL,
                                      norm_maxs       = NULL,
                                      classifier_type = "rf",
                                      indices_used    = NULL,
                                      trees_used      = NULL) {
  result <- list(
    model           = model,            # trained ranger/CART/SVM object (was rv$trained_classifier)
    model_prob      = model_prob,       # optional probability model (was rv$trained_classifier_prob)
    training_bands  = training_bands,   # character vector (was rv$training_bands)
    norm_mins       = norm_mins,        # named numeric or NULL (was rv$norm_mins)
    norm_maxs       = norm_maxs,        # named numeric or NULL (was rv$norm_maxs)
    class_labels    = class_labels,     # data.frame Class_ID/Name/Color (was rv$class_labels)
    ready_img       = ready_img,        # EE image prepped for classify() (was rv$classification_ready_img)
    source_img      = source_img,       # EE source composite (was rv$gee_landsat_img)
    classifier_type = classifier_type,  # "rf" | "cart" | "svm" (was rv$classifier_type_used)
    indices_used    = indices_used,     # character vector (was rv$training_indices_used)
    trees_used      = trees_used        # integer or NULL (was rv$rf_trees_used)
  )
  class(result) <- "classification_result"
  validate_classification_result(result)
  result
}

is_classification_result <- function(x) inherits(x, "classification_result")

# Fail-fast validation: the single place the cross-module contract is enforced.
# If a producer forgets a required piece, this throws HERE (at hand-off) with a
# clear message, instead of surfacing as a confusing downstream GIS error.
validate_classification_result <- function(x) {
  if (!is_classification_result(x)) stop("Not a classification_result object.")
  if (is.null(x$model))          stop("classification_result: `model` (trained classifier) is required.")
  if (is.null(x$training_bands) || length(x$training_bands) == 0)
    stop("classification_result: `training_bands` must be a non-empty character vector.")
  if (is.null(x$class_labels) || !is.data.frame(x$class_labels) || nrow(x$class_labels) == 0)
    stop("classification_result: `class_labels` must be a non-empty data.frame.")
  req_cols <- c("Class_ID", "Class_Name", "Class_Color")
  missing <- setdiff(req_cols, names(x$class_labels))
  if (length(missing) > 0)
    stop(sprintf("classification_result: class_labels is missing column(s): %s",
                 paste(missing, collapse = ", ")))
  invisible(x)
}

# Convenience: is a usable classification available? Consumer modules call this
# instead of the old `!is.null(rv$trained_classifier)` scattered checks.
has_classification <- function(x) {
  is_classification_result(x) && !is.null(x$model)
}

# -------------------------------------------------------------------------
# 2. BATCH RESULTS  — the mod_gee -> mod_stats hand-off (Batch Zonal Stats).
#    Replaces the bare rv$batch_results_df / rv$batch_results_meta pair.
# -------------------------------------------------------------------------
new_batch_results <- function(df, feature = NULL, units = NULL, meta = list()) {
  result <- list(df = df, feature = feature, units = units, meta = meta)
  class(result) <- "batch_results"
  result
}

is_batch_results <- function(x) inherits(x, "batch_results")

has_batch_results <- function(x) {
  is_batch_results(x) && is.data.frame(x$df) && nrow(x$df) > 0
}

# -------------------------------------------------------------------------
# 3. BOUNDARY CONTEXT  — the ROI/boundary shared by ALL modules.
#    Bundles the pieces that currently live as loose rv fields (mask_vect and
#    the two export snapshots) so "the current study area" is one named thing.
# -------------------------------------------------------------------------
new_boundary_context <- function(mask_vect = NULL, ext_shp_export = NULL, drawn_roi_export = NULL) {
  result <- list(
    mask_vect        = mask_vect,        # active analysis boundary (terra/sf)
    ext_shp_export   = ext_shp_export,   # extracted-boundary export snapshot
    drawn_roi_export = drawn_roi_export  # freehand-ROI export snapshot
  )
  class(result) <- "boundary_context"
  result
}

is_boundary_context <- function(x) inherits(x, "boundary_context")

has_boundary <- function(x) {
  is_boundary_context(x) && !is.null(x$mask_vect)
}

# =========================================================================
# ADOPTION GUIDE  (the reactive wiring — applied when the modules migrate)
# =========================================================================
# In app.R's server(), create ONE reactiveVal per contract and pass it (not the
# whole rv) to the modules that need it:
#
#   classification <- reactiveVal(NULL)   # holds a classification_result
#   mod_lulc_server("lulc_1",  ..., classification = classification)
#   mod_carto_server("carto_1", ..., classification = classification)
#   mod_stats_server("stats_1", ..., classification = classification)
#
# PRODUCER (mod_lulc.R, at the end of run_model, replacing the ~8 rv$ writes):
#   classification(new_classification_result(
#     model          = fit,
#     model_prob     = fit_prob,
#     training_bands = bands,
#     norm_mins      = norm_mins, norm_maxs = norm_maxs,
#     class_labels   = rv$class_labels,
#     ready_img      = ready_img,
#     source_img     = source_img,
#     classifier_type= classifier_type, indices_used = indices, trees_used = n_trees
#   ))
#
# CONSUMER (mod_carto.R / mod_stats.R, replacing rv$trained_classifier etc.):
#   cls <- classification()
#   req(has_classification(cls))
#   model <- cls$model ; bands <- cls$training_bands ; labels <- cls$class_labels
#
# Because reactiveVal is reactive, downstream renders still invalidate exactly
# as they do today — behaviour is identical; only the seam is now named and
# validated. Migrate one contract at a time; run the app after each.
# =========================================================================

# =========================================================================
# PHASE 2 — BOUNDARY CONTRACT  (append these to apps/test/R/app_contracts.R)
# =========================================================================
# Single source of truth for ALL boundary/ROI state. Replaces the scattered
# rv$mask_vect / rv$lulc_boundary_used / gee_rv$gee_boundary / rv$ext_shp_export
# / rv$drawn_roi_export with ONE store whose slots are owned per-module.
#
# Why this kills crosstalk STRUCTURALLY: there is no shared mutable slot any
# more. mod_lulc writes ONLY `lulc_active`/`lulc_trained`; mod_gee writes ONLY
# `gee`; the extractor writes ONLY `ext_shp`/`drawn_roi`. A consumer reads a
# NAMED slot, so one module physically cannot see another's boundary unless it
# asks for it by name. The `active` slot is a cosmetic "last boundary set
# anywhere", used only for export filenames / study-area labels — never for
# analysis geometry.
#
# The store itself is a reactiveValues created in app.R (see wiring in the
# PHASE2 guide). The functions below are pure accessors over it, so they
# unit-test with a plain list or environment (no Shiny needed).
# =========================================================================

# The canonical slot names — a single place that documents the contract shape.
BOUNDARY_SLOTS <- c("lulc_active", "lulc_trained", "gee", "active", "ext_shp", "drawn_roi")

# Record a module's boundary in its OWN slot, and update the cosmetic `active`
# pointer. Callers pass the store (reactiveValues) + the slot they own.
boundary_set <- function(store, slot, sf_obj) {
  if (!slot %in% BOUNDARY_SLOTS) stop(sprintf("boundary_set: unknown slot '%s'", slot))
  store[[slot]] <- sf_obj
  store$active  <- sf_obj    # cosmetic only (export label / study-area name)
  invisible(sf_obj)
}

# Read a named slot, with an EXPLICIT fallback chain. Never silently reaches
# into another module's live state — the fallback is spelled out by the caller.
boundary_get <- function(store, slot, fallback = NULL) {
  if (!slot %in% BOUNDARY_SLOTS) stop(sprintf("boundary_get: unknown slot '%s'", slot))
  val <- store[[slot]]
  if (is.null(val)) fallback else val
}

# The LULC render boundary: the snapshot captured at training time, else the
# module's current Step-1 boundary. This is the ONE place that fallback lives,
# replacing the `rv$lulc_boundary_used %||% rv$mask_vect` scattered in 3 files.
boundary_lulc_render <- function(store) {
  boundary_get(store, "lulc_trained", fallback = store$lulc_active)
}

has_boundary_slot <- function(store, slot) !is.null(boundary_get(store, slot))
