#' Workhorse function to invoke shiny gadgets for FacileData
#'
#' Provides `selection`, and other getter functions that will enable
#' reproducibility of the results explored and "finalized" within the shiny
#' gadget to be recovered within the R workspace.
#'
#' @export
#' @param x A "facile" object, like a `FacileAnalysisResult`.
#' @return An `FacileGadgetResult` object that encapsulates the "end product" of
#'   the exploration and interaction over `x` from within the gadget.
shine <- function(x, ...) {
  UseMethod("shine", x)
}

#' Retrieves selection information from a "shined"
#' @export
selection <- function(x, ...) {
  UseMethod("selection", x)
}

#' Updates a selected item from a module.
#'
#' This can apply to an item selected in a dropdown, ie. a selected covariate
#' in the categoricalSampleCovariate module, or perhaps a selected set of data
#' brushed somewhere(?)
#'
#' @export
update_selected <- function(x, ...) {
  UseMethod("update_selected", x)
}

# Labeled ======================================================================

# Labeled acts as something of an interface to reactive modules.
#
# Modules that implement this interface must return `label` and `name` reactive
# elemengs within them.
#
# We use these when something (like a `quantitativeAssayDataSelect`) needs
# a "computer friendly" name for itself (`name()`), or a more human readable
# name (`label()`)
name <- function(x, ...) {
  UseMethod("name", x)
}

name.Labeled <- function(x, ...) {
  assert_reacting()
  out <- x[["name"]]
  if (!is(out, "reactive")) "unnamed" else out()
}

label <- function(x, ...) {
  UseMethod("label")
}

label.NULL <- function(x, ...) NULL

label.Labeled <- function(x, ...) {
  assert_reacting()
  out <- x[["label"]]
  if (!is(out, "reactive")) "unlabeled" else out()
}

# Defined primarily for ReactiveFacileDataStore ================================

#' @rdname reactiveFacileDataStore
#' @export
active_assays <- function(x, ...) {
  UseMethod("active_assays", x)
}

#' @rdname reactiveFacileDataStore
#' @export
active_covariates <- function(x, ...) {
  UseMethod("active_covariates", x)
}

#' @rdname reactiveFacileDataStore
#' @export
active_samples <- function(x, ...) {
  UseMethod("active_samples", x)
}

#' @noRd
#' @export
user <- function(x, ...) {
  UseMethod("user", x)
}
