#' Extract user-specified annotations
#'
#' During the course of an analysis, users may annotate differente things
#' presented to them via the shiny interface. This function will pull out
#' the specficic feature type from the interacted object, should there be any.
#'
#' @export
#' @param x the interacted object
#' @param type In a particular shiny view, the user might be looking at a
#'   combination of samples or features. These both can be annotated, so the
#'   caller needs to specify which subset of annotations they want. If left
#'   `NULL` (default), the list of annotations will be returned
#' @return a sample- or feature-level covariate table. If `type = NULL`, then
#'   a list of all annotation tables is returnd
annotation <- function(x, type = NULL, ...) {
  UseMethod("annotation", x)
}

#' Tests if a module is initialized
#'
#' @export
#' @param a module
#' @return logical TRUE/FALSE
initialized <- function(x, ...) {
  UseMethod("initialized", x)
}

#' @noRd
#' @export
initialized.NULL <- function(x, ...) {
  FALSE
}

#' Retrieves selection information from a module
#'
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

#' Resets the state of a module
#'
#' I'm imagining we will want this when some catastrophic error gets thrown
#' during interaction with a shiny module, and we want to reset its original
#' state of zen.
#'
#' One time this might happen is within an interactive analysis where the
#' [active_samples()] change from under a module's feet, and some samples
#' or covariates that once existed are now gone, then: boom.
#'
#' TODO: Ubiquitously implement `reset()` functionality.
#'
#' @export
#' @param x The facile module to reset
reset <- function(x, ...) {
  UseMethod("reset", x)
}

#' Check that the data of a module comes from the expected fds.
#' 
#' This was a temporary (2 day) fix. I think the `module$in_sync()` paradigm
#' is a better one to ensure that the data that `module` is returning is still
#' the same internal `rfds` that it came from.
#' 
#' @export
#' @param x the moduleServer
#' @param fds the ReactivFacileDataStore to test
from_fds <- function(x, rfds, ...) {
  .Deprecated("Use module$in_sync() instead")
  UseMethod("from_fds", x)
}

# Labeled API ==================================================================
# name and label generic definitions come from FacileData

#' @noRd
#' @export
#' @rdname labeled
name.Labeled <- function(x, ...) {
  assert_reacting()
  out <- x[["name"]]
  if (!is(out, "reactive")) "unnamed" else out()
}

#' @noRd
#' @export
#' @rdname labeled
label.Labeled <- function(x, ...) {
  assert_reacting()
  out <- x[["label"]]
  if (!is(out, "reactive")) "unlabeled" else out()
}

# Defined primarily for ReactiveFacileDataStore ================================

#' @noRd
#' @export
active_assays <- function(x, ...) {
  UseMethod("active_assays", x)
}

#' @noRd
#' @export
active_covariates <- function(x, ...) {
  UseMethod("active_covariates", x)
}

#' @noRd
#' @export
active_samples <- function(x, ...) {
  UseMethod("active_samples", x)
}

#' @noRd
#' @export
user <- function(x, ...) {
  UseMethod("user", x)
}
