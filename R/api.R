#' Tests if a module is initialized
#'
#' @export
#' @param a module
#' @return logical TRUE/FALSE
initialized <- function(x, ...) {
  UseMethod("initialized", x)
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
# Labeled API ==================================================================

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
