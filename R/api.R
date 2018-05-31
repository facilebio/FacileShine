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
