#' Provides a selection widget to pick from a collection of FacileDataStores
#'
#' TODO: Final selection should be upload(?)
#'
#' @param fdslist a named list of FacileDataStore objects
#' @export
#' @rdname reactiveFacileDataStoreSelect
reactiveFacileDataStoreSelect <- function(input, output, session,
                                          fdslist, ...) {
  ns <- session$ns
  if (!is.list(fdslist) || !all(sapply(fdslist, is, "FacileDataStore"))) {
    stop("A list of FacileDataStore objects is required")
  }

  output$fdsSelectContainer <- renderUI({
    selectInput(ns("fdsSelect"), "Dataset", names(fds), selected = 1L)
  })


}

#' @export
#' @rdname reactiveFacileDataStoreSelect
reactiveFacileDataStoreSelectUI <- function(id, ...) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("fdsSelectContainer")))
}
