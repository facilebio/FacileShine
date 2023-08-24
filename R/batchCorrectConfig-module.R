#' A module to configure batch correction parameters for assay data.
#'
#' This module enables the user to select covariates that should be regressed
#' out in the data. At the same time a covariate can be selected that indicates
#' structure in the data to preserve as the "main" covariate.
#' 
#' Covariates entered in `batch` are removed from the selections available in
#' `main`.
#'
#' @export
batchCorrectConfigServer <- function(id, rfds, ..., debug = FALSE) {
  assert_class(rfds, "ReactiveFacileDataStore")
  
  moduleServer(id, function(input, output, session) {
    state <- reactiveValues(
      # Do we want to put the batch/main stuff in here?
    )
    
    batch <- categoricalSampleCovariateSelectServer(
      "batch", rfds, with_none = FALSE)
    main <- categoricalSampleCovariateSelectServer(
      "main", rfds, exclude = batch$covariate)
    
    vals <- list(
      batch = batch,
      main = main,
      .state = state,
      .ns = session$ns)
    class(vals) <- c("BatchCorrectConfigModule")
    vals
  })
}

#' @export
#' @noRd
batchCorrectConfigUI <- function(id, direction = c("horizontal", "vertical"),
                                 ..., debug = FALSE) {
  direction <- match.arg(direction)
  ns <- shiny::NS(id)
  batch <- categoricalSampleCovariateSelectInput(
    ns("batch"),
    label = "Batch Correct",
    multiple = TRUE)
  main <- categoricalSampleCovariateSelectInput(
    ns("main"),
    label = "Batch Preserve",
    multiple = FALSE)
  
  if (direction == "horizontal") {
    out <- shiny::fluidRow(
      shiny::column(6, batch), 
      shiny::column(6, main))
  } else {
    out <- shiny::tagList(batch, main)
  }
  
  out
}
