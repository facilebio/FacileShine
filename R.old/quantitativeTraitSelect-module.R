#' Provides a widget to select quantitative traits from samples
#'
#' This module enables the user to pull quantitative traits of all flavors
#' (ie. from assay data, or quantitative pData) from a `FacileDataStore`.
#' This module doesn't return the actual quantitative values selected for, but
#' rather parameterizations of the widget that the caller can use to extract
#' the values for the `FacileDataStore`.
#'
#' We assume that there are two sources of quanitative traits:
#'
#' 1. From assays (usually, dense matrix types of things (rnaseq)). These data
#'    are accessed within this module using the [assayFeatureSelect()]
#'    module; or
#'
#' 2. A quantitative sample covariate (RIN score, for instance). These data are
#'    accessed within using the [quantitativeSampleCovariateSelect()] module.
#'
#' @export
#' @rdname quantitativeTraitSelect
quantitativeTraitSelect <- function(input, output, session, rfds, ...,
                                    exclude = NULL, .reactive = TRUE) {

  assert_class(rfds, "ReactiveFacileDataStore")
  isolate. <- if (.reactive) base::identity else shiny::isolate

  # We keep track of state so that we don't refetch data when we don't have to.
  # Reactivitiy that is internal to this module should "react" to the state$xxx
  # variables. Facile modules that use this module should respond to the
  # list that is returned from here.
  state <- reactiveValues(
    # This will be either "covariate", or "assay", depending on whether the user
    # is selecting quantitative data from the
    # `quantitativeSampleCovariateSelect` or the `assayFeatureSelect`
    # modules, respectively.
    source = "__initializing__",

    # "labeled" API
    name = "__initializing__",
    label = "__initializing__")


  qassay <- callModule(assayFeatureSelect, "qassay", rfds, ...,
                       exclude = exclude, .reactive = .reactive)
  qcovariate <- callModule(quantitativeSampleCovariateSelect, "qassay", rfds,
                           ..., exclude = exclude, .reactive = .reactive)

  source <- reactive({
    # TODO: ...
    state$source
  })

  vals <- list(
    source = source,
    qassay = qassay,
    qcovariate = qcovariate,
    .ns = session$ns)
  class(vals) <- c("QuantitativeTraitSelect", "Labeled")
  return(vals)
}

#' @export
#' @rdname quantitativeTraitSelect
quantitativeTraitSelectUI <- function(id, ...) {
  ns <- NS(id)

  source_select <- selectInput(ns("source"), "Data Type:",
                               choices = c("assay", "covariate"),
                               selected = "assay")
  tagList(
    fluidRow(column(12, source_select)),
    conditionalPanel(
      condition = "input.type == 'assay'",
      fluidRow(
        column(12, assayFeatureSelectUI(ns("qassay"))))),
    conditionalPanel(
      condition = "input.type == 'covariate'",
      fluidRow(
        column(12, quantitativeSampleCovariateSelectUI(ns("qcovariate"))))))
}
