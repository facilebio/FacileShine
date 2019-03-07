#' A module to create a dropdown over the current assays defined for a datastore
#'
#' @export
#' @rdname assaySelect
#' @param rfds A ReactiveFacileDataStore
assaySelect <- function(input, output, session, rfds, ..., .reactive = TRUE)  {

  assert_class(rfds, "ReactiveFacileDataStore")
  isolate. <- if (.reactive) base::identity else shiny::isolate

  state <- reactiveValues(
    assay_info = tibble(
      assay = "__initializing__",
      assay_type = "__initializing__",
      feature_type = "__initializing__"))

  assays <- reactive({
    req(isolate.(active_assays(rfds)))
  })

  # Update the assay choice dropdown with the active assays over the current
  # set of samples
  observe({
    choices <- assays()$assay
    .ai <- state$assay_info
    if (.ai$assay %in% choices) {
      selected <- .ai$assay
    } else {
      selected <- choices[1L]
      state$assay_info <- FacileData::assay_info(rfds, selected)
    }
    updateSelectInput(session, "assay", choices = choices, selected = selected)
  })

  assay_info <- reactive({
    .assay <- req(input$assay)
    .ai <- state$assay_info
    if (.ai$assay != .assay) {
      state$assay_info <- FacileData::assay_info(rfds, .assay)
    }
    state$assay_info
  })

  features <- reactive({
    if (!is(state$assay_info, "facile_frame")) {
      out <- collect(assay_feature_info(rfds, default_assay(rfds)), n = 1L)
      out <- filter(out, FALSE)
    } else {
      out <- assay_feature_info(rfds, state$assay_info[["assay"]])
      out <- collect(arrange(out, name), n = Inf)
    }
    out
  })

  vals <- list(
    assay_info = assay_info,
    features = features,
    .state = state,
    .ns = session$ns)

  class(vals) <- c("AssaySelectInput")
  vals
}

#' @noRd
#' @export
#' @rdname assaySelect
assaySelectUI <- function(id, label = "Assay", choices = NULL, selected = NULL,
                          multiple = FALSE, selectize = TRUE,
                          width = NULL, size = NULL, ...) {
  ns <- NS(id)
  selectInput(ns("assay"), label = label,
              choices = choices, selected = selected, multiple = multiple,
              selectize = selectize, width = width, size = size)
}

#' Retrieve the features assocaitd with an assay
#'
#' @export
#' @param x An `AssaySelect` object, returned fom [assaySelect()]
#' @return a tibble of features
assay_feature_info.AssaySelectInput <- function(x, assay_name,
                                                feature_ids = NULL, ...) {
  assert_reacting()
  if (!missing(assay_name)) warning("`assay_name` parameter ignored")
  out <- x[["features"]]()
  if (!is.null(feature_ids)) {
    out <- filter(out, feature_id %in% feature_ids)
  }
  out
}
