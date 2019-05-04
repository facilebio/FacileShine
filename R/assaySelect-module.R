#' A module to create a dropdown over the current assays defined for a datastore
#'
#' @export
#' @rdname assaySelect
#' @importFrom shiny
#'   observe
#'   reactive
#'   reactiveValues
#'   updateSelectInput
#' @param rfds A ReactiveFacileDataStore
assaySelect <- function(input, output, session, rfds, ..., .reactive = TRUE)  {

  assert_class(rfds, "ReactiveFacileDataStore")
  isolate. <- if (.reactive) base::identity else shiny::isolate

  # store the current selected assay in state
  state <- reactiveValues(
    assay_info = tibble(
      assay = "__initializing__",
      assay_type = "__initializing__",
      feature_type = "__initializing__"))

  # This a tibble of the available assays available for the active_samples
  # of the rfds
  assays <- reactive({
    aa <- req(isolate.(active_assays(rfds)))
    ftrace("Updating available set of assays")
    aa
  })

  # Update the assay choice dropdown if necessary when the underlying samples
  # change. If the currently selected assay is still available, then keep it
  # selected.
  observe({
    available_assays <- assays()$assay
    .ai <- state$assay_info
    if (.ai$assay %in% available_assays) {
      selected <- .ai$assay
    } else {
      selected <- available_assays[1L]
      ftrace("A change in active_assays() changes selected assay in ",
             "{red}{bold}state{reset} variable")
      state$assay_info <- FacileData::assay_info(rfds, selected)
    }
    updateSelectInput(session, "assay", choices = available_assays,
                      selected = selected)
  })

  assay_info <- reactive({
    .assay <- req(input$assay)
    .ai <- state$assay_info
    if (.ai$assay != .assay) {
      ftrace("upated selected assay from input changes {red}{bold}state{reset}")
      state$assay_info <- FacileData::assay_info(rfds, .assay)
    }
    state$assay_info
  })

  features <- reactive({
    ftrace("updating available features")
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
  # assert_reacting()
  if (!missing(assay_name)) warning("`assay_name` parameter ignored")
  out <- x[["features"]]()
  if (!is.null(feature_ids)) {
    out <- filter(out, feature_id %in% feature_ids)
  }
  out
}
