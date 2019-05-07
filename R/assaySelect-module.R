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
      feature_type = "__initializing__"),
    universe = tibble(
      feature_type = character(),
      feature_id = character(),
      name = character()))

  # This a tibble of the available assays available for the active_samples
  # of the rfds
  assays <- reactive({
    req(initialized(rfds))
    aa <- req(isolate.(active_assays(rfds)))
    ftrace("Updating available set of assays stored internally: ",
           paste(aa$assay, collapse = ","))
    aa
  })

  # Update the assay choice dropdown if necessary when the underlying samples
  # change. If the currently selected assay is still available, then keep it
  # selected.
  observe({
    available_assays <- assays()$assay
    .ai <- isolate(state$assay_info)
    if (.ai$assay %in% available_assays) {
      selected <- .ai$assay
    } else {
      selected <- available_assays[1L]
    }

    if (isolate(state$assay_info$assay) != selected) {
      ftrace("A change in available assays (assays()$assay) changes ",
             "{red}{bold}assay_info state{reset} variable to: {bold}",
             selected, "{reset}")
      state$assay_info <- FacileData::assay_info(rfds, selected)
    }

    updateSelectInput(session, "assay", choices = available_assays,
                      selected = selected)
  })

  assay_info <- eventReactive(list(input$assay, state$assay_info), {
    req(!unselected(input$assay_info$assay))
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
    assay_info. <- state$assay_info
    if (!is(assay_info., "facile_frame")) {
      # Creates a 0-row tibble with correct columns
      out <- collect(assay_feature_info(rfds, default_assay(rfds)), n = 1L)
      out <- filter(out, FALSE)
    } else {
      out <- assay_feature_info(rfds, assay_info.[["assay"]])
      out <- collect(arrange(out, name), n = Inf)
    }
    out <- filter(out, grepl("^[a-zA-Z]", name))
    out <- select(out, assay, feature_type, feature_id, name)
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
