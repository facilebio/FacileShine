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

  # The names of the assays available over the current samples
  assay.names <- reactive({
    req(initialized(rfds))
    aa <- req(isolate.(active_assays(rfds)))
    anames <- aa$assay
    ftrace("Updating set of available assay (names): ",
           paste(anames, collapse = ","))
    anames
  })

  # The currently active cohort of smaples may have no assay data!
  no.assays <- reactive({
    req(initialized(rfds))
    length(assay.names()) == 0L
  })

  # Update the assay choice dropdown if necessary when the underlying samples
  # change. If the currently selected assay is still available, then keep it
  # selected.
  observeEvent(assay.names(), {
    available_assays <- assay.names()
    .ai <- state$assay_info

    if (no.assays()) {
      fwarn("{bold}{red}No assays available for current active_samples{reset}")
      selected <- NULL
      update.state <- nrow(.ai) != 0L
    } else if (.ai$assay %in% available_assays) {
      selected <- .ai$assay
      update.state <- .ai$assay != selected
    } else {
      selected <- available_assays[1L]
      update.state <- TRUE
    }

    if (update.state) {
      ftrace("A change in available assays (assays()$assay) changes ",
             "{red}{bold}assay_info state{reset} variable to: {bold}",
             selected, "{reset}")
      .ai <- collect(FacileData::assay_info(rfds, selected), n = Inf)
      if (is.null(selected)) {
        .ai <- filter(.ai, FALSE)
      }
      state$assay_info <- .ai
    }

    updateSelectInput(session, "assay", choices = available_assays,
                      selected = selected)
  }, priority = 10)

  # beware that the available assays may be the empty set
  observeEvent(input$assay, {
    req(initialized(rfds))
    selected <- input$assay
    # req(!is.null(selected)) # NULLflash (prophelactic: no error thrown yet)
    if (unselected(selected) && !no.assays()) {
      # We aren't completely initialized yet, bail
      req(FALSE)
    }

    .ai <- state$assay_info
    if (!isTRUE(selected == .ai$assay)) {
      ftrace("New assay selected from input changes {red}{bold}state{reset}",
             "{bold}", .ai$assay, "->", selected, "{reset}")
      if (unselected(selected)) selected <- NULL
      .ai <- collect(FacileData::assay_info(rfds, selected), n = Inf)
      if (is.null(selected)) .ai <- filter(.ai, FALSE)
      state$assay_info <- .ai
    }
  })

  assay_info <- reactive(state$assay_info)

  features. <- reactive({
    assay_info. <- assay_info()
    if (!is(assay_info., "facile_frame")) {
      # Creates a 0-row tibble with correct columns
      out <- collect(features(rfds, default_assay(rfds)), n = 1L)
      out <- filter(out, FALSE)
    } else {
      out <- features(rfds, assay_info.[["assay"]])
      out <- collect(arrange(out, name), n = Inf)
    }
    out <- filter(out, grepl("^[a-zA-Z]", name))
    out <- select(out, assay, feature_type, feature_id, name)
    ftrace("updating available features")
    out
  })

  vals <- list(
    assay_info = assay_info,
    features = features.,
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

#' @noRd
#' @export
initialized.AssaySelectInput <- function(x, ...) {
  ainfo <- x$assay_info()
  !(unselected(ainfo$assay) ||
      unselected(ainfo$assay_type) ||
      unselected(ainfo$feature_type))
}
