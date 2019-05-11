#' Retrieves assay features from a FacileDataStore for a given assay type.
#'
#' @export
#' @importFrom shiny
#'   callModule
#'   isolate
#'   observe
#'   reactive
#'   reactiveValues
#'   updateSelectizeInput
#' @rdname assayFeatureSelect
#' @return a list with the following elements:
#'   * `assay_info`: one row assay,assay_type,feature_type tibble
#'   * `features`: n-row feature_info_tbl() like tbl enumerating the assay
#'     features selected in this module
#'   * `features_all`: a tibble of all of the fdatures of this `feature_type`
#'     (we can possible axe this, but ...)
#'  * `label`: a "human readable" summary of the features selected within this
#'     module
#'  * `name`: a "computerfriendly" version of `label`
assayFeatureSelect <- function(input, output, session, rfds, ...,
                               .exclude = NULL, .reactive = TRUE) {
  assert_class(rfds, "ReactiveFacileDataStore")
  isolate. <- if (.reactive) base::identity else shiny::isolate

  state <- reactiveValues(
    selected = .no_features(),
    # "labeled" API
    name = "__initializing__",
    label = "__initializing__")


  if (!is.null(.exclude)) {
    # TODO: Are we excluding assays altogether, features from assays, or both?
  }

  assay_select <- callModule(assaySelect, "assay", rfds, .reactive = .reactive)

  # Update assayFeatureSelect with feature universe from assay_select
  observe({
    universe <- assay_select$features()
    choices <- setNames(universe[["feature_id"]], universe[["name"]])
    updateSelectizeInput(session, "features", choices = choices,
                         selected = NULL, server = TRUE)
  })

  selected <- reactive({
    req(initialized(rfds))
    current <- isolate(state$selected)
    selected_ids <- input$features
    universe <- assay_select$features()

    is.unselected <- unselected(selected_ids)
    bad.ids <- setdiff(selected_ids, universe[["feature_id"]])

    if (is.unselected || length(bad.ids)) {
      selected <- .no_features()
    } else {
      selected <- filter(universe, feature_id %in% selected_ids)
    }

    if (!setequal(current$feature_id, selected$feature_id)) {
      state$selected <- selected
    }

    state$selected
  })

  vals <- list(
    selected = selected,
    assay_info = assay_select$result,
    .state = state,
    .ns = session$ns)
  class(vals) <- c("AssayFeatureSelect", "FacileDataAPI", "Labeled")

  vals
}

#' @export
#' @importFrom shiny selectInput selectizeInput
#' @rdname assayFeatureSelect
#' @param multiple,... passed into the `"features"` `selectizeInput`
assayFeatureSelectUI <- function(id, label = NULL, multiple = TRUE, ...) {
  ns <- NS(id)

  tagList(
    fluidRow(
      column(9, selectizeInput(ns("features"), label = label, choices = NULL,
                               multiple = multiple, ...)),
      # column(3, selectInput(ns("assay"), label = NULL, choices = NULL))),
      column(3, assaySelectUI(ns("assay"), label = NULL, choices = NULL))),
    fluidRow(
      column(12,
             selectInput(ns("fset"), label = NULL,
                         choices = "GeneSetDb for assay"))))
}

# Labeled API ==================================================================

#' @noRd
#' @export
name.AssayFeatureSelect <- function(x, ...) {
  xf <- x[["selected"]]()
  out <- if (nrow(xf) == 0) {
    "nothing"
  } else if (nrow(xf) == 1) {
    xf$name
  } else {
    "score"
  }
  make.names(x[[".ns"]](out))
}

#' @noRd
#' @export
label.AssayFeatureSelect <- function(x, ...) {
  xf <- x[["selected"]]()
  if (nrow(xf) == 0) {
    "nothing"
  } else if (nrow(xf) == 1) {
    xf$name
  } else {
    paste(xf$name, collapse = ",")
  }
}

# Random =======================================================================
.no_features <- function() {
  tibble(
    assay = character(),
    feature_id = character(),
    name = character())
}
