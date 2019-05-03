#' Retrieves assay features from a FacileDataStore for a given assay type.
#'
#' @export
#' @rdname assayFeatureSelect
#'
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
    features = .no_features(),
    # "labeled" API
    name = "__initializing__",
    label = "__initializing__")


  if (!is.null(.exclude)) {
    # TODO: Are we excluding assays altogether, features from assays, or both?
  }

  assay <- callModule(assaySelect, "assay", rfds, .reactive = .reactive)

  # Update the features available for selection based on the value of the
  # currently selected assay
  observe({
    features. <- req(assay_feature_info(assay))
    choices <- with(
      filter(features., nchar(name) > 0),
      setNames(feature_id, name))
    updateSelectizeInput(session, "features", choices = choices,
                         server = TRUE, selected = NULL)
  })

  # I think this may invalidate twice, which causes double draws in fscatter
  features <- reactive({
    fid <- input$features
    is.empty <- unselected(fid)
    if (is.empty) fid <- character()

    current <- isolate(state$features)
    if (!setequal(fid, current$feature_id)) {
      if (is.empty) {
        state$features <- .no_features()
      } else {
        f.all <- isolate(assay_feature_info(assay))
        state$features <- filter(f.all, feature_id %in% fid)
      }
    }

    state$features
  })

  vals <- list(
    features = features,
    assay_info = assay$result,
    .state = state,
    .ns = session$ns)
  class(vals) <- c("AssayFeatureSelect", "FacileDataAPI", "Labeled")

  vals
}

#' @export
#' @rdname assayFeatureSelect
#' @param multiple,... passed into the `"features"` `selectizeInput`
assayFeatureSelectUI <- function(id, multiple = TRUE, ...) {
  ns <- NS(id)

  tagList(
    fluidRow(
      column(9, selectizeInput(ns("features"), label = NULL, choices = NULL,
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
  assert_reacting()
  xf <- x[["features"]]()
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
  assert_reacting()
  xf <- x[["features"]]()
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
