#' Retrieves quantitative assay data from a FacileDataStore
#'
#' We will compose this with the quantitativeSampleCovariate module to create
#' a super quantitativeTraitSelect, which enables the user to select assay
#' or quantitative covariates from a FacileDataStore
#'
#' @export
#' @rdname quantitativeAssayDataSelect
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
quantitativeAssayDataSelect <- function(input, output, session, rfds, ...,
                                        .exclude = NULL, .reactive = TRUE) {
  assert_class(rfds, "ReactiveFacileDataStore")
  isolate. <- if (.reactive) base::identity else shiny::isolate

  .no.features <- tibble(
    assay = character(),
    feature_id = character(),
    name = character())

  state <- reactiveValues(
    assay_info = tibble(
      assay = "__initializing__",
      assay_type = "__initializing__",
      feature_type = "__initializing__"),
    features = .no.features,
    features_all = .no.features,
    # "labeled" API
    name = "__initializing__",
    label = "__initializing__")


  if (!is.null(.exclude)) {
    # TODO: Are we excluding assays altogether, features from assays, or both?
  }

  assays <- reactive({
    # req(isolate.(rfds[["active_assays"]]()))
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
      state$assay_info <- FacileData::assay_info(fds(rfds), selected)
    }
    updateSelectInput(session, "assay", choices = choices, selected = selected)
  })

  observe({
    .ai <- state$assay_info
    state$features_all <- fds(rfds) %>%
      assay_feature_info(.ai$assay) %>%
      arrange(name)
    choices <- with(
      # filter(state$features_all, !grepl("^\\d", name)),
      filter(state$features_all, nchar(name) > 0),
      setNames(feature_id, name))
    updateSelectizeInput(session, "features", choices = choices, server = TRUE,
                         selected = NULL)
  })

  assay_info <- reactive({
    .assay <- req(input$assay)
    .ai <- state$assay_info
    if (.ai$assay != .assay) {
      state$assay_info <- FacileData::assay_info(fds(rfds), .assay)
    }
    state$assay_info
  })

  # Update the features available for selection based on the value of the
  # currently selected assay
  features_all <- reactive({
    state$features_all
  })

  features <- reactive({
    fid <- req(input$features)
    current <- isolate(state$features)
    if (!setequal(fid, current$feature_id)) {
      state$features <- isolate(state$features_all) %>%
        filter(feature_id %in% fid)
    }
    state$features
  })

  .name <- reactive({
    xf <- features()
    if (nrow(xf) == 0) {
      "nothing"
    } else if (nrow(xf) == 1) {
      xf$name
    } else {
      "score"
    }
  })

  .label <- reactive({
    xf <- features()
    if (nrow(xf) == 0) {
      "nothing"
    } else if (nrow(xf) == 1) {
      xf$name
    } else {
      paste(xf$name, collapse = ",")
    }
  })

  vals <- list(
    assay_info = assay_info,
    features = features,
    name = .name,
    label = .label)
  class(vals) <- c("QuantitativeAssayDataSelect", "Labeled")
  vals
}


#' @export
#' @rdname quantitativeAssayDataSelect
#' @param multiple,... passed into the `"features"` `selectizeInput`
quantitativeAssayDataSelectUI <- function(id, multiple = TRUE, ...) {
  ns <- NS(id)

  tagList(
    fluidRow(
      column(9, selectizeInput(ns("features"), label = NULL, choices = NULL,
                               multiple = multiple, ...)),
      column(3, selectInput(ns("assay"), label = NULL, choices = NULL))),
    fluidRow(
      column(12,
             selectInput(ns("fset"), label = NULL,
                         choices = "Feature(Gene)Sets for assay (todo)"))))
}
