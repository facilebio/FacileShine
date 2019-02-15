#' Retrieves quantitative assay data from a FacileDataStore
#'
#' We will compose this with the quantitativeSampleCovariate module to create
#' a super quantitativeTraitSelect, which enables the user to select assay
#' or quantitative covariates from a FacileDataStore
#'
#' @export
#' @rdname quantitativeAssayDataSelect
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
    data = "__initializing__",
    value = "__initializing__",
    features_all = .no.features)

  if (!is.null(.exclude)) {
    # TODO: Are we excluding assays altogether, features from assays, or both?
  }

  assays <- reactive({
    req(isolate.(rfds[["active_assays"]]()))
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

  # observe({
  #   choices <- with(state$features_all, setNames(name, feature_id))
  #   updateSelectizeInput(session, "features", choices = choices, server = TRUE,
  #                        selected = NULL)
  # })

  features <- reactive({
    fid <- req(input$features)
    current <- isolate(state$features)
    if (!setequal(fid, current$feature_id)) {
      state$features <- isolate(state$features_all) %>%
        filter(feature_id %in% fid)
    }
    state$features
  })

  vals <- list(
    assay_info = assay_info,
    features = features)

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
