

#' A select input for the user to pick features from an assay.
#'
#' If the user passes in the name of the assay as a reactive in the `assay`
#' parameter (for the module, or as the character string in the UI function),
#' then this module doesn't provide an "assay" dropdown, and only provides
#' the features for that assay.
#'
#' @param .exclude tibble of feauture_type,feature_id pairs that should not be
#'   included in this selector
assayFeatureSelect <- function(input, output, session, rfds, assay = NULL,
                               ..., .exclude = NULL, .reactive = TRUE) {
  state <- reactiveValues(
    assay = "__initializing__",
    feature_type = "__initializing__",
    features = "__initializing__")

  isolate. <- if (.reactive) base::identity else shiny::isolate

  # If external.assay == TRUE, then an external caller is dictating the assay to
  # select features from
  external.assay <- !is.null(assay)
  if (external.assay) {
    with.assay.select <- FALSE
    stopifnot(is(assay, "reactive"))
  } else {
    with.assay.select <- TRUE
  }

  assays <- reactive({
    isolate.(active_assays(rfds))
  })

  # Update the UI assay select box with the assays available from the current
  # state of the `rfds`.
  observe({
    .assays <- assays()
    selected <- state$assay
    if (!selected %in% .assays$assay) {
      selected <- intersect(default_assay(rfds[["fds"]]), assays$assay)
      if (length(selected) == 0L) selected <- .assays$assay[1L]
      state$assay <- selected
    }
    updateSelectInput(session, "assay", .assays$assay, selected = selected)
  })

  all.features <- reactive({
    .assay <- state$assay
    if (.assay == "__initializing__") return(NULL)
    out <- feature_info_tbl(rfds[["fds"]], assay_name = .assay)
    if (!is.null(.exclude)) {
      ex <- if (is(.exclude, "reactive")) isolate.(.exclude()) else .exclude
      out <- anti_join(out, ex, by = c("feature_type", "feature_id"))
    }
    out
  })

  assays <- NULL
  if (external.assay) {
    observe({
      .assay <- req(assay())
      if (.assay != state$assay) {
        state$assay <- .assay
      }
    })
  } else {
    # This module presents the user with an assay select.

    assays <- reactive()
  }

}

assayFeatureSelectUI <- function(id, assay = NULL, multiple = TRUE, ...,
                                 options = NULL, width = NULL) {
  ns <- NS(id)
  with.assay.select <- !testString(assay)

  tagList(
    if (with.assay.select) selectInput(ns("assay"), choices = NULL) else NULL,
    selectizeInput(ns("features"), multiple = multiple, ...,
                   options = options, width = width))
}
