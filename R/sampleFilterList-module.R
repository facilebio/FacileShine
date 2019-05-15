#' An expandable (and shrinkable) list of sampleFilter modules.
#'
#' Implementation largely inspired from the `filter_module.R` file in Joe
#' Cheng's rpharma-demo shiny app and a post from Barret Schloerke in the
#' RStudio community forums (cf. the "See Also" section).
#'
#' @export
#' @importFrom shiny insertUI removeUI setBookmarkExclude
#' @importFrom shinyjs toggleState
#' @seealso
#'   [Joe Cheng's rpharm-demo app](https://github.com/jcheng5/rpharma-demo),
#'   [Barret's Post](https://community.rstudio.com/t/22617/5)
sampleFilterList <- function(input, output, session, rfds, ..., debug = FALSE) {
  ns <- session$ns
  setBookmarkExclude(c("add_filter_btn"))

  state <- reactiveValues(
    filters = list())

  # onBookmark(function(state) {
  #   state$values$filter_field_names <- names(filter_fields)
  # })
  #
  # onRestore(function(state) {
  #   filter_field_names <- state$values$filter_field_names
  #   for (fieldname in filter_field_names) {
  #     addFilter(fieldname)
  #   }
  # })

  filters <- reactive({
    ftrace("filters() fired")
    state$filters
  })

  # Remove Filter button should only be enabled if there is >= 1 filters here
  observe({
    toggleState("rm_filter_btn", condition = length(filters()) > 0)
  })

  # Add button should only be enabled if we have no filters, or if the
  # current filter is empty
  observe({
    filters. <- filters()
    flen <- length(filters.)
    if (flen == 0) {
      enabled <- TRUE
    } else {
      f <- filters.[[flen]]
      enabled <- !unselected(f$values$values())
    }
    toggleState("add_filter_btn", condition = enabled)
  })

  observeEvent(input$rm_filter_btn, {
    filters. <- filters()
    flen <- length(filters.)
    req(flen > 0)

    id <- names(filters.)[flen]
    state$filters[[id]] <- NULL
    removeUI(selector = paste0("#", ns(paste0(id, "_container"))))
  })

  last.filter <- reactive({
    filters. <- filters()
    flen <- length(filters.)
    if (flen == 0) NULL else filters.[[flen]]
  })

  # When a new filter is added, or the last one is removed, the only
  # sampleFilter that should be editable is the last one on the list.
  observeEvent(last.filter(), {
    filters. <- filters()
    flen <- length(filters.)
    req(flen > 0)
    for (i in seq(flen)) {
      name <- names(filters.)[i]
      enabled <- i == flen
      select_id <- paste0(name, "-covariate-covariate")
      levels_id <- paste0(name, "-values-values")
      toggleState(select_id, condition = enabled)
      toggleState(levels_id, condition = enabled)
    }
  })

  reduced.samples <- reactive({
    lf <- last.filter()
    if (is.null(lf)) {
      out <- collect(samples(fds(rfds)), n = Inf)
    } else {
      out <- lf$active_samples()
    }
    out
  })

  # Add filters with functionality
  observeEvent(input$add_filter_btn, {
    filters. <- isolate(filters())
    flen <- length(filters.)

    index <- flen + 1L

    id <- paste0("sample_filter_", index)
    rmid <- paste0("remove_", id)

    if (flen == 0) {
      universe <- reactive(collect(samples(fds(rfds)), n = Inf))
    } else {
      universe <- reactive(isolate(filters.[[flen]]$active_samples()))
    }

    # Insert UI for sampleFilter
    insertUI(
      selector = paste0("#", ns("filter_container")),
      where = "beforeEnd",
      ui = shiny::tags$div(
        id = ns(paste0(id, "_container")),
        sampleFilterUI(ns(id),
                       covariate_label = if (index == 1) "Covariate" else NULL,
                       value_label = if (index == 1) "Value(s)" else NULL)),
      immediate = TRUE)
    state$filters[[id]] <- callModule(sampleFilter, id, rfds, universe)
  })

  if (debug) {
    output$samples <- DT::renderDT({
      samples. <- reduced.samples()
      covs <- sapply(filters(), function(sf) {
        cov <- sf$covariate$covariate()
        if (unselected(cov)) cov <- ""
        cov
      })
      covs <- setdiff(covs, "")
      if (length(covs)) {
        samples. <- with_sample_covariates(samples., covs)
      }
      samples.
    }, server = TRUE)
  }

  vals <- list(
    filters = filters,
    samples = reduced.samples,
    .ns = session$ns)
  vals
}

#' @noRd
#' @export
#' @importFrom shiny actionButton NS tagList tags
sampleFilterListUI <- function(id, ..., debug = FALSE) {
  ns <- NS(id)
  out <- tagList(
    tags$div(id = ns("filter_container")),
    actionButton(ns("rm_filter_btn"), "Remove Filter"),
    actionButton(ns("add_filter_btn"), "Add Filter"))

  if (debug) {
    out <- tagList(
      out,
      DT::DTOutput(ns("samples")))
  }

  out
}
