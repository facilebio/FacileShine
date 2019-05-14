#' A composable module to filter to a subset of samples from a FacileDataStore.
#'
#' This module will eventually be composed / replicated by adding more or less
#' filters to restrict (or expand) the sample space to be anlayzed.
#'
#' I think this means that the set of samples this filter can operate on are
#' fixed, but covariates can change given interactivity from the user during
#' an analysis session.
#'
#' @export
#' @rdname facileSampleFilter
#' @importFrom shiny updateSelectInput
facileSampleFilter <- function(input, output, session, rfds, ...,
                               .reactive = FALSE) {
  assert_class(rfds, "ReactiveFacileDataStore")

  covariate <- callModule(categoricalSampleCovariateSelect, "covariate",
                          rfds, .with_none = FALSE, .reactive = FALSE)
  values <- callModule(categoricalSampleCovariateLevels, "values",
                       rfds, covariate, .reactive = FALSE)

  isolate. <- if (.reactive) base::identity else shiny::isolate

  these.samples <- reactive({
    # where. <- "these.samples"
    # browser()
    req(initialized(rfds))
    ftrace("{bold}filter::these.samples(){reset}")
    req(isolate.(active_samples(rfds)))
  })

  these.covariates <- reactive({
    # where. <- "these.covariates"
    # browser()
    req(initialized(rfds))
    req(isolate(active_covariates(rfds)))
  })

  observe({
    cov.name <- covariate$covariate()
    cov.vals <- values$values()
    suniverse <- these.samples()
# browser()

    # it is possible that the elements in cov.vals can be stale due to cohort
    # narrowing, ie. the selected values stored in the select haven't updated
    # to a newly selected covariate. In this case, we try to intersect, or
    # blow out the selection entirely.

    # Is the user trying to restrict the sample space
    restrict.samples <- !unselected(cov.vals)

    if (restrict.samples) {
      selected.samples <- rfds %>%
        fetch_sample_covariates(suniverse, cov.name) %>%
        filter(value %in% !!cov.vals)
      if (nrow(selected.samples) == 0) browser()
    } else {
      selected.samples <- suniverse
    }

    update_reactive_samples(rfds, selected.samples)
  })

  vals <- list(
    covariate = reactive(input$covariate),
    values = reactive(input$values),
    .ns = session$ns)
  class(vals) <- "FacileSampleFilter"
  vals
}

#' @export
#' @rdname facileSampleFilter
#' @importFrom shiny column fluidRow NS
facileSampleFilterUI <- function(id, ...) {
  ns <- NS(id)

  fluidRow(
    column(
      4,
      categoricalSampleCovariateSelectUI(ns("covariate"), label = "Covariate")),
    column(
      4,
      categoricalSampleCovariateLevelsUI(ns("values"), label = "Value(s)",
                                         multiple = TRUE)))
}

# Accessor / Update Functions ==================================================

#' Returns a representaiton of the filters used to specify the active dataset
filters <- function(x, ...) {
  # TODO: Implemenet filters accessor for facileSampleFilter
}

#' Updates the filters used to set the active dataset
update_filters <- function(x, filters, ...) {
  # TODOO: Implement update_filters for facileSampleFilter
}
