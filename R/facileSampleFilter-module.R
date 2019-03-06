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
facileSampleFilter <- function(input, output, session, rfds, ...) {
  assert_class(rfds, "ReactiveFacileDataStore")

  covariate <- callModule(categoricalSampleCovariateSelect, "covariate",
                          rfds, .with_none = FALSE, .reactive = FALSE)
  values <- callModule(categoricalSampleCovariateLevels, "values",
                       rfds, covariate, .reactive = FALSE)

  these.samples <- reactive({
    req(isolate(active_samples(rfds)))
  })

  these.covariates <- reactive({
    req(isolate(active_covariates(rfds)))
  })

  observe({
    cov.name <- covariate$covariate()
    cov.vals <- values$values()
    suniverse <- these.samples()

    # Is the user trying to restrict the sample space
    restrict.samples <- length(cov.vals) > 0L &&
      !cov.vals[1L] %in% c("---", "__initializing__")

    if (restrict.samples) {
      selected.samples <- rfds %>%
        fetch_sample_covariates(suniverse, cov.name) %>%
        filter(value %in% !!cov.vals)
    } else {
      selected.samples <- suniverse
    }

    update_reactive_samples(rfds, selected.samples)
  })

  vals <- list(
    covariate = reactive(input$covariate),
    values = reactive(input$values))
}

#' @export
#' @rdname facileSampleFilter
#' @importFrom shiny selectInput selectizeInput
facileSampleFilterUI <- function(id, ...) {
  ns <- NS(id)

  tagList(
    categoricalSampleCovariateSelectUI(ns("covariate"), label = "Covariate"),
    categoricalSampleCovariateLevelsUI(ns("values"), label = "Value(s)",
                                       multiple = TRUE))
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
