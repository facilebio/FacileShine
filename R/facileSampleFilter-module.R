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
                          rfds, .with_none = TRUE, .reactive = FALSE)
  values <- callModule(categoricalSampleCovariateLevels, "values",
                       rfds, covariate, .reactive = FALSE)

  these.samples <- reactive({
    req(isolate(rfds[["active_samples"]]()))
  })

  these.covariates <- reactive({
    req(isolate(rfds[["active_covariates"]]()))
  })

  observe({
    all.covs <- these.covariates()
    cov.name <- covariate$covariate()
    cov.vals <- values$values()
    suniverse <- these.samples()
    # Is the user trying to restrict the sample space
    restrict.samples <- length(cov.vals) > 0L &&
      !cov.vals[1L] %in% c("---", "__initializing__")
    if (restrict.samples) {
      selected.vars <- all.covs %>%
        filter(variable == !!cov.name, value %in% !!cov.vals)
      selected.samples <- suniverse %>%
        semi_join(selected.vars, by = c("dataset", "sample_id"))
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
