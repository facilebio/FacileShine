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

  sample.universe <- active_samples(rfds[["fds"]])

  static.covariates <- reactive({
    req(isolate(rfds[["active_covariates"]]()))
  })

  sample.covariates <- reactive({
    # req(rfds[["active_covariates"]]()) %>%
    #   semi_join(sample.universe, by = c("dataset", "sample_id")) %>%
    #   # One day we will be able to filter against real-valued covariates.
    #   # For now, we restrict to filtering on categorical covariates
    #   filter(class == "categorical")
    req(static.covariates()) %>%
      filter(class == "categorical")
  })

  can.filter <- reactive({
    req(sample.covariates()) %>%
      # filter(n > 1) %>%
      arrange(variable) %>%
      distinct(variable) %>%
      pull(variable)
  })

  observe({
    updateSelectInput(session, "covariate", choices = can.filter())
  })

  observe({
    covariate <- req(input$covariate)
    choices <- req(sample.covariates()) %>%
      filter(variable == covariate) %>%
      pull(value)
    updateSelectizeInput(session, "values", choices = choices,
                         server = TRUE, selected = NULL)
  })

  observe({
    lvls <- input$values
    covariate <- req(isolate(input$covariate))
    covariates <- req(sample.covariates())
    if (length(lvls)) {
      selected.vars <- covariates %>%
        filter(variable == !!covariate, value %in% !!lvls)
      selected.samples <- sample.universe %>%
        semi_join(selected.vars, by = c("dataset", "sample_id"))
    } else {
      selected.samples <- sample.universe
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
    selectInput(ns("covariate"), label = "Covariate", choices = NULL),
    selectizeInput(ns("values"), label = "Value(s)", choices = NULL,
                   multiple = TRUE))
}
