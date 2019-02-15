# Covariate Selector ===========================================================

#' A selectInput that provides the categorical covariates over a FacileDataStore
#'
#' Use in conjunction with a categoricalSampleCovariateLevels module.
#'
#' Why am I using all of these seriously tortured names?
#'
#' @export
#' @importFrom shiny isolate reactive observe reactiveValues
#'
#' @rdname categoricalSampleCovariateSelect
#'
#' @param rfds A `ReactiveFacileDataStore`
#' @param .exclude a covariate tibble that incluees covariate,value pairs that
#'   should not be used in this covariate selector. This can be a reactive
#'   tibble if you want to be able to update the things one can't select for.
#' @param .reactive If `TRUE` (default), the module will constantly update the
#'   covariates made available based on changes to the
#'   `active_covariates(rfds)` tibble. Otherwise, the covariates made
#'   availabe (or .excluded) are fixed on module creation.
categoricalSampleCovariateSelect <- function(input, output, session, rfds, ...,
                                             .with_none = TRUE,
                                             .exclude = NULL,
                                             .reactive = TRUE) {
  assert_class(rfds, "ReactiveFacileDataStore")
  state <- reactiveValues(
    covariate = "__initializing__",
    levels = "__initializing__")

  if (!is.null(.exclude) && !.reactive) {
    assert_tibble(.exclude)
    assert_subset(names(.exclude), c("variable", "value"))
  }

  isolate. <- if (.reactive) base::identity else shiny::isolate

  sample.covariates <- reactive({
    covs <- req(isolate.(active_covariates(rfds)))
    out <- filter(covs, class == "categorical")

    if (!is.null(.exclude)) {
      ex <- if (is(.exclude, "reactive")) isolate.(.exclude()) else .exclude
      out <- anti_join(out, ex, by = c("variable", "value"))
    }

    out
  })

  sample.covariate.info <- reactive({
    .covs <- req(sample.covariates()) %>%
      group_by(variable) %>%
      mutate(n_samples = n(), n_levels = length(unique(value))) %>%
      group_by(variable, value) %>%
      mutate(n_in_level = n()) %>%
      ungroup()
    .covs
  })

  observe({
    choices <- req(sample.covariate.info()) %>%
      distinct(variable) %>%
      pull(variable)
    # TODO: Check if this is a factor and convert it with assigned level order
    #       for a "proper" sort.
    choices <- sort(choices)
    if (.with_none) {
      choices <- c("---", choices)
    }

    if (state$covariate %in% choices) {
      selected <- state$covariate
    } else {
      selected <- choices[1L]
    }
    updateSelectInput(session, "covariate", choices = choices,
                      selected = selected)
  })

  covariate <- reactive({
    cov <- req(input$covariate)
    if (cov != state$covariate) {
      state$covariate <- cov
    }
    state$covariate
  })

  cov.levels <- reactive({
    cov <- req(covariate())
    lvls <- req(sample.covariate.info()) %>%
      filter(variable == cov) %>%
      pull(value) %>%
      unique() %>%
      sort()
    if (!setequal(state$levels, lvls)) {
      state$levels <- lvls
    }
    state$levels
  })

  vals <- list(
    covariate = covariate,
    levels = cov.levels,
    covariates.all = sample.covariate.info)
  return(vals)
}

#' @noRd
#' @rdname categoricalSampleCovariateSelect
#' @importFrom shiny NS selectizeInput
categoricalSampleCovariateSelectUI <- function(id, label = "Covariate",
                                               choices = NULL, selected = NULL,
                                               multiple = FALSE,
                                               selectize = TRUE, width = NULL,
                                               size = NULL, ...) {
  ns <- NS(id)

  # tagList(
  #   selectInput(ns("covariate"), choices = NULL),
  #   selectizeInput(ns("values", choices = NULL)))

  tagList(
    selectInput(ns("covariate"), label = label, choices = choices,
                selected = selected, multiple = multiple, selectize = selectize,
                width = width, size = size))
}

# Covariate Levels Selector ====================================================

#' Use this with categoricalSampleCovariateSelect to enumerate its levels.
#'
#' @export
#' @param covaraite the `categoricalSampleCovariateSelect` module.
categoricalSampleCovariateLevels <- function(input, output, session, rfds,
                                             covariate, ...,
                                             .exclude = NULL, .reactive = TRUE) {
  assert_class(rfds, "ReactiveFacileDataStore")
  state <- reactiveValues(
    values = "__initializing__")

  observe({
    cov.levels <- covariate$levels()
    updateSelectizeInput(session, "values", choices = cov.levels, server = TRUE)
  })

  values <- reactive({
    vals <- input$values
    if (!setequal(vals, state$values)) {
      state$values <- vals
    }
    state$values
  })

  vals <- list(
    values = values)

  return(vals)
}

categoricalSampleCovariateLevelsUI <- function(id, ..., choices = NULL,
                                               options = NULL, width = NULL) {
  ns <- NS(id)

  tagList(
    selectizeInput(ns("values"), ..., choices = choices,
                   options = options, width = width))
}

