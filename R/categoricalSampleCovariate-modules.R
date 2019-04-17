# Covariate Selector ===========================================================

#' A selectInput that provides the categorical covariates over a FacileDataStore
#'
#' Use in conjunction with a categoricalSampleCovariateLevels module.
#'
#' Why am I using all of these seriously tortured names?
#'
#' @export
#' @importFrom shiny
#'   isolate
#'   observe
#'   reactive
#'   reactiveValues
#'   req
#'   updateSelectInput
#' @rdname categoricalSampleCovariateSelect
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
    summary = .empty_covariate_summary(rfds),
    levels = "__initializing__")

  if (!is.null(.exclude) && !.reactive) {
    assert_tibble(.exclude)
    assert_subset(names(.exclude), c("variable", "value"))
  }

  isolate. <- if (.reactive) base::identity else shiny::isolate

  active.covariates <- reactive({
    all.covs <- isolate.(active_covariates(rfds))
    cat.covs <- filter(all.covs, class == "categorical")
    cat.covs
  })

  # Updates the covariate selectInput object. This is a bit more complicated
  # than you might think it needs to be because the active.covariates() my
  # fire, but we don't want to change the currently selected covariate if it
  # is available in the update covariates.
  observe({
    choices <- req(active.covariates()) %>%
      filter(nlevels > 1) %>%
      pull(variable)

    choices <- sort(choices)
    if (.with_none) {
      choices <- c("---", choices)
    }

    # overlap <- intersect(state$covariate, choices)
    selected <- input$covariate
    overlap <- intersect(selected, choices)
    if (length(overlap)) {
      if (!setequal(state$covariate, overlap)) state$covariate <- overlap
      selected <- overlap
    } else {
      selected <- NULL
      state$covariate <- ""
    }

    updateSelectInput(session, "covariate", choices = choices,
                      selected = selected)
  })

  # A reactive for the currently selected covariate in the selectInput
  covariate <- reactive({
    cov <- input$covariate
    if (unselected(cov)) cov <- ""
    if (!setequal(cov, state$covariate)) {
      state$covariate <- cov
    }
    state$covariate
  })

  active.samples <- reactive({
    isolate.(active_samples(rfds))
  })


  covariate.summary <- reactive({
    covariate. <- state$covariate
    allcovs. <- active.covariates()
    notselected <- unselected(covariate.) ||
      !covariate. %in% allcovs.[["variable"]]
    if (notselected) {
      out <- .empty_covariate_summary(rfds)
    } else {
      scovs <- fetch_sample_covariates(rfds, active.samples(), covariate.)
      out <- summary(scovs, expanded = TRUE)
    }
    out
  })

  cov.levels <- reactive({
    # browser()
    ci <- req(covariate.summary())
    lvls <- ci[["level"]]
    if (!setequal(state$levels, lvls)) {
      state$levels <- lvls
    }
    state$levels
  })

  vals <- list(
    covariate = covariate,
    summary = covariate.summary,
    levels = cov.levels,
    .state = state,
    .ns = session$ns)
  class(vals) <- c("CategoricalCovariateSelect",
                   "CovariateSelect",
                   "FacileDataAPI",
                   "Labeled")
  return(vals)
}

#' @noRd
#' @export
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

update_selected <- function(x, covariate, ...) {
  # TODO: enable callback/update of the selected categorical covariate
}

#' @noRd
#' @export
name.CategoricalCovariateSelect <- function(x, ...) {
  x[["covariate"]]()
}

#' @noRd
#' @export
label.CategoricalCovariateSelect <- function(x, ...) {
  warning("TODO: Need to provide labels for categorical covariates")
  x[["covariate"]]()
}


# Covariate Levels Selector ====================================================

#' Use this with categoricalSampleCovariateSelect to enumerate its levels.
#'
#' @export
#' @rdname categoricalSampleCovariateLevels
#' @param covaraite the `categoricalSampleCovariateSelect` module.
categoricalSampleCovariateLevels <- function(input, output, session, rfds,
                                             covariate, ...,
                                             .exclude = NULL,
                                             .reactive = TRUE) {
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

#' @noRd
#' @export
#' @rdname categoricalSampleCovariateLevels
categoricalSampleCovariateLevelsUI <- function(id, ..., choices = NULL,
                                               options = NULL, width = NULL) {
  ns <- NS(id)

  tagList(
    selectizeInput(ns("values"), ..., choices = choices,
                   options = options, width = width))
}

# Internal Helper Functions ====================================================

#' @noRd
.empty_covariate_summary <- function(.fds = NULL) {
  out <- tibble(
    variable = character(),
    class = character(),
    nsamples = integer(),
    level = character(),
    ninlevel = integer())
  if (!is.null(.fds)) {
    assert_facile_data_store(.fds)
    out <- as_facile_frame(out, .fds, .valid_sample_check = FALSE)
  }
  out
}
