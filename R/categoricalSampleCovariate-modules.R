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

  active.samples <- reactive({
    req(initialized(rfds))
    isolate.(active_samples(rfds))
  })

  active.covariates <- reactive({
    req(initialized(rfds))
    all.covs <- isolate.(active_covariates(rfds))
    cat.covs <- filter(all.covs, class == "categorical")
    cat.covs
  })

  # Updating the covariate select dropdown is a little tricky because we want
  # to support the situation where the current active.covariates change in
  # response to the current set of active_samples changing.
  #
  # For now we just try to keep the same covariate selected if it still remains
  # in the ones that are available after the underlying set of active_samples
  # and nactive_covariates shifts.
  observe({
    choices <- req(active.covariates()) %>%
      filter(nlevels > 1) %>%
      pull(variable)

    ftrace("Updating available covariates to select from")

    choices <- sort(choices)
    if (.with_none) {
      choices <- c("---", choices)
    }

    selected <- isolate(input$covariate)
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

  covariate.summary <- reactive({
    covariate. <- covariate()
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
                                             .reactive = TRUE,
                                             debug = FALSE) {
  assert_class(rfds, "ReactiveFacileDataStore")
  state <- reactiveValues(
    values = "__initializing__")

  observe({
    cov.levels <- covariate$levels()
    selected. <- intersect(cov.levels, isolate(input$values))
    if (length(selected.) == 0L) selected. <- NULL
    updateSelectizeInput(session, "values", choices = cov.levels,
                         selected = selected., server = TRUE)
  })

  values <- reactive({
    vals <- input$values

    # For some reason, when updateSelectizeInput updates the available
    # values, this isn't propogated immediately to the inputs. I
    # think shiny goes through a complete round of responding to
    # the reactivity before the input$ values get updated.
    invalid <- setdiff(vals, covariate$levels())

    if (length(invalid)) {
      state$values <- ""
    } else if (!setequal(vals, state$values)) {
      state$values <- vals
    }

    state$values
  })

  if (debug) {
    output$selected <- renderText(values())
  }

  vals <- list(
    values = values,
    .state = state,
    .ns = session$ns)

  return(vals)
}

#' @noRd
#' @export
#' @rdname categoricalSampleCovariateLevels
#' @importFrom shiny NS selectizeInput tagList textOutput
categoricalSampleCovariateLevelsUI <- function(id, ..., choices = NULL,
                                               options = NULL, width = NULL,
                                               debug = FALSE) {
  ns <- NS(id)

  out <- tagList(
    selectizeInput(ns("values"), ..., choices = choices,
                   options = options, width = width))
  if (debug) {
    out <- tagList(
      out,
      tags$p("Selected levels:", textOutput(ns("selected"))))
  }

  out
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
