#' A categoricalSampleCovariate selector
#'
#' This is the second version of this module, which was updated to allow for a
#' `universe` argument, which specifies a subset of samples from `rfds` to get
#' covariate data from. The original implementation always retrieved covariates
#' from `active_samples(rfds)`. If `universe = NULL`, original behavior is
#' kept.
#'
#' @section Excluding covariates:
#'
#' TODO: Let's talk about how to exclude covariates and their levels, and point
#' users to [update_exclude()]
#'
#' @rdname categoricalSampleCovariateSelect
#' @export
#' @importFrom shiny
#'   isolate
#'   observe
#'   reactive
#'   reactiveValues
#'   req
#'   updateSelectInput
#' @param default_covariate the name of a covariate to use as the default
#'   one that is selected here, if available
#' @param a reactive character vector, which specifies the covariates to ignore
#'   or a tibble with "variable" (and optional "value") columns. If a character
#'   vector, or tibble with just has a "variable" column are provide, then
#'   the variable names enumerated there will not be included in this dropdown.
#'   A tibble with "variable" and "value" columns can be used so that only
#'   specific levels of a covariate are ignored.
categoricalSampleCovariateSelectServer <- function(id, rfds, include1 = TRUE,
                                             default_covariate = NULL,
                                             ...,
                                             .with_none = TRUE,
                                             .exclude = NULL,
                                             ignoreNULL = TRUE,
                                             debug = FALSE) {
  assert_class(rfds, "ReactiveFacileDataStore")
  assert_flag(include1)

  moduleServer(id, function(input, output, session) {
    state <- reactiveValues(
      rfds_name = "__initializing__",
      covariate = "__initializing__",
      levels = "__initializing__",
      summary = .empty_covariate_summary(),
      active_covariates = "__initializing__",
      universe = "__initializing__",
      exclude = tibble(variable = character(), value = character()))
    
    categorical_covariates <- eventReactive(active_covariates(rfds), {
      req(initialized(rfds))
      out <- rfds |>
        active_covariates() |>
        dplyr::filter(.data$class == "categorical") |>
        dplyr::arrange(variable, level)
      if (!include1) {
        single_level_covariates <- out |> 
          dplyr::summarize(nlevels = n(), .by = "variable") |> 
          dplyr::filter(nlevels == 1L)
        out <- anti_join(out, single_level_covariates, by = "variable")
      }
      if (state$rfds_name != name(rfds)) {
        state$rfds_name <- name(rfds)
      }
      
      out
    }, label = "categorical_covariates")
    
    # Updating the covariate select dropdown is a little tricky because we want
    # to support the situation where the current active.covariates change in
    # response to the current set of active_samples changing.
    #
    # Because this event listens to the underlying active.covariates() and can
    # update the internal state of the selected covariate, we want crank up the
    # priority to 10, so that if a covariate "goes missing" in a cohort swap,
    # the selected covariate gets reset to unselected
    observeEvent(categorical_covariates(), {
      choices <- unique(categorical_covariates()$variable)
      
      ftrace(
        "Updating available covariates to select from:\n  ",
        paste(choices, collapse = ";;"))
      
      if (.with_none) {
        choices <- c("---", choices)
      }
      
      selected <- input$covariate
      if (unselected(selected)) selected <- default_covariate
      
      overlap <- intersect(selected, choices)
      if (length(overlap)) {
        if (!setequal(state$covariate, overlap)) {
          state$covariate <- overlap
        }
        selected <- overlap
      } else {
        selected <- if (.with_none) "---" else NULL
        state$covariate <- ""
      }
      
      updateSelectInput(session, "covariate", choices = choices,
                        selected = selected)
    })
    
    observeEvent(input$covariate, {
      cov <- input$covariate
      ftrace("{bold}covariate selectInput has fired: `", cov, "`")
      current <- state$covariate
      if (unselected(cov)) cov <- ""
      if (!setequal(cov, current)) {
        ftrace("{bold}Updating covariate state: {current} -> {cov}{reset}",
               current = current, cov = cov)
        state$covariate <- cov
      }
    })
    
    covariate <- reactive({
      ftrace("covariate selection updated: ", state$covariate)
      state$covariate
    })
    
    covariate.summary <- reactive({
      covariate. <- covariate()
      allcovs. <- categorical_covariates()
      notselected <- unselected(covariate.) ||
        !covariate. %in% allcovs.[["variable"]]
      
      ftrace("Calculating covariate({red}", covariate., "{reset}) summary")
      
      if (notselected) {
        out <- .empty_covariate_summary()
      } else {
        # Need to isolate the [sample] universe.() because that is updated at a
        # higher priority, and sometimes when samples changed via cohort
        # selection, it was triggering this code block to fire before the selected
        # covariate could be reset to one that exists in this cohort (ie. if the
        # cohort shift removes the currently selected covariate from the cohort).
        # univ <- isolate(universe.())
        # scovs <- try(fetch_sample_covariates(rfds, univ, covariate.), silent = TRUE)
        # req(!is(scovs, "try-error"))
        # out <- summary(scovs, expanded = TRUE)
        out <- allcovs. |> 
          filter(.data$variable == covariate.)
      }
      out
    })
    
    observeEvent(req(covariate.summary()), {
      csummary <- covariate.summary()
      lvls <- csummary[["level"]]
      if (!setequal(state$levels, lvls)) {
        ftrace("Resetting available levels for {red}", covariate(), "{reset}")
        state$levels <- lvls
      }
    })
    
    cov.levels <- reactive({
      if (unselected(state$levels)) {
        "" 
      } else {
        state$levels
      }
    })
    
    vals <- list(
      covariate = covariate,
      summary = covariate.summary,
      levels = cov.levels,
      catcovs = categorical_covariates,
      .state = state,
      .ns = session$ns)
    class(vals) <- c("CategoricalCovariateSelectModule",
                     "CovariateSelect",
                     "FacileDataAPI",
                     "Labeled")
    return(vals)
  })
}

#' UI for the categorical sample covariate selector
#' 
#' @noRd
#' @export
#' @rdname categoricalSampleCovariateSelect
#' @importFrom shiny NS selectInput
categoricalSampleCovariateSelectInput <- function(
    id, label = "Covariate",
    choices = NULL, selected = NULL,
    multiple = FALSE,
    selectize = TRUE, width = NULL,
    size = NULL, ...) {
  ns <- NS(id)
  
  tagList(
    selectInput(ns("covariate"), label = label, choices = choices,
                selected = selected, multiple = multiple, selectize = selectize,
                width = width, size = size))
}

#' @export
#' @noRd
initialized.CategoricalCovariateSelectModule <- function(x, ...) {
  check <- c("covariate", "levels")
  ready <- sapply(check, \(s) !unselected(x$.state[[s]]))
  all(ready)
}

#' @export
#' @noRd
from_fds.CategoricalCovariateSelectModule <- function(x, rfds, ...) {
  isolate(x[[".state"]]$rfds_name == name(rfds))
}

#' @noRd
#' @export
name.CategoricalCovariateSelect <- function(x, ...) {
  out <- x[["covariate"]]()
  if (unselected(out)) NULL else out
}

#' @noRd
#' @export
label.CategoricalCovariateSelect <- function(x, ...) {
  warning("TODO: Need to provide labels for categorical covariates")
  x[["covariate"]]()
}

# Selector for levels ----------------------------------------------------------
#' Use this with categoricalSampleCovariateSelect to enumerate its levels.
#'
#' @export
#' @rdname categoricalSampleCovariateLevels
#' @param covaraite the `categoricalSampleCovariateSelect` module.
#' @param missing_sentinel This is a reactive (string). When it's NULL, no
#'   missing sentinel is added. The parent covariate selector can pass in a
#'   value here to show to indicate a level that's not included in the
#'   covariate's level.
#' @importFrom shiny updateSelectizeInput
categoricalSampleCovariateLevelsSelectServer <- function(
    id, covariate, ..., missing_sentinel = NULL, .exclude = NULL, 
    debug = FALSE) {
  assert_class(covariate, "CategoricalCovariateSelectModule")
  
  if (!is.null(missing_sentinel)) {
    # This should be a reactive string
    if (!is(missing_sentinel, "reactive")) {
      fwarn("missing_sentinal is not a reactive")
    }
  }
  
  moduleServer(id, function(input, output, session) {
    state <- reactiveValues(
      values = "__initializing__",
      levels = character(),
      exclude = character())
    
    # observe({
    #   req(initialized(rfds))
    #   update_exclude(state, isolate(.exclude), type = "covariate_levels")
    # })
    
    exclude. <- reactive(state$exclude)
    
    observeEvent(covariate$levels(), {
      levels. <- covariate$levels()
      if (!is.null(missing_sentinel)) {
        levels. <- unique(c(levels., missing_sentinel()))
      }
      
      ignore <- exclude.()
      newlevels <- setdiff(levels., ignore)
      if (!setequal(state$levels, newlevels)) {
        ftrace("Updating levels from (", state$levels, "), to (",
               newlevels, ")")
        state$levels <- newlevels
      }
    # }, priority = 10)
    })
    
    levels <- reactive(state$levels)
    
    observeEvent(levels(), {
      levels. <- levels()
      selected. <- input$values
      if (unselected(selected.)) selected. <- ""
      
      overlap. <- intersect(selected., levels.)
      if (unselected(overlap.)) overlap. <- ""
      if (!isTRUE(setequal(overlap., state$values))) {
        ftrace("change in availavble levels (",
               paste(levels., collapse = ","),
               ") updates selected level to: `", overlap., "`")
        state$values <- overlap.
      }
      updateSelectizeInput(session, "values", choices = levels.,
                           selected = overlap., server = TRUE)
    # }, priority = 10)
    })
    
    observeEvent(input$values, {
      selected. <- input$values
      # This is required because ignoreNULL is set to `FALSE`. We set it to
      # false so that when all selected levels are removed from
      # covariateSelectLevels, the values are released "back to the pool".
      # When ignoreNULL is false, however, there are intermediate in the
      # reactivity cycle when input$values is NULL even though its value
      # hasn't changed.
      #
      # The latter situation hit me when I was trying to make "mutually
      # exclusive" categoricalSampleCovariateLevels that are populated
      # from the same categoricalCovariateSelect by using the .exclude
      # mojo
      
      # THIS IS SO CLOSE: I need to put the req(!is.null()) here for the
      # mutually-excluve categoricalCovariateSelectLevel modules to work.
      # TODO: Finish categoricalSampleCovariateLevelsMutex
      # req(!is.null(selected.))
      if (unselected(selected.)) {
        selected. <- ""
      }
      if (!isTRUE(setequal(selected., state$values))) {
        ftrace("Change of selected input$values changes internal state from ",
               "`", isolate(state$values), "` ",
               "to {bold}{magenta}`", selected., "`{reset}")
        # logical covariates are stored as 0/1 when retrieved out of SQLite
        # database, let's convert T/F to 0/1 here, too
        csummary <- covariate$summary()
        if (nrow(csummary) > 0L && csummary[["class"]][1L] == "logical") {
          selected. <- ifelse(selected. == "TRUE", "1", selected.)
          selected. <- ifelse(selected. == "FALSE", "0", selected.)
        }
        state$values <- selected.
      }
    }, ignoreNULL = FALSE)
    
    values <- reactive(state$values)
    
    if (debug) {
      output$selected <- renderText(values())
    }
    
    vals <- list(
      values = values,
      levels = levels,
      .state = state,
      .ns = session$ns)
    class(vals) <- "CategoricalCovariateSelectLevels"
    return(vals)
  })
}
  
  #' @noRd
  #' @export
#' @rdname categoricalSampleCovariateLevels
#' @importFrom shiny NS selectizeInput tagList textOutput
categoricalSampleCovariateLevelsSelectInput <- function(
    id, ..., 
    label = NULL, choices = NULL,
    options = NULL, width = NULL,
    debug = FALSE) {
  ns <- NS(id)
  
  out <- tagList(
    selectizeInput(ns("values"), label = label, choices = choices,
                   options = options, width = width))
  if (debug) {
    out <- tagList(
      out,
      tags$p("Selected levels:", textOutput(ns("selected"))))
  }
  
  out
}
