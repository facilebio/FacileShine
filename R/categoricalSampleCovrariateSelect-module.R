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
                                             with_none = TRUE,
                                             exclude = reactive(NULL),
                                             ignoreNULL = with_none,
                                             debug = FALSE) {
  assert_class(rfds, "ReactiveFacileDataStore")
  assert_flag(include1)

  moduleServer(id, function(input, output, session) {
    state <- reactiveValues(
      rfds_name = "__initializing__",
      covariate = "__initializing__",
      multiple = "__initializing__",
      summary = .empty_covariate_summary(),
      exclude = character())
    
    observeEvent(exclude(), {
      ignore <- exclude()
      if (unselected(ignore)) ignore <- character()
      if (!is.character(ignore)) {
        ftrace("{reset}{red}illegal type of variable passed to exclude: ", 
               class(ignore)[1] ,"{reset}")
        ignore <- character()
      }
      if (!setequal(ignore, state$exclude)) {
        state$exclude <- ignore
      }
    })
    
    excluded <- reactive({
      ftrace("state||exclude has been updated: ", state$exclude)
      state$exclude
    })
    
    categorical_covariates <- eventReactive({
      active_covariates(rfds)
      excluded()
    }, {
      req(initialized(rfds))
      out <- rfds |>
        active_covariates() |>
        dplyr::filter(.data$class == "categorical") |>
        dplyr::filter(!.data$variable %in% state$exclude) |> 
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
    
    # Update the available covariates in the UI in response to a change in the
    # covariates that are available from the underlying set of samples
    # (or updated set of `exclusions`)
    observeEvent(categorical_covariates(), {
      choices <- unique(categorical_covariates()$variable)
      
      ftrace(
        "Updating available covariates to select from:\n  ",
        paste(choices, collapse = ";;"))
      
      if (with_none) {
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
        selected <- if (with_none) "---" else NULL
        state$covariate <- ""
      }
      
      if (length(state$covariate) > 1) {
        if (isFALSE(state$multiple)) state$multiple <- TRUE
      } else {
        if (isTRUE(state$multiple)) state$multiple <- FALSE
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
    }, ignoreNULL = ignoreNULL)
    
    covariate <- reactive({
      ftrace("covariate selection updated: ", state$covariate)
      # Before we return the covariate, let's make that the underlying samples
      # haven't changed and we aren't returning a stale covariate that used
      # to be available but has been replaced/fitered out.
      if (!unselected(state$covariate)) {
        req(all(state$covariate %in% categorical_covariates()$variable))
      }
      state$covariate
    })
    
    covariate.summary <- reactive({
      covariate. <- covariate()
      allcovs. <- categorical_covariates()
      notselected <- unselected(covariate.) ||
        !(all(covariate. %in% allcovs.[["variable"]]))
      
      ftrace(
        "Calculating covariate({red}", 
        paste(covariate., collapse = ","),
        "{reset}) summary")
      
      if (notselected) {
        out <- .empty_covariate_summary()
      } else {
        out <- allcovs. |> 
          # could be multiple so we test with %in% not ==
          filter(.data$variable %in% covariate.)
      }
      out
    })

    levels <- reactive({
      covariate.summary() |> 
        filter(ninlevel > 0) |> 
        dplyr::pull(level)
    })
    
    vals <- list(
      multiple = reactive(state$multiple),
      covariate = covariate,
      summary = covariate.summary,
      levels = levels,
      covariates_all = categorical_covariates,
      excluded = excluded,
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
name.CategoricalCovariateSelectModule <- function(x, ...) {
  out <- x[["covariate"]]()
  if (unselected(out)) NULL else out
}

#' @noRd
#' @export
label.CategoricalCovariateSelectModule <- function(x, ...) {
  warning("TODO: Need to provide labels for categorical covariates")
  x[["covariate"]]()
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
