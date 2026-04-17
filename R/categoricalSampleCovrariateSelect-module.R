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
#' @param default_covariate A character vector of preferred covarites to have
#'   selected by default. If this is `lenght() > 1` the first covariate found
#'   will be selected. If no levels match the covariate, then no preference
#'   to covariate is triggered
#' @param a reactive character vector, which specifies the covariates to ignore
#'   or a tibble with "variable" (and optional "value") columns. If a character
#'   vector, or tibble with just has a "variable" column are provide, then
#'   the variable names enumerated there will not be included in this dropdown.
#'   A tibble with "variable" and "value" columns can be used so that only
#'   specific levels of a covariate are ignored.
categoricalSampleCovariateSelectServer <- function(
    id,
    rfds,
    include1 = TRUE,
    include_id_like = FALSE,
    id_like_threshold = 0.85,
    default_covariate = NULL,
    ...,
    include_sample_id = FALSE,
    with_none = TRUE,
    exclude = reactive(NULL),
    ignoreNULL = with_none,
    debug = FALSE
) {
  assert_class(rfds, "ReactiveFacileDataStore")
  assert_flag(include1)

  moduleServer(id, function(input, output, session) {
    state <- reactiveValues(
      rfds_name = "__initializing__",
      selected = "__initializing__",
      multiple = "__initializing__",
      summary = .empty_covariate_summary(),
      exclude = character())
    
    in_sync <- reactive({
      state$rfds_name == name(rfds)
    })
    
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
      # active_covariates(rfds)
      rfds$active_covariates()
      excluded()
    }, {
      req(initialized(rfds))
      asamples <- rfds$active_samples()
      
      out <- rfds$active_covariates() |> 
        dplyr::filter(.data$class == "categorical") |>
        dplyr::filter(!.data$variable %in% state$exclude) |> 
        dplyr::arrange(variable, level)
      level_count <- out |> 
        dplyr::summarize(nlevels = n(), .by = "variable")
      
      if (!include1) {
        single_level_covariates <- level_count |> 
          dplyr::filter(nlevels == 1L)
        out <- dplyr::anti_join(out, single_level_covariates, by = "variable")
      }
      if (!include_id_like) {
        id_like <- level_count |> 
          dplyr::filter(nlevels / nrow(asamples) >= id_like_threshold)
        out <- dplyr::anti_join(out, id_like, by = "variable")
      }
      
      if (state$rfds_name != name(rfds)) {
        state$rfds_name <- name(rfds)
      }
      
      if (include_sample_id) {
        sids <- asamples |> 
          dplyr::transmute(
            variable = "sample_id",
            class = "categorical",
            nsamples = nrow(asamples),
            level = .data$sample_id,
            ninlevel = 1L
          )
        out <- out |> dplyr::bind_rows(sids)
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
      if (unselected(selected)) {
        selected <- default_covariate
      }
      
      overlap <- intersect(selected, choices)
      if (length(overlap) > 0L) {
        # it's possible length(overlap) > 1 because default_covariate can be
        # many options, so we just take the first element by default if we
        # can't have multiple select.
        if (isFALSE(state$multiple)) {
          overlap <- overlap[1L]
        }
        if (!setequal(state$covariate, overlap)) {
          state$selected <- overlap
        }
        selected <- overlap
      } else {
        selected <- if (with_none) "---" else NULL
        state$selected <- ""
      }
      
      if (length(state$selected) > 1) {
        if (isFALSE(state$multiple)) state$multiple <- TRUE
      } else {
        if (isTRUE(state$multiple)) state$multiple <- FALSE
      }
      # updateSelectInput(session, "covariate", choices = choices,
      #                   selected = selected)
      shinyWidgets::updatePickerInput(
        session,
        "covariate",
        choices = choices,
        selected = selected
      )
    })
    
    observeEvent(input$covariate, {
      cov <- input$covariate
      ftrace("{bold}covariate selectInput has fired: `", cov, "`")
      current <- state$selected
      if (unselected(cov)) cov <- ""
      if (!setequal(cov, current)) {
        ftrace("{bold}Updating covariate state: {current} -> {cov}{reset}",
               current = current, cov = cov)
        state$selected <- cov
      }
    }, ignoreNULL = ignoreNULL)
    
    
    selected <- reactive({
      ftrace("covariate selection updated: ", state$covariate)
      # Before we return the covariate, let's make that the underlying samples
      # haven't changed and we aren't returning a stale covariate that used
      # to be available but has been replaced/fitered out.
      req(in_sync())
      if (!unselected(state$selected)) {
        req(all(state$selected %in% categorical_covariates()$variable))
      }
      state$selected
    })
    
    selected_summary <- reactive({
      covariate. <- selected()
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
      selected_summary() |> 
        filter(ninlevel > 0) |> 
        dplyr::pull(level)
    })
    
    vals <- list(
      selected = selected,
      summary = selected_summary,
      levels = levels,
      in_sync = in_sync,
      multiple = reactive(state$multiple),
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
    size = NULL,
    pickerOptions = list(
      dropdownAlignRight = FALSE,
      dropupAuto = TRUE
    ),
    ...) {
  ns <- NS(id)
  
  pickerargs <- as.list(formals(shinyWidgets::pickerOptions))
  for (oname in intersect(names(pickerargs), names(pickerOptions))) {
    pickerargs[[oname]] <- pickerOptions[[oname]]
  }
  pickerargs[["..."]] <- NULL

  pickeropts <- do.call(shinyWidgets::pickerOptions, pickerargs)
  tagList(
    # selectInput(ns("covariate"), label = label, choices = choices,
    #             selected = selected, multiple = multiple, selectize = selectize,
    #             width = width, size = size)
    shinyWidgets::pickerInput(
      ns("covariate"),
      label = label,
      choices = choices,
      selected = selected,
      multiple = multiple,
      width = width,
      options = pickeropts
    )
  )
}

#' @export
#' @noRd
initialized.CategoricalCovariateSelectModule <- function(x, ...) {
  check <- c("selected", "levels")
  ready <- sapply(check, \(s) !unselected(x$.state[[s]]))
  all(ready)
}

#' @export
#' @noRd
from_fds.CategoricalCovariateSelectModule <- function(x, rfds, ...) {
  .Deprecated("Use x$in_sync() instead. See `?from_fds`")
  isolate(x[[".state"]]$rfds_name == name(rfds))
}

#' @noRd
#' @export
name.CategoricalCovariateSelectModule <- function(x, ...) {
  out <- x$selected()
  if (unselected(out)) NULL else out
}

#' @noRd
#' @export
label.CategoricalCovariateSelectModule <- function(x, ...) {
  warning("TODO: Need to provide labels for categorical covariates")
  x$selected()
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
