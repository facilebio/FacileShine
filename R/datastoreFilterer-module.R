datastoreFilterer <- function(input, output, session, rfds, ...) {

  flist <- callModule(filterList, "flist", rfds, ...)
}

# FilterList module ============================================================

#' Implementation of the filterlist module is inspired by Joe Cheng's
#' `filter_module.R` from the rpharma-demo shiny app:
#' https://github.com/jcheng5/rpharma-demo
#'
#' @noRd
#' @export
#' @importFrom shiny setBookmarkExclude
filterList <- function(input, output, session, rfds, ..., debug = FALSE) {
  ns <- session$ns
  setBookmarkExclude(c("add_filter_btn"))

  state <- reactiveValues(
    filters = list(),
    samples = "__initializing__")

  # onBookmark(function(state) {
  #   state$values$filter_field_names <- names(filter_fields)
  # })
  #
  # onRestore(function(state) {
  #   filter_field_names <- state$values$filter_field_names
  #   for (fieldname in filter_field_names) {
  #     addFilter(fieldname)
  #   }
  # })

  filters <- reactive(state$filters)
  flength <- reactive(length(filters()))

  observeEvent(flength(), {
    flen <- flength()
    if (flen == 0L) {
      samples. <- samples(fds(rfds))
    } else {
      samples. <- filters[[flen]]$active_samples
    }
    req(test_sample_subset(samples.))
    state$samples <- samples.
  })

  reduced.samples <- reactive({
    state$samples
  })

  # Add filters with functionality
  observeEvent(input$add_filter_btn, {
    index <- isolate(flength())
    id <- paste0("filter__", index)
    rmid <- paste0("remove_", id)

    # create the filter
    # state$filters[[index]] <- callModule(sampleFilter, id)
    state$filters[[id]] <- callModule(sampleFilter, id, rfds, reduced.samples)
    shiny::freezeReactiveValue(input, id)

    shiny::insertUI(
      selector = paste0("#", ns("filter_container")),
      where = "beforeEnd",
      ui = shiny::tagList(
        sampleFilterUI(ns(id)),
        shiny::actionButton(ns(rmid), "Remove")))

    # respond to move button
    observeEvent(input[[rmid]], {
      state$filters[[id]] <- NULL
      removeUI(selector = paste0("#", id))
    })
  })

  vals <- list(
    filters = filters,
    samples = reduced.samples,
    .ns = session$ns)
  vals
}

#' @noRd
#' @export
#' @importFrom shiny actionButton NS tagList tags
filterListUI <- function(id, ..., debug = FALSE) {
  ns <- NS(id)
  tagList(
    tags$div(id = ns("filter_container")),
    actionButton(ns("add_filter_btn"), "Add Filter"))
}

# Sample Filter Module ========================================================

# This version of the sampleFilter accepts the ReactiveFacileDataStore and the
# sample_universe that are to be acted on. `sample_universe` is not reactive.

#' @noRd
#' @export
sampleFilter <- function(input, output, session, rfds, sample_universe = NULL,
                         ..., .reactive = FALSE, debug = FALSE) {
  isolate. <- if (.reactive) base::identity else shiny::isolate
  if (is.null(sample_universe)) {
    sample_universe <- reactive(isolate.(active_samples(rfds)))
  }

  assert_class(rfds, "ReactiveFacileDataStore")
  assert_class(sample_universe, "reactive")

  covariate <- callModule(categoricalSampleCovariateSelect, "covariate",
                          rfds, sample_universe, .with_none = FALSE,
                          .reactive = FALSE)
  values <- callModule(categoricalSampleCovariateLevels, "values",
                       rfds, covariate, .reactive = FALSE)

  active.samples <- reactive({
    cov.name <- covariate$covariate()
    cov.vals <- values$values()
    suniverse <- isolate.(sample_universe())

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
      if (nrow(selected.samples) == 0) {
        fwarn("Cohort updates have set the active samples to the empty set")
      }
    } else {
      selected.samples <- suniverse
    }

    distinct(selected.samples, dataset, sample_id)
  })

  if (debug) {
    output$samples <- DT::renderDT({
      samples. <- active.samples()
      cov <- covariate$covariate()
      if (!unselected(cov)) {
        samples. <- with_sample_covariates(samples., cov)
      }
      samples.
    }, server = TRUE)
  }
  vals <- list(
    active_samples = active.samples,
    covariate = covariate,
    values = values,
    .ns = session$ns)
  class(vals) <- "FacileSampleFilter"
  vals
}

#' @noRd
#' @export
#' @importFrom shiny column fluidRow NS
sampleFilterUI <- function(id, ..., debug = FALSE) {
  ns <- NS(id)
  out <- fluidRow(
    column(
      4,
      categoricalSampleCovariateSelectUI(ns("covariate"), label = "Covariate")),
    column(
      4,
      categoricalSampleCovariateLevelsUI(ns("values"), label = "Value(s)",
                                         multiple = TRUE)))
  if (debug) {
    out <- tagList(
      out,
      DT::DTOutput(ns("samples")))
  }

  out
}

