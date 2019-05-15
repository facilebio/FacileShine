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
                          rfds, sample_universe, include1 = FALSE,
                          .with_none = FALSE,
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
sampleFilterUI <- function(id, covariate_label = "Covariate",
                           value_label = "Values(s))", ...,
                           debug = FALSE) {
  ns <- NS(id)

  out <- fluidRow(
    column(
      4,
      categoricalSampleCovariateSelectUI(ns("covariate"),
                                         label = covariate_label)),
    column(
      4,
      categoricalSampleCovariateLevelsUI(ns("values"),
                                         label = value_label,
                                         multiple = TRUE)))
  if (debug) {
    out <- tagList(
      out,
      DT::DTOutput(ns("samples")))
  }

  out
}
