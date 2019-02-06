#' Represents an active FacileDataStore used in the shiny context
#'
#' In order to fully work, facile shiny modules simply require a FacileDataStore
#' and a sample descriptor that defines the "active" sampels under scrutiny.
#'
#' This module will return a list (`rfds`) that includes:
#'
#' 1. A non-reactive `FacileDataStore` (`rfds$fds`)
#' 2. A `$user` key for use when fetching "personal" covariates over samples,
#'    using the `custom_key` parameter.
#' 3. A reactive `active_samples` tibble (`rfds$active_samples`). This will
#'    correspond to the same tibble returned from
#'    `FacileData::active_samples(rfds$fds)`.
#' 4. A reactive `active_covariates()` (long) tibble. This is a tibble
#'    (or lazy query) that corresponds to a (perhaps superset) of covariates
#'    that are defined over the `$active_samples()`.
#' 5. A list of `$trigger$something()` functions that external modules can tickle to
#'    update the state of the dataset. The `rfds$trigger$covariates()` function
#'    may be pulled when a user provides custom covariates over the
#'    active_samples via interacting with a plot.
#'
#' Active samples and active covariates can be manipulated externally, ie. from
#' a user interacting with another module, using the `update_reactive_samples()`
#' and `update_reactive_covariates()` functions.
#'
#' @export
#' @rdname reactiveFacileDataStore
#'
#' @importFrom shiny selectInput renderUI uiOutput
#' @param dataset A (list of) FacileDataStore(s)
#' @param user a key to use for custom covariates
reactiveFacileDataStore <- function(input, output, session, dataset,
                                    active_samples = NULL,
                                    user = Sys.getenv("USER"), ...) {
  ns <- session$ns
  assert_class(dataset, "FacileDataStore")
  if (!is.null(active_samples)) {
    assert_sample_subset(active_samples, dataset)
    active_samples(dataset) <- active_samples
  }

  # In the FacileWorkbench, the active FacileDataStore may be swapped, in which
  # case we want to listen for that.
  sample.trigger <- makeReactiveTrigger()

  # Interactive modules can communicate back into the ReactiveDataStore to
  # inform it that the user has provided interactive covariates for the samples
  # they are interacting with.
  cov.trigger <- makeReactiveTrigger()

  vals <- list(
    fds = dataset,
    user = user,
    trigger = list(
      samples = sample.trigger$trigger,
      covariates = cov.trigger$trigger))
  class(vals) <- c("ReactiveFacileDataStore", class(vals))

  active.samples <- reactive({
    sample.trigger$depend()
    active_samples(vals[["fds"]]) %>%
      collect(n = Inf)
  })

  active.covariates <- reactive({
    cov.trigger$depend()
    .samples <- req(active.samples())
    fetch_sample_covariates(vals[["fds"]], .samples, custom_key = user)
  })


  output$sample_trigger <- shiny::renderText({
    # sample.trigger$depend()
    .samples <- req(active.samples())
    sprintf("Sample Trigger: %d, nsamples: %d",
            sample.trigger$counter(),
            nrow(.samples))
  })

  output$covariate_trigger <- shiny::renderText({
    .covs <- active.covariates()
    sprintf("Covariate Trigger: %d, ncovs: %d",
            cov.trigger$counter(), nrow(.covs))
  })

  # Let's pass a reactiveFacileDataStore into a filterFacileSamples module
  # and have it communicate back to the the reactiveFacileDataStore by:
  # {
  #   active_samples(rfds$fds) <- the.filtered.samples
  #   rfds$fds.trigger$trigger()
  # }
  #
  # returns a sample selector and the active covoariates over these samples
  # for the faciledatset
  # ff <- callModule(filterFacileSamples, "sampleSelector", rfds, user, ...)

  vals[["active_samples"]] <- active.samples
  vals[["active_covariates"]] <- active.covariates
  vals
}

#' @export
#' @rdname reactiveFacileDataStore
#' @importFrom shiny NS tagList textOutput
reactiveFacileDataStoreUI <- function(id, ...) {
  ns <- NS(id)

  # There really is no UI, right?
  # tagList(
  #   uiOutput(ns("fdsSelectContainer")),
  #   filterFacileSamplesUI(ns("sampleSelector")),
  #   textOutput(ns("debug_nsamples")))
  tagList(
    textOutput(ns("sample_trigger")),
    textOutput(ns("covariate_trigger")))
}

# setters and getters ==========================================================

#' @rdname reactiveFacileDataStore
update_reactive_samples <- function(rfds, active_samples, criterion = NULL,
                                    ...) {
  assert_class(rfds, "ReactiveFacileDataStore")
  .as <- assert_sample_subset(active_samples, rfds[["fds"]]) %>%
    collect(n = Inf)

  current <- active_samples(rfds[["fds"]]) %>%
    collect(n = Inf)

  is.same <- setequal(
    with(current, paste(dataset, sample_id)),
    with(.as, paste(dataset, sample_id)))

  if (!is.same) {
    active_samples(rfds[["fds"]]) <- .as
    rfds[["trigger"]]$samples()
  }

  invisible(.as)
}

#' @rdname reactiveFacileDataStore
update_reactive_covariates <- function(rfds, covariates, namespace = NULL,
                                       ...) {
  assert_class(rfds, "ReactiveFacileDataStore")
  assert_sample_covariates(covariates)

  # Update the covariates correctly

  rfds[["trigger"]]$covariates()
  invisible(covariates)
}

#' @rdname reactiveFacileDataStore
update_reactive_datastore <- function(rfds, dataset, active_samples = NULL,
                                      ...) {
  assert_class(rfds, "ReactiveFacileDataStore")
  assert_class(dataset, "FacileDataStore")
  if (is.null(active_samples)) {
    active_samples <- active_samples(dataset)
  } else {
    assert_sample_subset(active_samples, dataset)
  }

  rfds[["fds"]] <- dataset
  update_reactive_samples(rfds, active_samples)
  rfds
}

