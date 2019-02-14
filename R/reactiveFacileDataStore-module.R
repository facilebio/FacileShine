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

  # If the user has passed in a subset of active samples, narrow down the
  # FacileDataStore to those before we get started
  if (!is.null(active_samples)) {
    assert_sample_subset(active_samples, dataset)
    active_samples(dataset) <- active_samples
  }

  # In the FacileWorkbench, the active FacileDataStore may be swapped, in which
  # case we want to listen for that.
  sample.trigger <- makeReactiveTrigger()

  # Tickle this when the state of the FacileDataStore has changed, ie. when a
  # user actually saves a temporal covaraite into the datastore.
  cov.trigger <- makeReactiveTrigger()

  # Temporary annotations that modules can register with the faciledatastore
  # for users to be able to annotate samples and features mid-analysis.
  # * samples: looks like your normal sample_covariate_tbl
  #   - `variable` column is the namespace of the brushing module
  # * features: looks like a GeneSetDb, almost:
  #   - collection is the feature-annotation-module namespace
  #   - name is the name of the featureset
  #   - feature_id is the feature_id
  #   - feature_type is the feature_type
  annotations <- reactiveValues(
    samples = .empty_sample_annotation_tbl(),
    features = .empty_feature_annotation_tbl())

  vals <- list(
    fds = dataset,
    user = user,
    annotations = annotations,
    trigger = list(
      samples = sample.trigger$trigger,
      covariates = cov.trigger$trigger))
  class(vals) <- c("ReactiveFacileDataStore", class(vals))

  active.samples <- reactive({
    sample.trigger$depend()
    active_samples(vals[["fds"]]) %>%
      collect(n = Inf)
  })

  active.assays <- reactive({
    assay_info_over_samples(vals[["fds"]], active.samples())
  })

  # The covariates in this tibble consist of only those that are pulled from
  # the FacileDataStore itself.
  fds.sample.covariates <- reactive({
    cov.trigger$depend()
    .samples <- req(active.samples())
    fds.cov <- vals[["fds"]] %>%
      fetch_sample_covariates(.samples, custom_key = user) %>%
      arrange(variable) %>%
      mutate(source = "serialized")
    fds.cov
  })

  # Combines the serialized covariates w/ the active/temporal ones
  active.covariates <- reactive({
    req(fds.sample.covariates()) %>%
      bind_rows(vals[["annotations"]][["samples"]])
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

  vals[["active_samples"]] <- active.samples
  vals[["active_covariates"]] <- active.covariates
  vals[["active_assays"]] <- active.assays
  vals
}

.empty_sample_annotation_tbl <- function() {
  tibble(
    dataset = character(),
    sample_id = character(),
    variable = character(),
    class = character(),
    type = character(),
    date_entered = integer())
}

.empty_feature_annotation_tbl <- function() {
  tibble(
    collection = character(),
    name = character(),
    feature_id = character(),
    feature_type = character())
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
#' @section Reactive Samples:
#'
#' A facile analysis session is perfomed on some subset of the samples
#' provided by a `FacileDataStore`. The samples under scrutiny can change as
#' the user dips into and out of different analyses. These are defined
#' interactively by the user via the use of the `facileSampleFilter` module(s)
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
#' @export
#'
#' @section Reactive Covariates:
#'
#' Updates the temporal covariates during an analysis.
#'
#' In addition to the sample covariates in the FacileDataStore, covariates
#' can be added to the samples by different modules via user interaction and
#' brushing during an analysis session.
update_reactive_covariates <- function(rfds, covariates, namespace, ...) {
  assert_class(rfds, "ReactiveFacileDataStore")
  assert_sample_covariates(covariates)

  # Update the covariates correctly
  rfds[["trigger"]]$covariates()

  out <- tibble(
    dataset = character(),
    sample_id = character(),
    variable = character(),
    value = character(),
    class = character(), # "categorical",
    type = "interactive",
    date_entered = integer()) # as.integer(Sys.time())

  out %>%
    mutate(source = "interactive") %>%
    invisible()
}

save_reactive_covariates <- function(rfds, namespace, ...) {
  # 1. saves the temporal sample coviarates for the "namespace" module to the
  #    FacileDataStore.
  # 2. Reloads the active.covariates()
  # 3. Initializes the covariates for the namespace module to NULL

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

