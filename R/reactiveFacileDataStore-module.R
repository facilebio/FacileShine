#' Represents an active FacileDataStore used in the shiny context
#'
#' This module returns a `ReactiveFacileDataStore` object, which is a functional
#' subclass of a `FacileDataStore`. This object is meant to be used in a
#' shiny / reactive context. Importantly, it supports ephemeral sample and
#' feature annotations which are meant to enable users to add custom sample
#' and feature annotations that are uncovered during the course of an analysis.
#'
#' **Note:** The behavior of this object is undefined (untested) when used in a
#' non-reactive context.
#'
#' @section Custom Sample Annotations:
#' Analysts may uncover interesting subsets of samples (outliers, subtypes)
#' within a dataset during the course of an exploratory analysis. This object
#' supports downstream modules to add sample annoations via the use of the
#' [update_reactive_covaraites()] function.
#'
#' These annotations are stored in an eav table that looks a lot like the
#' eav_covariates table returned from [FacileData::fetch_sample_covaraites()].
#' The columns are:
#' * `dataset`; `sample_id`: the primary key of the sample
#' * `variable`: the name of the variable -- users create this during brushin
#' * `value`: the value of the variable
#' * `class`: categorical or real (just categorical is supported for now)
#' * `source`: the name (namespace) of the module that is adding the covariate
#'
#' @section Custom Feature Annotations:
#' This will largely look like a GeneSetDb, with the following columns:
#' * `collection`: the feature-annotation-module namespace
#' * `name`: is the name of the featureset (provided by the uesr)
#' * `feature_id`: the feature_id of the thing
#' * `feature_type`: the "feature space" these features (entrez, ensgid, etc.),
#'    these values need to be one of the elements returned from calling
#'    [FacileData::feature_types()] on the internal `datastore`.
#'
#' @section An augmented FacileDataStore:
#' The `ReactiveFacileDataStore` is a FacileDataStore with the following list
#' of values stored in its `"reactive"` slot (or list-store), depending on
#' whether the underyling FacileDataStore is implemented as an S3 or S4 object.
#'
#' A list (named `"reactive"`) is added to the `FacileDataStore`, which holds
#' the reactive elements listed below:
#'
#' * `user`: non-reactive, the user id of the person using this datastore. This
#'   is used in conjunction with the `custom_key` parameter in the faciledata
#'   API to retrieve user-specific annotations.
#' * `annotations$samples`: The tibble that stores ephemeral sample annotations
#'   added from arbitrary modules.
#' * `annotations$features`: A tibble to store ephemeral feature annotations
#'   (aka genesets)
#' * `trigger$samples`; `trigger$covariates`: These are reactive triggers
#'   (see [makeReactiveTrigger()]), that can be tickled in order to update
#'   the current samples (and their annotations) under analysis.
#' * `active_samples`: A tibble of the currently active samples in the
#'   datastore. This is analagous to the [FacileData::active_samples()]
#'   tibble, but is reactive so consumers can listen to changes in state.
#' * `active_covaraites`: A (reactive) summary table of the current covariates
#'   defined on the samples. This looks like the output from the
#'   `FacileData::summary.eav_covariates` function, and enables modules to
#'   respond to new covariates defined during analyses that have not been saved
#'   yet.
#' * `active_assays`: A reactive version of the output from the
#'   [FacileData::assay_info_over_samples()] function. This enables modules
#'   to query what assays are "in play" for analysis over the current samples.
#'
#' Active samples and active covariates can be manipulated externally, ie. from
#' a user interacting with another module, using the `update_reactive_samples()`
#' and `update_reactive_covariates()` defined over the `ReactiveFacileDataStore`
#' object.
#'
#' @export
#' @rdname reactiveFacileDataStore
#'
#' @importFrom shiny selectInput renderUI uiOutput
#' @param dataset The FacileDataStore to make reactive
#' @param active_samples A predefined set of samples to subset the dataset to.
#' @param user the custom_key to use for ephemeral sample and feature
#'   annotations that are saved back to the `dataset`.
reactiveFacileDataStore <- function(input, output, session, dataset,
                                    active_samples = NULL,
                                    user = Sys.getenv("USER"), ...) {
  assert_class(dataset, "FacileDataStore")

  # If the user has passed in a subset of active samples, narrow down the
  # FacileDataStore to those before we get started
  if (!is.null(active_samples)) {
    assert_sample_subset(active_samples, dataset)
    active_samples(dataset) <- active_samples
  }

  active.samples <- reactive({
    depend(dataset, "samples")
    asamples <- FacileData:::active_samples.FacileDataSet(dataset)
    out <- collect(asamples, n = Inf)
    class(out) <- c("reactive_facile_frame", class(out))
    out
  })

  active.assays <- reactive({
    assay_info_over_samples(dataset, active.samples())
  })

  active.covariates <- reactive({
    covs <- fetch_sample_covariates(dataset, samples = active.samples())
    summary(covs)
  })

  # The `reactives` list provides the shiny magic for the FacileDataStore.
  # On "the way out", this list will be stored in a class-specific place
  # within the returned ReactiveFacileDataStore.
  # Check the `reactives()` function for more details.
  reactives <- list(
    active_samples = active.samples,
    active_covariates = active.covariates,
    active_assays = active.assays,
    user = user,
    annotations = reactiveValues(
      samples = .empty_sample_annotation_tbl(),
      features = .empty_feature_annotation_tbl()),
    trigger = list(
      samples = makeReactiveTrigger(),
      covariates = makeReactiveTrigger()))

  if (!isS4(dataset)) {
    dataset[[".reactive."]] <- reactives
  } else {
    stop("ReactiveFacileDataStore not yet implemented over S4 objects")
  }


  # Debug Outputs ==============================================================
  output$sample_count <- shiny::renderText({
    .samples <- req(active.samples())
    sprintf("Number of active samples: %d", nrow(.samples))
  })

  output$covariate_trigger <- shiny::renderText({
    .covs <- req(active.covariates())
    sprintf("Covariate Trigger: %d, ncovs: %d",
            triggered(dataset, "covariates"),
            nrow(.covs))
  })
  # ============================================================================

  class(dataset) <- c("ReactiveFacileDataStore", class(dataset))
  dataset
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
    textOutput(ns("sample_count")),
    textOutput(ns("covariate_trigger")))
}

# New methods over a ReactiveFacileDataStore ===================================

#' Extracts the "reactive" list from a FacileDataStore
#'
#' An S3 ReactiveFacileDataStore (`rfds`) stores the reactives list in
#' `rfds[[".reactive."]]`. An S4 ReactiveFacileDataStore (ie. a FacileDataStore
#' over a `SingleCellExperiment`) will store this somehwere else.
#'
#' @export
reactives <- function(x, name = NULL, ...) {
  assert_class(x, "ReactiveFacileDataStore")
  if (isS4(x)) {
    stop("ReactiveFacileDataStore not yet implemented over S4 assay containers")
  } else {
    out <- x[[".reactive."]]
  }
  if (!is.null(name)) {
    assert_choice(name, names(out))
    out <- out[[name]]
  }

  out
}

#' @rdname reactiveFacileDataStore
#' @section Triggers:
#' We use reactive triggers to tickle the state of the ReactiveFacileDataStore
#' when responding to user interactivity.
#'
#' This is done via the `depend(rfds)` and `trigger(rfds)` functions, ie. to
#' ensure a function is run when the active covariate state changes, the
#' `fetch_sample_covariates.ReactiveFacileDataStore` function embeds a
#' `depend(x, "covariates")` call. To have a function trigger that, you would
#' have it call `trigger(x, "covariates")`
depend <- function(x, name = "covariates", ...) {
  assert_reacting()
  trigger(x, name, trigger. = FALSE)$depend()
}

#' @noRd
#' @export
trigger <- function(x, name = "covariates", trigger. = TRUE, ...) {
  trgr <- assert_class(reactives(x, "trigger")[[name]], "ReactiveTrigger")
  if (trigger.) {
    assert_reacting()
    trgr$trigger()
  }
  invisible(trgr)
}

#' @noRd
#' @export
#' @return returns the number of times the trigger has been pulled
triggered <- function(x, name = "covariates", ...) {
  trigger(x, name, trigger. = FALSE)$counter()
}

#' @noRd
#' @export
user.ReactiveFacileDataStore <- function(x, ...) {
  reactives(x, "user")
}

#' @noRd
#' @export
active_covariates.ReactiveFacileDataStore <- function(x, ...) {
  assert_reacting()
  reactives(x, "active_covariates")()
}


#' @rdname reactiveFacileDataStore
#' @export
active_assays.ReactiveFacileDataStore <- function(x, ...) {
  assert_reacting()
  reactives(x, "active_assays")()
}

#' @rdname reactiveFacileDataStore
#' @section Reactive Samples:
#'
#' A facile analysis session is perfomed on some subset of the samples
#' provided by a `FacileDataStore`. The samples under scrutiny can change as
#' the user dips into and out of different analyses. These are defined
#' interactively by the user via the use of the `facileSampleFilter` module(s)
update_reactive_samples <- function(rfds, active_samples, criterion = NULL,
                                    ...) {
  assert_reacting()
  assert_class(rfds, "ReactiveFacileDataStore")

  active_samples(rfds) <- active_samples
  active_samples
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
  stop("update_reactive_covariates(rfds) is not yet implemented")
  assert_reacting()
  assert_class(rfds, "ReactiveFacileDataStore")
  assert_sample_covariates(covariates)

  # Update the covariates correctly
  trigger(x, "covariates")

  out <- tibble(
    dataset = character(),
    sample_id = character(),
    variable = character(),
    value = character(),
    class = character(), # "categorical",
    type = "interactive",
    date_entered = integer()) # as.integer(Sys.time())

  out <- mutate(out, source = "ephemeral")
  invisible(out)
}

save_reactive_covariates <- function(rfds, namespace, ...) {
  stop("save_reactive_covariates is not yet implemented")
  # 1. saves the temporal sample coviarates for the "namespace" module to the
  #    FacileDataStore.
  # 2. Reloads the active.covariates()
  # 3. Initializes the covariates for the namespace module to NULL

}

# FacileData API ===============================================================

# Most facile functions shouldn't need to be modified to work correctly
# over a ReactiveFacileDataStore, except the following:
#
# * fetch_sample_covariates: Because a ReactiveFacileDataStore can support
#   new / ephemeral sample covaraites that are brushed on by the user, we need
#   to support the retrieval of these covariates via overriding this function.
#   If this is done corectly, this should take care of many other facile
#   functions.

#' Transparently supports retrieval of both stored and ephemeral covariates
#'
#' @noRd
#' @export
fetch_sample_covariates.ReactiveFacileDataStore <- function(
    x, samples = NULL, covariates = NULL, custom_key = user(x),
    with_source = TRUE, ...) {
  assert_reacting()
  if (!is.null(samples)) assert_sample_subset(samples)

  # First retrieve covarites from the current/temporal brushing annotations,
  # then query the datastore for the rest.
  # If the two-step results in multiple dataset,sample_id,covariate values for
  # the same samples, we keep the first.
  depend(x, "covariates")
  if (!is.null(covariates)) assert_character(covariates)

  active.covs <- reactives(x, "annotations")[["samples"]]
  if (!is.null(covariates)) {
    active.covs <- filter(active.covs, variable %in% covariates)
  }
  if (!is.null(samples)) {
    active.covs <- semi_join(active.covs, samples,
                             by = c("dataset", "sample_id"))
  }
  if (with_source && nrow(active.covs)) {
    active.covs <- mutate(active.covs, source = "ephemeral")
  }

  stored.covs <- NextMethod()

  out <- bind_rows(active.covs, stored.covs)

  unknown <- setdiff(covariates, out[["variable"]])
  if (length(unknown)) {
    warning("Unknown covariates: ", paste(unknown, collapse = ","))
  }

  out <- distinct(out, dataset, sample_id, variable, .keep_all = TRUE)
  as_facile_frame(out, fds(x), "eav_covariates", .valid_sample_check = FALSE)
}

#' This S3 is defined in `FacileData`
#'
#' @noRd
#' @export
#' @rdname reactiveFacileDataStore
active_samples.ReactiveFacileDataStore <- function(x, ...) {
  assert_reacting()
  reactives(x, "active_samples")()
}

#' @rdname active_samples
#' @export
`active_samples<-.ReactiveFacileDataStore` <- function(x, value) {
  .as <- value %>%
    assert_sample_subset() %>%
    collect(n = Inf)

  current <- active_samples(x) %>%
    collect(n = Inf)

  is.same <- setequal(
    with(current, paste(dataset, sample_id)),
    with(.as, paste(dataset, sample_id)))

  if (!is.same) {
    # active_samples(rfds) <- .as
    NextMethod(value = .as)
    trigger(x, "samples")
  }

  x
}

# Inner helpers ================================================================

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
