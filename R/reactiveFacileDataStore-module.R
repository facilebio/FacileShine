#' Creates a (swappable) ReactiveFacileDataSet
#'
#' This module creates a reactive container for a FacileDataStore, by providing
#' a (reactive) `path` parameter that points to an accessible FacileDataStore.
#' The reactivity of this input parameter allows the datastore to be swapped out
#' "live".
#'
#' The object returned by this module is a facade for a FacileDataStore. It
#' needs to define implementation for the FacileData API. Most of the time this
#' will simply be a pass-through to the same function call on the internal
#' fds() of this thing, the exception is the implementations of
#' fetch_sample_covariates and whatever function we use to enable feature
#' brushing (ie. new gene set definitions).
#'
#' @details
#' To restrict the set of samples "in play" for the datastore, for instance when
#' invoking an interactive gadget over these samples, the analyst (or gadget)
#' can do, you can pass in a sample descriptor into the `restrict_samples.`
#' parameter. This parameter isn't reactive and isn't really meant to be
#' used outside of the simple context of pulling up a subset of a
#' FacileDataStore for interrogation via a gadget.
#'
#' To allow interactive expansion and collapse of samples in play while
#' interactively working with a FacileDataStore with shiny, you should use
#' the [filteredReactiveDataStore()] module.
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
#' @export
#' @importFrom shiny observeEvent reactiveValues
#' @rdname reactiveFacileDataStore-module
#' @param path A "reactive string" that points to the url/directory to
#'   instantiate the ReactiveFacileDataStore from.
reactiveFacileDataStore <- function(intput, output, session, path,
                                    user = Sys.getenv("USER"), ...,
                                    restrict_samples. = NULL, debug = FALSE) {
  if (!is.null(restrict_samples.)) {
    assert_sample_subset(collect_samples.)
    restrict_samples. <- collect(collect_samples., n = Inf)
  }

  state <- reactiveValues(
    fds = "__initializing__",
    name = "__initializing__",
    active_samples = "__initializing__",
    active_assays = "__initializing__",
    active_covariates = "__initializing__",
    # ephemeral annotations provided by user during interactive exploration
    esample_annotation = .empty_sample_annotation_tbl(),
    efeature_annotation = .empty_feature_annotation_tbl(),
    efacets = .empty_facet_tbl())

  vals <- local({
    v <- list(
      user = user,
      universe = restrict_samples.,
      trigger = list(
        dataset = makeReactiveTrigger(),
        samples = makeReactiveTrigger(),
        covariates = makeReactiveTrigger()),
      .state = state,
      .ns = session$ns)
    classes <- c("ReactiveFacileDataStore", "FacileDataStore")
    if (!is.null(restrict_samples.)) {
      classes <- c("RestrictedReactiveFacileDataStore", classes)
    }
    class(v) <- classes
    v
  })

  observeEvent(path(), {
    path. <- req(path())
    assert_string(path.)
    assert_directory_exists(path., "r")

    fds. <- FacileDataSet(path.)

    # Setting this to __initializing__ so that things "reacting" on an
    # initialized(rfds) wait until the first pass of
    # observeEvent(state$active_samples) goes through before they use the
    # new ReactiveFacileDataSet again
    state[["name"]] <-  "__initializing__"

    # Wrapping this as "BoxedFacileDataStore" so we can intercept some facile
    # API calls and allow them to accept ephemeral annotations.
    class(fds.) <- c("BoxedFacileDataStore", class(fds.))
    state[["fds"]] <- fds.

    # Reset the ephemeral stuff
    state[["active_samples"]] <- local({
      asamples <- collect(samples(fds.), n = Inf)
      if (!is.null(restrict_samples.)) {
        # restrict_samples. is here for convenience to enable launching an
        # gadget over a restricted set of samples.
        assert_sample_subset(restrict_samples.)
        asamples <- semi_join(asamples, restrict_samples.,
                              by = c("dataset", "sample_id"))
      }
      as_facile_frame(asamples, fds., "reactive_facile_frame")
    })
    state[["esample_annotation"]] <- .empty_sample_annotation_tbl()
    state[["efeature_annotation"]] <- .empty_feature_annotation_tbl()
    state[["efacets"]] <- .empty_facet_tbl()
  })

  observeEvent(state$active_samples, {
    fds. <- state[["fds"]]
    req(is(fds., "BoxedFacileDataStore"))
    samples. <- state[["active_samples"]]
    ecovs <- state[["esample_annotation"]]

    ftrace("Observed update to {bold}{red}state$active_samples{reset}")

    # TODO: test that replacements for state variables are different than
    #       what is already stored there (maybe this will avoid double firing)
    # 1. state$active_covariates
    # 2. state$active_assays
    # Update active covariates
    acovs <- fetch_sample_covariates(fds., samples = samples.,
                                     custom_key = user, with_source = TRUE,
                                     extra_covariates = ecovs)
    acovs <- summary(acovs)

    # Test if available covariates have changed
    # we are crashing on dataset switches because samples. is empty
    state$active_covariates <- acovs

    # Update assay info over samples
    state$active_assays <- fds. %>%
      assay_info_over_samples(samples.) %>%
      collect(n = Inf)

    # This should trigger reactives in other modules waiting on
    # req(initialized(rfds)) to take another crack at what they need.
    if (state[["name"]] != name(fds.)) {
      state[["name"]] <- name(fds.)
    }
  }, priority = 10)

  output$fdsname <- shiny::renderText({
    state$name
  })

  if (debug) {
    output$active_covariates <- DT::renderDT({
      req(!is.character(state$active_covariates))
      state$active_covariates
    }, server = TRUE)
    output$nsamples <- shiny::renderText({
      nrow(state$active_samples)
    })
  }

  return(vals)
}

#' @noRd
#' @export
#' @importFrom shiny NS tags textOutput
#' @importFrom DT DTOutput
reactiveFacileDataStoreUI <- function(id, ..., debug = FALSE) {
  ns <- NS(id)

  out <- tagList(
    tags$h3("ReactiveFacileDataStore"),
    textOutput(ns("fdsname")))
  if (debug) {
    out <- tagList(
      out,
      tags$p("Number of active samples:", textOutput(ns("nsamples"))),
      DT::DTOutput(ns("active_covariates")))
  }
  out
}

#' @section ReactiveFacileDataStore:
#' We also provide a constructor-like-looking function which can be used
#' instead of the `callModule(reactiveFacileDataStore, ...)` stuff in server
#' functions, like so:
#'
#' ```r
#' rfds <- ReactiveFacileDataStore(path, "rfds", samples = s)
#' ```
#'
#' @noRd
#' @export
#' @rdname reactiveFacileDataStore-module
#'
#' @param x The path to the datsstore
#' @param id the id of the shiny module
#' @return a reactiveFacileDataStore module
ReactiveFacileDataStore <- function(x, id, user = Sys.getenv("USER"),
                                    samples = NULL, ...,
                                    debug = FALSE) {
  pf <- parent.frame()
  assert_string(id)

  if (is.character(x)) {
    path <- reactive(x[1L])
  } else if (is(x, "FacileDataSet")) {
    path <- reactive(x[["parent.dir"]])
  } else if (is(x, "facile_frame")) {
    path <- reactive(fds(x)[["parent.dir"]])
    samples <- collect(distinct(x, dataset, sample_id), n = Inf)
  } else if (is(x, "reactiveExpr")) {
    path <- x
  } else if (is(x, "ReactiveFacileDataStore")) {
    path <- reactive(fds(x))
  } else {
    stop("You are passing an illegal type of argument")
  }

  # out <- eval({
  #   callModule(reactiveFacileDataStore, id, path, user = user, ...,
  #              restrict_samples. = samples, debug = debug)
  # }, envir = pf)

  out <- eval({
    callModule(filteredReactiveFacileDataStore, id, path, user = user, ...,
               restrict_samples. = samples, debug = debug)
  }, envir = pf)


  out
}

# ReactiveFacile API ===========================================================
# (defined in FacileShine)

#' Note that calls to this should only "fire" reactivity when the "name"
#' attribute of the inner faciledatastore is changed.
#'
#' @noRd
#' @export
initialized.ReactiveFacileDataStore <- function(x, ...) {
  # is(fds(x), "FacileDataStore") && !unselected(name(x))
  # Not using fds() because fds() itself calls req(initialized(x))

  fds.name <- name(x)
  fds. <- isolate(x[[".state"]][["fds"]])

  # Check if any internal state elements are __initializing__
  check <- c("active_samples", "active_assays", "active_covariates")
  initing <- sapply(check, function(var) {
    obj <- isolate(x[[".state"]][[var]])
    is.character(obj) && obj == "__initializing__"
  })

  !any(initing) && is(fds., "FacileDataStore") && !unselected(name(x))
}

#' @noRd
#' @export
initialized.BoxedFacileDataStore <- function(x, ...) {
  TRUE
}

#' @noRd
#' @export
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
  trigger(x, name, trigger. = FALSE)$depend()
}

#' Returns the reactive trigger or triggers it, depending on value of `trigger.`
#'
#' When `trigger.` is `TRUE` (default), will pull the trigger, else it will
#' return the trigger.
#'
#' @noRd
#' @export
trigger <- function(x, name = "covariates", trigger. = TRUE, ...) {
  # trgr <- assert_class(reactives(x, "trigger")[[name]], "ReactiveTrigger")
  trgr <- assert_class(x[["trigger"]][[name]], "ReactiveTrigger")
  if (trigger.) {
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
active_assays.ReactiveFacileDataStore <- function(x, ...) {
  req(initialized(x))
  x[[".state"]][["active_assays"]]
}

#' @noRd
#' @export
active_covariates.ReactiveFacileDataStore <- function(x, active_only = TRUE,
                                                      ...) {
  req(initialized(x))
  as_facile_frame(x[[".state"]][["active_covariates"]], x,
                  .valid_sample_check = FALSE)
}

#' @noRd
#' @export
active_samples.ReactiveFacileDataStore <- function(x, ...) {
  req(initialized(x))
  as_facile_frame(x[[".state"]][["active_samples"]], x,
                  .valid_sample_check = FALSE)
}

#' @noRd
#' @export
user.ReactiveFacileDataStore <- function(x, ...) {
  req(initialized(x))
  x[["user"]]
}

# Facile API ===================================================================

# This is partially an exercise to ensure that we have a well defined facile
# api. We could have all FacileShine modules just extract the fds from the
# ReactiveFDS first then move on with life, but ... this is correct.

#' @noRd
#' @export
assay_feature_info.ReactiveFacileDataStore <- function(x, assay_name,
                                                       feature_ids = NULL,
                                                       ...) {
  req(initialized(x))
  assay_feature_info(fds(x), assay_name, feature_ids, ...)
}

#' @noRd
#' @export
#' @importFrom shiny req
assay_info.ReactiveFacileDataStore <- function(x, assay_name = NULL, ...) {
  req(initialized(x))
  req(initialized(x))
  assay_info(fds(x), assay_name = assay_name, ...)
}

#' @noRd
#' @export
#' @importFrom shiny req
assay_names.ReactiveFacileDataStore <- function(x, default_first = TRUE, ...) {
  req(initialized(x))
  assay_names(fds(x), default_first = default_first, ...)
}

#' @noRd
#' @export
covariate_definitions.ReactiveFacileDataStore <- function(x, as.list = TRUE,
                                                          ...) {
  covariate_definitions(fds(x), ...)
}

#' @noRd
#' @export
fds.ReactiveFacileDataStore <- function(x) {
  req(initialized(x))
  x[[".state"]][["fds"]]
}

#' @noRd
#' @export
default_assay.ReactiveFacileDataStore <- function(x, ...) {
  req(initialized(x))
  default_assay(fds(x))
}

#' @noRd
#' @export
facet_frame.ReactiveFacileDataStore <- function(x, name = "default", ...) {
  req(initialized(x))
  as_facile_frame(bind_rows(x[["efacets"]], facet_frame(fds(x))), x,
                  .valid_sample_check = FALSE)
}

#' @noRd
#' @export
fetch_assay_data.ReactiveFacileDataStore <- function(x, features, samples=NULL,
                                                     assay_name=default_assay(x),
                                                     normalized=FALSE,
                                                     as.matrix=FALSE, ...,
                                                     aggregate.by=NULL) {
  req(initialized(x))
  fetch_assay_data(fds(x), features = features, samples = samples,
                   assay_name = assay_name, normalized = normalized,
                   as.matrix = as.matrix, aggregate.by = aggregate.by, ...)
}

#' @noRd
#' @export
fetch_sample_covariates.BoxedFacileDataStore <- function(
    x, samples = NULL, covariates = NULL, custom_key = user(x),
    with_source = TRUE, ..., extra_covariates = NULL) {
  req(initialized(x))

  if (!is.null(extra_covariates)) {
    extra_covariates <- collect(extra_covariates, n = Inf)
    assert_sample_covariates(extra_covariates)
    if (!is.null(covariates)) {
      assert_character(covariates)
      extra_covariates <- filter(extra_covariates, variable %in% covariates)
    }
    if (!is.null(samples)) {
      assert_sample_subset(samples)
      extra_covariates <- semi_join(extra_covariates, collect(samples, n = Inf),
                                    by = c("dataset", "sample_id"))
    }
    if (with_source && nrow(extra_covariates)) {
      extra_covariates <- mutate(extra_covariates, source = "ephemeral")
    }
  }

  stored_covs <- NextMethod(samples = samples, covariates = covariates,
                            custom_key = custom_key, with_source = with_source)
  stored_covs <- collect(stored_covs, n = Inf)

  out <- bind_rows(extra_covariates, stored_covs)
  out <- set_fds(out, x)
  out <- distinct(out, dataset, sample_id, variable, .keep_all = TRUE)
  as_facile_frame(out, x, "eav_covariates", .valid_sample_check = FALSE)
}

#' @noRd
#' @export
fetch_sample_covariates.ReactiveFacileDataStore <- function(
    x, samples = active_samples(x), covariates = NULL, custom_key = user(x),
    with_source = TRUE, ...) {
  req(initialized(x))

  extra_covs <- x[["state."]][["esample_annotation"]]
  out <- fetch_sample_covariates(fds(x), samples = samples,
                                 covariates = covariates,
                                 custom_key = custom_key,
                                 with_source = with_source,
                                 extra_covariates = extra_covs)
  as_facile_frame(out, x, .valid_sample_check = FALSE)
}

#' @noRd
#' @export
fetch_custom_sample_covariates.ReactiveFacileDataStore <- function(
    x, samples = NULL, covariates = NULL, custom_key = user(x),
    with_source = FALSE, file.prefix = "facile", ...) {
  req(initialized(x))

  # Get this to delegate to the *.FacileDataSet version
  fetch_custom_sample_covariates(fds(x), samples = samples,
                                 covariates = covariates,
                                 custom_key = custom_key,
                                 with_source = with_source,
                                 file.prefix = file.prefix, ...)
}

#' @noRd
#' @export
filter_samples.ReactiveFacileDataStore <- function(x, ...,
                                                   with_covariates = FALSE) {
  stop("fiter_samples.ReactiveFacileDataStore not implemented")
  req(isolate(initialized(x)))

  if (!is(..1, "FacileSampleFilter")) NextMethod()
}

#' Note that this fires reactivity when callers ask for the name()
#' @noRd
#' @export
name.ReactiveFacileDataStore <- function(x, ...) {
  name. <- x[[".state"]][["name"]]
  assert_string(name.)
}

#' @noRd
#' @export
samples.ReactiveFacileDataStore <- function(x, ...) {
  as_facile_frame(active_samples(x), x, .valid_sample_check = FALSE)
}

# with_assay_data.ReactiveFacileDataStore <- function(x, ...) {
#
# }
#
# with_sample_covariates.ReactiveFacileDataStore <- function(x, ...) {
#
# }

# External Manipulation ========================================================

#' @noRd
#' @export
update_rfds <- function(x, dataset, ...) {
  assert_class(x, "ReactiveFacileDataStore")
  assert_class(dataset, "FacileDataStore")

  class(dataset) <- c("BoxedFacileDataStore", class(dataset))

  # Reset the ephemeral stuff
  x[[".state"]][["fds"]] <- dataset
  # x[[".state"]][["name"]] <- name(dataset)
  # x[[".state"]][["filters"]] <- list()
  x[[".state"]][["name"]] <-  "__initializing__"
  x[[".state"]][["active_samples"]] <- collect(samples(dataset), n = Inf)
  x[[".state"]][["esample_annotation"]] <- .empty_sample_annotation_tbl()
  x[[".state"]][["efeature_annotation"]] <- .empty_feature_annotation_tbl()
  x[[".state"]][["efacets"]] <- .empty_facet_tbl()

  invisible(x)
}

#' Update reactive samples and annotations
#'
#' We want to enable external actors to update the state of a reactive facile
#' datasetore.
#'
#' @export
#' @rdname update_reactive_fdstore
update_reactive_samples <- function(x, actives_samples, criterion = NULL, ...) {
  UseMethod("update_reactive_samples", x)
}

#' @noRd
#' @export
update_reactive_samples.ReactiveFacileDataStore <- function(x, active_samples,
                                                            criterion = NULL,
                                                            ...) {
  req(initialized(x))
  .as <- active_samples %>%
    assert_sample_subset() %>%
    distinct(dataset, sample_id) %>%
    collect(n = Inf)

  current <- collect(active_samples(x), n = Inf)

  is.same <- setequal(
    with(current, paste(dataset, sample_id)),
    with(.as, paste(dataset, sample_id)))

  if (!is.same) {
    if (nrow(.as) == 0L) {
      fwarn("{bold}{red}Updating active samples to empty set{reset}")
    }
    .as <- as_facile_frame(.as, fds(x), "reactive_facile_frame")
    x[[".state"]][["active_samples"]] <- .as
  }

  invisible(x)
}

#' @export
#' @rdname update_reactive_fdstore
update_reactive_covariates <- function(x, covariates, namespace, ...) {
  UseMethod("update_reactive_covariates", x)
}

#' @noRd
#' @export
update_reactive_covariates.ReactiveFacileDataStore <- function(x, covariates,
                                                               namespace, ...) {
  req(initialized(x))
  stop("update_reactive_covariates(rfds) is not yet implemented")
  # Do something with x[[".state"]][["active_covariates"]]
  trigger(x, "covariates")
  invisible(x)
}


# Inner helpers ================================================================

.empty_sample_annotation_tbl <- function() {
  tibble(
    dataset = character(),
    sample_id = character(),
    variable = character(),
    value = character(),
    class = character(),
    type = character(),
    date_entered = integer())
}

.empty_facet_tbl <- function() {
  tibble(
    dataset = character(),
    sample_id = character(),
    facet = character(),
    description = character())
}

.empty_feature_annotation_tbl <- function() {
  tibble(
    collection = character(),
    name = character(),
    feature_id = character(),
    feature_type = character())
}
