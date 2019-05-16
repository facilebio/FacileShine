#' Composition of modules to provide a filtering schema over a FaclieDataStore.
#'
#' We compose the [reactiveFacileDataStore()] and a list of
#' [facileSampleFilter()] modules to make a super-module.
#'
#' @export
#' @importFrom shiny callModule reactiveValues renderText
#' @rdname filteredReactiveFacileDataStore
filteredReactiveFacileDataStore <- function(input, output, session, path,
                                            user = Sys.getenv("USER"), ...,
                                            debug = FALSE) {
  state <- reactiveValues(nsamples = "__initializing__")

  rfds <- callModule(reactiveFacileDataStore, "rfds", path, user, ...,
                     debug = debug)
  class(rfds) <- c("FilteredReactiveFacileDataStore", class(rfds))

  filters <- callModule(sampleFilterList, "filters", rfds, ..., debug = debug)

  observeEvent(active_samples(filters), {
    samples. <- active_samples(filters)
    state$nsamples <- nrow(samples.)
    update_reactive_samples(rfds, samples.)
  })

  # I was trying to have the page say "Initializing" until the dataset finishes
  # loading, but ... that's not working.
  output$infotext <- renderText("Initializing")
  observeEvent(state$nsamples, {
    nsamples <- state$nsamples
    if (!is.numeric(nsamples)) {
      txt <- "Initializing"
    } else {
      nfilters <- length(isolate(filters$filters()))
      req(is.numeric(nsamples))
      txt <- sprintf("%d samples, %d filters", nsamples, nfilters)
    }
    output$infotext <- renderText(txt)
  })

  rfds[["filters"]] <- filters
  rfds
}

#' @noRd
#' @export
#' @importFrom shiny tagList textOutput
filteredReactiveFacileDataStoreUI <- function(id, ..., debug = FALSE) {
  ns <- NS(id)

  out <- tagList(
    tags$h3("Cohort Selection"),
    tags$p("Status: ", textOutput(ns("infotext"), inline = TRUE)),
    sampleFilterListUI(ns("filters"), debug = debug))

  out
}


# SingleFilterReactiveFacileDataStore works ====================================


#' This was a convenience module that was built to initially exercising how
#' downstream components reacted to a ReactiveFacileDataStore with the ability
#' to restrict the active_samples() -- just one filter deep.
#'
#' After filtereReactiveFacileDataStore was put together and tested, this has
#' become obsolete, since that functionality supersedes this, but we'll keep it
#' here in case we need to do more testing in the future.
#'
#' We compose the [reactiveFacileDataStore()] and a list of
#' [facileSampleFilter()] modules to make a super-module.
#'
#' @noRd
singleFilteredReactiveFacileDataStore <- function(input, output, session, path,
                                            user = Sys.getenv("USER"),
                                            nfilters = 1, ...) {
  rfds <- callModule(reactiveFacileDataStore, "rfds", path, user, ...)
  class(rfds) <- c("FilteredReactiveFacileDataStore", class(rfds))

  rfds[["filters"]] <- sapply(paste0("ffilter_", seq(nfilters)), function(id) {
    callModule(facileSampleFilter, id, rfds)
  }, simplify = FALSE)

  rfds
}

#' @noRd
singleFilteredReactiveFacileDataStoreUI <- function(id, nfilters = 1, ...) {
  ns <- NS(id)

  filters <- sapply(paste0("ffilter_", seq(nfilters)), function(id) {
    facileSampleFilterUI(ns(id))
  }, simplify = FALSE)

  tagList(
    tags$h3("Data Filters"),
    filters,
    reactiveFacileDataStoreUI(ns("rfds")))
}
