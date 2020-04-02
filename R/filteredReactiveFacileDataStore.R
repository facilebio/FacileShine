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

  rfds <- callModule(reactiveFacileDataStore, "rfds", path, user = user, ...,
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
