#' Composition of modules to provide a filtering schema over a FaclieDataStore.
#'
#' We compose the [reactiveFacileDataStore()] and a list of
#' [facileSampleFilter()] modules to make a super-module.
#'
#' @export
#' @importFrom shiny callModule reactiveValues renderText
#' @rdname filteredReactiveFacileDataStore
filteredReactiveFacileDataStoreServer <- function(id, path,
                                                  user = Sys.getenv("USER"), ...,
                                                  debug = FALSE) {
  shiny::moduleServer(id, function(input, output, session) {
    state <- reactiveValues(
      nsamples = "__initializing__",
      load_time = NULL)
    
    rfds <- callModule(reactiveFacileDataStore, "rfds", path, user = user, ...,
                       debug = debug)
    
    filters <- callModule(sampleFilterList, "filters", rfds, ..., debug = debug)
    
    observeEvent(active_samples(filters), {
      tstart <- Sys.time()
      samples. <- active_samples(filters)
      state$nsamples <- nrow(samples.)
      update_reactive_samples(rfds, samples.)
      state$load_time <- Sys.time() - tstart
      fdebug("sample update load time: ", format(state$load_time))
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

    if (debug) {
      observeEvent(state$load_time, {
        msg <- sprintf("sample update time: %s", format(state$load_time))
        output$debug <- shiny::renderText(msg)
      })
    }
    
    rfds[["filters"]] <- filters
    class(rfds) <- c("FilteredReactiveFacileDataStore", class(rfds))
    
    rfds
  })
}

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
  state <- reactiveValues(
    nsamples = "__initializing__",
    load_time = NULL)

  rfds <- callModule(reactiveFacileDataStore, "rfds", path, user = user, ...,
                     debug = debug)
  class(rfds) <- c("FilteredReactiveFacileDataStore", class(rfds))

  filters <- callModule(sampleFilterList, "filters", rfds, ..., debug = debug)

  observeEvent(active_samples(filters), {
    tstart <- Sys.time()
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
  
  if (debug) {
    observeEvent(state$load_time, {
      msg <- sprintf("sample update time: %s", format(state$load_time))
      output$debug <- shiny::renderText(msg)
    })
  }
  rfds[["filters"]] <- filters
  rfds
}

#' @noRd
#' @export
#' @importFrom shiny tagList textOutput
filteredReactiveFacileDataStoreUI <- function(id, ..., debug = FALSE) {
  ns <- NS(id)

  out <- tagList(
    tags$p("Status: ", textOutput(ns("infotext"), inline = TRUE)),
    if (debug) tags$p(textOutput(ns("debug"))) else NULL,
    sampleFilterListUI(ns("filters"), debug = FALSE))

  out
}
