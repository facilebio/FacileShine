#' Composition of modules to provide a filtering schema over a FaclieDataStore.
#'
#' We compose the [reactiveFacileDataStore()] and a list of
#' [facileSampleFilter()] modules to make a super-module.
#'
#' @export
#' @rdname filteredReactiveFacileDataStore
filteredReactiveFacileDataStore <- function(input, output, session, path,
                                            user = Sys.getenv("USER"),
                                            nfilters = 1, ...) {
  rfds <- callModule(reactiveFacileDataStore, "rfds", path, user, ...)
  class(rfds) <- c("FilteredReactiveFacileDataStore", class(rfds))

  rfds[["filters"]] <- callModule(filterList, "filters", rfds, ...)
  rfds
}

#' @noRd
#' @export
filteredReactiveFacileDataStoreUI <- function(id, nfilters = 1, ...) {
  ns <- NS(id)

  tagList(
    tags$h3("Data Filters"),
    filterListUI("filters"),
    reactiveFacileDataStoreUI(ns("rfds")))
}


# SingleFilterReactiveFacileDataStore works ====================================
#' @section Single Filtered:
#'
#' We compose the [reactiveFacileDataStore()] and a list of
#' [facileSampleFilter()] modules to make a super-module.
#'
#' @export
#' @rdname filteredReactiveFacileDataStore
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

#' @export
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
