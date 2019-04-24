#' Composition of modules to provide a filtering schema over a FaclieDataStore.
#'
#' We compose the [reactiveFacileDataStore()] and [facileSampleFilter()] modules
#' to make a super-module. The return objects from these two modules are
#' provided as "top-level" elements of the list returned by this module.
#'
#' @export
#' @rdname filteredReactiveFacileDataStore_v1
filteredReactiveFacileDataStore_v1 <- function(input, output, session, dataset,
                                               active_samples = NULL,
                                               user = Sys.getenv("USER"), ...) {
  assert_class(dataset, "FacileDataStore")
  rfds <- callModule(reactiveFacileDataStore_v1, "rfds", dataset, user = user, ...)
  rfilter <- callModule(facileSampleFilter, "rfdsFilter", rfds)

  rfds[[".reactive."]][["filter"]] <- rfilter
  rfds[[".reactive."]][[".ns"]] <- session$ns
  class(rfds) <- c("FilteredReactiveFacileDataStore", class(rfds))
  rfds
}

#' @export
#' @rdname filteredReactiveFacileDataStore_v1
filteredReactiveFacileDataStore_v1UI <- function(id, ...) {
  ns <- NS(id)
  tagList(
    reactiveFacileDataStoreUI(ns("rfds")),
    facileSampleFilterUI(ns("rfdsFilter")))
}

#' Returns a representaiton of the filters used to specify the active dataset
filters <- function(x, ...) {
  # TODO: Implemenet filters accessor for filteredReactiveFacileDataStore
}

#' Updates the filters used to set the active dataset
update_filters <- function(x, filters, ...) {
  # TODOO: Implement update_filters for filteredReactiveFacileDataStore
}
