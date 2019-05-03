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

# FilterList module ============================================================

#' Implementation of the filterlist module is inspired by Joe Cheng's
#' `filter_module.R` from the rpharma-demo shiny app:
#' https://github.com/jcheng5/rpharma-demo
#'
#' @noRd
#' @export
#' @importFrom shiny setBookmarkExclude
filterList <- function(input, output, session, rfds, ...) {
  ns <- session$ns
  setBookmarkExclude(c("add_filter_btn"))

  # names(filters) will be the name of the sample covariate that is used for
  # the filtering, because ... that's why.
  filters <- list()
  makeReactiveBinding("filters")

  # onBookmark(function(state) {
  #   state$values$filter_field_names <- names(filter_fields)
  # })
  #
  # onRestore(function(state) {
  #   filter_field_names <- state$values$filter_field_names
  #   for (fieldname in filter_field_names) {
  #     addFilter(fieldname)
  #   }
  # })

  available.covs <- reactive({
    active_covariates(rfds) %>%
      filter(class == "categorical")

    filter()
  })
  observeEvent(input$add_filter_btn, {

  })
}

#' @noRd
#' @export
#' @importFrom shiny actionButton NS tagList tags
filterListUI <- function(id, ...) {
  ns <- NS(id)
  tagList(
    tags$div(id = ns("filter_container")),
    actionButton(ns("add_filter_btn"), "Add Filter"))
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
