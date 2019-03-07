library(FacileShine)

# The following to apps should produce the same output. We'll use the
# `filteredReactiveFacileDataStore` going forward in our proof of concept
# modules to encapsulate everythign that will be there.

manual <- shiny::shinyApp(
  ui = shiny::fluidPage(
    reactiveFacileDataStoreUI("rfds"),
    facileSampleFilterUI("rfdsFilter"),
    NULL),

  server = function(input, output) {
    fds <- FacileData::exampleFacileDataSet()
    user <- Sys.getenv("USER")
    rfds <- callModule(reactiveFacileDataStore, "rfds", fds, user = user)
    rfilter <- callModule(facileSampleFilter, "rfdsFilter", rfds)
  }
)

composed <- shiny::shinyApp(
  ui = shiny::fluidPage(
    filteredReactiveFacileDataStoreUI("rfds")),

  server = function(input, output) {
    fds <- FacileData::exampleFacileDataSet()
    user <- Sys.getenv("USER")
    rfds <- callModule(filteredReactiveFacileDataStore, "rfds", fds,
                       user = user)
  }
)
