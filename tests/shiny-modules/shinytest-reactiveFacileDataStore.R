library(FacileData)

fds <- FacileData::exampleFacileDataSet()
user <- Sys.getenv("USER")

devtools::load_all(".")

# The following to apps should produce the same output. We'll use the
# `filteredReactiveFacileDataStore` going forward in our proof of concept
# modules to encapsulate everythign that will be there.

manual <- shiny::shinyApp(
  ui = shiny::fluidPage(
    reactiveFacileDataStoreUI("ds"),
    facileSampleFilterUI("rfdsFilter"),
    NULL),

  server = function(input, output) {
    rfds <- callModule(reactiveFacileDataStore, "ds", fds, user = user)
    rfilter <- callModule(facileSampleFilter, "rfdsFilter", rfds)
  }
)

composed <- shiny::shinyApp(
  ui = shiny::fluidPage(filteredReactiveFacileDataStoreUI("ds")),
  server = function(input, output) {
    rfds <- callModule(filteredReactiveFacileDataStore, "ds", fds, user = user)
  }
)
