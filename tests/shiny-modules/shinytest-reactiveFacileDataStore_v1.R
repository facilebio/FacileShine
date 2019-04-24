library(FacileData)

# fds <- FacileData::exampleFacileDataSet()
# user <- Sys.getenv("USER")

devtools::load_all(".")

# The following to apps should produce the same output. We'll use the
# `filteredReactiveFacileDataStore` going forward in our proof of concept
# modules to encapsulate everythign that will be there.

# manual <- shiny::shinyApp(
#   ui = shiny::fluidPage(
#     reactiveFacileDataStoreUI("ds"),
#     facileSampleFilterUI("rfdsFilter"),
#     NULL),
#
#   server = function(input, output) {
#     rfds <- callModule(reactiveFacileDataStore, "ds", fds, user = user)
#     rfilter <- callModule(facileSampleFilter, "rfdsFilter", rfds)
#   }
# )

shiny::shinyApp(
  ui = shiny::fluidPage(filteredReactiveFacileDataStore_v1UI("ds")),
  server = function(input, output) {
    rfds <- callModule(filteredReactiveFacileDataStore_v1, "ds",
                       FacileData::exampleFacileDataSet(),
                       filteredReactiveFacileDataStore_v1UI)
  }
)
