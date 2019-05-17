library(FacileData)
library(shiny)

options(facile.log.level.fshine = "trace")

devtools::load_all(".")

efds <- exampleFacileDataSet()

# With filter
single <- shinyApp(
  ui = fluidPage(
    # singleFilteredReactiveFacileDataStoreUI("rfds")
    filteredReactiveFacileDataStoreUI("rfds", debug = TRUE)),
  server = function(input, output) {
    path <- reactive(efds$parent.dir)
    rfds <- callModule(filteredReactiveFacileDataStore, "rfds", path,
                       debug = TRUE)
  }
)
