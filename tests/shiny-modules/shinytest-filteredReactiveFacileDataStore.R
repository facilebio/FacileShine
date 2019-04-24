library(FacileData)
library(shiny)

devtools::load_all(".")

efds <- exampleFacileDataSet()

# With filter
single <- shinyApp(
  ui = fluidPage(
    singleFilteredReactiveFacileDataStoreUI("rfds")),
  server = function(input, output) {
    path <- reactive(efds$parent.dir)
    rfds <- callModule(singleFilteredReactiveFacileDataStore, "rfds", path)
  }
)
