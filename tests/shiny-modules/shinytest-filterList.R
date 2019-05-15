library(FacileData)
library(shiny)

options(facile.log.level.fshine = "trace")
debug <- TRUE

devtools::load_all(".")

efds <- exampleFacileDataSet()

# With filter
single <- shinyApp(
  ui = fluidPage(
    reactiveFacileDataStoreUI("rfds"),
    tags$hr(),
    filterListUI("flist", debug = debug)),

  server = function(input, output) {
    path <- reactive(efds$parent.dir)
    rfds <- ReactiveFacileDataStore(path, "rfds")
    flist <- callModule(filterList, "flist", rfds, debug = debug)
  }
)
