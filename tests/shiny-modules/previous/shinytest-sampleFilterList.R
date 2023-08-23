library(FacileData)
library(shiny)

options(facile.log.level.fshine = "trace")
debug <- TRUE

devtools::load_all(".")

efds <- exampleFacileDataSet()

single <- shinyApp(
  ui = fluidPage(
    shinyjs::useShinyjs(),
    reactiveFacileDataStoreUI("rfds", debug = FALSE),
    tags$hr(),
    sampleFilterListUI("flist", debug = debug)),

  server = function(input, output) {
    rfds <- ReactiveFacileDataStore(efds, "rfds", debug = FALSE)
    flist <- callModule(sampleFilterList, "flist", rfds, debug = debug)
  }
)
