library(FacileDenaliDataSets)
# library(FacileShine)
devtools::load_all(".")


debug <- TRUE

efds <- FacileData::exampleFacileDataSet()
# efds <- FacileDenaliDataSet("mouse")
user <- Sys.getenv("USER")

options(facile.log.level.fshine = "trace")

shiny::shinyApp(
  ui = shiny::fluidPage(
    shinyjs::useShinyjs(),
    # singleFilteredReactiveFacileDataStoreUI("rfds"),
    filteredReactiveFacileDataStoreUI("rfds"),
    tags$h2("facileScatterPlot"),
    facileScatterPlotUI("scatter")),

  server = function(input, output) {
    # rfds <- shiny::callModule(singleFilteredReactiveFacileDataStore, "rfds", ds$path)
    rfds <- ReactiveFacileDataStore(efds, "rfds")
    scatter <- callModule(facileScatterPlot, "scatter", rfds, ndim = 3)
  }
)

