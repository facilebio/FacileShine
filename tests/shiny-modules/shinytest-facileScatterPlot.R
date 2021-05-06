# library(FacileDenaliDataSets)
# library(FacileShine)
devtools::load_all(".")


debug <- TRUE

efds <- FacileData::exampleFacileDataSet()
# efds <- FacileDenaliDataSet("mouse")
user <- Sys.getenv("USER")
gdb <- sparrow::exampleGeneSetDb()

options(facile.log.level.fshine = "trace")

shiny::shinyApp(
  ui = shiny::fluidPage(
    shinyjs::useShinyjs(),
    filteredReactiveFacileDataStoreUI("rfds"),
    tags$h2("facileScatterPlot"),
    facileScatterPlotUI("scatter")),

  server = function(input, output) {
    rfds <- ReactiveFacileDataStore(efds, "rfds")
    scatter <- callModule(facileScatterPlot, "scatter", rfds,
                          gdb = gdb,
                          ndim = 3)
  }
)

