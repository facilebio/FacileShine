library(FacileDenaliDataSets)
# library(FacileShine)
devtools::load_all(".")

user <- Sys.getenv("USER")
debug <- TRUE
options(facile.log.level.fshine = "trace")

shinyApp(
  ui = fluidPage(
    datasetSelectUI("dataselect"),
    singleFilteredReactiveFacileDataStoreUI("rfds"),
    tags$h2("facileScatterPlot"),
    facileScatterPlotUI("scatter")),
  server = function(input, output) {
    ds <- callModule(datasetSelect, "dataselect")
    rfds <- callModule(singleFilteredReactiveFacileDataStore, "rfds", ds$path)
    scatter <- callModule(facileScatterPlot, "scatter", rfds, ndim = 3)
  }
)
