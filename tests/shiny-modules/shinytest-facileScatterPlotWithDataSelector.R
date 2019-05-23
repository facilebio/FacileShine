library(FacileDenaliDataSets)
# library(FacileShine)
devtools::load_all(".")

user <- Sys.getenv("USER")
debug <- TRUE
options(facile.log.level.fshine = "trace")

shinyApp(
  ui = fluidPage(
    datasetSelectUI("dataselect"),
    filteredReactiveFacileDataStoreUI("rfds"),
    tags$h3("facileScatterPlot"),
    facileScatterPlotUI("scatter")),
  server = function(input, output) {
    ds <- callModule(datasetSelect, "dataselect")
    rfds <- callModule(filteredReactiveFacileDataStore, "rfds", ds$path)
    scatter <- callModule(facileScatterPlot, "scatter", rfds, ndim = 3)
  }
)
