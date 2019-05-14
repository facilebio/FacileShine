library(FacileDenaliDataSets)
# library(FacileShine)
devtools::load_all(".")

user <- Sys.getenv("USER")
debug <- TRUE
options(facile.log.level.fshine = "trace")

shiny::shinyApp(
  ui = shiny::fluidPage(
    shinyjs::useShinyjs(),
    datasetSelectUI("dataselect"),
    singleFilteredReactiveFacileDataStoreUI("rfds"),
    shiny::tags$h2("facileBoxPlot"),
    facileBoxPlotUI("box")),
  server = function(input, output) {
    ds <- shiny::callModule(datasetSelect, "dataselect")
    rfds <- shiny::callModule(singleFilteredReactiveFacileDataStore, "rfds", ds$path)
    boxplot <- shiny::callModule(facileBoxPlot, "box", rfds)
  }
)