devtools::load_all(".")

user <- Sys.getenv("USER")
debug <- TRUE
options(facile.log.level.fshine = "trace")

efds <- FacileData::exampleFacileDataSet()

shiny::shinyApp(
  ui = shiny::fluidPage(
    shinyjs::useShinyjs(),
    # singleFilteredReactiveFacileDataStoreUI("rfds"),
    filteredReactiveFacileDataStoreUI("rfds"),
    shiny::tags$h3("facileBoxPlot"),
    facileBoxPlotUI("box")),
  server = function(input, output) {
    rfds <- ReactiveFacileDataStore(efds, "rfds")
    boxplot <- shiny::callModule(facileBoxPlot, "box", rfds)
  }
)
