# library(FacileShine)
devtools::load_all(".")

efds <- FacileData::exampleFacileDataSet()
user <- Sys.getenv("USER")


options(facile.log.level.fshine = "trace")

shiny::shinyApp(
  ui = shiny::fluidPage(
    shiny::wellPanel(singleFilteredReactiveFacileDataStoreUI("rfds")),
    tags$h2("facileScatterPlot"),
    facileScatterPlotUI("scatter")),

  server = function(input, output) {
    path <- reactive(efds$parent.dir)
    rfds <- callModule(singleFilteredReactiveFacileDataStore, "rfds", path)
    scatter <- callModule(facileScatterPlot, "scatter", rfds, ndim = 3)
  }
)

