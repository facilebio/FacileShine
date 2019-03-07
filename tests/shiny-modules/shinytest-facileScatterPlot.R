library(FacileData)
library(shiny)

fds <- FacileData::exampleFacileDataSet()
user <- Sys.getenv("USER")

devtools::load_all(".")

shiny::shinyApp(
  ui = shiny::fluidPage(
    shiny::wellPanel(filteredReactiveFacileDataStoreUI("ds")),
    tags$h2("facileScatterPlot"),
    facileScatterPlotUI("scatter")),

  server = function(input, output) {
    rfds <- callModule(filteredReactiveFacileDataStore, "ds", fds, user = user)
    scatter <- callModule(facileScatterPlot, "scatter", rfds, ndim = 3)
  }
)

