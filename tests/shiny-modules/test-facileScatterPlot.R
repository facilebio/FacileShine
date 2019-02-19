# library(FacileShine)
# library(dplyr)
# library(plotly)
devtools::load_all(".")
library(FacileData)
library(FacileViz)
library(shiny)

shiny::shinyApp(
  ui = shiny::fluidPage(
    filteredReactiveFacileDataStoreUI("rfds"),
    shiny::tags$hr(),
    facileScatterPlotUI("scatter")),

  server = function(input, output) {
    fds <- FacileData::exampleFacileDataSet()
    user <- Sys.getenv("USER")
    rfds <- callModule(filteredReactiveFacileDataStore, "rfds", fds,
                       user = user)
    scatter <- callModule(facileScatterPlot, "scatter", rfds, ndim = 3)
  }
)

