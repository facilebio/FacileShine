# library(FacileShine)
# library(dplyr)
# library(plotly)

library(FacileData)
library(shiny)
devtools::load_all(".")

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

