library(FacileShine)

shiny::shinyApp(
  ui = shiny::fluidPage(
    reactiveFacileDataStoreUI("rfds"),
    facileSampleFilterUI("rfdsFilter")),
  server = function(input, output) {
    fds <- exampleFacileDataSet()
    user <- Sys.getenv("USER")

    rfds <- callModule(reactiveFacileDataStore, "rfds", fds, user = user)
    rfilter <- callModule(facileSampleFilter, "rfdsFilter", rfds)
  }
)
