library(FacileData)
library(shiny)

devtools::load_all(".")

options(facile.log.level.fshine = "trace")

shinyApp(
  ui = fluidPage(
    shinyjs::useShinyjs(),
    datasetSelectUI("dataselect"),
    filteredReactiveFacileDataStoreUI("rfds")),
  server = function(input, output) {
    ds <- callModule(datasetSelect, "dataselect")
    rfds <- callModule(filteredReactiveFacileDataStore, "rfds", ds$path,
                       user = Sys.getenv("USER"))
  }
)

# With filter
# shinyApp(
#   ui = fluidPage(
#     datasetSelectUI("dataselect"),
#     reactiveFDSUI("rfds"),
#     filteredReactiveFDSUI("ffds")),
#   server = function(input, output) {
#     ds <- callModule(datasetSelect, "dataselect")
#     rfds <- callModule(reactiveFDS, "rfds", ds$path)
#     ffds <- callModule(filteredReactiveFDS, "ffds", rfds)
#   }
# )