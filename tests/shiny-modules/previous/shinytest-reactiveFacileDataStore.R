library(FacileData)
library(shiny)

devtools::load_all(".")
options(facile.log.level.fshine = "trace")

efds <- exampleFacileDataSet()
universe. <- filter_samples(efds, indication == "BLCA")
# universe. <- NULL

# With filter
shinyApp(
  ui = fluidPage(
    reactiveFacileDataStoreUI("rfds"),
    # facileSampleFilterUI("f1"),
    categoricalSampleCovariateSelectUI("cov")),

  server = function(input, output) {
    # path <- reactive(efds$parent.dir)
    # rfds <- callModule(reactiveFacileDataStore, "rfds", path)
    rfds <- ReactiveFacileDataStore(efds, "rfds", samples = universe.)
    cov <- callModule(categoricalSampleCovariateSelect, "cov", rfds)
  }
)
