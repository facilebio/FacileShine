library(FacileData)
library(shiny)

devtools::load_all(".")

efds <- exampleFacileDataSet()
s <- filter_samples(efds, indication == "BLCA")
s <- NULL

# With filter
shinyApp(
  ui = fluidPage(
    reactiveFacileDataStoreUI("rfds"),
    # facileSampleFilterUI("f1"),
    categoricalSampleCovariateSelectUI("cov")
    ),
  server = function(input, output) {
    path <- reactive(efds$parent.dir)
    # rfds <- callModule(reactiveFacileDataStore, "rfds", path)
    # rfds <- ReactiveFacileDataStore(path, "rfds")
    # f1 <- callModule(facileSampleFilter, "f1", rfds)

    # rfds <- callModule(reactiveFacileDataStore, "rfds", path)
    rfds <- ReactiveFacileDataStore(path, "rfds", samples = s)
    cov <- callModule(categoricalSampleCovariateSelect, "cov", rfds)
  }
)
