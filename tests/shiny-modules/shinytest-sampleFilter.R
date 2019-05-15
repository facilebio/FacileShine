library(FacileData)
library(shiny)

options(facile.log.level.fshine = "trace")
debug <- TRUE

devtools::load_all(".")

efds <- exampleFacileDataSet()

# With filter
single <- shinyApp(
  ui = fluidPage(
    reactiveFacileDataStoreUI("rfds"),
    tags$h3("Sample filter with no preset universe"),
    sampleFilterUI("sf", debug = debug),
    tags$h3("Sample filter, universe ==  all samples"),
    sampleFilterUI("sfall", debug = debug),
    tags$h3("Sample filter, universe == BLCA"),
    sampleFilterUI("sfblca", debug = debug)),

  server = function(input, output) {
    path <- reactive(efds$parent.dir)
    rfds <- ReactiveFacileDataStore(efds, "rfds")

    sf <- callModule(sampleFilter, "sf", rfds, debug = debug)

    # Specifying the universe
    all.samples <- reactive(collect(samples(efds), n = Inf))
    blca.samples <- reactive(filter_samples(efds, indication == "BLCA"))

    sfall <- callModule(sampleFilter, "sfall", rfds, all.samples, debug = debug)
    sfblca <- callModule(sampleFilter, "sfblca", rfds, blca.samples, debug = debug)
  }
)
