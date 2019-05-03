# library(FacileShine)
devtools::load_all(".")


efds <- FacileData::exampleFacileDataSet()
user <- Sys.getenv("USER")
debug <- TRUE

options(facile.log.level.fshine = "trace")

shiny::shinyApp(
  ui = shiny::fluidPage(
    shiny::tagList(
      shiny::wellPanel(singleFilteredReactiveFacileDataStoreUI("rfds")),
      categoricalSampleCovariateSelectUI("covariate", label = "Covariate"),
      categoricalSampleCovariateLevelsUI("values", label = "Value(s)",
                                         multiple = TRUE, debug = debug))
  ),

  server = function(input, output) {
    path <- reactive(efds$parent.dir)
    rfds <- callModule(singleFilteredReactiveFacileDataStore, "rfds", path)
    scov <-  callModule(categoricalSampleCovariateSelect, "covariate",
                        rfds, .with_none = FALSE, .reactive = TRUE,
                        debug = debug)
    vals <- callModule(categoricalSampleCovariateLevels, "values", rfds, scov,
                       .reactive = TRUE, debug = debug)

  }
)
