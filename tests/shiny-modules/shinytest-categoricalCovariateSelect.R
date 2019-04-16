library(FacileData)
devtools::load_all(".")

efds <- exampleFacileDataSet()
user <- Sys.getenv("USER")

shiny::shinyApp(
  ui = shiny::fluidPage(
    shiny::tagList(
      categoricalSampleCovariateSelectUI("covariate", label = "Covariate"),
      categoricalSampleCovariateLevelsUI("values", label = "Value(s)",
                                         multiple = TRUE))
  ),

  server = function(input, output) {
    rfds <- callModule(reactiveFacileDataStore, "ds", efds, user = user)
    scov <-  callModule(categoricalSampleCovariateSelect, "covariate",
                        rfds, .with_none = FALSE, .reactive = TRUE)
    vals <- callModule(categoricalSampleCovariateLevels, "values", rfds, scov,
                       .reactive = TRUE)

  }
)
