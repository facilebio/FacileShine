# library(FacileShine)
devtools::load_all(".")


efds <- FacileData::exampleFacileDataSet()
user <- Sys.getenv("USER")
debug <- TRUE

with.sample.filter <- TRUE

options(facile.log.level.fshine = "trace")

shiny::shinyApp(
  ui = shiny::fluidPage(
    shiny::tagList(
      if (with.sample.filter) {
        shiny::wellPanel(singleFilteredReactiveFacileDataStoreUI("rfds"))
      } else {
        reactiveFacileDataStoreUI("rfds")
      },
      categoricalSampleCovariateSelectUI("covariate", label = "Covariate"),
      categoricalSampleCovariateLevelsUI("values", label = "Value(s)",
                                         multiple = TRUE, debug = debug),
      tags$h4("Exlude entire covariate"),
      categoricalSampleCovariateSelectUI("excov", multiple = TRUE,
                                         label = "iCovariate"),
      tags$h4("Exclude levels from main covariate"),
      categoricalSampleCovariateLevelsUI("exvals", label = "Value(s)",
                                         multiple = TRUE, debug = debug)

  )),

  server = function(input, output) {
    path <- reactive(efds$parent.dir)
    if (with.sample.filter) {
      rfds <- callModule(singleFilteredReactiveFacileDataStore, "rfds", path)
    } else {
      rfds <- callModule(reactiveFacileDataStore, "rfds", path)
    }

    scov <-  callModule(categoricalSampleCovariateSelect, "covariate",
                        rfds, .with_none = FALSE, .reactive = TRUE,
                        debug = debug)
    vals <- callModule(categoricalSampleCovariateLevels, "values", rfds, scov,
                       .reactive = TRUE, debug = debug)
    excov <- callModule(categoricalSampleCovariateSelect, "excov",
                        rfds, .with_none = FALSE, .reactive = TRUE,
                        .exclude = scov$covariate, debug = debug)
    exvals <- callModule(categoricalSampleCovariateLevels, "exvals", rfds, scov,
                         .exclude = vals$values, .reactive = TRUE,
                         debug = debug)
  }
)


