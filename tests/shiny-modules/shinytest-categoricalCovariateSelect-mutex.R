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
      categoricalSampleCovariateLevelsUI("val1", label = "Value(s)",
                                         multiple = TRUE, debug = debug),
      categoricalSampleCovariateLevelsUI("val2", label = "Value(s)",
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
    val1 <- callModule(categoricalSampleCovariateLevels, "val1", rfds, scov,
                       .reactive = TRUE, debug = debug)
    val2 <- callModule(categoricalSampleCovariateLevels, "val2", rfds, scov,
                       .reactive = TRUE, debug = debug)
    observeEvent(val1$values(), {
      v2 <- isolate(val2$values())
      update_exclude(val2, val1$values)
    })
    observeEvent(val2$values(), {
      update_exclude(val1, val2$values)
    })
  }
)


