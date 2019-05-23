debug <- TRUE

# TODO: complete categoricalSampleCovariateLevelsMutex
efds <- FacileData::exampleFacileDataSet()
# efds <- FacileDenaliDataSet("mouse")
user <- Sys.getenv("USER")

options(facile.log.level.fshine = "trace")

shiny::shinyApp(
  ui = shiny::fluidPage(
    shinyjs::useShinyjs(),
    filteredReactiveFacileDataStoreUI("rfds"),
    categoricalSampleCovariateSelectUI("covariate", label = "Covariate"),
    categoricalSampleCovariateLevelsUI("val1", label = "Val1",
                                       multiple = TRUE, debug = debug),
    categoricalSampleCovariateLevelsUI("val2", label = "Val2",
                                       multiple = TRUE, debug = debug)),

  server = function(input, output) {
    rfds <- ReactiveFacileDataStore(efds, "rfds")

    scov <-  callModule(categoricalSampleCovariateSelect, "covariate",
                        rfds, .with_none = FALSE, .reactive = TRUE,
                        debug = debug)
    val1 <- callModule(categoricalSampleCovariateLevels, "val1", rfds, scov,
                       .reactive = TRUE, debug = debug)
    val2 <- callModule(categoricalSampleCovariateLevels, "val2", rfds, scov,
                       .reactive = TRUE, debug = debug)

    # Make the levels available in the numer and denom covariates
    # mutually exclusive
    observeEvent(val1$values(), {
      update_exclude(val2, val1$values)
    })
    observeEvent(val2$values(), {
      update_exclude(val1, val2$values)
    })
  }
)

