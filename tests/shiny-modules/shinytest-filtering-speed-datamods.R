devtools::load_all(".")

user <- Sys.getenv("USER")
datadir <- "~/workspace/facilebio/data/"
debug <- TRUE
options(facile.log.level.fshine = "trace")

kfds <- FacileData::FacileDataSet("~/workspace/facilebio/data/BulkKPMPDataSet")
nfds <- FacileData::FacileDataSet("~/workspace/facilebio/data/FacileNightingaleDataSet")

if (FALSE) {
  lcovs <- fetch_sample_covariates(kfds)
  cov.sum.short <- summary(lcovs)
  cov.sum.exp <- summary(lcovs, expanded = TRUE)
}

# reactlog::reactlog_enable()

shiny::shinyApp(
  ui = shiny::fluidPage(
    shinyjs::useShinyjs(),
    
    FacileShine::facileDataSetSelectInput("fdslist"),
    FacileShine::datamodFacileDataStoreUI("rfds", debug = debug),
    
    shiny::tags$h2("Debugg Module"),
    shiny::verbatimTextOutput("rfdsdebug"),
    
    # Categorical Select -------------------------------------------------------
    shiny::tags$h2("Categorical Select"),
    # categoricalSampleCovariateSelectUI("categorical"),
    categoricalSampleCovariateSelectInput("cov1"),
    categoricalSampleCovariateLevelsSelectInput("cov1levels"),
    
    # Assay Select -------------------------------------------------------------
    assaySelectInput("assay", debug = debug),
    
    # Assay Feature Select -----------------------------------------------------
    assayFeatureSelectInput("features", debug = debug),

    # Box Plot -----------------------------------------------------------------
    # shiny::tags$h2("fboxPlot"),
    # fboxPlotUI("boxplot"),
    
    shiny::tags$h2("End")),
  
  server = function(input, output) {
    fdslist <- FacileShine::facileDataSetSelectServer(
      "fdslist", reactive(datadir))
    
    rfds <- FacileShine::datamodFacileDataStoreServer(
      "rfds", fdslist$path, user = user, debug = debug)
    
    output$rfdsdebug <- shiny::renderText({
      output <- "not initialized"
      wtf <- try(req(initialized(rfds)))
      if (isTRUE(wtf)) {
        output <- paste("nsamples:", nrow(active_samples(rfds)))
      }
      output
    })
    
    cov1 <- categoricalSampleCovariateSelectServer(
      "cov1", rfds, default_covariate = "hardy_scale")
    
    clevels <- categoricalSampleCovariateLevelsSelectServer(
      "cov1levels", cov1)
    
    assay <- assaySelectServer("assay", rfds, debug = debug)
    afeatures <- assayFeatureSelectServer("afeatures", rfds, gdb = debug = debug)
    # boxplot <- fboxPlotServer("boxplot", rfds)
    # covariate <- shiny::callModule(
    #   categoricalSampleCovariateSelect, "categorical", rfds)
    # 
    # boxplot <- shiny::callModule(facileBoxPlot, "box", rfds)
  }
)

# reactlog::reactlog_enable()
shiny::reactlogShow()
