devtools::load_all(".")

user <- Sys.getenv("USER")
datadir <- "~/workspace/facilebio/data/"
debug <- TRUE
options(facile.log.level.fshine = "trace")

aes_color <- TRUE
aes_shape <- TRUE
aes_facet <- TRUE
aes_hover <- TRUE
aes_group <- TRUE

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
    shiny::fluidRow(
      shiny::column(
        width = 3,
        FacileShine::facileDataSetSelectInput("fdslist"),
        FacileShine::facileSampleFiltersSelectInput("rfds", debug = debug),
        
      shiny::tags$h2("Debug Module"),
      shiny::verbatimTextOutput("rfdsdebug")),
      
      shiny::column(
        width = 9,

        # Assay Select ----------------------------------------------------------
        shiny::tags$h2("Assay Select"),
        assaySelectInput("assay", label = "Assay", debug = debug),

        # Assay Feature Select -------------------------------------------------
        shiny::tags$h2("Assay Feature Select"),
        assayFeatureSelectInput("features", label = "Features", debug = debug),
        
        # Categorical Select ---------------------------------------------------
        shiny::tags$h2("Categorical Select"),
        categoricalSampleCovariateSelectInput("cov1"),
        categoricalSampleCovariateLevelsSelectInput("cov1levels"),

        # Categorical AES Map Select -------------------------------------------
        shiny::tags$h2("Categorical aes map"),
        categoricalAestheticMapInput("aes", color = aes_color,
                                     shape = aes_shape, facet = aes_facet,
                                     hover = aes_hover, group = aes_group,
                                     debug = debug),
        
        # Box Plot -------------------------------------------------------------
        shiny::tags$h2("fboxPlot"),
        fboxPlotUI("boxplot"),
        
        shiny::tags$h2("End")
      ))
  ),
  
  server = function(input, output) {
    fdslist <- FacileShine::facileDataSetSelectServer(
      "fdslist", reactive(datadir))
    
    rfds <- FacileShine::datamodFacileDataStoreServer(
      "rfds", fdslist$path, user = user, debug = debug)
    
    output$rfdsdebug <- shiny::renderText({
      output <- "not initialized"
      wtf <- try(req(initialized(rfds)), silent = TRUE)
      if (isTRUE(wtf)) {
        output <- paste("nsamples:", nrow(active_samples(rfds)))
      }
      output
    })
    
    
    assay <- assaySelectServer("assay", rfds, debug = debug)
    afeatures <- assayFeatureSelectServer(
      "features", rfds, gdb = fdslist$gdb, debug = debug)
    
    cov1 <- categoricalSampleCovariateSelectServer(
      "cov1", rfds, default_covariate = "hardy_scale")
    clevels <- categoricalSampleCovariateLevelsSelectServer(
      "cov1levels", cov1)
    
    aes <- categoricalAestheticMapServer(
      "aes", rfds, color = aes_color, shape = aes_shape, facet = aes_facet,
      hover = aes_hover, group = aes_group, debug = debug)
    
    boxplot <- fboxPlotServer("boxplot", rfds)
  }
)

# reactlog::reactlog_enable()
shiny::reactlogShow()
