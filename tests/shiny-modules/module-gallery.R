devtools::load_all(".")

user <- Sys.getenv("USER")

datadir <- "~/workspace/facilebio/data/"
datadir <- "~/workspace/projects/trv/chemoproteomics/faciledata"
datadir <- "~/workspace/projects/maze/maze-faciledatasets"
debug <- TRUE

options(facile.log.level.fshine = "trace")

aes_color <- TRUE
aes_shape <- TRUE
aes_facet <- TRUE
aes_hover <- TRUE
aes_group <- TRUE

# kfds <- FacileData::FacileDataSet("~/workspace/facilebio/data/BulkKPMPDataSet")
# nfds <- FacileData::FacileDataSet("~/workspace/facilebio/data/FacileNightingaleDataSet")

# afds <- FacileData::an_fds()
# asamples <- FacileData::samples(afds) |> FacileData::with_sample_covariates()
# 
# s0 <- FacileData::filter_samples(dfds, cell_line %in% c("JEKO", "KG"))
# s1 <- dplyr::filter(asamples, cell_abbrev == "CNT") # 10 samples
# s2 <- dplyr::filter(asamples, cell_abbrev == "IC")  # 14 samples

# afds <- FacileData::FacileDataSet("~/workspace/projects/revir/data/facileisr/FacileVWMXtalpiDataSet")
# afds <- FacileData::FacileDataSet("~/workspace/projects/revir/data/faciledata/FacileCompoundDataSet/")
asamples <- FacileData::samples(afds) |> FacileData::with_sample_covariates() |> arrange(group)
s1 <- dplyr::filter(asamples, genotype == "WT")
s2 <- dplyr::filter(asamples, genotype == "R191H", treatment == "vehicle")

if (FALSE) {
  lcovs <- fetch_sample_covariates(kfds)
  cov.sum.short <- summary(lcovs)
  cov.sum.exp <- summary(lcovs, expanded = TRUE)
}

s3 <- dplyr::bind_rows(s1, s2)


# reactlog::reactlog_enable()

shiny::shinyApp(
  ui = shiny::fluidPage(
    shinyjs::useShinyjs(),
    shiny::fluidRow(
      shiny::column(
        width = 3,
        shiny::wellPanel(
          FacileShine::facileDataSetSelectInput("fdslist"),
          FacileShine::facileSampleFiltersSelectInput("rfds", debug = debug)),
        
      shiny::tags$h3("Debug Filters"),
      shiny::verbatimTextOutput("rfdsdebug"),
      
      shiny::tags$h3("Subsetted FDS"),
      shiny::wellPanel(
        FacileShine::facileSampleFiltersSelectInput("rfdssub", debug = debug))
      ),
      
      
      
      
      shiny::column(
        width = 9,

        # Assay Select ---------------------------------------------------------
        shiny::tags$h2("Assay Select"),
        assaySelectInput("assay", label = "Assay", debug = debug),

        # Assay Feature Select -------------------------------------------------
        shiny::tags$hr(),
        shiny::tags$h2("Assay Feature Select"),
        assayFeatureSelectInput("features", label = "Features", debug = debug),
        
        # Categorical Select ---------------------------------------------------
        shiny::tags$hr(),
        shiny::tags$h2("Categorical Select"),
        shiny::fluidRow(
          shiny::column(
            width = 6,
            categoricalSampleCovariateSelectInput("cov1"),
            categoricalSampleCovariateLevelsSelectInput("cov1levels")),
          shiny::column(
            width = 6,
            categoricalSampleCovariateSelectInput("cov2", "Excluded"),
            categoricalSampleCovariateLevelsSelectInput("cov2levels"))),

        # Categorical AES Map Select -------------------------------------------
        shiny::tags$hr(),
        shiny::tags$h2("Categorical aes map"),
        categoricalAestheticMapInput("aes", color = aes_color,
                                     shape = aes_shape, facet = aes_facet,
                                     hover = aes_hover, group = aes_group,
                                     debug = debug),
        
        # Box Plot -------------------------------------------------------------
        shiny::tags$hr(),
        shiny::tags$h2("fboxPlot"),
        fboxPlotUI("boxplot"),
        
        # Scatter Plot ---------------------------------------------------------
        shiny::tags$hr(),
        shiny::tags$h2("fscaterPlot"),
        fscatterPlotUI("scatterplot"),
        
        # Filtered Samples Table -----------------------------------------------
        shiny::tags$hr(),
        shiny::tags$h2("Filtered Samples Table"),
        filteredSamplesTable("fdstable"),
        
        # Prefiltered FacileDataStore [CNT samples (10)] -----------------------
        shiny::tags$hr(),
        shiny::tags$h2("samples-fixed faciledatastore: CNT, 10 samples"),
        filteredSamplesTable("fdscnttable"),
        
        # Prefiltered FacileDataStore [IC samples (14)] ------------------------
        shiny::tags$hr(),
        shiny::tags$h2("samples-fixed faciledatastore: IC, 14 samples"),
        # facileDataStoreUI("rfdsic", with_filters = TRUE),
        filteredSamplesTable("fdsictable"),
        
        # End ------------------------------------------------------------------
        shiny::tags$hr(),
        shiny::tags$h2("End")
      ))
  ),
  
  server = function(input, output) {
    fdslist <- FacileShine::facileDataSetSelectServer(
      "fdslist", reactive(datadir))
    
    rfds <- FacileShine::facileDataStoreServer(
      "rfds", fdslist$path, 
      user = user, debug = debug)
    filterSamplesTableServer("fdstable", rfds)
    
    rfdssub <- FacileShine::facileDataStoreServer(
      "rfdssub", reactive(afds), samples_subset = reactive(s3),
      user = user, debug = debug)
    
    rfdscnt <- FacileShine::facileDataStoreServer(
      "rfdscnt", reactive(afds), samples_subset = reactive(s1),
      with_filters = FALSE,
      user = user, debug = debug)
    filterSamplesTableServer("fdscnttable", rfdscnt)
    
    rfdsic <- FacileShine::facileDataStoreServer(
      "rfdsic", reactive(afds), samples_subset = reactive(s2),
      with_filters = FALSE,
      user = user, debug = debug)
    filterSamplesTableServer("fdsictable", rfdsic)
    

    output$rfdsdebug <- shiny::renderText({
      output <- "not initialized"
      wtf <- try(req(initialized(rfds)), silent = TRUE)
      if (isTRUE(wtf)) {
        output <- paste("nsamples:", nrow(rfds$active_samples()))
      }
      output
    })
    
    
    assay <- assaySelectServer("assay", rfds, debug = debug)
    afeatures <- assayFeatureSelectServer(
      "features", rfds, gdb = fdslist$gdb, debug = debug)
    
    cov1 <- categoricalSampleCovariateSelectServer(
      "cov1", rfds, default_covariate = "hardy_scale")
    c1levels <- categoricalSampleCovariateLevelsSelectServer(
      "cov1levels", cov1)
    
    # these update the things available by ignoring things selected in cov1
    cov2 <- categoricalSampleCovariateSelectServer(
      "cov2", rfds, exclude = cov1$selected)
    c2levels <- categoricalSampleCovariateLevelsSelectServer(
      "cov2levels", cov2, exclude = c1levels$values)
    
    aes <- categoricalAestheticMapServer(
      "aes", rfds, color = aes_color, shape = aes_shape, facet = aes_facet,
      hover = aes_hover, group = aes_group, debug = debug)
    
    boxplot <- fboxPlotServer("boxplot", rfds, gdb = fdslist$gdb)
    
    scatterplot <- fscatterPlotServer("scatterplot", rfds, gdb = fdslist$gdb)
  }
)

# reactlog::reactlog_enable()
# shiny::reactlogShow()
