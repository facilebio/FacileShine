# library(FacileDenaliDataSets)
# library(FacileShine)
devtools::load_all(".")


debug <- TRUE

efds <- FacileData::exampleFacileDataSet()
# efds <- FacileDenaliDataSet("mouse")
user <- Sys.getenv("USER")

gdbs <- list(
  hallmark = sparrow::getMSigGeneSetDb("H", id.type = "entrez"),
  kegg = sparrow::getKeggGeneSetDb("human", "entrez"),
  null = NULL)

options(facile.log.level.fshine = "trace")

shiny::shinyApp(
  ui = shiny::fluidPage(
    shinyjs::useShinyjs(),
    filteredReactiveFacileDataStoreUI("rfds"),
    shiny::selectInput("gdb", "GeneSetDb", c("hallmark", "kegg", "null")),
    tags$h2("facileScatterPlot"),
    # facileScatterPlotUI("scatter")
    fscatterPlotUI("scatter")),

  server = function(input, output, session) {
    rfds <- ReactiveFacileDataStore(efds, "rfds")
    gdb <- reactive({
      gdbs[[input$gdb]]
    })

    # scatter <- fscatterPlotServer("scatter", rfds, gdb = gdb, ndim = ndim)
    scatter <- callModule(facileScatterPlot, "scatter", rfds,
                          gdb = gdb,
                          ndim = 3)
  }
)

