devtools::load_all(".")
# library(FacileShine)

user <- Sys.getenv("USER")
debug <- TRUE
options(facile.log.level.fshine = "trace")

efds <- FacileData::exampleFacileDataSet()
gdb <- sparrow::exampleGeneSetDb()

shiny::shinyApp(
  ui = shiny::fluidPage(
    shinyjs::useShinyjs(),
    filteredReactiveFacileDataStoreUI("rfds"),
    shiny::tags$h3("facileBoxPlot"),
    facileBoxPlotUI("box")),
  server = function(input, output) {
    rfds <- ReactiveFacileDataStore(efds, "rfds")
    boxplot <- shiny::callModule(facileBoxPlot, "box", rfds, gdb = gdb)
  }
)
