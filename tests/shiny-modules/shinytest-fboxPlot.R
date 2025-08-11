devtools::load_all(".")
# library(FacileShine)

user <- Sys.getenv("USER")
debug <- TRUE
options(facile.log.level.fshine = "trace")

efds <- FacileData::exampleFacileDataSet()
bfds <- FacileBiocData::example_bioc_data("DGEList", efds) |> FacileBiocData:::facilitate.DGEList()
gdb <- sparrow::exampleGeneSetDb()

shiny::shinyApp(
  ui = shiny::fluidPage(
    shinyjs::useShinyjs(),
    # filteredReactiveFacileDataStoreUI("rfds"),
    facileDataStoreUI("rfds"),
    shiny::tags$h3("facileBoxPlot"),
    fboxPlotUI("box")
  ),
  server = function(input, output) {
    # rfds <- facileDataStoreServer("rfds", shiny::reactive(efds))
    rfds <- facileDataStoreServer("rfds", shiny::reactive(bfds))
    boxplot <- fboxPlotServer("box", rfds, gdb = shiny::reactive(gdb))
  }
)
