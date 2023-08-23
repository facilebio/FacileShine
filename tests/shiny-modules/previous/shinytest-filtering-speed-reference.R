devtools::load_all(".")

user <- Sys.getenv("USER")
datadir <- "~/workspace/facilebio/data/"
debug <- TRUE
options(facile.log.level.fshine = "trace")
xfds <- FacileData::FacileDataSet("~/workspace/facilebio/data/FacileNightingaleDataSet")

shiny::shinyApp(
  ui = shiny::fluidPage(
    shinyjs::useShinyjs(),
    
    FacileShine::facileDataSetSelectInput("fdslist"),
    FacileShine::filteredReactiveFacileDataStoreUI("rfds", debug = debug),
    
    shiny::tags$h2("facileBoxPlot"),
    facileBoxPlotUI("box")),
  
  server = function(input, output) {
    fdslist <- FacileShine::facileDataSetSelectServer(
      "fdslist", reactive(datadir))
    rfds <- FacileShine::filteredReactiveFacileDataStoreServer(
      "rfds", path = fdslist$path, user = user, debug = debug)
    
    # boxplot <- shiny::callModule(facileBoxPlot, "box", rfds)
  }
)
