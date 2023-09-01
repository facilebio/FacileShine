library(FacileData)
# library(FacileShine)
devtools::load_all(".")

root.dir <- "/datasets"
options(facile.log.level.fshine = "trace")
config <- yaml::yaml.load_file(file.path(root.dir, "/eyrie-config.yaml"))
config$datastores$datastores <- lapply(config$datastores$datastores, function(d) {
  d$path <- file.path(root.dir, "/datastores", d$path)
  d
})

shiny::shinyApp(
  ui = shiny::fluidPage(
    shinyjs::useShinyjs(),
    datasetSelectUI("dataselect"),
    filteredReactiveFacileDataStoreUI("rfds")),
  server = function(input, output) {
    ds <- shiny::callModule(datasetSelect, "dataselect", config)
    rfds <- shiny::callModule(filteredReactiveFacileDataStore, "rfds", ds$path,
                              user = Sys.getenv("USER"))
  })
