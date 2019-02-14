library(FacileShine)

shiny::shinyApp(
  ui = shiny::fluidPage(
    reactiveFacileDataStoreUI("rfds"),
    facileSampleFilterUI("rfdsFilter")),

  server = function(input, output) {
    fds <- FacileData::exampleFacileDataSet()
    user <- Sys.getenv("USER")
    rfds <- callModule(reactiveFacileDataStore, "rfds", fds, user = user)
    rfilter <- callModule(facileSampleFilter, "rfdsFilter", rfds)
  }
)

if (FALSE) {
  library(FacileData)
  library(esquisse)
  fds <- exampleFacileDataSet()

  samples <- samples(fds)
  features <- filter_features(fds, name %in% c("GZMA", "PRF1"))
  fdat <- samples %>%
    with_assay_data(features, assay_name = "rnaseq", normalized = TRUE) %>%
    with_sample_covariates()

  esquisser(fdat)
}
