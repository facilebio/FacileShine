#' A filterable ReactiveFacileDataStore based on the datamods mojo
#' 
#' @export
#' @param id the id of the module
#' @param x the thing that will instantiate a FacileDataSet
#' @param user the user using it
#' @return a DatamodFacileDataStore,ReactiveFacileDataStore, ...,
datamodFacileDataStoreServer <- function(id, x, ..., user = Sys.getenv("USER"),
                                         debug = FALSE) {
  shiny::moduleServer(id, function(input, output, session) {
    state <- shiny::reactiveValues(
      fds = "__initializing__",
      name = "__initializing__",
      active_samples = "__initializing__",
      active_assays = "__initializing__",
      active_covariates = "__initializing__",
      
      # ephemeral annotations provided by user during interactive exploration
      esample_annotation = .empty_sample_annotation_tbl(),
      efeature_annotation = .empty_feature_annotation_tbl(),
      
      efacets = .empty_facet_tbl(),
      
      # added this for the filteredReactiveFacileDataStore2 datamod impl
      sample_covariate_universe = tibble(
        dataset = character(), sample_id = character()))
    
    obj <- list(
      user = user,
      universe = NULL,
      trigger = list(
        dataset = makeReactiveTrigger(),
        samples = makeReactiveTrigger(),
        assays = makeReactiveTrigger(),
        covariates = makeReactiveTrigger()),
      .state = state,
      .ns = sessoin$ns)
    class(obj) <- c(
      "DatamodFacileDataStore", "ReactiveFacileDataStore", "FacileDataStore")
    obj
  })
}

