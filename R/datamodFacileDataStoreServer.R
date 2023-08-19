#' A filterable ReactiveFacileDataStore based on the datamods mojo
#' 
#' @export
#' @param id the id of the module
#' @param x the thing that will instantiate a FacileDataSet
#' @param user the user using it
#' @return a DatamodFacileDataStore,ReactiveFacileDataStore, ...,
datamodFacileDataStoreServer <- function(id, x, ...,
                                         ignore_vars = shiny::reactive(NULL),
                                         samples_subset = shiny::reactive(NULL),
                                         user = Sys.getenv("USER"),
                                         debug = FALSE) {
  shiny::moduleServer(id, function(input, output, session) {
    state <- shiny::reactiveValues(
      name = "__initializing__",
      active_samples = "__initializing__",
      active_assays = "__initializing__",
      active_covariates = "__initializing__",
      
      # ephemeral annotations provided by user during interactive exploration
      esample_annotation = .empty_sample_annotation_tbl(),
      efeature_annotation = .empty_feature_annotation_tbl(),
      
      efacets = .empty_facet_tbl(),
      
      # gross: the full (wide) sample_covarates for this faciledataset
      pdata = "__initializing__",
      active_pdata = "__initializing__")

    triggers <- list(
      dataset = makeReactiveTrigger(),
      samples = makeReactiveTrigger(),
      assays = makeReactiveTrigger(),
      covariates = makeReactiveTrigger())
    
    fds <- shiny::eventReactive(x(), {
      # Setting this to __initializing__ so that things "reacting" on an
      # initialized(rfds) wait until the first pass of
      # observeEvent(state$active_samples) goes through before they use the
      # new ReactiveFacileDataSet again
      ftrace("{bold}{red}initializing new fds()")
      out <- FacileDataStoreFactory(x(), ...)
      req(is(out, "FacileDataStore"))
      
      state$name <- name(out)
      state$active_samples <- "__initializing__"
      state$active_assays <- "__initializing__"
      state$active_covariates <- "__initializing__"
      state$esample_annotation <- .empty_sample_annotation_tbl()
      state$efeature_annotation <- .empty_feature_annotation_tbl()
      state$efacets <- .empty_facet_tbl()
      pdata <- "__initializing__"
      out
    })
    
    pdata <- shiny::reactive({
      req(is(fds(), "FacileDataStore"))
      ftrace("{bold}{red}configuring fds()")
      tstart <- Sys.time()
      ss <- samples_subset()
      if (is.null(ss) || !is(ss, "facile_frame")) {
        ss <- FacileData::samples(fds())
      }
      ss <- distinct(ss, dataset, sample_id)
      res <- FacileData::with_sample_covariates(ss)
      ftrace("... configuring end: ", format(Sys.time() - tstart))
      res
    })
    
    vars2filter <- reactive({
      req(is(pdata(), "tbl"))
      fme <- colnames(pdata())
      ignore <- c("dataset", "sample_id")
      imore <- ignore_vars()
      if (is.character(imore)) ignore <- unique(c(ignore, imore))
      setdiff(fme, ignore)
    })
    
    # filters$filtered() are coming back as a wide_covariates, facile_frame
    filters <- datamods::filter_data_server(
      id = "filtering",
      data = pdata,
      vars = vars2filter,
      name = reactive(state$name),
      # defaults = reactive(list("pass", "preset", "filters", "here"))
      drop_ids = TRUE,
      widget_char = "picker")
    
    # The big state updates are here ===========================================
    shiny::observeEvent(filters$filtered(), {
      state$active_pdata <- filters$filtered()
      state$active_samples <- local({
        asamples <- distinct(state$active_pdata, dataset, sample_id)
        class(asamples) <- setdiff(class(asamples), "wide_covariates")
        asamples
      })

      shinyWidgets::updateProgressBar(
        session = session, id = "pbar",
        value = nrow(state$active_samples), total = nrow(pdata()))
    })
 
    output$table <- reactable::renderReactable({
      req(is(state$active_samples, "tbl"))
      reactable::reactable(state$active_samples, server = TRUE)
    })
    
    obj <- list(
      fds = fds,
      filters = filters,
      user = user,
      universe = NULL,
      trigger = triggers,
      .state = state,
      .ns = session$ns)
    class(obj) <- c(
      "DatamodFacileDataStore", "ReactiveFacileDataStore", "FacileDataStore")
    obj
  })
}

#' @export
#' @noRd
datamodFacileDataStoreUI <- function(id, ..., debug = FALSE) {
  ns <- shiny::NS(id)
  shiny::fluidRow(
    shiny::column(
      width = 3,
      datamods::filter_data_ui(ns("filtering"), show_nrow = TRUE)
    ),
    shiny::column(
      width = 9,
      shinyWidgets::progressBar(
        id = ns("pbar"), value = 100, total = 100, display_pct = TRUE
      ),
      reactable::reactableOutput(outputId = ns("table"))
    )
  )
}

#' @noRd
#' @export
initialized.DatamodFacileDataStore <- function(x, ...) {
  # just like initialized.ReactiveFacileDataStore but we don't put `fds`
  # in the state

  # Check if any internal state elements are __initializing__
  check <- c("active_samples", "active_assays", "active_covariates", "name")
  initing <- sapply(check, function(var) {
    obj <- shiny::isolate(x[[".state"]][[var]])
    # is.character(obj) && obj == "__initializing__"
    test_string(obj) && unselected(obj)
  })
  
  !any(initing) && is(shiny::isolate(x$fds()), "FacileDataStore")
}

#' Helper function to create a FacileDataStore (really a FacileDataSet).
#' 
#' This function figures out what `x` is and turns into into a FacileDataStore.
#' 
#' @param x a path to a faciledataset on disk, a faciledataset itself, etc.
#' @param ... dunno what else
#' @return A FacileDataStore[Set]
FacileDataStoreFactory <- function(x, ...) {
  if (test_directory_exists(x)) {
    x <- FacileData::FacileDataSet(x, ...)
  }
  if (is(x, "FacileDataSet")) {
    # Wrapping this as "BoxedFacileDataStore" so we can intercept some facile
    # API calls and allow them to accept ephemeral annotations.
    class(x) <- c("BoxedFacileDataStore", class(x))
    return(x)
  }
  
  stop("There are other things we can do, but not now ...")
  # You can imagine x is path to a serialized bioconductor object, or the
  # bioconductor object itself
  if (test_file_exists(x, extension = "rds")) {
    x <- readRDS(x)
  }
  if (test_multi_class(x, c("SummarizedExperiment", "DGEList"))) {
    reqpkg("FacileBiocData")
    out <- FacileBiocData::facilitate(x, ...)
    class(out) <- c("BoxedFacileDataStore", class(out))
    return(out)
  }
 }