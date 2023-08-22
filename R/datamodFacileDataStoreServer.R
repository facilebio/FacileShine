#' A filterable ReactiveFacileDataStore based on the datamods mojo
#' 
#' @export
#' @param id the id of the module
#' @param x the thing that will instantiate a FacileDataSet
#' @param user the user using it
#' @return a DatamodFacileDataStore,ReactiveFacileDataStore, ...,
datamodFacileDataStoreServer <- function(id, x, ...,
                                         ignore_sample_covariates = reactive(NULL),
                                         samples_subset = reactive(NULL),
                                         samples_filter_init = reactive(NULL),
                                         user = Sys.getenv("USER"),
                                         debug = FALSE) {
  moduleServer(id, function(input, output, session) {
    state <- reactiveValues(
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
    
    fds <- eventReactive(x(), {
      # Setting this to __initializing__ so that things "reacting" on an
      # initialized(rfds) wait until the first pass of
      # observeEvent(state$active_samples) goes through before they use the
      # new ReactiveFacileDataSet again
      ftrace("{bold}{red}initializing new fds()")
      out <- FacileDataStoreFactory(x(), ...)
      req(is(out, "FacileDataStore"))
      
      state$name <- name(out)
      # state$active_samples <- "__initializing__"
      # state$active_assays <- "__initializing__"
      # state$active_covariates <- "__initializing__"
      # state$pdata <- "__initializing__"
      state$esample_annotation <- .empty_sample_annotation_tbl()
      state$efeature_annotation <- .empty_feature_annotation_tbl()
      state$efacets <- .empty_facet_tbl()
      out
    }, label = "fds")
    
    # Defines the universe of covariates from within the FacileDataSet
    pdata_fds <- reactive({
      req(initialized(fds()))
      ftrace("{bold}{red}Defining pdata universe from faciledataset")
      tic()
      ss <- samples_subset()
      if (is.null(ss) || !is(ss, "facile_frame")) {
        ss <- FacileData::samples(fds())
      }
      ss <- distinct(ss, dataset, sample_id)
      res <- FacileData::with_sample_covariates(ss)
      tt <- toq()
      ftrace("... configuring end: ", tt$ss)
      res
    }, label = "pdata_fds")
    
    # This is the full universe of pdata across all the samples in the
    # FacileDataSet
    pdata <- eventReactive({ pdata_fds(); state$esample_annotation }, {
      out <- pdata_fds()
      req(is(pdata_fds(), "facile_frame"))
      
      if (nrow(state$esample_annotation)) {
        ftrace("{bold}{red}adding ephemeral sample covariates")
        # TODO: pivot into wide form and presere type
        ephemeral <- state$esample_annotation |> 
          select(dataset, sample_id, variable, value) |> 
          tidyr::pivot_wider(
            id_cols = c("dataset", "sample_id"),
            names_from = "variable")
        out <- left_join(out, ephemeral, by = c("dataset", "sample_id"))
      }
      
      for (cname in colnames(out)) {
        if (is.logical(out[[cname]])) {
          out[[cname]] <- ifelse(out[[cname]], "yes", "no")
        }
      }
      
      out
    }, label = "pdata")
    
    
    # what sample_covariate columns do we want to include in the filtering?
    vars2filter <- reactive({
      req(is(pdata(), "tbl"))
      flog("defining sample_covariates to use for datamod filters")
      fme <- colnames(pdata())
      ignore <- c("dataset", "sample_id")
      imore <- ignore_sample_covariates()
      if (is.character(imore)) ignore <- unique(c(ignore, imore))
      setdiff(fme, ignore)
    }, label = "var2filter")
    
    # filters$filtered() are coming back as a wide_covariates, facile_frame
    filters <- datamods::filter_data_server(
      id = "filtering",
      data = pdata,
      vars = vars2filter,
      name = reactive(isolate(state$name)),
      # name = reactive("dataset"),
      defaults = samples_filter_init,
      drop_ids = TRUE,
      widget_char = "picker")
    
    # The big state updates are here ===========================================
    
    # state$active_samples can only be updated when people mess with filters
    observeEvent(filters$filtered(), {
      filtered <- filters$filtered()
      ftrace("retrieving ", nrow(filtered), " new sample subset from ",
             "datamod::filters")
      fsamples <- distinct(filtered, dataset, sample_id)
      
      # check if fsamples is different from state$active_samples
      update <- unselected(state$active_samples) || 
        !setequal(fsamples$sample_id, state$active_samples$sample_id)
      if (update) {
        flog("updating state$active_samples")
        state$active_samples <- fsamples
        
        shinyWidgets::updateProgressBar(
          session = session, id = "pbar",
          value = nrow(fsamples), total = nrow(pdata()))
      } else {
        flog("filters$filtered() fired but active_samples didn't change: no-op")
      }
    }, label = "observeEvent(filtered$filtered())")
    
    active_samples <- reactive(state$active_samples)
    
    active_assays <- reactive({
      req(is(state$active_samples, "facile_frame"))
      flog("updating active_assays")
      fds() |> 
        FacileData::assay_summary(state$active_samples) |> 
        collect(n = Inf)
    }, label = "active_assays")

    active_pdata <- reactive({
      req(is(pdata(), "facile_frame"))
      req(is(state$active_samples, "facile_frame"))
      ftrace("updating active_pdata")
      semi_join(pdata(), state$active_samples, by = c("dataset", "sample_id"))
    })
    
    active_covariates <- reactive({
      req(is(active_pdata(), "facile_frame"))
      ftrace("updating active_covariates")
      summary(active_pdata(), expanded = TRUE)
    }, label = "active_covariates")
    
    # 
    # # whenever active_samples are updated, we will have to update:
    # # 1. active_assays
    # # 2. active_covariates
    # observeEvent(state$active_samples, {
    #   fds. <- req(fds())
    #   req(!unselected(state$active_samples))
    #   
    #   state$active_assays <- fds. |> 
    #     FacileData::assay_summary(state$active_samples) |> 
    #     collect(n = Inf)
    # })
    # 
    # observeEvent(state$active_pdata, {
    #   req(!unselected(state$active_pdata))
    #   nr <- nrow(state$active_pdata) #|> as.character()
    #   # ftrace("active_pdata updated, nrows: ", as.character(nr))
    #   ftrace("active_pdata updated, nrows: ", nr)
    #   state$active_covariates <- summary(state$active_pdata)
    # }, ignoreInit = TRUE)
    
    # active_samples <- eventReactive(filters$filtered(), {
    #   flog("updating active_samples")
    #   ff <- filters$filtered()
    #   # browser()
    #   asamples <- distinct(filters$filtered(), dataset, sample_id)
    #   class(asamples) <- setdiff(class(asamples), "wide_covariates")
    #   asamples
    # }, ignoreInit = TRUE)
    
    # observeEvent(active_samples(), {
    #   flog("Updaing sample-selection percentage bar")
    #   shinyWidgets::updateProgressBar(
    #     session = session, id = "pbar",
    #     value = nrow(active_samples()), total = nrow(pdata()))
    # })
    
    
    # Visuals ==================================================================
    output$table <- reactable::renderReactable({
      # req(is(state$active_samples, "tbl"))
      # reactable::reactable(state$active_samples, server = TRUE)
      req(is(active_pdata(), "tbl"))
      reactable::reactable(active_pdata(), server = TRUE)
    })
    
    obj <- list(
      fds = fds,
      filters = filters,
      active_samples = active_samples,
      active_assays = active_assays,
      active_pdata = active_pdata,
      active_covariates = active_covariates,
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
  # check <- c("active_samples", "active_assays", "active_covariates", "name")
  # initing <- sapply(check, function(var) {
  #   obj <- isolate(x[[".state"]][[var]])
  #   test_string(obj) && unselected(obj)
  # })
  # 
  # !any(initing) && initialized(isolate(x$fds()))
  # initialized(isolate(x$fds()))
  initialized(x$fds())
}

#' @noRd
#' @export
active_assays.DatamodFacileDataStore <- function(x, ...) {
  req(initialized(x))
  # depend(x, "assays")
  # x[[".state"]][["active_assays"]]
  x$active_assays()
}

#' @noRd
#' @export
active_covariates.DatamodFacileDataStore <- function(x, active_only = TRUE,
                                                      ...) {
  ftrace("{bold}{green}active_covariates(module)")
  req(initialized(x))
  # depend(x, "covariates")
  # as_facile_frame(x[[".state"]][["active_covariates"]], x,
  #                 .valid_sample_check = FALSE)
  x$active_covariates()
}

#' @noRd
#' @export
active_samples.DatamodFacileDataStore <- function(x, ...) {
  req(initialized(x))
  # depend(x, "samples")
  # as_facile_frame(x[[".state"]][["active_samples"]], x,
  #                 .valid_sample_check = FALSE)
  x$active_samples()
}

#' @noRd
#' @export
user.ReactiveFacileDataStore <- function(x, ...) {
  req(initialized(x))
  x[["user"]]
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