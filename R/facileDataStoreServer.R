#' A filterable ReactiveFacileDataStore based on the datamods mojo
#'
#' @section active_samples:
#' The samples that are currently selected
#' 
#' @section active_assays:
#' A [FacileData::assay_summary()] tibble over the `active_samples(x)`:
#' 
#' ```
#' assay      ndatasets nsamples assay_type feature_type nfeatures
#' <chr>          <int>    <int> <chr>      <chr>            <int>
#' rnaseq             1       66 rnaseq     ensgid           23487
#' proteomics         1       22 lognorm    ensgid            4350
#' ```
#' 
#' @section active_pdata:
#' The subset of the pdata universe of the samples
#' 
#' @section active_covariates:
#' The `FacileData::summary.wide_covariates(active_pdata, expanded = TRUE)`
#' that describes the covariates available over the samples
#' 
#' ```
#' variable      class       nsamples level                          ninlevel
#' <chr>         <chr>          <int> <chr>                             <dbl>
#' structure     categorical       74 proximal tubules                      9
#' structure     categorical       74 thick ascending limb                  9
#' structure     categorical       74 tubolointerstitium                   11
#' tissue_source categorical       74 KPMP Pilot                           48
#' tissue_source categorical       74 KPMP Tissue Interrogation Site       20
#' tissue_source categorical       74 unknown                               6
#' ```
#' 
#' @export
#' @param id the id of the module
#' @param x the thing that will instantiate a FacileDataSet
#' @param filter_vars the variables to use for filtering. Default is `NULL`,
#'   which means to use all of the variables.
#' @param user the user using it
#' @return a DatamodFacileDataStore module with the following named elemets:
#'   * `fds`: the `BoxedFacileDataStore`
#'   * `filters`: [datamods::filter_data_server()]
#'   * `active_samples`: the `dataset,sample_id` facile_frame of the samples
#'      that match the current filters.
#'   * `active_assays`: [FacileData::assay_summary()] over the current samples
#'   * `active_pdata`: A wide data.frame of all the covariates (ugh)
#'   * `active_covariates`: A `FacileData::summary(active_pdata, expanded = TRUE)`
#'   * `user`: the name of the current user
#'   * `trigger`: reactive triggers over samples, assays, covariates. I don't
#'     think we need to use these now ...
facileDataStoreServer <- function(id, x, ...,
                                  with_filters = TRUE,
                                  filter_vars = reactive(NULL),
                                  samples_subset = reactive(NULL),
                                  samples_filter_init = reactive(NULL),
                                  samples_filter_categorical_only = TRUE,
                                  user = Sys.getenv("USER"),
                                  debug = FALSE) {
  assert_flag(with_filters)
  
  moduleServer(id, function(input, output, session) {
    state <- reactiveValues(
      name = "__initializing__",
      active_samples = "__initializing__",
      
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
      ftrace("{bold}{red}initializing new fds() [fds]")
      out <- .xfdsFactory(x(), ...)
      req(is(out, "FacileDataStore"))
      
      state$name <- name(out)
      # state$active_samples <- "__initializing__"
      
      state$esample_annotation <- .empty_sample_annotation_tbl()
      state$efeature_annotation <- .empty_feature_annotation_tbl()
      state$efacets <- .empty_facet_tbl()
      
      out
    }, label = "fds")
    
    # Defines the universe of covariates from within the FacileDataSet
    pdata_fds <- reactive({
      req(initialized(fds()))
      ftrace("{bold}{red}Defining pdata universe from FDS [pdata_fds]")
      tic()
      ss <- samples_subset()
      if (is.null(ss) || !is(ss, "facile_frame")) {
        ss <- FacileData::samples(fds())
      }
      ss <- distinct(ss, dataset, sample_id)
      res <- FacileData::with_sample_covariates(ss)
      tt <- toq()
      ftrace("... pdata_fds universe construction: ", tt$ss)
      res
    }, label = "pdata_fds")
    
    # pdata --------------------------------------------------------------------
    # This is the full universe of pdata across all the samples in the
    # FacileDataSet
    pdata <- eventReactive({ pdata_fds(); state$esample_annotation }, {
      out <- pdata_fds()
      req(is(pdata_fds(), "facile_frame"))
      ftrace("... adding ephemeral sample covariates [pdata]")
      tic()
      
      if (nrow(state$esample_annotation)) {
        ftrace("{bold}{red}adding ephemeral sample covariates [pdata]")
        is_long <- local({
          check <- c("variable", "value")
          all(check %in% colnames(state$esample_annotation))
        })
        if (is_long) {
          # TODO: pivot into wide form and presere type
          ephemeral <- state$esample_annotation |> 
            select(dataset, sample_id, variable, value) |> 
            tidyr::pivot_wider(
              id_cols = c("dataset", "sample_id"),
              names_from = "variable")
        } else {
          ephemeral <- state$esample_annotation
        }
        
        # run the join so that the ephemeral values take precedence over the
        # internal ones
        out <- left_join(out, ephemeral, by = c("dataset", "sample_id"),
                         suffix = c(".infds", ""))
      }
      
      for (cname in colnames(out)) {
        if (is.logical(out[[cname]])) {
          out[[cname]] <- ifelse(out[[cname]], "yes", "no")
        }
      }
      
      tt <- toq()
      ftrace("... ephemeral pdata addition: ", tt$ss, " [pdata]")
      out
    }, label = "pdata")
    
    active_samples <- reactive({
      req(is(state$active_samples, "facile_frame"))
      state$active_samples
    })
    
    if (with_filters) {
      # datamods::filters ------------------------------------------------------
      # what sample_covariate columns do we want to include in the filtering?
      vars2filter <- reactive({
        req(is(pdata(), "tbl"))
        flog("defining sample_covariates to use for datamod filters")
        
        pdat <- pdata()
        fvars <- unique(c(filter_vars(), meta_info(fds())[["sample_filters"]]))
        fall <- colnames(pdat)
        
        if (test_flag(fvars) && isFALSE(fvars)) {
          # TODO: Disables filtering
          ftrace("Disable all filters ...")
          fme <- character()
        } else if (test_character(fvars)) {
          fme <- intersect(fall, fvars)
        } else {
          ftrace("No modification to filter variables")
          fme <- NULL
        }
        
        ignore <- c("dataset", "sample_id")
        fme <- setdiff(fme, ignore)
        
        if (samples_filter_categorical_only) {
          # NOTE: we are only filtering on categorical varialbes for now.
          ignore.numeric <- sapply(pdat, is.numeric)
          fme <- setdiff(fme, fall[ignore.numeric])
        }
        
        # NOTE: When se support "ephemeral annotations" we should include
        # all those by default.
        if (test_character(fme, min.len = 1)) {
          ftrace("Filtering dataset using these covariates: ",
                 paste(fme, collapse = ";;"))
        } else if (test_character(fme, max.len = 0)) {
          ftrace("Empty filtering variables means disable filtering")
        }
        
        fme
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
      
      # active_samples update --------------------------------------------------    
      # state$active_samples can only be updated when people mess with filters
      observeEvent(
        filters$filtered(),
        ignoreInit = TRUE,
        label = "observeEvent(filtered$filtered())",
        {
          filtered <- filters$filtered()
          ftrace("retrieving ", nrow(filtered), " new sample subset from ",
                 "datamod::filters [observeEvent||filtered||filtered()")
          fsamples <- distinct(filtered, dataset, sample_id)
          
          # check if fsamples is different from state$active_samples
          update <- unselected(state$active_samples) || 
            !setequal(
              paste0(fsamples$dataset, fsamples$sample_id),
              paste0(state$active_samples$dataset, state$active_samples$sample_id))
          
          if (update) {
            flog(
              "{reset}{bold}{green}updating state$active_samples{reset}: ", 
              nrow(fsamples))
            state$active_samples <- fsamples |> 
              as_facile_frame(classes = "reactive_facile_frame")
          } else {
            flog("filters$filtered() fired but active_samples didn't change: no-op")
          }
        })
      
      observeEvent(active_samples(), {
        shinyWidgets::updateProgressBar(
          session = session, id = "pbar",
          value = nrow(active_samples()), total = nrow(pdata()))
      })
    } else {
      filters <- reactive(NULL)
      
      observeEvent(pdata(), {
        pdat <- req(pdata(), is(pdata(), "tbl"))
        psamples <- distinct(pdat, dataset, sample_id)
        update <- unselected(state$active_samples) || 
          !setequal(
            paste0(psamples$dataset, psamples$sample_id),
            paste0(state$active_samples$dataset, state$active_samples$sample_id))
        if (update) {
          flog(
            "{reset}{bold}{green}updating state$active_samples{reset}: ", 
            nrow(pdat))
          state$active_samples <- psamples |> 
            as_facile_frame(classes = "reactive_facile_frame")
        } else {
          flog("pdata() fired but active_samples didn't change: no-op")
        }
      })
      
      output$fdsdebug <- shiny::renderText({
        output <- "not initialized"
        asamples <- active_samples()
        if (is(asamples, "tbl")) {
          output <- paste("nsamples:", nrow(rfds$active_samples()))
        }
        output
      })
    }

    # active_assays ------------------------------------------------------------
    active_assays <- eventReactive(state$active_samples, {
      req(is(state$active_samples, "facile_frame"))
      flog("updating active_assays [active_assays]")
      fds() |> 
        FacileData::assay_summary(state$active_samples) |> 
        collect(n = Inf)
    }, label = "active_assays")

    active_pdata <- reactive({
      req(is(pdata(), "facile_frame"))
      req(is(state$active_samples, "facile_frame"))
      ftrace("updating active_pdata [active_pdata]")
      semi_join(pdata(), state$active_samples, by = c("dataset", "sample_id"))
    }, label = "active_pdata")
    
    active_covariates <- reactive({
      req(is(active_pdata(), "facile_frame"))
      ftrace("updating active_covariates")
      active_pdata() |> 
        summary(expanded = TRUE) |> 
        # if we don't include this step, sampleCovariateLevelSelect dropdowns
        # can return levels that have 0 support given the current set of
        # active samples
        filter(.data$ninlevel > 0)
    }, label = "active_covariates")
    
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

#' Create a server module form a non-reactive context.
#' 
#' @export
#' @param id the id of the server module
#' @param x A thing, this will be passed into the reactive
#'   `facileDataStoreServer,x` parameter.
#' @param ... things to pass into the `facileDataStoreServer(...)`
#' @return a [facileDataStoreServer()] module
FacileDataStoreServer <- function(id, x, samples_subset = NULL, ...) {
  assert_string(id)
  if (is(x, "reactive")) {
    stop("You can only use this in non-reactive context")
  }
  if (is(samples_subset, "reactive")) {
    stop("You can only use this in non-reactive context")
  }
  
  if (test_string(x) || is(x, "FacileDataStore")) {
    xx <- reactive(x)
  } else if (is(x, "facile_frame")) {
    xx <- reactive(fds(x))
    samples_subset <- reactive(x)
  } else {
    stop("Not sure what to do with x of type: ", class(x)[1L])
  }
  
  facileDataSetSelectServer(
    id, xx, samples_subset = reactive(samples_subset), ...)
}

#' Helper function to create a FacileDataStore (really a FacileDataSet).
#' 
#' This function figures out what `x` is and turns into into a FacileDataStore.
#' 
#' @param x a path to a faciledataset on disk, a faciledataset itself, etc.
#' @param ... dunno what else
#' @return A FacileDataStore[Set]
.xfdsFactory <- function(x, ...) {
  if (test_directory_exists(x)) {
    x <- FacileData::FacileDataSet(x, ...)
  }
  if (is(x, "FacileDataStore")) {
    # Wrapping this as "BoxedFacileDataStore" so we can intercept some facile
    # API calls and allow them to accept ephemeral annotations.
    class(x) <- c("BoxedFacileDataStore", class(x))
    return(x)
  }
  
  stop("`x` of type '", class(x)[1L], "' not handled yet")
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

# UI stuffs --------------------------------------------------------------------

#' @export
#' @noRd
facileSampleFiltersSelectInput <- function(id, progress_bar = TRUE, ...,
                                           debug = FALSE) {
  ns <- shiny::NS(id)
  shiny::tagList(
    if (progress_bar) {
      shinyWidgets::progressBar(
        id = ns("pbar"), value = 100, total = 100, display_pct = TRUE)
    } else {
      NULL
    },
    datamods::filter_data_ui(ns("filtering"), show_nrow = TRUE))
}

#' Show the filtered samples table for a ReactiveFacileDataSet
#' 
#' @export
filterSamplesTableServer <- function(id, dfds, ..., debug = FALSE) {
  assert_class(dfds, "DatamodFacileDataStore")
  moduleServer(id, function(input, output, session) {
    # Visuals ==================================================================
    output$table <- reactable::renderReactable({
      req(is(dfds$active_pdata(), "tbl"))
      reactable::reactable(dfds$active_pdata(), server = TRUE)
    })
  })
}

#' @export
#' @noRd
filteredSamplesTable <- function(id, ..., debug = FALSE) {
  ns <- shiny::NS(id)
  reactable::reactableOutput(ns("table"))
}

#' This has the sample table and filters rolled into one
#' For debugging purposes only.
#' 
#' @export
#' @noRd
facileDataStoreUI <- function(id, with_filters = TRUE, ..., debug = FALSE) {
  ns <- shiny::NS(id)
  if (with_filters) {
    out <- shiny::fluidRow(
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
  } else {
    out <- shiny::verbatimTextOutput("fdsdebug")
  }
  out
}

# FacileDataStore methods ------------------------------------------------------

#' @noRd
#' @export
initialized.BoxedFacileDataStore <- function(x, ...) {
  is(x, "FacileDataStore")
}

#' @noRd
#' @export
initialized.DatamodFacileDataStore <- function(x, ...) {
  # check <- c("name", "active_samples")
  # ready <- sapply(check, \(s) !unselected(x$.state[[s]]))
  check <- c("name")
  ready <- sapply(check, \(s) !unselected(x$.state[[s]]))
  initialized(x$fds()) && all(ready) && !unselected(x$active_samples())
}

#' @noRd
#' @export
active_assays.DatamodFacileDataStore <- function(x, ...) {
  ftrace("{bold}{green}active_assays(module)")
  req(initialized(x))
  x$active_assays()
}

#' @noRd
#' @export
active_covariates.DatamodFacileDataStore <- function(x, active_only = TRUE,
                                                      ...) {
  ftrace("{bold}{green}active_covariates(module)")
  req(initialized(x))
  x$active_covariates()
}

#' @noRd
#' @export
active_samples.DatamodFacileDataStore <- function(x, ...) {
  ftrace("{bold}{green}active_samples(module)")
  req(initialized(x))
  x$active_samples()
}

#' @noRd
#' @export
user.DatamodFacileDataStore <- function(x, ...) {
  req(initialized(x))
  x[["user"]]
}

#' @noRd
#' @export
name.DatamodFacileDataStore <- function(x, ...) {
  req(initialized(x))
  # x[[".state"]][["name"]]
  name(x$fds())
}

# Utility Functions ============================================================

#' @noRd
.empty_sample_annotation_tbl <- function() {
  tibble(
    dataset = character(),
    sample_id = character(),
    variable = character(),
    value = character(),
    class = character(),
    type = character(),
    date_entered = integer())
}

#' @noRd
.empty_facet_tbl <- function() {
  tibble(
    dataset = character(),
    sample_id = character(),
    facet = character(),
    description = character())
}

#' @noRd
.empty_feature_annotation_tbl <- function() {
  tibble(
    collection = character(),
    name = character(),
    feature_id = character(),
    feature_type = character())
}
