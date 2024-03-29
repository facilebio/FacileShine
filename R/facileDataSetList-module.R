#' Lists the FacileDataSet objects in a parent directory for a user to choose.
#' 
#' This module provides a list of FacileDataSet objects to choose from that
#' live in a parent directory. It will be of most use when building an
#' application that allows the exploratory data analysis across a number of
#' different sources of data.
#' 
#' Enumerates the available dataset objects in `datadir` once upon invocation.
#' A `meta.yaml` file in `datadir` will tell it how to present (name) and group
#' the datasets from a dropdown select.
#' 
#' @section Data Directory: 
#' The `datadir` must present itself as a locally accessible directory to the
#' system running this module.
#' 
#' @export
#' @param id the ID of the module
#' @param datadir the directory that holds the FacileDataSet directories
#' @return A list with reactive components:
#' 1. `$path()`: The path to the FacileDataSet
#' 2. `$gdb()`: A GeneSetDb object to match the organism of the FDS at `$path`.
facileDataSetSelectServer <- function(id, datadir, metafn = NULL, ...) {
  moduleServer(id, function(input, output, session) {
    state <- reactiveValues(
      organism = "__initializing__")
    
    # Initialize the selectUI on startup
    dinfo <- reactive({
      .parse_dataset_directory(datadir(), metafn)
    })
    
    observeEvent(dinfo(), {
      dinfo. <- req(dinfo())
      choices <- .parse_dataset_choices(dinfo.)
      selected <- attr(choices, "selected")
      shiny::updateSelectInput(session, "dataselect", choices = choices,
                               selected = selected)
    })
    
    dataset_info <- reactive({
      chosen <- req(input$dataselect)
      ftrace("updating selected dataset: ", chosen)
      info <- filter(isolate(dinfo()), .data$name == .env$chosen)
      if (isolate(state$organism) != info$organism) {
        ftrace("updating organism: ", info$organism)
        state$organism <- info$organism
      }
      info
    })
    
    fds.path <- reactive(dataset_info()$path)
    
    gdb <- reactive({
      out <- NULL
      org <- state$organism
      if (!unselected(org)) {
        gspath <- file.path(datadir(), "_metadata", org, "genesets.qs")
        if (file.exists(gspath)) {
          ftrace("Updating GeneSetDb: ", gspath)
          tic()
          out <- qs::qread(gspath)
          tt <- toq()
          ftrace("GeneSetDb load: ", tt$ss)
        } else {
          ftrace("GeneSetDb not found: ", gspath)
        }
      }
      out
    })
    
    out <- list(
      selected = dataset_info,
      path = fds.path,
      datasets = dinfo,
      gdb = gdb,
      .state = state,
      .ns = session$ns)
    class(out) <- c("FacileDataSetSelectModule", class(out))
    out
  })
}


#' @noRd
#' @export
facileDataSetSelectInput <- function(id, label = "Select Dataset",
                                     choices = NULL, selected = NULL,
                                     multiple = FALSE,
                                     selectize = TRUE, width = NULL,
                                     size = NULL, ...) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::selectInput(
      ns("dataselect"), label = label, choices = choices,
      selected = selected, multiple = multiple, selectize = selectize,
      width = width, size = size))
}

#' Parses the output of .parse_dataset_directory into a selectize nested list
#'
#' The order of the choices will be as they are listed in the `dinfo` tibble.
#' 
#' @noRd
#' @param dinfo the tibble with dataset information from the
#'   `.parse_dataset_directory()` function
#' @return a nested list (or named character vector) that can be used as the
#'   `choices` for [shiny::selectizeInput()]
.parse_dataset_choices <- function(dinfo, selectizeInput = NULL) {
  choices <- sapply(unique(dinfo$group), function(grp) {
    xc <- filter(dinfo, .data$group == .env$grp)
    stats::setNames(xc$name, xc$label)
  }, simplify = FALSE)
  if (length(choices) == 1L) {
    choices <- choices[[1L]]
  }
  attr(choices, "selected") <- filter(dinfo, default)[["name"]]
  choices
}

#' Lists the FacileDataSet looking directories in a parent directory.
#' 
#' Currently the directories under `datadir` are recognized as a FacileDataSet
#' root if they have a `meta.yaml` file with top-level `name` and `organism`
#' attributes.
#' 
#' @noRd
#' @param datadir the parent directory that holds the FacileDataSet directories
#' @param metafn an optional argument that identifies the yaml file that holds
#'   metadata about the datasets in `datadir`
.parse_dataset_directory <- function(datadir, metafn = NULL, ...) {
  if (FALSE) {
    datadir <- system.file("testdata", "fds-directory", package = "FacileShine")
    metafn <- file.path(datadir, "meta.yaml")
  }
  assert_directory_exists(datadir, "r")
  
  paths <- dir(datadir, full.names = TRUE)
  fddirs <- sapply(paths, test_facile_dataset_directory)
  paths <- paths[fddirs]
  
  if (length(paths) == 0) {
    none <- tibble(name = character(), label = name, path = name,
                   organism = name, gdb = name, gdb_load = logical(),
                   meta = list(), default = logical())
    return(none)
  }
  
  ds.meta <- sapply(basename(paths), function(fname) {
    yaml::read_yaml(file.path(datadir, fname, "meta.yaml"))
  }, simplify = FALSE)
  
  info <- tibble(
    name = basename(paths),
    label = sapply(ds.meta, "[[", "name"),
    path = paths,
    organism = sapply(ds.meta, "[[", "organism"),
    gdb = file.path(datadir, "_metadata", organism, "genesets.qs"),
    gdb_load = file.exists(gdb),
    meta = ds.meta,
    default = FALSE)
  
  # meta.fn <- file.path(datadir, metafn)
  if (is.null(metafn)) metafn <- file.path(datadir, "meta.yaml")
  if (file.exists(metafn)) {
    meta <- yaml::read_yaml(metafn)[["datasets"]]
    if (is.list(meta)) {
      # 1. Identify default dataset, if specified
      if (checkmate::test_string(meta$default)) {
        info[["default"]] <- info[["name"]] == meta$default
      }
      # 2. Parse dataset <> grouping information
      groups <- meta$groups
      if (is.list(groups)) {
        group.xref <- lapply(names(groups), function(grp) {
          dplyr::tibble(name = unlist(groups[[grp]]), group = grp)
        })
        group.xref <- dplyr::bind_rows(group.xref)
        info <- info |> 
          left_join(group.xref, by = "name") |> 
          mutate(group = ifelse(is.na(group), "ungrouped", group))
        # We will arrange the outgoing tibble to be in the same order as was
        # listed in the meta.yaml file
        dorder <- unique(c(group.xref$name, info$name))
        info <- info |> 
          mutate(name = factor(name, dorder)) |> 
          arrange(name) |> 
          mutate(name = as.character(name))
      }
    }
  } else {
    info[["group"]] <- "ungrouped"
  }
  
  if (!any(info$default)) {
    info$default[1L] <- TRUE
  }
  
  if (nrow(info) == 0) {
    info <- NULL
  }
  
  info
}

parse_dataset_directory_meta <- function(metafn) {
  if (!file.exists(metafn)) {
    warning("No metadata exists for data directory")
    return(NULL)
  }
}
