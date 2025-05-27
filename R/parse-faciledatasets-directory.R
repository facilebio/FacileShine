#' Lists the FacileDataSet-looking directories in a parent directory.
#' 
#' Currently the directories under `datadir` are recognized as a FacileDataSet
#' root if they have a `meta.yaml` file with top-level `name` and `organism`
#' attributes.
#' 
#' @export
#' @param datadir the parent directory that holds the FacileDataSet directories
#' @param metafn an optional argument that identifies the yaml file that holds
#'   metadata about the datasets in `datadir`
#' @return a tibble of information for the FacileDataSets in the `datadir`
#'   directory.
parse_faciledatasets_directory <- function(
    datadir, 
    metafn = NULL, 
    list_referenced_datasets_only = TRUE,
    ...
) {
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
  
  if (isTRUE(list_referenced_datasets_only)) {
    info <- dplyr::filter(info, .data$group != "ungrouped")
  }

  dupes <- dplyr::filter(info, duplicated(.data$name))
  if (nrow(dupes) > 0) {
    warning("Removing duplicated datasets from meta.fn: ", paste(dupes$name, collapse = ","))
    info <- dplyr::distinct(info, .data$name, .keep_all = TRUE)
  }
  
    
  if (nrow(info) == 0L) {
    stop("No datasets available to show")
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
