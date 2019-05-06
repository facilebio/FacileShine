#' Returns a config object for the workbench
#'
#' @family workbench functions
#' @export
load_config <- function(config = NULL) {
  if (is.null(config)) {
    config <- system.file("extdata", "workbench-config.yaml",
                          package = "FacileShine")
  }
  if (test_string(config)) {
    assert_file_exists(config, access = "r", extension = c("yaml", "yml"))
    config <- yaml.load_file(config)
  }
  assert_list(config)
  assert_subset(c("datastores", "modules"), names(config))
  config
}

#' Returns available datasets
#'
#' @export
#' @importFrom yaml yaml.load_file
#' @importFrom stats setNames
datastores_info <- function(config = NULL, as_selectize_list = FALSE) {
  config <- load_config(config)

  ds.config <- config[["datastores"]]
  datasets <- lapply(names(ds.config[["datastores"]]), function(key) {
    mutate(as_tibble(ds.config[["datastores"]][[key]]), key = key)
  })
  datasets <- bind_rows(datasets)
  datasets <- select(datasets, group, key, name, everything())

  if (as_selectize_list) {
    groups <- sort(unique(datasets[["group"]]))
    group_order <- ds.config[["params"]][["group_order"]]
    if (is.character(group_order)) {
      groups <- unique(c(intersect(group_order, groups), groups))
    }
    datasets <- sapply(groups, function(grp) {
      dat <- filter(datasets, group == grp)
      setNames(dat[["key"]], dat[["name"]])
    }, simplify = FALSE)
  }

  datasets
}
