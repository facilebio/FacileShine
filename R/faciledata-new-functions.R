#' Provides a tibble summary of the covariates available over a sample space.
#' 
#' There is a FacileData::summary.eav_covariates function, but this is down
#' stream of a `collect()`-ed long sample_covariate table, which doesn't allow
#' us to take advantage of SQL manipulation prior to `collect()`
#' 
#' @noRd
#' @export
sample_covariate_summary <- function(x, ..., detailed = FALSE) {
  UseMethod("sample_covariate_summary", x)
}

#' @noRd
#' @export
sample_covariate_summary.facile_frame <- function(x, ..., detailed = FALSE) {
  sample_covariate_summary(
    fds(x), 
    samples = distinct(x, dataset, sample_id),
    detailed = detailed,
    ...)
}

#' @noRd
#' @export
sample_covariate_summary.FacileDataSet <- function(x, samples = NULL, ...,
                                                   categorical_only = TRUE,
                                                   with_levels = FALSE,
                                                   detailed = FALSE,
                                                   verbose = FALSE) {
  if (FALSE) {
    x <- fds
    samples <- samples.all
    samples <- FacileData::samples(x)
  }
  sctbl <- sample_covariate_tbl(x)
  
  do_semi <- !is.null(samples)
  if (is.null(samples)) {
    samples <- samples(x)
  } else {
    if (!same_src(sctbl, samples)) {
      stop("doing a semi_join against an external data.frame is deadly.")
    }
  }
  
  assert_class(samples, "facile_frame")
  assert_flag(detailed)
  assert_flag(categorical_only)
  assert_flag(with_levels)
  
  if (missing(categorical_only) && with_levels) {
    if (verbose) message("Setting categorical_only to TRUE")
    categorical_only <- TRUE
  } else if (missing(with_levels) && !categorical_only) {
    with_levels <- FALSE
  }

  if (categorical_only) {
    query <- filter(sctbl, .data$class == "categorical")
  } else {
    query <- sctbl
  }
  
  scols <- c("dataset", "sample_id", "variable", "value", "class", "type")
  if (do_semi) {
    query <- semi_join(query, samples, by = c("dataset", "sample_id"))
  }
  scols <- setdiff(scols, c("dataset", "sample_id"))
  if (!with_levels) scols <- setdiff(scols, "value")
  
  query <- select(query, all_of(scols))
  
  result <- distinct(query, .keep_all = TRUE)
  
  if (detailed) {
    # figure out distribution of real-valued covariates,
    # nlevels of categorical ones
    # number of samples each apply to?
  } else {
    # what to do?
  }
  
  collect(result, n = Inf)
}
