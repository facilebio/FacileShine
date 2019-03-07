#' Provides re-usable a widget bar to map data to aesthetics.
#'
#' @export
#' @rdname categoricalAestheticMap
categoricalAestheticMap <- function(input, output, session, rfds,
                                    color = TRUE, shape = TRUE, group = FALSE,
                                    facet = TRUE, ..., .with_none = TRUE,
                                    .exclude = NULL, .reactive = TRUE) {
  assert_class(rfds, "ReactiveFacileDataStore")
  isolate. <- if (.reactive) base::identity else shiny::isolate

  mod.include <- .module_list(color, shape, group, facet)

  modules <- sapply(names(mod.include), function(name) {
    callModule(categoricalSampleCovariateSelect, name, rfds, ...,
               .with_none = .with_none, .exclude = .exclude,
               .reactive = .reactive)
  }, simplify = FALSE)

  covariates <- reactive({
    out <- lapply(modules, function(mod) mod$covariate())
    out[!out %in% c("---", "")]
  })

  vals <- list(
    color = modules$color,
    shape = modules$shape,
    group = modules$group,
    facet = modules$facet,
    covariates = covariates,
    .ns = session$ns)
  class(vals) <- c("CategoricalAesMap")
  return(vals)
}

#' @export
#' @importFrom tools toTitleCase
#' @importFrom shiny NS column fluidRow
#' @rdname categoricalAestheticMap
categoricalAestheticMapUI <- function(id, color = TRUE, shape = TRUE,
                                      group = FALSE,  facet = TRUE,
                                      horizontal = TRUE, ...) {
  ns <- NS(id)

  mod.include <- .module_list(color, shape, group, facet)
  ncol <- floor(12 / length(mod.include))

  aes.tags <- sapply(names(mod.include), function(aname) {
    label <- toTitleCase(aname)
    ui <- categoricalSampleCovariateSelectUI(ns(aname), label = label)
    if (horizontal) ui <- column(ncol, ui)
    ui
  }, simplify = FALSE)

  if (horizontal) {
    aes.tags <- fluidRow(aes.tags)
  }

  aes.tags
}

available_aes <- function(x, ...) {
  assert_class(x, "CategoricalAesMap")
  aes.all <- sapply(x, )
}
update_aes <- function(x, aesthethic, covariate, ...) {
  # TODO: enable callback/update of aesthetic map
}

# Helper Functions =============================================================
.module_list <- function(color = TRUE, shape = TRUE, group = TRUE,
                         facet = TRUE) {
  mod.include <- list(
    color = color, shape = shape,
    group = group, facet = facet)
  mod.include[unlist(mod.include)]
}
