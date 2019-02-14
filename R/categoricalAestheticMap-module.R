categoricalAestheticMap <- function(input, output, session, rfds,
                                    color = TRUE, shape = TRUE, group = FALSE,
                                    facet = TRUE, ..., .with_none = TRUE,
                                    .exclude = NULL, .reactive = TRUE) {
  if (color) {
    color_select <- callModule(categoricalSampleCovariateSelect,
                               "color", rfds, ..., .with_none = .with_none,
                               .exclude = .exclude, .reactive = .reactive)
  } else {
    color_select <- NULL
  }

  if (shape) {
    shape_select <- callModule(categoricalSampleCovariateSelect,
                               "shape", rfds, ..., .with_none = .with_none,
                               .exclude = .exclude, .reactive = .reactive)
  } else {
    shape_select <- NULL
  }

  if (group) {
    group_select <- callModule(categoricalSampleCovariateSelect,
                               "group", rfds, ..., .with_none = .with_none,
                               .exclude = .exclude, .reactive = .reactive)
  } else {
    group_select <- NULL
  }

  if (facet) {
    facet_select <- callModule(categoricalSampleCovariateSelect,
                               "facet", rfds, ..., .with_none = .with_none,
                               .exclude = .exclude, .reactive = .reactive)
  } else {
    facet_select <- NULL
  }

  vals <- list(
    color = color_select,
    shape = shape_select,
    group = group_select,
    facet = facet_select)

  return(vals)
}

categoricalAestheticMapUI <- function(id, color = TRUE, shape = TRUE,
                                      group = FALSE,  facet = TRUE, ...) {
  ns <- NS(id)
  .selectUI <- categoricalSampleCovariateSelectUI
  tagList(
    if (color) .selectUI(ns("color"), "Color") else NULL,
    if (shape) .selectUI(ns("color"), "Shape") else NULL,
    if (group) .selectUI(ns("group"), "Group") else NULL,
    if (facet) .selectUI(ns("facet"), "Facet") else NULL)
}
