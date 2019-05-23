#' Provides customizable widget bar to map data to aesthetics.
#'
#' A widget-bar that provides selectize dropdowns that list the categorical
#' covariates available for the current set of `active_samples(rfds)` and can be
#' assocaited to color, facet, group, hover, and shape aesthetics.
#'
#' @section Response to Cohott Selection:
#' Due to the possibility that the set of underlying samples (and, therefore,
#' covariates) can change from "underneath" the user's feet, based on cohort
#' selection, the categorical aesthetic maps are "reactive" at a higher priority
#' (10) than the default (0). If some aesthetics are mapped to covariates that
#' are "removed from play" due to narrowhing of a cohort, the values for these
#' aesthetic will be set to "---" before any default reactivity fires. This
#' should prevent some of the crashing behavior that may happen when decorating
#' data in downstream modules using the following pattern:
#'
#' ```r
#' aes <- callModule(categoricalAestheticMap, "aes", rfds,
#'                   color = TRUE, facet = TRUE, hover = TRUE,
#'                   ..., .reactive = .reactive)
#' dat <- reactive({
#'   # ... retrieve `core.data` from somewhere
#'   aes.covs <- aes$map()
#'   with_sample_covariates(core.data, aes$map())
#'
#' })
#' ```
#'
#' @export
#' @rdname categoricalAestheticMap
categoricalAestheticMap <- function(input, output, session, rfds,
                                    color = FALSE, facet = FALSE, group = FALSE,
                                    hover = FALSE, shape = FALSE, ...,
                                    .with_none = TRUE, .exclude = NULL,
                                    .reactive = TRUE) {
  assert_class(rfds, "ReactiveFacileDataStore")
  state <- reactiveValues(
    color = "",
    facet = "",
    group = "",
    hover = "",
    shape = "")

  # Not using categoricalSampleCovariateSelect N times, because trying to be
  # more efficient. Basically the different aesthetics can just select from
  # the same set of covariates, so lets just pull that down ones.

  if (!is.null(.exclude) && !.reactive) {
    assert_tibble(.exclude)
    assert_subset(names(.exclude), c("variable", "value"))
  }

  isolate. <- if (.reactive) base::identity else shiny::isolate

  active.samples <- reactive({
    req(initialized(rfds))
    isolate.(active_samples(rfds))
  })

  active.covariates <- reactive({
    req(initialized(rfds))
    all.covs <- isolate.(active_covariates(rfds))
    cat.covs <- filter(all.covs, class == "categorical")
    cat.covs
  })

  # Updating the covariate select dropdown is a little tricky because we want
  # to support the situation where the current active.covariates change in
  # response to the current set of active_samples changing.
  #
  # For now we just try to keep the same covariate selected if it still remains
  # in the ones that are available after the underlying set of active_samples
  # and nactive_covariates shifts.
  observe({
    choices <- req(active.covariates()) %>%
      filter(nlevels > 1) %>%
      pull(variable)

    ftrace("Updating available covariates for aesthetic  map")

    choices <- sort(choices)
    if (.with_none) {
      choices <- c("---", choices)
    }

    for (aes. in names(state)) {
      include <- get(aes.)
      if (include) {
        selected <- isolate(input[[aes.]])
        if (unselected(selected)) selected <- ""
        overlap <- intersect(selected, choices)
        if (length(overlap)) {
          if (!setequal(state[[aes.]], overlap)) state[[aes.]] <- overlap
          selected <- overlap
        } else {
          selected <- NULL
          state[[aes.]] <- ""
        }
        updateSelectInput(session, aes., choices = choices,
                          selected = selected)
      }
    }
  }, priority = 10)

  observeEvent(input$color, {
    if (!color) return(invisible(NULL))
    selected <- input$color
    # req(!is.null(selected)) # NULLflash (prophelactic: no error thrown yet)
    if (unselected(selected)) selected <- ""
    if (!is.null(selected) && !setequal(selected, state$color)) {
      state$color <- selected
    }
  })

  observeEvent(input$facet, {
    if (!facet) return(invisible(NULL))
    selected <- input$facet
    if (unselected(selected)) selected <- ""
    if (!is.null(selected) && !setequal(selected, state$facet)) {
      state$facet <- selected
    }
  })

  observeEvent(input$group, {
    if (!group) return(invisible(NULL))
    selected <- input$group
    if (unselected(selected)) selected <- ""
    if (!is.null(selected) && !setequal(selected, state$group)) {
      state$group <- selected
    }
  })

  observeEvent(input$hover, {
    if (!hover) return(invisible(NULL))
    selected <- input$hover
    if (unselected(selected)) selected <- ""
    if (!is.null(selected) && !setequal(selected, state$hover)) {
      state$hover <- selected
    }
  })

  observeEvent(input$shape, {
    if (!shape) return(invisible(NULL))
    selected <- input$shape
    if (unselected(selected)) selected <- ""
    if (!is.null(selected) && !setequal(selected, state$shape)) {
      state$shape <- selected
    }
  })

  # Character vector: names are the aesthetics, values are the covariate names
  aes.covs <- reactive({
    covs <- list(color = state$color, facet = state$facet,
                 group = state$group,
                 hover = state$hover,
                 shape = state$shape)
    covs[sapply(covs, function(val) all(nchar(val) > 0))]
  })

  vals <- list(
    map = aes.covs,
    .state = state,
    .ns = session$ns)
  class(vals) <- "CategoricalAesMap"
  vals
}

#' @section with_aesthetics:
#' **Experimental** We override the FacileViz::with_aesthetics method to make
#' programming with this module "more natural".
#'
#' Explicit coding might look like:
#'
#' ```r
#' aes <- callModule(categoricalAestheticMap, "aes", rfds,
#'                   color = TRUE, facet = TRUE, hover = TRUE,
#'                   ..., .reactive = .reactive)
#' dat <- reactive({
#'   # ... retrieve `core.data` from somewhere
#'   aes.covs <- aes$map()
#'   with_sample_covariates(core.data, aes$map())
#'
#' })
#' ```
#'
#' But it may look more natural to do something like:
#'
#' ```r
#' aes <- callModule(categoricalAestheticMap, "aes", rfds,
#'                   color = TRUE, facet = TRUE, hover = TRUE,
#'                   ..., .reactive = .reactive)
#' dat <- reactive({
#'   # ... retrieve `core.data` from somewhere
#'   # aes.covs <- aes$map()
#'   with_aesthetics(core.data, aes)
#' })
#' ```
#'
#' @export
#' @importFrom FacileViz with_aesthetics
#' @rdname categoricalAestheticMap
with_aesthetics.reactive_facile_frame <- function(dat, aes_mod, ...) {
  # This only works in a reactive context
  # req(nrow(rdat) > 0)
  if (missing(aes_mod) || !is(aes_mod, "CategoricalAesMap")) {
    return(NextMethod())
  }
  aes.map <- aes_mod$map()
  aes.covs <- setdiff(unlist(unname(aes.map)), colnames(dat))
  if (length(aes.covs)) {
    ftrace("retrieving aes covariates using categoricalAestheticMap Module")
    dat <- with_sample_covariates(dat, aes.covs)
  }
  dat
}

# UI ===========================================================================
#' @export
#' @importFrom tools toTitleCase
#' @importFrom shiny NS column fluidRow selectizeInput
#' @rdname categoricalAestheticMap
categoricalAestheticMapUI <- function(id, color = FALSE, facet = FALSE,
                                      group = FALSE, hover = FALSE,
                                      shape = FALSE, horizontal = TRUE, ...) {
  ns <- NS(id)

  mod.include <- .module_list(color, facet, group, hover, shape)
  ncol <- floor(12 / length(mod.include))

  aes.tags <- sapply(names(mod.include), function(aname) {
    label <- toTitleCase(aname)
    ui <- selectizeInput(ns(aname), label = aname, choices = NULL,
                         multiple = aname == "hover")
    if (horizontal) ui <- column(ncol, ui)
    ui
  }, simplify = FALSE)

  if (horizontal) {
    aes.tags <- fluidRow(aes.tags)
  }

  aes.tags
}

update_aes <- function(x, aesthethic, covariate, ...) {
  # TODO: enable callback/update of aesthetic map
}

#' @noRd
#'
# Helper Functions =============================================================
.module_list <- function(color = FALSE, facet = FALSE, group = FALSE,
                         hover = FALSE, shape = FALSE) {
  mod.include <- list(
    color = color, facet = facet, group = group, hover = hover, shape = shape)
  mod.include[unlist(mod.include)]
}
