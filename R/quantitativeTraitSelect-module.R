#' Provides a widget to select quantitative traits from samples
#'
#' This module will pull quantitative traits from a `FacileDataStore`.
#'
#' We assume that there are two sources of quanitative traits:
#'
#' 1. From assays (usually, dense matrix types of things (rnaseq)); or
#' 2. A quantitative sample covariate (RIN score, for instance)
#'
#' When type is "assay", multiple entries can be used. In these cases these
#' entries might be treated as a geneset, or individual scores.
#'
#' Quantitative traits can only be selected one at a time. Further, let's assume
#' they are normalized for now,
quantitativeTraitSelect <- function(input, output, session, rfds, ...,
                                    .exclude = NULL, .reactive = TRUE) {

  assert_class(rfds, "ReactiveFacileDataStore")
  isolate. <- if (.reactive) base::identity else shiny::isolate

  # We keep track of state so that we don't refetch data when we don't have to.
  # Reactivitiy that is internal to this module should "react" to the state$xxx
  # variables. Facile modules that use this module should respond to the
  # list that is returned from here.
  #
  state <- reactiveValues(
    # If this is 'covariate': this is a pData-like column, and $covariate tells
    #                         us the columnn name of pData;
    # anything else is the name of the assay that the feature are pulled from.
    type = "__initializing__",

    # If type == "covariate", this is the column name of the `pData`; otherwise
    # selection to "assay" type will set this to NULL.
    covariate = "__initializing__",

    # If type == 'assay', this is a tibble of feature_info rows that are
    # selected. If type == "covariate", this is set to NULL.
    features = "__initializing__",

    # matrix of feature-level data. rows are samples, columns are covariate.
    # If type == 'covariate', this is NULL. User should use the `value` column.
    data = "__initializing__",

    # vector of summarized data. If there are multiple genes in the columns
    # of `data`, this would be the sing-sample geneset score, for instance.
    # If covariate is from a real valued sample covariate, this is just a
    # the first column of `data`
    value = "__initializing__")

  if (!is.null(exclude) && !.reactive) {
    assert_tibble(exclude)
    assert_subset(names(exclude), c("variable", "value"))
  }


  active.samples <- reactive({
    req(isolate.(rfds[["active_samples"]]()))
  })

  assays <- reactive({
    isolate.(rfds[["active_assays"]]())
  })

  # Deal with quantitative sample-level covariates =============================
  sample.covariates <- reactive({
    covs <- req(isolate.(active_covariates(rfds)))
    out <- filter(covs, class == "real")

    if (!is.null(exclude)) {
      ex <- if (is(exclude, "reactive")) isolate.(exclude()) else exclude
      if (nrow(ex)) {
        out <- anti_join(out, ex, by = c("variable"))
      }
    }

    out <- out %>%
      group_by(variable, class) %>%
      summarize(n = n()) %>%
      ungroup()

    out
  })

  # Interact with the UI =======================================================

  # Update the assays available to select from over the samples
  observe({
    assays <- req(assays())
    selected <- intersect(default_assay(fds(rfds)), assays$assay)
    if (length(selected) == 0L) selected <- assays$assay[1L]
    updateSelectInput(session, "assay", choices = assays$assay,
                      selected = selected)
  })

  # Update the quantitative sample level covariates in the UI
  observe({
    covs <- sample.covariates()
    updateSelectInput(session, "covariates", choices = covs$variable,
                      selected = selected)
  })

  # Check user inputs against current state, and update them as reactive
  # values, which will returned form the  module.

  type <- reactive({
    type <- req(input$type)
    if (type != isolate(state$type)) {
      state$data <- NULL
      state$value <- NULL
      if (type == "covariate") {
        # reset assay-dependent values
        state$features <- NULL
      } else {
        state$covariate <- NULL
      }
    }
    state$type <- type
    type
  })

  is.assay <- reactive({
    !type() %in% c("covariate", "__initializing__")
  })

  covariate <- reactive({
    cov <- req(input$covariate)
    if (is.null(state$covariate) || cov != state$covariate) {
      state$covariate <- cov
    }
    cov
  })

  features.all <- reactive({
    .type <- state$covariate
    if (!is.null(.type) && .type == "assay") {
      f <- fds(rfds) %>%
        feature_info_tbl() %>%
        filter(feature_)
    }
  })

  data <- reactive({

  })

  value <- reactive({

  })

  vals <- list(
    type = type,
    covariate = covariate,
    features = features,
    data = data,
    value = value)

  return(vals)
}

quantitativeTraitSelectUI <- function(id, ...) {
  ns <- NS(id)

  tagList(
    fluidRow(
      column(
        8,
        selectInput(ns("type"), "Type:",
                    choices = c("assay", "covariate"),
                    selected = "assay")),
      column(
        4,
        conditionalPanel(
          condition = "input.type == 'assay'", # ns = ns,
          selectInput(ns("assay"), choices = NULL)))),

    # display for sample covariate
    conditionalPanel(
      condition = "input.type == 'covariate'", # ns = ns,
      fluidRow(12, selectInput(ns("covariates"), "Covariate", choices = NULL))),

    # display for assay feature selection
    conditionalPanel(
      condition = "input.type == 'assay'",     # ns = ns
      fluidRow(12, selectizeInput(ns("features"), choices = NULL)),
      fluidRow(12, tags$h4("FeatureSet Select"))))
}
