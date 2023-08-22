#' A module to pull out features from an assay with optional GeneSetDb support.
#' 
#' @export
assayFeatureSelectServer <- function(id, rfds, gdb = reactive(NULL), ...,
                                     exclude = NULL, debug = FALSE) {
  assert_class(rfds, "ReactiveFacileDataStore")
  moduleServer(id, function(input, output, session) {
    state <- reactiveValues(
      rfds_name = "__initializing__",
      selected = .no_features(),
      # "labeled" API
      name = "__initializing__",
      label = "__initializing__")
    
    assay <- assaySelectServer("assay", rfds, debug = FALSE, ...)
    
    features_all <- eventReactive(assay$assay_name(), {
      req(from_fds(assay, rfds))
      if (name(rfds) != state$rfds_name) {
        state$rfds_name <- name(rfds)
      }
      rfds$fds() |> 
        features(assay_name = assay$assay_name())
    })

    observeEvent(features_all(), {
      shiny::updateSelectizeInput(
        session, "features", 
        choices = setNames(features_all()$feature_id, features_all()$name),
        selected = NULL, server = TRUE)
    })
    
    observeEvent(input$features, {
      req(nrow(features_all()))
      ifeatures <- input$features
      if (unselected(ifeatures)) {
        out <- .no_features()
      } else {
        out <- filter(features_all(), .data$feature_id %in% ifeatures)
      }
      
      if (!setequal(out$feature_id, state$selected$feature_id)) {
        ftrace("updating selected features: ", 
               paste(out$feature_id, collapse = ","))
        state$selected <- out
      }
    }, ignoreNULL = FALSE)
    
    selected <- reactive(state$selected)
    
    # ................................................................... genesets
    observe({
      # Only show the UI element if a GeneSetDb was passed in.
      shinyjs::toggleElement("genesetbox", condition = !is.null(gdb()))
    })
    
    geneset <- callModule(
      sparrow.shiny::reactiveGeneSetSelect, "geneset", gdb, ...)
    observeEvent(geneset$membership(), {
      req(initialized(assay))
      req(nrow(features_all()))
      
      gfeatures <- geneset$membership()
      out <- semi_join(features_all(), gfeatures, by = "feature_id")
      
      if (!setequal(out$feature_id, state$selected$feature_id)) {
        # state$selected <- out
        choices <- setNames(features_all()$feature_id, features_all()$name)
        updateSelectizeInput(session, "features", selected = out$feature_id,
                             choices = choices, server = TRUE)
      }
    }, ignoreNULL = TRUE)
    
    vals <- list(
      selected = selected,
      features_all = features_all,
      assay = assay,
      .state = state,
      .ns = session$ns)
    class(vals) <- c("AssayFeatureSelectModule", "FacileDataAPI", "Labeled")    
    vals
  })
}

#' @noRd
#' @export
initialized.AssayFeatureSelectModule <- function(x, ...) {
  check <- c("assay_names", "assay_info")
  ready <- sapply(check, \(s) !unselected(x$.state[[s]]))
  initialized(x$assay) && all(ready) && is(x$features_all(), "tbl")
}

#' @noRd
#' @export
from_fds.AssayFeatureSelectModule <- function(x, rfds, ...) {
  if (!from_fds(x$assay, rfds)) return(FALSE)
  isolate(x[[".state"]]$rfds_name == name(rfds))
}

#' @export
#' @importFrom shiny selectInput selectizeInput
#' @rdname assayFeatureSelect
#' @param multiple,... passed into the `"features"` `selectizeInput`
assayFeatureSelectInput <- function(id, label = NULL, multiple = TRUE, ...) {
  ns <- NS(id)
  
  if (is.null(label)) {
    assay.style <- ""
  } else {
    assay.style <- "padding-top: 1.7em"
  }
  
  out <- tagList(
    fluidRow(
      column(9, selectizeInput(ns("features"), label = label, choices = NULL,
                               multiple = multiple)),
      column(
        3,
        tags$div(style = assay.style,
                 assaySelectInput(ns("assay"), label = NULL, choices = NULL)))),
    shinyjs::hidden(
      tags$div(
        id = ns("genesetbox"),
        sparrow.shiny::reactiveGeneSetSelectUI(ns("geneset"))))
  )
  
  out
}

#' assayFeatureSelect with genesetdb support
#'
#' @export
#' @noRd
#' @importFrom shiny
#'   callModule
#'   isolate
#'   observe
#'   reactive
#'   reactiveValues
#'   updateSelectizeInput
#' @rdname assayFeatureSelect
#' @return a list with the following elements:
#'   * `assay_info`: one row assay,assay_type,feature_type tibble
#'   * `features`: n-row feature_info_tbl() like tbl enumerating the assay
#'     features selected in this module
#'   * `features_all`: a tibble of all of the fdatures of this `feature_type`
#'     (we can possible axe this, but ...)
#'  * `label`: a "human readable" summary of the features selected within this
#'     module
#'  * `name`: a "computerfriendly" version of `label`
assayFeatureSelect2 <- function(input, output, session, rfds, 
                                gdb = shiny::reactive(NULL), ...,
                                .exclude = NULL, .reactive = TRUE) {
  assert_class(rfds, "ReactiveFacileDataStore")

  isolate. <- if (.reactive) base::identity else shiny::isolate

  state <- reactiveValues(
    selected = .no_features(),
    # "labeled" API
    name = "__initializing__",
    label = "__initializing__")


  if (!is.null(.exclude)) {
    # TODO: Are we excluding assays altogether, features from assays, or both?
  }

  assay_select <- callModule(assaySelect, "assay", rfds, .reactive = .reactive)

  # Update assayFeatureSelect with feature universe from assay_select
  observe({
    universe <- assay_select$features()
    choices <- setNames(universe[["feature_id"]], universe[["name"]])
    updateSelectizeInput(session, "features", choices = choices,
                         selected = NULL, server = TRUE)
  })

  observeEvent(input$features, {
    req(initialized(rfds))
    ifeatures <- input$features
    if (unselected(ifeatures)) {
      out <- .no_features()
    } else {
      universe <- assay_select$features()
      out <- filter(universe, .data$feature_id %in% ifeatures)
    }

    if (!setequal(out$feature_id, state$selected$feature_id)) {
      state$selected <- out
    }
  }, ignoreNULL = FALSE)

  selected <- reactive(state$selected)

  # ................................................................... genesets
  observe({
    # Only show the UI element if a GeneSetDb was passed in.
    shinyjs::toggleElement("genesetbox", condition = !is.null(gdb()))
  })

  geneset <- callModule(
    sparrow.shiny::reactiveGeneSetSelect, "geneset", gdb, ...)
  observeEvent(geneset$membership(), {
    gfeatures <- geneset$membership()
    universe <- assay_select$features()
    out <- semi_join(universe, gfeatures, by = "feature_id")

    if (!setequal(out$feature_id, state$selected$feature_id)) {
      # state$selected <- out
      choices <- setNames(universe[["feature_id"]], universe[["name"]])
      updateSelectizeInput(session, "features", selected = out$feature_id,
                           choices = choices, server = TRUE)
    }
  }, ignoreNULL = TRUE)

  vals <- list(
    selected = selected,
    assay_info = assay_select$result,
    .state = state,
    .ns = session$ns)
  class(vals) <- c("AssayFeatureSelect", "FacileDataAPI", "Labeled")

  vals
}

#' @export
#' @importFrom shiny selectInput selectizeInput
#' @rdname assayFeatureSelect
#' @param multiple,... passed into the `"features"` `selectizeInput`
assayFeatureSelect2UI <- function(id, label = NULL, multiple = TRUE, ...) {
  ns <- NS(id)

  if (is.null(label)) {
    assay.style <- ""
  } else {
    assay.style <- "padding-top: 1.7em"
  }

  out <- tagList(
    fluidRow(
      column(9, selectizeInput(ns("features"), label = label, choices = NULL,
                               multiple = multiple, ...)),
      column(
        3,
        tags$div(style = assay.style,
                 assaySelectUI(ns("assay"), label = NULL, choices = NULL)))),
    shinyjs::hidden(
      tags$div(
      id = ns("genesetbox"),
      sparrow.shiny::reactiveGeneSetSelectUI(ns("geneset"))))
  )

  out
}

# Labeled API ==================================================================

#' @noRd
#' @export
name.AssayFeatureSelect <- function(x, ...) {
  xf <- x[["selected"]]()
  out <- if (nrow(xf) == 0) {
    "nothing"
  } else if (nrow(xf) == 1) {
    xf$name
  } else {
    "score"
  }
  make.names(x[[".ns"]](out))
}

#' @noRd
#' @export
label.AssayFeatureSelect <- function(x, ...) {
  xf <- x[["selected"]]()
  if (nrow(xf) == 0) {
    "nothing"
  } else if (nrow(xf) == 1) {
    xf$name
  } else if (nrow(xf) < 10) {
    paste(xf$name, collapse = ",")
  } else {
    "score"
  }
}

# Random =======================================================================
.no_features <- function() {
  tibble(
    assay = character(),
    feature_id = character(),
    name = character())
}
