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
    
    in_sync <- reactive({
      req(assay$in_sync())
    })
    
    features_all <- eventReactive(assay$selected(), {
      req(in_sync())
      features(rfds$fds(), assay_name = assay$selected())
    })

    observeEvent(features_all(), {
      shiny::updateSelectizeInput(
        session, "features", 
        choices = setNames(features_all()$feature_id, features_all()$name),
        selected = NULL, server = TRUE)
    })
    
    observeEvent(input$features, {
      fall <- features_all()
      req(nrow(fall))
      ifeatures <- input$features

      if (unselected(ifeatures)) {
        out <- .no_features()
      } else {
        fx <- tibble(assay = assay$selected(), feature_id = ifeatures)
        out <- semi_join(fall, fx, by = c("assay", "feature_id"))
      }
      
      update <- !setequal(
        paste(out$assay, out$feature_id),
        paste(state$selected$assay, state$selected$feature_id))

      if (update) {
        ftrace("updating selected features: ", 
               paste(out$feature_id, collapse = ","))
        state$selected <- out
      }
      
    }, ignoreNULL = FALSE)
    
    selected <- reactive({
      req(in_sync())
      semi_join(state$selected, features_all(), by = c("assay", "feature_id"))
    })
  
    # ................................................................. genesets
    observe({
      # Only show the UI element if a GeneSetDb was passed in.
      shinyjs::toggleElement("genesetbox", condition = !is.null(gdb()))
    })
    
    geneset <- callModule(
      sparrow.shiny::reactiveGeneSetSelect, "geneset", gdb, ...)
    
    # We tend to use the same genesets between transcriptomics and proteomics
    # (based on ensembl id). When users switch to proteomics, firing on
    # assay$selected() gives us a shot to update the geneset again, unless
    # the proteomics assay covers all of the same features as transcriptomics.
    observeEvent({ geneset$membership(); assay$selected() }, {
      in_sync()
      fall <- req(features_all())
      req(nrow(fall))
      
      gfeatures <- geneset$membership()
      out <- semi_join(fall, gfeatures, by = "feature_id")
      
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
      in_sync = in_sync,
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
  .Deprecated("Use x$in_sync() instead")
  if (!x$assay$in_sync()) return(FALSE)
  isolate(x[[".state"]]$rfds_name == name(rfds))
}

#' @export
#' @noRd
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
    shiny::fluidRow(
      shiny::column(
        width = 9,
        selectizeInput(ns("features"), label = label, choices = NULL,
                       multiple = multiple)),
      shiny::column(
        width = 3,
        shiny::tags$div(
          style = assay.style,
          assaySelectInput(ns("assay"), label = NULL, choices = NULL)))),
    shinyjs::hidden(
      shiny::tags$div(
        id = ns("genesetbox"),
        sparrow.shiny::reactiveGeneSetSelectUI(ns("geneset"))))
  )
  
  out
}

# Labeled API ==================================================================

#' @noRd
#' @export
name.AssayFeatureSelectModule <- function(x, ...) {
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
label.AssayFeatureSelectModule <- function(x, ...) {
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
