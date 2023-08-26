#' Dropdown widget to select assays over the set of active_samples.
#' 
#' @export
assaySelectServer <- function(id, rfds, ..., debug = FALSE) {
  assert_class(rfds, "ReactiveFacileDataStore")
  
  moduleServer(id, function(input, output, session) {
    state <- reactiveValues(
      rfds_name = "__initializing__",
      assay_names = "__initializing__",
      # this is the single-row of information for the selected assay
      assay_info = tibble(
        assay = "__initializing__",
        assay_type = "__initializing__",
        feature_type = "__initializing__",
        nfeatures = "__initializing__"))
    
    observeEvent(active_assays(rfds), {
      aa <- active_assays(rfds)
      update <- !setequal(state$assay_names, aa$assay)
      if (name(rfds) != state$rfds_name) {
        ftrace("Updating assaySelect due to new rfds")
        state$rfds_name <- name(rfds)
        update <- TRUE
      }
      
      if (update) {
        ftrace("Updating set of available assay (names): ",
               paste(aa$assay, collapse = ","))
        state$assay_names <- aa$assay
      }
    })
    
    assay_names <- reactive({
      if (unselected(state$assay_names)) character() else state$assay_names
    })
    assay_count <- reactive({
      length(assay_names())
    })
    
    # Update the assaySelectInput (and its selection) in response to a change
    # in the underlying samples. Note that when the samples shift, the assay
    # that was previously selected can dissaper.
    observeEvent(assay_names(), {
      anames <- assay_names()
      ainfo <- state$assay_info
      
      update <- FALSE

      if (assay_count() == 0L) {
        fwarn("{bold}{red}No assays available for current active_samples{reset}")
        selected <- NULL
        update <- nrow(ainfo) != 0L
      } else {
        update <- TRUE
        if (ainfo$assay %in% anames) {
          selected <- ainfo$assay
        } else {
          selected <- anames[1L]
        }
      }
      
      if (update) {
        ftrace("A change in available assays (assays()$assay) changes ",
               "{red}{bold}assay_info state{reset} variable to: {bold}",
               selected, "{reset}")

        updateSelectInput(session, "assay", choices = anames, 
                          selected = selected)
      }
    })
    
    observeEvent(input$assay, {
      selected <- input$assay
      if (!unselected(selected)) {
        ftrace("Updating selected assay: ", selected)
        state$assay_info <- active_assays(rfds) |> 
          filter(.data$assay == selected)
      }
    })
    
    assay_name <- reactive({
      if (unselected(state$assay_info)) "" else state$assay_info$assay
    })
    
    assay_info <- reactive({
      active_assays(rfds) |> 
        filter(.data$assay == assay_name())
    })
    
    features <- reactive({
      req(!unselected(assay_name()))
      FacileData::features(rfds$fds(), assay_name = assay_name())
    })
    
    vals <- list(
      assay_name = assay_name,
      assay_names = assay_names,
      assay_info = assay_info,
      features = features,
      .state = state,
      .ns = session$ns)
    class(vals) <- c("AssaySelectModule", "Labeled")
    vals
  })
}

#' @noRd
#' @export
assaySelectInput <- function(id, label = "Assay", choices = NULL, selected = NULL,
                             multiple = FALSE, selectize = TRUE,
                             width = NULL, size = NULL, ...) {
  ns <- NS(id)
  selectInput(ns("assay"), label = label,
              choices = choices, selected = selected, multiple = multiple,
              selectize = selectize, width = width, size = size)
}


#' @noRd
#' @export
initialized.AssaySelectModule <- function(x, ...) {
  check <- c("rfds_name", "assay_names", "assay_info")
  ready <- sapply(check, \(s) !unselected(x$.state[[s]]))
  congruent <- x$assay_name() %in% x$assay_names()
  # when the underlying fds is swapped, sometimes the selected assay does not
  # match the available assays
  all(ready) && !unselected(x$assay_name()) && congruent
}

#' @noRd
#' @export
from_fds.AssaySelectModule <- function(x, rfds, ...) {
  isolate({
    initialized(x) &&
      initialized(rfds) &&
      x$.state$rfds_name == name(rfds) &&
      x$assay_name() %in% active_assays(rfds)$assay
  })
}
