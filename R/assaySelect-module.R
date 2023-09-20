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
    
    observeEvent(rfds$active_assays(), {
      # aa <- active_assays(rfds)
      aa <- rfds$active_assays()
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
        state$assay_info <- rfds$active_assays() |> # active_assays(rfds) |>
          filter(.data$assay == selected)
      }
    })
    
    # This .selected() and selected() thing is weird, come back to fix it
    # one day.
    .selected <- reactive({
      if (unselected(state$assay_info)) "" else state$assay_info$assay
    })
    
    selected <- reactive({
      req(in_sync())
      .selected()
    })
    
    in_sync <- reactive({
      req(initialized(rfds))
      if (state$rfds_name != name(rfds)) {
        out <- FALSE
      } else {
        sel <- .selected()
        out <- !unselected(sel) && sel %in% rfds$active_assays()$assay
      }
      out
    })
    
    assay_info <- reactive({
      req(in_sync())
      rfds$active_assays() |>
        filter(.data$assay == selected())
    })
    
    features <- reactive({
      req(in_sync())
      req(!unselected(selected()))
      FacileData::features(rfds$fds(), assay_name = selected())
    })
    
    vals <- list(
      selected = selected,
      assay_names = assay_names,
      assay_info = assay_info,
      features = features,
      in_sync = in_sync,
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
  congruent <- x$selected() %in% x$assay_names()
  # when the underlying fds is swapped, sometimes the selected assay does not
  # match the available assays
  all(ready) && !unselected(x$selected()) && congruent
}

#' @noRd
#' @export
from_fds.AssaySelectModule <- function(x, rfds, ...) {
  .Deprecated(("Use module$in_sync() instead"))
  isolate({
    initialized(x) &&
      initialized(rfds) &&
      x$.state$rfds_name == name(rfds) &&
      x$selected() %in% rfds$active_assays()$assay
  })
}
