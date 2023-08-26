# Selector for levels ----------------------------------------------------------

#' Use this with categoricalSampleCovariateSelect to enumerate its levels.
#'
#' @export
#' @rdname categoricalSampleCovariateLevels
#' @param covaraite the `categoricalSampleCovariateSelect` module.
#' @param missing_sentinel This is a reactive (string). When it's NULL, no
#'   missing sentinel is added. The parent covariate selector can pass in a
#'   value here to show to indicate a level that's not included in the
#'   covariate's level.
#' @importFrom shiny updateSelectizeInput
categoricalSampleCovariateLevelsSelectServer <- function(
    id, covariate, ..., missing_sentinel = NULL, exclude = reactive(NULL), 
    debug = FALSE) {
  assert_class(covariate, "CategoricalCovariateSelectModule")
  
  if (!is.null(missing_sentinel)) {
    # This should be a reactive string
    if (!is(missing_sentinel, "reactive")) {
      fwarn("missing_sentinal is not a reactive")
    }
  }
  
  moduleServer(id, function(input, output, session) {
    state <- reactiveValues(
      values = "__initializing__",
      levels = character(),
      exclude = character())
    
    observeEvent(exclude(), {
      ignore <- exclude()
      if (unselected(ignore)) ignore <- character()
      if (!is.character(ignore)) {
        ftrace("{reset}{red}illegal type of variable passed to exclude: ", 
               class(ignore)[1] ,"{reset}")
        ignore <- character()
      }
      if (!setequal(ignore, state$exclude)) {
        state$exclude <- ignore
      }
    })
    
    excluded <- reactive({
      ftrace("state||exclude has been updated: ", state$exclude)
      state$exclude
    })
    
    
    observeEvent({
      covariate$levels()
      excluded()
    } , {
      req(covariate$in_sync())
      levels. <- covariate$levels()
      if (!is.null(missing_sentinel)) {
        levels. <- unique(c(levels., missing_sentinel()))
      }
      
      ignore <- excluded()
      newlevels <- setdiff(levels., ignore)
      if (!setequal(state$levels, newlevels)) {
        ftrace("Updating levels from (", state$levels, "), to (",
               newlevels, ")")
        state$levels <- newlevels
      }
    })
    
    levels <- reactive({
      state$levels
    })
    
    observeEvent(levels(), {
      levels. <- levels()
      selected. <- input$values
      if (unselected(selected.)) selected. <- ""
      
      overlap. <- intersect(selected., levels.)
      if (unselected(overlap.)) overlap. <- ""
      if (!isTRUE(setequal(overlap., state$values))) {
        ftrace("change in availavble levels (",
               paste(levels., collapse = ","),
               ") updates selected level to: `", overlap., "`")
        state$values <- overlap.
      }
      updateSelectizeInput(session, "values", choices = levels.,
                           selected = overlap., server = TRUE)
    })
    
    observeEvent(input$values, {
      selected. <- input$values
      # Note that when ignoreNULL and all selected levels are removed from
      # covariateSelectLevels, the values are released "back to the pool".
      if (unselected(selected.)) {
        selected. <- ""
      }
      if (!isTRUE(setequal(selected., state$values))) {
        ftrace("Change of selected input$values changes internal state from ",
               "`", isolate(state$values), "` ",
               "to {bold}{magenta}`", selected., "`{reset}")
        # logical covariates are stored as 0/1 when retrieved out of SQLite
        # database, let's convert T/F to 0/1 here, too
        csummary <- covariate$summary()
        if (nrow(csummary) > 0L && csummary[["class"]][1L] == "logical") {
          selected. <- ifelse(selected. == "TRUE", "1", selected.)
          selected. <- ifelse(selected. == "FALSE", "0", selected.)
        }
        state$values <- selected.
      }
    }, ignoreNULL = FALSE)
    
    values <- reactive({
      # before we return the levels, let's make sure they are still valid
      # levels of the covariate and the underlying sampe-space hasn't moved
      # from under us in this reactivity cycle
      if (!unselected(state$values)) {
        req(all(state$values %in% levels()))
        req(all(state$values %in% covariate$levels()))
      }
      state$values
    })
    
    if (debug) {
      output$selected <- renderText(values())
    }
    
    vals <- list(
      values = values,
      levels = levels,
      excluded = excluded,
      covariate = covariate,
      .state = state,
      .ns = session$ns)
    class(vals) <- "CategoricalCovariateSelectLevels"
    return(vals)
  })
}

#' @noRd
#' @export
#' @rdname categoricalSampleCovariateLevels
#' @importFrom shiny NS selectizeInput tagList textOutput
categoricalSampleCovariateLevelsSelectInput <- function(
    id, label = NULL, choices = NULL, selected = NULL, 
    multiple = FALSE, width = NULL, size = NULL,
    options = NULL, ..., debug = FALSE) {
  ns <- NS(id)
  
  out <- tagList(
    selectizeInput(ns("values"), label = label, choices = choices,
                   selected = selected, multiple = multiple, width = width,
                   size = size, options = options))
  if (debug) {
    out <- tagList(
      out,
      shiny::tags$p("Selected levels:", textOutput(ns("selected"))))
  }
  
  out
}
