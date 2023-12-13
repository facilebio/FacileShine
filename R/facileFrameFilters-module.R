#' Dynamic facile_frame filters that leverage the power of a lazy facile_frame
#' 
#' @export
#' @param id the id of the module
#' @param x a reactive(facile_frame)
#' @param vars the names of the columns (sample_covariates)
facileFrameFilterServer <- function(id, x, ..., vars = reactive(NULL)) {
  shiny::moduleServer(id, function(id, output, session) {
    var_summary <- reactive({
      fsamples <- x()
      fvars <- vars()
      if (is.null(fvars)) {
        
      }
    })
  })
}

facileFrameFilterUI <- function(id, ..., show_nrow = TRUE, max_height = NULL, 
                                nrow_label = "Number of rows:", debug = FALSE) {
  ns <- shiny::NS(id)
  if (!is.null(max_height)) {
    max_height <- paste0(
      "overflow-y: auto; overflow-x: hidden; max-height:", 
      htmltools::validateCssUnit(max_height), ";")
  }
  htmltools::tagList(
    htmltools::singleton(
      htmltools::tags$style(
        ".selectize-big .selectize-input {height: 72px; overflow-y: scroll;}"
      )
    ),
    if (isTRUE(show_nrow)) {
      htmltools::tags$span(
        nrow_label,
        shiny::uiOutput(outputId = ns("nrow"), inline = TRUE))
    },
    shiny::uiOutput(outputId = ns("placeholder_filters"), style = max_height)
  )
}