#' Module server for boxplots
#' 
#' @export
fboxPlotServer <- function(id, rfds, ..., gdb = NULL, x = NULL, y = NULL,
                           event_source = session$ns("selection"),
                           .reactive = TRUE, debug = FALSE) {
  assert_class(rfds, "ReactiveFacileDataStore")
  shiny::moduleServer(id, function(input, output, session) {
    shiny::callModule(facileBoxPlot, id, rfds, gdb = gdb, x = x, y = y,
                      event_source = event_source, .reactive = .reactive,
                      debug = debug)
  })
}

#' @noRd
#' @export
fboxPlotUI <- function(id, ..., debug = FALSE) {
  facileBoxPlotUI(id, ..., debug = FALSE)
}
