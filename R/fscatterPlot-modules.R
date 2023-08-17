#' Module server for scatter plots
#' 
#' @export
fscatterPlotServer <- function(id, rfds, ..., gdb = shiny::reactive(NULL),
                               ndim = 3, x = NULL, y = NULL, z = NULL,
                               event_source = session$ns("selection"),
                               .reactive = TRUE, debug = FALSE) {
  assert_class(rfds, "ReactiveFacileDataStore")
  shiny::moduleServer(id, function(input, output, session) {
    shiny::callModule(facileScatterPlot, id, rfds, gdb = gdb, x = x, y = y,
                      z = z, event_source = event_source, .reactive = .reactive,
                      debug = debug)
  })
}

#' @noRd
#' @export
fscatterPlotUI <- function(id, ..., with_download = TRUE, debug = FALSE) {
  facileScatterPlotUI(id, ..., with_download = with_download, debug = FALSE)
}
