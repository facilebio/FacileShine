#' @noRd
#' @export
#' @family workbench functions
#' @importFrom shiny
#'   updateSelectizeInput
datasetSelect <- function(input, output, session, config = NULL,
                          user = Sys.getenv("USER"), with_upload = TRUE, ...) {
  config <- load_config(config)
  datasets <- datastores_info(config)
  choices <- datastores_info(config, as_selectize_list = TRUE)

  if (with_upload) {
    choices <- c(
      list(`Upload Dataset` = c("Custom Dataset" = "upload")),
      choices)
  }

  updateSelectizeInput(session, "dataset", choices = choices, server = TRUE,
                       selected = "dnli_mouse")

  fdskey <- reactive({
    key. <- req(input$dataset)
    key.
  })

  fdsinfo <- reactive({
    key. <- req(fdskey())
    info <- filter(datasets, key == key.)
    req(nrow(info) == 1L)
    info
  })

  fdspath <- reactive({
    req(fdsinfo()) %>%
      pull(path)
  })

  vals <- list(
    selected = fdskey,
    info = fdsinfo,
    path = fdspath,
    datasets = datasets)

  return(vals)
}

#' @noRd
#' @export
#' @importFrom shiny
#'   NS
#'   selectizeInput
#'   tagList
#'   wellPanel
datasetSelectUI <- function(id, ...) {
  ns <- NS(id)
  out <- tagList(
    selectizeInput(ns("dataset"), label = "Datasets", choices = NULL))
  out
}