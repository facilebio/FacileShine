#' @noRd
#' @export
#' @family workbench functions
#' @importFrom shiny
#'   updateSelectizeInput
datasetSelect <- function(input, output, session, config = NULL,
                          default_dataset = NULL,
                          user = Sys.getenv("USER"), with_upload = TRUE, ...) {
  config <- load_config(config)
  datasets <- datastores_info(config)
  choices <- datastores_info(config, as_selectize_list = TRUE)

  if (with_upload) {
    choices <- c(
      list(`Upload Dataset` = c("Custom Dataset" = "upload")),
      choices)
  }

  # updateSelectizeInput(session, "dataset", choices = choices, server = TRUE,
  #                      selected = default_dataset)

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
    path <- req(fdsinfo()) %>%
      pull(path)
    req(dir.exists(path))
    path
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
datasetSelectUI <- function(id, choices = NULL, selected = NULL, ...) {
  ns <- NS(id)
  out <- tagList(
    selectizeInput(ns("dataset"), label = "Datasets", choices = choices,
                   selected = selected))
  out
}
