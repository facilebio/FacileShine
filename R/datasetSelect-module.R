#' @noRd
#' @export
#' @family workbench functions
#' @importFrom shiny
#'   updateSelectizeInput
datasetSelect <- function(input, output, session, config,
                          default_dataset = NULL, user = Sys.getenv("USER"),
                          with_upload = TRUE, ...) {
  stop("Don't use this right now ...")
  rconfig <- reactive({
    rc <- if (is(config, "reactive")) config() else config
    load_config(rc)
  })

  datastores <- reactive({
    rc <- rconfig()
    datastores_info(rc)
  })

  observeEvent(datastores(), {
    dstores <- datastores()
    choices <- datastores_info(rconfig(), as_selectize_list = TRUE)
    if (with_upload) {
      choices <- c(
        list(`Upload Dataset` = c("Custom Dataset" = "upload")),
        choices)
    }
    if (test_string(default_dataset)) {
      sel <- if (default_dataset %in% dstores$key) default_dataset else NULL
    } else {
      sel <- NULL
    }

    updateSelectizeInput(session, "dataset", choices = choices, server = TRUE,
                         selected = sel)
  })

  fdskey <- reactive({
    key. <- req(input$dataset)
    key.
  })

  fdsinfo <- reactive({
    key. <- req(fdskey())
    info <- filter(datastores(), key == key.)
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
    datastores = datastores)

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
    selectizeInput(ns("dataset"), label = "Datasets",
                   choices = choices, selected = selected))
  out
}
