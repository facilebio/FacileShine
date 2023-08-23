
#' A module to configure batch correction parameters for assay data.
#'
#' This module enables the user to select covariates that should be regressed
#' out in the data. At the same time a covariate can be selected that indicates
#' structure in the data to preserve.
#'
#' @export
#' @rdname batchCorrectConfig
#' @examples
#' \dontrun{
#' # ui code
#' batchCorrectConfigUI(ns("batch"), direction = "horizontal")
#'
#' # server code
#' batch <- callModule(batchCorrectConfig, "batch", rfds)
#' dat <- reactive({
#'   these_samples() |>
#'     with_assay_data(these_features(), normalized = TRUE,
#'                     batch = name(batch$batch),
#'                     main = name(batch$main))
#' })
#' }
batchCorrectConfig <- function(input, output, session, rfds,
                                    color = FALSE, facet = FALSE, group = FALSE,
                                    hover = FALSE, shape = FALSE, ...,
                                    with_none = TRUE, exclude = NULL,
                                    .reactive = TRUE) {
  state <- reactiveValues(
    # Do we want to put the batch/main stuff in here?
  )

  # the "batch" covariate is what I'm calling the extra/batch-level
  # covariates. the entry selected in the testcov is removed from the
  # available elemetns to select from here
  batch <- callModule(categoricalSampleCovariateSelect, "batchcov",
                      rfds, include1 = FALSE, ..., with_none = FALSE,
                      exclude = NULL, reactive = .reactive,
                      ignoreNULL = FALSE)

  main <- callModule(categoricalSampleCovariateSelect, "batchmain",
                     rfds, include1 = FALSE, ..., with_none = TRUE,
                     exclude = batch$covariate, reactive = .reactive)

  vals <- list(
    batch = batch,
    main = main,
    .state = state,
    .ns = session$ns)
  class(vals) <- c("BatchCorrectConfig")
  vals
}

#' @noRd
#' @export
#' @importFrom shiny fluidRow NS tagList
batchCorrectConfigUI <- function(id, direction = c("horizontal", "vertical"),
                                 ...) {
  direction <- match.arg(direction)
  ns <- NS(id)
  batch <- categoricalSampleCovariateSelectUI(
    ns("batchcov"),
    label = "Batch Correct",
    multiple = TRUE)
  main <- categoricalSampleCovariateSelectUI(
    ns("batchmain"),
    label = "Batch Preserve",
    multiple = FALSE)

  if (direction == "horizontal") {
    out <- fluidRow(column(6, batch), column(6, main))
  } else {
    out <- tagList(batch, main)
  }

  out
}
