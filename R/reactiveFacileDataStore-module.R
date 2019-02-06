#' Represents an active FacileDataStore used in the shiny context
#'
#' In order to fully work, facile shiny modules simply require a FacileDataStore
#' and a sample descriptor that defines the "active" sampels under scrutiny.
#'
#' @export
#' @rdname reactiveFacileDataStore
#'
#' @importFrom shiny selectInput renderUI uiOutput
#' @param fds A (list of) FacileDataStore(s)
#' @param user a key to use for custom covariates
reactiveFacileDataStore <- function(input, output, session, fds,
                                    user = Sys.getenv("USER"), ...) {
  ns <- session$ns

  if (is(fds, "FacileDataStore")) {
    fds <- list(dataset = fds)
  }
  if (!is.list(fds) || !all(sapply(fds, is, "FacileDataStore"))) {
    stop("A list of FacileDataStore objects is required")
  }

  output$fdsSelectContainer <- renderUI({
    selectInput(ns("fdsSelect"), "Dataset", names(fds), selected = 1L)
  })

  rfds <- reactive({
    idx <- req(input$fdsSelect)
    fds[[idx]]
  })

  # returns a sample selector and the active covoariates over these samples
  # for the faciledatset
  ffilter <- callModule(filterFacileSamples, "sampleSelector", rfds, user, ...)

  active.covariates <- reactive({
    .fds <- req(rfds())
    .samples <- req(ffilter$active_samples())
    active_samples(.fds) <- .samples
    fetch_sample_covariates(.fds, .samples, custom_key = user)
  })

  output$debug_nsamples <- reactive({
    nrow(req(ffilter$active_samples()))
  })

  vals <- list(
    fds = rfds,
    active_samples = ffilter$active_samples,
    active_covariates = active.covariates)

  return(vals)
}

#' @export
#' @rdname reactiveFacileDataStore
reactiveFacileDataStoreUI <- function(id, ...) {
  ns <- NS(id)

  tagList(
    uiOutput(ns("fdsSelectContainer")),
    filterFacileSamplesUI(ns("sampleSelector")),
    textOutput(ns("debug_nsamples")))
}

# filterFacileSamples module ==================================================

#' @export
#' @rdname filterFacileSamples
#' @importFrom shiny updateSelectInput
filterFacileSamples <- function(input, output, session, rfds, user, ...) {

  active.samples <- reactive({
    .fds <- req(rfds())
    active_samples(.fds)
  })

  active.covariates <- reactive({
    .fds <- req(rfds())
    .samples <- req(active.samples())
    .fds %>%
      fetch_sample_covariates(.samples, custom_key = user) %>%
      filter(class == "categorical")
  })

  covariate.values <- reactive({
    ac <- req(active.covariates())
    count(ac, variable, value)
  })

  can.filter <- reactive({
    cv <- req(covariate.values())
    cv %>%
      filter(n > 1) %>%
      distinct(variable) %>%
      pull(variable)
  })

  observe({
    updateSelectInput(session, "covariate", choices = can.filter())
  })

  observe({
    .covariate <- req(input$covariate)
    cv <- isolate(req(covariate.values()))
    .choices <- cv %>%
      filter(variable == input$covariate) %>%
      pull(value)
    updateSelectizeInput(session, "values", choices = .choices,
                         server = TRUE, selected = NULL)
  })

  selected.samples <- reactive({
    lvls <- input$values
    .samples <- req(active.samples()) %>% collect(n = Inf)
    if (length(lvls)) {
      ac <- active.covariates()
      keep <- ac[["variable"]] == input$covariate & ac[["value"]] %in% lvls
      .samples <- distinct(ac[keep,], dataset, sample_id)
    }
    .samples
  })

  vals <- list(active_samples = selected.samples)
}

#' @export
#' @rdname filterFacileSamples
#' @importFrom shiny selectInput selectizeInput
filterFacileSamplesUI <- function(id, ...) {
  ns <- NS(id)

  tagList(
    selectInput(ns("covariate"), label = "Covariate", choices = NULL),
    selectizeInput(ns("values"), label = "Value(s)", choices = NULL,
                   multiple = TRUE))
}

update_reactive_covariates <- function(rfds, covariates, ...) {

}

if (FALSE) {
  shiny::shinyApp(
    ui = shiny::fluidPage(reactiveFacileDataStoreUI("rfdsSelect")),
    server = function(input, output) {
      fds <- exampleFacileDataSet()
      callModule(reactiveFacileDataStore, "rfdsSelect", fds, Sys.getenv("USER"))
    }
  )
}
