#' @export
#' @rdname facileSampleFilter
#' @importFrom shiny updateSelectInput
facileSampleFilter <- function(input, output, session, rfds, ...) {
  assert_class(rfds, "ReactiveFacileDataStore")

  categorical <- reactive({
    .samples <- req(rfds$active_samples())
    rfds$fds %>%
      fetch_sample_covariates(.samples, custom_key = rfds$user) %>%
      filter(class == "categorical")
  })

  cov.values <- reactive({
    ac <- req(categorical())
    count(ac, variable, value)
  })

  can.filter <- reactive({
    cv <- req(cov.values())
    cv %>%
      filter(n > 1) %>%
      distinct(variable) %>%
      pull(variable)
  })

  current.covariate <- reactive(input$covariate)
  current.values <- reactive(input$values)

  observe({
    updateSelectInput(session, "covariate", choices = can.filter())
  })

  observe({
    .covariate <- req(current.covariate())
    cv <- req(cov.values())
    .choices <- cv %>%
      filter(variable == .covariate) %>%
      pull(value)
    updateSelectizeInput(session, "values", choices = .choices,
                         server = TRUE, selected = NULL)
  })


  observe({
    covariate <- req(current.covariate())
    lvls <- req(current.values())

    .samples <- req(rfds$active_samples()) %>% collect(n = Inf)

    if (length(lvls)) {
      ac <- categorical()
      keep <- ac[["variable"]] == covariate & ac[["value"]] %in% lvls
      .samples <- distinct(ac[keep,], dataset, sample_id)
    }

    update_reactive_samples(rfds, .samples)
  })

  vals <- list(covariate = current.covariate, values = current.values)
}

#' @export
#' @rdname facileSampleFilter
#' @importFrom shiny selectInput selectizeInput
facileSampleFilterUI <- function(id, ...) {
  ns <- NS(id)

  tagList(
    selectInput(ns("covariate"), label = "Covariate", choices = NULL),
    selectizeInput(ns("values"), label = "Value(s)", choices = NULL,
                   multiple = TRUE))
}
