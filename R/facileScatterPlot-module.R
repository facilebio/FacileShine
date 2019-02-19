#' An interactive scatterplot module
#'
#' @export
#' @importFrom plotly renderPlotly
#' @rdname facileScatterPlot
facileScatterPlot <- function(input, output, session, rfds, ...,
                              event_source = session$ns("selection"),
                              .reactive = TRUE) {
  assert_class(rfds, "ReactiveFacileDataStore")
  isolate. <- if (.reactive) base::identity else shiny::isolate

  aes <- callModule(categoricalAestheticMap, "aes", rfds,
                    group = FALSE, ..., .reactive = .reactive)

  xaxis <- callModule(quantitativeAssayDataSelect, "xaxis", rfds)
  yaxis <- callModule(quantitativeAssayDataSelect, "yaxis", rfds)

  aes.covs <- reactive({
    aes$covariates()
  })

  qcolnames <- reactive({
    c(name(xaxis), name(yaxis))
  })

  rdat <- reactive({
    xf <- req(xaxis$features())
    yf <- req(yaxis$features())
    xs <- active_samples(rfds)
    covs <- unlist(aes.covs())

    out <- xs %>%
      with_assay_data(xf, aggregate.by = "ewm") %>%
      with_assay_data(yf, aggregate.by = "ewm") %>%
      collect(n = Inf)
    colnames(out)[3:4] <- qcolnames()

    if (length(covs)) {
      out <- with_sample_covariates(out, covs, .fds = fds(rfds))
    }

    out
  })

  fscatter <- reactive({
    dat <- req(rdat())
    axes <- qcolnames()
    .aes <- aes.covs()

    fscatterplot(dat, axes, color_aes = .aes$color, shape_aes = .aes$shape,
                 facet_aes = .aes$facet,
                 xlabel = label(xaxis), ylabel = label(yaxis),
                 source = event_source)
  })

  output$scatter <- renderPlotly({
    fs <- fscatter()
    req(fs, "FacileScatterViz")
    plot(fs)
  })

  vals <- list(
    xaxis = xaxis,
    yaxis = yaxis,
    viz = fscatter)

  class(vals) <- c("FacileScatterPlot", "ReactiveFacileViz")
  vals
}

#' @export
#' @importFrom shiny column tagList wellPanel
#' @importFrom plotly plotlyOutput
#' @rdname facileScatterPlot
facileScatterPlotUI <- function(id, ...) {
  ns <- NS(id)

  tagList(
    fluidRow(
      column(6, wellPanel(quantitativeAssayDataSelectUI(ns("xaxis")))),
      column(6, wellPanel(quantitativeAssayDataSelectUI(ns("yaxis"))))),
    fluidRow(
      column(
        12,
        wellPanel(categoricalAestheticMapUI(ns("aes"), group = FALSE)))),
    fluidRow(
      column(12, plotlyOutput(ns("scatter")))))
}

