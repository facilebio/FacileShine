#' An interactive scatterplot module
#'
#' @export
#' @importFrom plotly renderPlotly
#' @rdname facileScatterPlot
facileScatterPlot <- function(input, output, session, rfds, ...,
                              .reactive = TRUE) {
  assert_class(rfds, "ReactiveFacileDataStore")
  isolate. <- if (.reactive) base::identity else shiny::isolate

  aesthetx <- callModule(categoricalAestheticMap, "aesthetx", rfds,
                         group = FALSE, ..., .reactive = .reactive)

  xaxis <- callModule(quantitativeAssayDataSelect, "xaxis", rfds)
  yaxis <- callModule(quantitativeAssayDataSelect, "yaxis", rfds)

  aes.covs <- reactive({
    aesthetx$covariates()
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
    if (length(covs)) {
      out <- with_sample_covariates(out, covs, .fds = fds(rfds))
    }
    out
  })

  fscatter <- reactive({
    dat <- req(rdat())
    axes <- c(name(xaxis), name(yaxis))
    .aes <- aes.covs()
    fscatterplot(dat, axes, color_aes = .aes$color, shape_aes = .aes$shape,
                 facet_aes = .aes$facet)
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
        wellPanel(categoricalAestheticMapUI(ns("aesthetx"), group = FALSE)))),
    fluidRow(
      column(12, plotlyOutput(ns("scatter")))))
}

