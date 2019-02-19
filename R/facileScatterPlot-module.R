#' An interactive scatterplot module (2-3 dimensions).
#'
#' This module presents all three dimensions for plotting. If the user only
#' specifies two dimensions in a weird way (x and z, only), then z will be
#' collapsed to y.
#'
#' We can make this more user-proof by incrementally activating "the next"
#' dimension by ensuring the previous dimension is selected.
#'
#' A 1d scatter plot might just default to a density plot.
#'
#' @export
#' @importFrom shiny column wellPanel
#' @importFrom plotly renderPlotly
#' @rdname facileScatterPlot
facileScatterPlot <- function(input, output, session, rfds, ...,
                              ndim = 3, x = NULL, y = NULL, z = NULL,
                              event_source = session$ns("selection"),
                              .reactive = TRUE) {
  assert_class(rfds, "ReactiveFacileDataStore")
  assert_int(ndim, lower = 2L, upper = 3L)
  ns <- session$ns

  state <- reactiveValues(
    ndim = ndim)

  isolate. <- if (.reactive) base::identity else shiny::isolate

  aes <- callModule(categoricalAestheticMap, "aes", rfds,
                    group = FALSE, ..., .reactive = .reactive)

  # Handle dynamic number of axes
  axnames <- head(c("x", "y", "z"), ndim)

  output$axes <- renderUI({
    ncol <- floor(12 / ndim)
    out <- sapply(axnames, function(axis) {
      column(ncol, wellPanel(quantitativeAssayDataSelectUI(ns(axis))))
    }, simplify = FALSE)
  })

  axes <- sapply(axnames, function(axis) {
    callModule(quantitativeAssayDataSelect, axis, rfds)
  }, simplify = FALSE)

  features <- reactive({
    lapply(axes, function(axis) {
      tryCatch(axis$features(), error = function(e) NULL)
    })
  })

  .ndim <- reactive({
    out <- 0L
    for (f in features()) {
      if (nrow(f) > 0L) out <- out + 1L
    }
    out
  })

  aes.covs <- reactive({
    aes$covariates()
  })

  qcolnames <- reactive({
    out <- sapply(axes, name)
    out[!grepl("\\.nothing$", out)]
  })

  qlabels <- reactive({
    out <- sapply(axes, label)
    out[!grepl("^nothing$", out)]
  })

  rdat <- reactive({
    xdim <- .ndim()
    req(xdim >= 2L)
    f.all <- features()
    covs <- unlist(aes.covs())

    # The "fluent" facile data access call on a 2d plot looks like:
    # out <- rfds %>%
    #   active_samples() %>%
    #   with_assay_data(axes$x$features(), aggregate.by = "ewm") %>%
    #   with_assay_data(axes$y$features(), aggregate.by = "ewm") %>%
    #   with_sample_covariates(covs)
    #   collect(n = Inf)
    out <- active_samples(rfds)
    for (f in f.all) {
      if (nrow(f)) {
        out <- with_assay_data(out, f, aggregate.by = "ewm")
      }
    }
    out <- collect(out, n = Inf)
    colnames(out) <- c("dataset", "sample_id", qcolnames())
    if (length(covs)) {
      out <- with_sample_covariates(out, covs, .fds = fds(rfds))
    }
    out
  })

  fscatter <- reactive({
    dat <- req(rdat())
    .axes <- qcolnames()
    .aes <- aes.covs()
    .labels <- qlabels()

    fscatterplot(dat, .axes, color_aes = .aes$color, shape_aes = .aes$shape,
                 facet_aes = .aes$facet,
                 xlabel = .labels[1], # label(axes$x),
                 ylabel = .labels[2], # label(axes$y),
                 zlabel = .labels[3], # label(axes$z),
                 source = event_source)
  })

  output$scatter <- renderPlotly({
    fs <- fscatter()
    req(fs, "FacileScatterViz")
    plot(fs)
  })

  vals <- list(
    ndim = .ndim,
    xaxis = axes$x,
    yaxis = axes$y,
    zaxis = axes$z,
    viz = fscatter)

  class(vals) <- c("FacileScatterPlot", "ReactiveFacileViz")
  vals
}

#' @export
#' @importFrom shiny column tagList wellPanel uiOutput
#' @importFrom plotly plotlyOutput
#' @rdname facileScatterPlot
facileScatterPlotUI <- function(id, ...) {
  ns <- NS(id)

  tagList(
    fluidRow(
      # column(6, wellPanel(quantitativeAssayDataSelectUI(ns("xaxis")))),
      # column(6, wellPanel(quantitativeAssayDataSelectUI(ns("yaxis"))))),
      uiOutput(ns("axes"))),
    fluidRow(
      column(
        12,
        wellPanel(categoricalAestheticMapUI(ns("aes"), group = FALSE)))),
    fluidRow(
      column(12, plotlyOutput(ns("scatter")))))
}

