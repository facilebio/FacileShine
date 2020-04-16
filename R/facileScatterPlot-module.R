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
#' @importFrom plotly renderPlotly plotlyOutput
#' @importFrom shiny column downloadHandler renderUI wellPanel
#' @importFrom shinycssloaders withSpinner
#' @rdname facileScatterPlot
#'
#' @param ndim Defaults to 3. When any two dimensions are provided, a plot will
#'   be drawn, so provides both 2d and 3d functionality. If set to 2, then only
#'   2d functionality would be enabled.
facileScatterPlot <- function(input, output, session, rfds, gdb = NULL, ...,
                              ndim = 3, x = NULL, y = NULL, z = NULL,
                              event_source = session$ns("selection"),
                              .reactive = TRUE) {
  assert_class(rfds, "ReactiveFacileDataStore")
  assert_int(ndim, lower = 2L, upper = 3L)
  ns <- session$ns

  isolate. <- if (.reactive) base::identity else shiny::isolate

  aes <- callModule(categoricalAestheticMap, "aes", rfds,
                    color = TRUE, shape = TRUE, facet = TRUE, hover = TRUE,
                    group = FALSE, ..., .reactive = .reactive)

  xaxis <- callModule(assayFeatureSelect2, "xaxis", rfds, gdb)
  yaxis <- callModule(assayFeatureSelect2, "yaxis", rfds, gdb)
  zaxis <- callModule(assayFeatureSelect2, "zaxis", rfds, gdb)

  features <- reactive({
    list(x = xaxis$selected(), y = yaxis$selected(), z = zaxis$selected())
  })

  .ndim <- reactive({
    out <- 0L
    for (f in features()) {
      if (!is.null(f) && nrow(f) > 0L) out <- out + 1L
    }
    out
  })

  # aes.covs <- reactive({
  #   ftrace("aes$covariates() fires in facileScatterPlot")
  #   aes$covariates()
  # })

  qcolnames <- reactive({
    out <- c(x = name(xaxis), y = name(yaxis), z = name(zaxis))
    out[!grepl("\\.nothing$", out)]
  })

  qlabels <- reactive({
    out <- c(x = label(xaxis), y = label(yaxis), z = label(zaxis))
    out[!grepl("^nothing$", out)]
  })

  # The quantitative data to plot, without aesthetic mappings
  rdat.core <- reactive({
    xdim <- .ndim()
    req(xdim >= 2L)
    f.all <- features()

    ftrace("Retrieving assay data for scatterplot")

    out <- active_samples(rfds)
    for (f in f.all) {
      if (nrow(f)) {
        out <- req(with_assay_data(out, f, aggregate = TRUE))
      }
    }
    out <- collect(out, n = Inf)
    colnames(out) <- c("dataset", "sample_id", qcolnames())
    out
  })

  rdat <- reactive({
    with_aesthetics(rdat.core(), aes)
  })

  observe({
    dat. <- tryCatch(rdat.core(), error = function(e) NULL)
    enabled <- is.data.frame(dat.) && nrow(dat.) > 0L
    shinyjs::toggleState("dldata", condition = enabled)
  })

  output$dldata <- downloadHandler(
    filename = function() "scatterplot-data.csv",
    content = function(file) {
      req(rdat.core()) %>%
        with_sample_covariates() %>%
        write.csv(file, row.names = FALSE)
    }
  )

  fscatter <- reactive({
    dat <- req(rdat())
    .axes <- qcolnames()
    .aes <- isolate(aes$map())
    .labels <- qlabels()
    hover. <- unique(unlist(unname(.aes), recursive = TRUE))
    if (length(hover.) == 0) hover. <- NULL
    ftrace("drawing scatterplot")
    if (length(.axes) == 3L) {
      .aes$facet <- NULL
      height <- 800
      width <- 900
    } else {
      height <- 600
      width <- 700
    }

    fscatterplot(dat, .axes, color_aes = .aes$color, shape_aes = .aes$shape,
                 facet_aes = .aes$facet, hover = hover.,
                 webgl = nrow(dat) > 1000,
                 xlabel = .labels[1], # label(axes$x),
                 ylabel = .labels[2], # label(axes$y),
                 zlabel = .labels[3], # label(axes$z),
                 height = height, width = width,
                 event_source = event_source)
  })

  plotsize <- reactive({
    fs <- fscatter()
    req(fs, "FacileScatterViz")
    list(width = plot(fs)$width, height = plot(fs)$height)
  })

  observeEvent(plotsize(), {
    psize <- req(plotsize())
    output$scatterplot <- renderPlotly(plot(fscatter()))
    output$plotlybox <- renderUI({
      withSpinner(plotlyOutput(session$ns("scatterplot"),
                               width = psize$width,
                               height = psize$height))
    })
  })

  vals <- list(
    ndim = .ndim,
    xaxis = xaxis,
    yaxis = yaxis,
    zaxis = zaxis,
    aes = aes,
    viz = fscatter,
    .ns = session$ns)

  class(vals) <- c("FacileScatterPlot", "ReactiveFacileViz")
  vals
}

#' @export
#' @importFrom shiny column downloadButton tagList wellPanel uiOutput
#' @importFrom plotly plotlyOutput
#' @rdname facileScatterPlot
facileScatterPlotUI <- function(id, with_download = TRUE, ...) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(4, wellPanel(assayFeatureSelect2UI(ns("xaxis"), "X axis"))),
      column(4, wellPanel(assayFeatureSelect2UI(ns("yaxis"), "Y axis"))),
      column(4, wellPanel(assayFeatureSelect2UI(ns("zaxis"), "Z axis")))),
    fluidRow(
      column(
        12,
        wellPanel(
          categoricalAestheticMapUI(
            ns("aes"), color = TRUE, shape = TRUE, facet = TRUE, hover = TRUE,
            group = FALSE)))),
    shinyjs::disabled(downloadButton(ns("dldata"), "Download Data")),
    fluidRow(
      column(12, uiOutput(ns("plotlybox"))))
    )
}

update_aes <- function(x, aesthethic, covariate, ...) {
  # TODO: enable callback/update of aesthetic map
}
