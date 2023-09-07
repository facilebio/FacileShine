#' Module server for scatter plots
#' 
#' @export
fscatterPlotServer <- function(id, rfds, ..., 
                               gdb = shiny::reactive(NULL),
                               ndim = 3,
                               x = NULL, y = NULL, z = NULL,
                               event_source = NULL,
                               debug = FALSE) {
  assert_class(rfds, "ReactiveFacileDataStore")
  assert_int(ndim, lower = 2, upper = 3)
  moduleServer(id, function(input, output, session) {
    if (is.null(event_source)) {
      event_source <- session$ns("selection")
    }
    
    dims <- sapply(.dimnames(ndim), function(dname) {
      assayFeatureSelectServer(dname, rfds, gdb = gdb, debug = debug, ...)
    }, simplify = FALSE)
    
    features <- reactive({
      lapply(dims, function(d) {
        out <- d$selected()
        # I've convinced myself that the `$in_sync()` calls has to come second,
        # but I don't think that should be the case.
        req(d$in_sync())
        out
      })
    })
    ndim. <- reactive(sum(!sapply(features(), unselected)))

    aes <- categoricalAestheticMapServer(
      "aes", rfds, color = TRUE, shape = TRUE, facet = TRUE, hover = TRUE, ...)
    
    batch <- batchCorrectConfigServer("batch", rfds, debug = debug)
    
    qcolnames <- reactive({
      out <- sapply(dims, name)
      out[!grepl("\\.nothing$", out)]
    })
    qlabels <- reactive({
      out <- sapply(dims, label)
      out[!grepl("^nothing$", out)]
    })
    
    # The quantitative data to plot, without aesthetic mappings
    rdat.core <- reactive({
      xdim <- ndim.()
      req(xdim >= 2L)
      features. <- features()
      
      ftrace("Retrieving assay data for scatterplot")
      
      out <- rfds$active_samples()
      batch. <- name(batch$batch)
      main. <- name(batch$main)

      for (f in features.) {
        if (nrow(f)) {
          out <- req(with_assay_data(out, f, aggregate = TRUE,
                                     normalize = TRUE, batch = batch.,
                                     main = main., prior.count = 0.1))
        }
      }
      out <- collect(out, n = Inf)
      colnames(out) <- c("dataset", "sample_id", qcolnames())
      out
    })
    
    rdat <- reactive({
      add_aesthetic_covariates(aes, rdat.core())
    })
    
    observe({
      dat. <- tryCatch(rdat.core(), error = function(e) NULL)
      enabled <- is.data.frame(dat.) && nrow(dat.) > 0L
      shinyjs::toggleState("dldata", condition = enabled)
    })
    output$dldata <- downloadHandler(
      filename = function() "scatterplot-data.csv",
      content = function(file) {
        req(rdat.core()) |>
          with_sample_covariates() |>
          write.csv(file, row.names = FALSE)
      }
    )
    
    fscatter <- reactive({
      dat <- req(rdat())
      .axes <- qcolnames()
      aes. <- aes$map()
      .labels <- qlabels()
      
      hover. <- c(
        "sample_id",
        unlist(unname(aes.), recursive = TRUE)) |> 
        unique()
      
      if (length(hover.) == 0) hover. <- NULL
      ftrace("drawing scatterplot")
      if (length(.axes) == 3L) {
        aes.$facet <- NULL
        height <- 800
        width <- 900
      } else {
        height <- 600
        width <- 700
      }
      
      fscatterplot(dat, .axes, color_aes = aes.$color, shape_aes = aes.$shape,
                   facet_aes = aes.$facet, hover = hover.,
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
      output$scatterplot <- plotly::renderPlotly(plot(fscatter()))
      output$plotlybox <- shiny::renderUI({
        waiter::withWaiter({
          plotly::plotlyOutput(session$ns("scatterplot"),
                               width = psize$width,
                               height = psize$height)
        })
      })
    })
    
    vals <- list(
      ndim = ndim.,
      dims = dims,
      aes = aes,
      viz = fscatter,
      .ns = session$ns)
    
    class(vals) <- c("FacileBoxPlot", "ReactiveFacileViz")
    vals
    
  })
}

#' @noRd
#' @export
fscatterPlotUI <- function(id, ndim = 3, with_download = TRUE, ...,
                           debug = FALSE) {
  assert_int(ndim, lower = 2, upper = 3)
  ns <- shiny::NS(id)
  select_ncol <- 12 / ndim

  dims <- sapply(.dimnames(ndim), function(dname) {
    label <- paste(toupper(dname), "Axis")
    shiny::column(
      width = select_ncol,
      assayFeatureSelectInput(ns(dname), label))
  }, simplify = FALSE)
  
  shiny::tagList(
    shiny::fluidRow(dims),
    shiny::wellPanel(
      batchCorrectConfigUI(ns("batch"), direction = "horizontal"),
    ),
    shiny::fluidRow(
      shiny::column(
        width = 12,
        shiny::wellPanel(
          categoricalAestheticMapInput(
            ns("aes"), color = TRUE, shape = TRUE, facet = TRUE, hover = TRUE,
            group = FALSE)))),
    shinyjs::disabled(shiny::downloadButton(ns("dldata"), "Download Data")),
    shiny::fluidRow(
      shiny::column(width = 12, shiny::uiOutput(ns("plotlybox"))))
  )
}

# Utility Functions ------------------------------------------------------------

.dimnames <- function(ndim, universe = c("x", "y", "z")) {
  assert_int(ndim, lower = 1L, upper = 3)
  head(universe, ndim)
}
