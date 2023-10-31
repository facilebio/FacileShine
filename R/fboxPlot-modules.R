#' Module server for boxplots
#' 
#' @export
fboxPlotServer <- function(id, rfds, ..., 
                           gdb = shiny::reactive(NULL), 
                           x = NULL, y = NULL,
                           event_source = NULL,
                           debug = FALSE) {
  assert_class(rfds, "ReactiveFacileDataStore")
  
  moduleServer(id, function(input, output, session) {
    if (is.null(event_source)) {
      event_source <- session$ns("selection")
    }
    
    aes <- categoricalAestheticMapServer(
      "aes", rfds, color = TRUE, facet = TRUE, hover = TRUE, ...)
    
    xaxis <- categoricalSampleCovariateSelectServer(
      "xaxis", rfds, with_none = FALSE)
    
    yaxis <- assayFeatureSelectServer("yaxis", rfds, gdb = gdb, ...)
    
    batch <- batchCorrectConfigServer("batch", rfds, debug = debug)
    
    # "Individual" checkbox is only active when multiple genes are entered
    # in the y-axis selector
    observe({
      ys <- req(yaxis$selected())
      # browser()
      shinyjs::toggleState("individual", condition = nrow(ys) > 1)
    })
    
    # Due to the limitations of the curent boxplot implementation, if the user
    # wants to show multiple genes at once, faceting is disabled, ie. multiple
    # genes are selected in y-axis and "Individual" box is checked
    observe({
      disabled <- isFALSE(input$individual) || nrow(yaxis$selected()) <= 1
      shinyjs::toggleState("aes-facet", condition = disabled)
    })
    
    # The quantitative data to plot, without aesthetic mappings
    rdat.core <- reactive({
      indiv. <- input$individual
      yvals. <- yaxis$selected()
      ftrace("{reset}{bold}{red}rdat.core: testing if yaxis is ready ...")
      # req(!unselected(yvals.), yaxis$in_sync())
      req(!unselected(yvals.))
      
      xaxis. <- xaxis$selected()
      ftrace("rdat.core: testing if xaxis is ready ...")
      # req(!unselected(xaxis.), xaxis$in_sync())
      req(!unselected(xaxis.))
      
      samples. <- rfds$active_samples()
      ftrace("Retrieving assay data for boxplot")

      agg. <- !indiv.
      batch. <- name(batch$batch)
      main. <- name(batch$main)
      
      out <- rfds$fds() |> 
        fetch_assay_data(
          yvals., samples., normalized = TRUE,
          prior.count = 0.1, aggregate = agg.,
          batch = batch., main = main.)
      out <- with_sample_covariates(out, xaxis.)
      out
    })
    
    observe({
      dat. <- tryCatch(rdat.core(), error = function(e) NULL)
      enabled <- is.data.frame(dat.) && nrow(dat.) > 0L
      shinyjs::toggleState("dldata", condition = enabled)
    })
    
    output$dldata <- downloadHandler(
      filename = function() "boxplot-data.csv",
      content = function(file) {
        req(rdat.core()) |>
          with_sample_covariates() |>
          write.csv(file, row.names = FALSE)
      }
    )
    
    ylabel <- reactive({
      yf <- yaxis$selected()
      if (unselected(yf)) {
        out <- NULL
      } else {
        aname <- yf$assay[1L]
        out <- assay_units(rfds$fds(), aname, normalized = TRUE)
      }
      out
    })
    
    rdat <- reactive({
      add_aesthetic_covariates(aes, rdat.core())
    })
    
    fbox <- eventReactive(rdat(), {
      dat <- req(rdat())
      yvals. <- yaxis$selected()
      xaxis. <- xaxis$selected()
      aes. <- aes$map()
      indiv. <- input$individual
      
      hover. <- c(
        xaxis.,
        "feature_name",
        unlist(unname(aes.), recursive = TRUE)) |> 
        unique()
      
      
      if (length(hover.) == 0) hover. <- NULL
      ftrace("drawing boxplot")

      if (nrow(yvals.) > 1 && indiv.) {
        # We want to draw multiple boxplots per x-value. For now we leverage
        # faceting for this
        plt <- fboxplot(dat, "feature_name", "value", with_points = TRUE,
                        facet_aes = xaxis., color_aes = aes.$color,
                        hover = hover., na_x = "keep",
                        event_source = event_source)
      } else {
        plt <- fboxplot(dat, xaxis., "value", with_points = TRUE,
                        facet_aes = aes.$facet, color_aes = aes.$color,
                        hover = hover., na_x = "keep",
                        ylabel = ylabel(),
                        event_source = event_source)
      }
      plt
    }, label = "fbox")
    
    plotsize <- reactive({
      plot. <- fbox()
      req(plot., "FacileScatterViz")
      list(width = plot(plot.)$width, height = plot(plot.)$height)
    })
    
    observeEvent(plotsize(), {
      psize <- req(plotsize())
      output$boxplot <- plotly::renderPlotly(plot(fbox()))
      output$plotlybox <- shiny::renderUI({
        shinycssloaders::withSpinner({
          plotly::plotlyOutput(session$ns("boxplot"),
                               width = psize$width,
                               height = psize$height)
        })
      })
    })
    
    vals <- list(
      xaxis = xaxis,
      yaxis = yaxis,
      aes = NULL,# aes,
      viz = fbox,
      .ns = session$ns)
    
    class(vals) <- c("FacileBoxPlot", "ReactiveFacileViz")
    vals
  })
}

#' @noRd
#' @export
fboxPlotUI <- function(id, ..., debug = FALSE) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fluidRow(
      shiny::column(
        width = 4,
        categoricalSampleCovariateSelectInput(ns("xaxis"), "X Axis")),
      shiny::column(
        width = 4,
        shiny::wellPanel(assayFeatureSelectInput(ns("yaxis"), "Y Axis"))),
      shiny::column(
        width = 4,
        shiny::checkboxInput(
          ns("individual"),
          label = "Plot Genes Individually",
          value = FALSE),
        batchCorrectConfigUI(ns("batch"), direction = "vertical"))),
    shiny::fluidRow(
      shiny::column(
        width = 12,
        shiny::wellPanel(
          categoricalAestheticMapInput(
            ns("aes"), color = TRUE, facet = TRUE, hover = TRUE)))),
    shinyjs::disabled(shiny::downloadButton(ns("dldata"), "Download Data")),
    shiny::fluidRow(
      shiny::column(12, shiny::uiOutput(ns("plotlybox"))))
  )
}
