#' Module server for boxplots
#' 
#' @export
fboxPlotServer <- function(id, rfds, ..., 
                           gdb = shiny::reactive(NULL), 
                           x = NULL, y = NULL,
                           event_source = NULL,
                           .reactive = TRUE, debug = FALSE) {
  assert_class(rfds, "ReactiveFacileDataStore")
  shiny::moduleServer(id, function(input, output, session) {
    if (is.null(event_source)) {
      event_source <- session$ns("selection")
    }
    
    # aes <- callModule(categoricalAestheticMap, "aes", rfds,
    #                   color = TRUE, facet = TRUE, hover = TRUE,
    #                   ..., .reactive = .reactive)
    
    xaxis <- categoricalSampleCovariateSelectServer(
      "xaxis", rfds, .with_none = FALSE)
    
    yaxis <- assayFeatureSelectServer("yaxis", rfds, gdb = gdb, ...)
    # batch <- callModule(batchCorrectConfig, "batch", rfds)
    
    # "Individual" checkbox is only active when multiple genes are entered
    # in the y-axis selector
    observe({
      nvals <- nrow(yaxis$selected())
      shinyjs::toggleState("individual", condition = nvals > 1)
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
      req(!unselected(yvals.), from_fds(yaxis, rfds))
      
      xsum <- xaxis$summary()
      xaxis. <- xaxis$covariate()
      req(!unselected(xaxis.), from_fds(xaxis, rfds))
      
      samples. <- active_samples(rfds)
      ftrace("Retrieving assay data for boxplot")
      # agg.by <- if (!indiv.) "ewm" else NULL
      agg. <- !indiv.
      # batch. <- name(batch$batch)
      # main. <- name(batch$main)
      batch. <- main. <- NULL

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
      # with_aesthetics(rdat.core(), aes)
      rdat.core()
    })
    
    fbox <- eventReactive(rdat(), {
      dat <- req(rdat())
      yvals. <- yaxis$selected()
      xaxis. <- xaxis$covariate()
      # aes. <- aes$map()
      aes. <- NULL
      indiv. <- input$individual
      
      hover. <- unique(c(xaxis., "feature_name", unlist(unname(aes.), recursive = TRUE)))
      
      
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
      output$boxplot <- renderPlotly(plot(fbox()))
      output$plotlybox <- renderUI({
        withSpinner(plotlyOutput(session$ns("boxplot"),
                                 width = psize$width,
                                 height = psize$height))
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
  ns <- NS(id)
  tagList(
    fluidRow(
      # column(4, wellPanel(categoricalSampleCovariateSelectUI(ns("xaxis"), "X Axis"))),
      column(4, categoricalSampleCovariateSelectInput(ns("xaxis"), "X Axis")),
      column(4, wellPanel(assayFeatureSelectInput(ns("yaxis"), "Y Axis"))),
      column(
        4,
        checkboxInput(ns("individual"),
                      label = "Plot Genes Individually",
                      value = FALSE),
        batchCorrectConfigUI(ns("batch"), direction = "vertical"))),
    fluidRow(
      column(
        12,
        wellPanel(
          categoricalAestheticMapUI(
            ns("aes"), color = TRUE, facet = TRUE, hover = TRUE)))),
    shinyjs::disabled(downloadButton(ns("dldata"), "Download Data")),
    fluidRow(
      column(12, uiOutput(ns("plotlybox"))))
  )
}
