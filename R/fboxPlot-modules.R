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
    
    yaxis <- assayFeatureSelectServer("yaxis", rfds, gdb = gdb, debug = debug, ...)
    
    batch <- batchCorrectConfigServer("batch", rfds, debug = debug)
    
    # "Individual" checkbox is only active when multiple genes are entered
    # in the y-axis selector
    observe({
      ys <- req(yaxis$selected())
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
      yvals. <- yaxis$selected() |> dplyr::arrange(name)
      xaxis. <- xaxis$selected()
      aes. <- aes$map()
      indiv. <- input$individual
      
      multigene <- nrow(yvals.) > 1
      sig.plot <- multigene && !indiv.
      
      hover. <- c(
        xaxis.,
        "feature_name",
        unlist(unname(aes.), recursive = TRUE)) |> 
        unique()
      
      
      if (length(hover.) == 0) hover. <- NULL
      ftrace("drawing boxplot")
      
      gg <- ggplot2::ggplot(dat)
      if (!sig.plot) {
        gg <- gg + ggplot2::aes(
          x = .data[[xaxis.]],
          y = .data[["value"]],
          fill = .data[["feature_name"]]
        )
      } else {
        gg <- gg + ggplot2::aes(
          x = .data[[xaxis.]],
          y = .data[["value"]]
        )
      }
      
      if (!sig.plot && multigene) {
        gg <- gg + 
          ggplot2::geom_boxplot(
            outliers = FALSE,
            position = ggplot2::position_dodge(width = 0.75)
          ) +
          ggplot2::geom_point(
            position = ggplot2::position_jitterdodge(
              jitter.width = 0.1,
              dodge.width = 0.75
            )
          )
      } else {
        gg <- gg + 
          ggplot2::geom_boxplot(outliers = FALSE) +
          ggplot2::geom_jitter(width = 0.1)
      }
      
      if (!is.null(aes.$facet)) {
        gg <- gg + ggplot2::facet_wrap(
          ggplot2::vars(!!rlang::sym(aes.$facet)))
      }
      
      gg <- gg + ggplot2::scale_x_discrete(
        guide = ggplot2::guide_axis(n.dodge = 2),
        labels = \(x) gsub("__", "\n", x)
      )

      units <- "abundance"
      if (yvals.$assay_type[1] %in% c("pseudobulk", "rnaseq") && !sig.plot) {
        units <- "CPM"
      }

      ylab <- sprintf("log2(%s)", units)
      if (sig.plot) {
        gtitle <- "Activity Score"
      } else {
        gtitle <- paste(head(yvals.$name, 8), collapse = ",")
        if (nrow(yvals.) > 8) {
          nmore <- nrow(yvals.) - 8
          gtitle <- paste0(gtitle, ", +", nmore, " more")
        }
      }

      gg <- gg + ggplot2::labs(
        title = gtitle,
        x = "",
        y = ylab
      )
      
      out <- plotly::ggplotly(gg)
      if (!sig.plot && multigene) {
        out <- plotly::layout(out, boxmode = "group")
      }
      
      out
    }, label = "fbox")
    
    output$jqboxplot <- plotly::renderPlotly({
      # plot(req(fbox()))
      req(fbox())
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
    
    # shiny::fluidRow(
    #   shiny::column(12, shiny::uiOutput(ns("plotlybox"))))
    
    shiny::fluidRow(
      shiny::column(12, shinycssloaders::withSpinner({
        shinyjqui::jqui_resizable(plotly::plotlyOutput(ns("jqboxplot")))
      })))
      
  )
}
