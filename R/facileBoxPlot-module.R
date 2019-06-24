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
#' @importFrom shiny column renderUI wellPanel
#' @importFrom shinycssloaders withSpinner
#' @importFrom shinyjs toggleState
#' @importFrom plotly renderPlotly plotlyOutput
#' @rdname facileScatterPlot
#'
#' @param ndim Defaults to 3. When any two dimensions are provided, a plot will
#'   be drawn, so provides both 2d and 3d functionality. If set to 2, then only
#'   2d functionality would be enabled.
facileBoxPlot <- function(input, output, session, rfds, ...,
                          x = NULL, y = NULL,
                          event_source = session$ns("selection"),
                          .reactive = TRUE) {
  assert_class(rfds, "ReactiveFacileDataStore")
  ns <- session$ns

  isolate. <- if (.reactive) base::identity else shiny::isolate

  aes <- callModule(categoricalAestheticMap, "aes", rfds,
                    color = TRUE, facet = TRUE, hover = TRUE,
                    ..., .reactive = .reactive)

  xaxis <- callModule(categoricalSampleCovariateSelect, "xaxis", rfds,
                      .with_none = FALSE)
  yaxis <- callModule(assayFeatureSelect, "yaxis", rfds)

  yvals <- reactive({
    out <- yaxis$selected()
    if (unselected(out)) {
      ftrace("No features selected for yaxis")
    } else {
      ftrace("yaxis$selected() features updated:", paste(out$feature_id, collapse = ","))
    }
    out
  })

  # aes.covs <- reactive({
  #   ftrace("aes$covariates() fires in facileBoxPlot")
  #   aes$covariates()
  # })

  # "Individual" checkbox is only active when multiple genes are entered
  # in the y-axis selector
  observe({
    toggleState("individual", condition = nrow(yvals()) > 1)
  })

  # Due to the limitations of the curent boxplot implementation, if the user
  # wants to show multiple genes at once, faceting is disabled, ie. multiple
  # genes are selected in y-axis and "Individual" box is checked
  observe({
    toggleState("aes-facet",
                condition = !input$individual || nrow(yvals()) <= 1)
  })

  # The quantitative data to plot, without aesthetic mappings
  rdat.core <- reactive({
    indiv. <- input$individual

    yvals. <- yvals()
    req(!unselected(yvals.))

    xsum <- xaxis$summary()
    xaxis. <- xaxis$covariate()
    req(!unselected(xaxis.))

    samples. <- active_samples(rfds)
    ftrace("Retrieving assay data for boxplot")
    agg.by <- if (!indiv.) "ewm" else NULL

    out <- fetch_assay_data(rfds, yvals., samples., normalized = TRUE,
                            prior.count = 0.25, aggregate.by = agg.by)
    out <- with_sample_covariates(out, xaxis.)
    out
  })

  # rdat <- reactive({
  #   out <- req(rdat.core())
  #   req(nrow(out) > 0)
  #   aes.map <- aes$map()
  #   aes.covs <- setdiff(unlist(unname(aes.map)), colnames(out))
  #   if (length(aes.covs)) {
  #     ftrace("retrieving aes covariates for boxplot")
  #     out <- with_sample_covariates(out, aes.covs)
  #   }
  #   out
  # })

  rdat <- reactive({
    with_aesthetics(rdat.core(), aes)
  })

  fbox <- eventReactive(rdat(), {
    dat <- req(rdat())
    yvals. <- yvals()
    xaxis. <- xaxis$covariate()
    aes. <- aes$map()
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
                      event_source = event_source)
    }
    plt
  })

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
    aes = aes,
    viz = fbox,
    .ns = session$ns)

  class(vals) <- c("FacileBoxPlot", "ReactiveFacileViz")
  vals
}

#' @export
#' @importFrom shiny checkboxInput column tagList wellPanel uiOutput
#' @importFrom plotly plotlyOutput
#' @rdname facileScatterPlot
facileBoxPlotUI <- function(id, ...) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(4, wellPanel(categoricalSampleCovariateSelectUI(ns("xaxis"), "X Axis"))),
      column(4, wellPanel(assayFeatureSelectUI(ns("yaxis"), "Y Axis"))),
      column(1, checkboxInput(ns("individual"), label = "Individually", value = FALSE))),
    fluidRow(
      column(
        12,
        wellPanel(
          categoricalAestheticMapUI(
            ns("aes"), color = TRUE, facet = TRUE, hover = TRUE)))),
    fluidRow(
      column(12, uiOutput(ns("plotlybox"))))
    )
}

update_aes <- function(x, aesthethic, covariate, ...) {
  # TODO: enable callback/update of aesthetic map
}
