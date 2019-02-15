# library(FacileShine)
devtools::load_all(".")
library(FacileData)
library(FacileViz)
library(dplyr)
library(plotly)
library(shiny)

shiny::shinyApp(
  ui = shiny::fluidPage(
    filteredReactiveFacileDataStoreUI("rfds"),
    shiny::tags$hr(),
    fluidRow(
      column(6, shiny::wellPanel(quantitativeAssayDataSelectUI("xaxis"))),
      column(6, shiny::wellPanel(quantitativeAssayDataSelectUI("yaxis")))),
    shiny::tags$hr(),
    plotlyOutput("scatter")),

  server = function(input, output) {
    fds <- FacileData::exampleFacileDataSet()
    user <- Sys.getenv("USER")
    rfds <- callModule(filteredReactiveFacileDataStore, "rfds", fds,
                       user = user)
    xaxis <- callModule(quantitativeAssayDataSelect, "xaxis", rfds)
    yaxis <- callModule(quantitativeAssayDataSelect, "yaxis", rfds)

    xlabel <- reactive({
      xf <- xaxis$features()
      if (nrow(xf) == 0) {
        "nothing"
      } else if (nrow(xf) == 1) {
        xf$name
      } else {
        "xscore"
      }
    })

    ylabel <- reactive({
      yf <- yaxis$features()
      if (nrow(yf) == 0) {
        "nothing"
      } else if (nrow(yf) == 1) {
        yf$name
      } else {
        "yscore"
      }
    })

    rdat <- reactive({
      xlab <- req(xlabel())
      ylab <- req(ylabel())
      xf <- xaxis$features()
      yf <- yaxis$features()
      xs <- rfds[["active_samples"]]()

      out <- xs %>%
        with_assay_data(xf, aggregate.by = "ewm") %>%
        with_assay_data(yf, aggregate.by = "ewm") %>%
        collect(n = Inf)
      colnames(out)[3:4] <- c(xlab, ylab)
      out
    })

    fscatter <- reactive({
      dat <- req(rdat())
      fscatterplot(dat, c(xlabel(), ylabel()))
    })

    output$scatter <- renderPlotly({
      fs <- fscatter()
      req(fs, "FacileScatterViz")
      plot(fs)
    })
  }
)
