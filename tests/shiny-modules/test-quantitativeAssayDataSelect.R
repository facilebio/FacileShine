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

    rdat <- reactive({
      xf <- req(xaxis$features())
      yf <- req(yaxis$features())
      xs <- active_samples(rfds)

      out <- xs %>%
        with_assay_data(xf, aggregate.by = "ewm") %>%
        with_assay_data(yf, aggregate.by = "ewm") %>%
        collect(n = Inf)
      colnames(out)[3:4] <- c(name(xaxis), name(yaxis))
      out
    })

    fscatter <- reactive({
      dat <- req(rdat())
      axes <- c(name(xaxis), name(yaxis))
      fscatterplot(dat, axes)
    })

    output$scatter <- renderPlotly({
      fs <- fscatter()
      req(fs, "FacileScatterViz")
      plot(fs)
    })
  }
)
