# TODO: delete this
# library(FacileShine)
library(FacileData)
library(FacileViz)
library(shiny)

fds <- FacileData::exampleFacileDataSet()
user <- Sys.getenv("USER")

devtools::load_all(".")

shiny::shinyApp(
  ui = shiny::fluidPage(
    shiny::wellPanel(filteredReactiveFacileDataStoreUI("ds")),
    shiny::tags$h2("assayFeatureSelect"),
    fluidRow(
      column(6, shiny::wellPanel(assayFeatureSelectUI("xaxis"))),
      column(6, shiny::wellPanel(assayFeatureSelectUI("yaxis")))),
    shiny::tags$h2("Scatter Plot"),
    plotlyOutput("scatter")),

  server = function(input, output) {
    rfds <- callModule(filteredReactiveFacileDataStore, "ds", fds, user = user)
    xaxis <- callModule(assayFeatureSelect, "xaxis", rfds)
    yaxis <- callModule(assayFeatureSelect, "yaxis", rfds)

    rdat <- reactive({
      xf <- xaxis$selected()
      yf <- yaxis$selected()
      req(nrow(xf) > 0, nrow(yf) > 0)

      xs <- active_samples(rfds)

      out <- xs |>
        with_assay_data(xf, aggregate.by = "ewm") |>
        with_assay_data(yf, aggregate.by = "ewm") |>
        collect(n = Inf)
      colnames(out)[3:4] <- c(name(xaxis), name(yaxis))
      out
    })

    fscatter <- reactive({
      dat <- req(rdat())
      axes <- c(name(xaxis), name(yaxis))
      fscatterplot(dat, axes, xlabel = label(xaxis), ylabel = label(yaxis))
    })

    output$scatter <- renderPlotly({
      fs <- fscatter()
      req(fs, "FacileScatterViz")
      plot(fs)
    })
  }
)
