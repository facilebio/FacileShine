# TODO: delete this
# library(FacileShine)

library(FacileData)
library(FacileViz)
library(shiny)

user <- Sys.getenv("USER")
fds <- FacileData::exampleFacileDataSet()

gdbs <- list(
  hallmark = sparrow::getMSigGeneSetDb("H", id.type = "entrez"),
  kegg = sparrow::getKeggGeneSetDb("human", "entrez"),
  null = NULL)

devtools::load_all(".")

shiny::shinyApp(
  ui = shiny::fluidPage(
    shinyjs::useShinyjs(),
    shiny::wellPanel(filteredReactiveFacileDataStoreUI("ds")),
    
    shiny::tags$h2("assayFeatureSelect2"),
    shiny::selectInput("gdb", "GeneSetDb", c("hallmark", "kegg", "null")),
    
    fluidRow(
      column(6, shiny::wellPanel(assayFeatureSelect2UI("xaxis"))),
      column(6, shiny::wellPanel(assayFeatureSelect2UI("yaxis")))),
    shiny::tags$h2("Scatter Plot"),
    plotlyOutput("scatter")),

  server = function(input, output, session) {
    rfds <- callModule(
      filteredReactiveFacileDataStore, "ds", 
      reactive(fds$parent.dir),
      user = user)
    
    gdb <- reactive({
      gdbs[[input$gdb]]
    })
    
    xaxis <- callModule(assayFeatureSelect2, "xaxis", rfds, gdb = gdb)
    yaxis <- callModule(assayFeatureSelect2, "yaxis", rfds, gdb = gdb)

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
