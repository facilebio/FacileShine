# When working with monstrous (in samples) dataset, like the UKBB, the covarite
# filters really slow things down. Let's see if we can unwind the steps and
# make things faster.
library(FacileData)
library(dplyr)
library(dbplyr)
devtools::load_all(".")

# The Nightingale dataset has 117,947 samples and 10 covariates
xfds <- FacileDataSet("~/workspace/facilebio/data/FacileNightingaleDataSet")

# These are some things that happen when a new set of samples rolls its way
# through the covariate picker.

# Bring up the samples facile_frame:
system.time(samples.all <- samples(xfds) |> collect(n = Inf))
#  user  system elapsed 
# 0.330   0.076   0.612 

# How long does it take to test that this samples.all frame belongs to
# the FDS: 0.1s
system.time(test_sample_subset(samples.all, xfds))
#  user  system elapsed 
# 0.120   0.008   0.130 

# Sequence of events that updates the sample covariates: -----------------------

# 1a. retrieve all the sample covariate names.
#    the first time, this takes ~7s, but subsequent calls are closer to 2.5s
system.time(acovs <- fetch_sample_covariates(samples.all))
#  user  system elapsed 
# 3.930   0.823   7.629 
#  user  system elapsed 
# 2.530   0.098   2.635 

system.time(wcovs <- spread_covariates(acovs))

# ==============================================================================
library(shiny)
library(shinyWidgets)
library(datamods)

shiny::shinyApp(
  ui = fluidPage(
    tags$h2("Filter data.frame"),
    actionButton("saveFilterButton","Save Filter Values"),
    actionButton("loadFilterButton","Load Filter Values"),
    
    fluidRow(
      column(
        width = 3,
        filter_data_ui("filtering"),
      ),
      column(
        width = 9,
        progressBar(
          id = "pbar", value = 100,
          total = 100, display_pct = TRUE
        ),
        reactable::reactableOutput(outputId = "table"),
        tags$b("Code dplyr:"),
        verbatimTextOutput(outputId = "code_dplyr"),
        tags$b("Expression:"),
        verbatimTextOutput(outputId = "code"),
        tags$b("Filtered data:"),
        verbatimTextOutput(outputId = "res_str")
      )
    )
  ),
  
  server = function(input, output, session) {
    savedFilterValues <- reactiveVal()
    data <- reactive(wcovs)
    
    vars <- reactive(setdiff(names(data()), c("dataset", "sample_id")))
    
    observeEvent(input$saveFilterButton,{
      savedFilterValues <<- res_filter$values()
    },ignoreInit = T)
    
    defaults <- reactive({
      input$loadFilterButton
      savedFilterValues
    })
    
    res_filter <- filter_data_server(
      id = "filtering",
      data = data,
      # name = reactive("Nightingale Covariates"),
      vars = vars,
      defaults = defaults,
      widget_char = "picker",
      widget_char = "select",
      widget_num = "slider",
      widget_date = "slider",
      label_na = "Missing"
    )
    
    observeEvent(res_filter$filtered(), {
      updateProgressBar(
        session = session, id = "pbar",
        value = nrow(res_filter$filtered()), total = nrow(data())
      )
    })
    
    output$table <- reactable::renderReactable({
      reactable::reactable(res_filter$filtered(), server = TRUE)
    })
    
    
    output$code_dplyr <- renderPrint({
      res_filter$code()
    })
    output$code <- renderPrint({
      res_filter$expr()
    })
    
    output$res_str <- renderPrint({
      str(res_filter$filtered())
    })
    
  }
)

  