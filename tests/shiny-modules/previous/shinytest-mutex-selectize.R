library(FacileData)
library(shiny)

efds <- exampleFacileDataSet()
universe <- head(letters, 5)

shinyApp(
  ui = fluidPage(
    selectizeInput("box1", label = "box1", choices = universe, selected = NULL,
                   multiple = TRUE),
    selectizeInput("box2", label = "box2", choices = universe, selected = NULL,
                   multiple = TRUE)),

  server = function(input, output, session) {
    observeEvent(input$box1, {
      req(!is.null(input$box1))
      message("input$box1 updated: ", paste(input$box1, collapse = ","))
      remaining <- setdiff(universe, input$box1)
      updateSelectizeInput(session, "box2", choices = remaining, server = TRUE,
                           selected = input$box2)
    }, ignoreNULL = FALSE)
    observeEvent(input$box2, {
      req(!is.null(input$box1))
      message("input$box2 updated: ", paste(input$box2, collapse = ","))
      remaining <- setdiff(universe, input$box2)
      freezeReactiveValue(input, "box2")
      updateSelectizeInput(session, "box1", choices = remaining, server = TRUE,
                           selected = input$box1)
    })
  }
)
