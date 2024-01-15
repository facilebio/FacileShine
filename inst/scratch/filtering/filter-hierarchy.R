# https://community.rstudio.com/t/reactivity-in-shiny-modules-for-hierarchal-pickers/173236/2
# 9/12/2023

library(shiny)
library(dplyr)
library(shinyWidgets)
library(plotly)

# mock dataset

x <- tibble(level1 = c(rep("A", 100), rep("B", 100), rep("C", 100), rep("D", 100)),
            level2 = c(rep("A1", 50), rep("A2", 50), rep("B1", 50), rep("B2", 50),
                       rep("C1", 50), rep("C2", 50), rep("D1", 50), rep("D2", 50)),
            level3 = c(rep("A21", 25), rep("A22", 25), rep("A23", 25), rep("A24", 25),
                       rep("B21", 25), rep("B22", 25), rep("B23", 25), rep("B24", 25),
                       rep("C21", 25), rep("C22", 25), rep("C23", 25), rep("C24", 25),
                       rep("D21", 25), rep("D22", 25), rep("D23", 25), rep("D24", 25)),
            level4 = c(rep("A31", 10), rep("A32", 10), rep("A33", 10), rep("A34", 10), rep("A35", 10),
                       rep("A36", 10), rep("A37", 10), rep("A38", 10), rep("A39", 10), rep("A310", 10),
                       rep("B31", 10), rep("B32", 10), rep("B33", 10), rep("B34", 10), rep("B35", 10),
                       rep("B36", 10), rep("B37", 10), rep("B38", 10), rep("B39", 10), rep("B310", 10),
                       rep("C31", 10), rep("C32", 10), rep("C33", 10), rep("C34", 10), rep("C35", 10),
                       rep("C36", 10), rep("C37", 10), rep("C38", 10), rep("C39", 10), rep("C310", 10),
                       rep("D31", 10), rep("D32", 10), rep("D33", 10), rep("D34", 10), rep("D35", 10),
                       rep("D36", 10), rep("D37", 10), rep("D38", 10), rep("D39", 10), rep("D310", 10))) %>%
  mutate(value = runif(400, 0, 100))

# module UI

moduleUI <- function(id, label, choices = NULL) {
  ns <- NS(id)
  tagList(pickerInput(ns("select"), label= label, choices = choices,
                      selected = choices, multiple = TRUE, 
                      options = list(`actions-box` = TRUE, `live-search`=TRUE)))
}

# module server root

moduleRootController <- function(id) {
  
  moduleServer(id, function(input, output, session) {
    
    return(reactive({input$select}))
    
  })
}

# module server

moduleController <- function(id, data, selector, input_val, output_val) {
  
  moduleServer(id, function(input, output, session) {
    
    observeEvent(selector(), {
      choices=data %>%
        filter({{input_val}} %in% selector()) %>%
        distinct({{output_val}}) %>%
        arrange({{output_val}}) %>%
        pull({{output_val}})
      updatePickerInput(session, "select", choices = choices, selected = choices)
    }, ignoreNULL = FALSE)
    
    return(reactive({input$select}))
    
  })
}

ui_heirarchy <- function(id){
  ns <- NS(id)
  tagList(moduleUI(ns("ModuleRoot"), label = "Root Label", choices=c("A", "B", "C", "D")),
          moduleUI(ns("Module1"), label = "Test Label 1"),
          moduleUI(ns("Module2"), label = "Test Label 2"),
          moduleUI(ns("Module3"), label = "Test Label 3"))
}

server_heirarchy <- function(id) {
  
  moduleServer(id, function(input, output, session) {
    
    mod0 <- moduleRootController("ModuleRoot")
    mod1 <- moduleController("Module1", x, reactive({mod0()}), level1, level2)
    mod2 <- moduleController("Module2", x, reactive({mod1()}), level2, level3)
    mod3 <- moduleController("Module3", x, reactive({mod2()}), level3, level4)
    
    return(list(mod0 = mod0, mod1 = mod1, mod2 = mod2, mod3 = mod3))
    
  })
}

# ui / server / app

ui <- fixedPage(
  ui_heirarchy("test"),
  plotlyOutput("plot")
)

server <- function(input, output, session) {
  
  out <- server_heirarchy("test")
  
  process <- reactive({Sys.sleep(5)})
  
  output$plot <- renderPlotly({
    
    req(out$mod3())
    
    process()
    
    x %>%
      filter(level1 %in% out$mod0()) %>%
      filter(level2 %in% out$mod1()) %>%
      filter(level3 %in% out$mod2()) %>%
      filter(level4 %in% out$mod3()) %>%
      plot_ly(x = ~value, type = 'histogram') %>%
      layout(title = 'A Figure Displaying Itself',
             plot_bgcolor='#e5ecf6', 
             xaxis = list( 
               zerolinecolor = '#ffff', 
               zerolinewidth = 2, 
               gridcolor = 'ffff'), 
             yaxis = list( 
               zerolinecolor = '#ffff', 
               zerolinewidth = 2, 
               gridcolor = 'ffff'))
  })
  
}

shinyApp(ui, server)


