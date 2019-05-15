# https://stackoverflow.com/questions/54762013

library(shiny)
library(shinydashboard)

# Example data

a<-(letters)
b<-rnorm(length(letters), 4,2)
c<-rnorm(length(letters), 10,15)
d<-c(1:10,20:30,45:49)

data<-data.frame(a,b,c,d)
names(data)<-c("name","v1","v2","v3")

# UI

ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(
    actionButton("add", "Add"),
    radioButtons("add_elements","", c("Element1",   "Element2"))
  ),
  dashboardBody(
    uiOutput("myUI")
  ))

# Server Logic

server <- function(input, output, session) {

  alld <- reactiveValues()
  alld$ui <- list()

  output$myUI <- renderUI({
    alld$ui
  })

  # Observer
  observeEvent(input$add, {
    id_add <- length(alld$ui)+1

    alld$ui[[id_add]] <-  list(
      plotOutput(paste0("plt",id_add)),
      actionButton(paste0("remove_button", id_add), "Remove")
    )


    if (input$add_elements == "Element1"){
      output[[paste0("plt",id_add)]] <- renderPlot(plot(data[,1],data[,2]))
    } else {
      output[[paste0("plt",id_add)]] <- renderPlot(plot(data[,1],data[,4]))
    }
  })
}

shinyApp(ui = ui, server = server)
