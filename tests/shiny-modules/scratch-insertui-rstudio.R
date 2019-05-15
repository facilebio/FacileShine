# https://community.rstudio.com/t/dynamically-removing-ui-elements-using-insertui-and-removeui/22617/3

library(shiny)
library(shinydashboard)

# Dummy data

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
    radioButtons("add_elements","", c("Element1",	"Element2"))
  ),
  dashboardBody(
    fluidRow( tags$div(id="placeholder")
    )
  ))

server <- function(input, output, session) {
  # Observer
  observeEvent(input$add, {
    # make ids for the plot, the remove button, and the element to remove
    id_add <- paste0(input$add, input$add_elements)
    remove_id <- paste0("remove_", id_add)
    ele_id <- paste0("ele_", id_add)

    # insert all of the ui at once inside a larger UI element (div) with a id value
    insertUI(
      selector = '#placeholder',
      where = "afterEnd",
      ui= tags$div(
        id = ele_id,
        switch(input$add_elements,
               'Element1'= plotOutput(id_add),
               'Element2' = plotOutput(id_add)
        ),
        actionButton(remove_id, "Remove")
      )
    )

    if (input$add_elements == "Element1") {
      output[[id_add]] <- renderPlot({
        plot(data[,1],data[,2])
      })
    } else if (input$add_elements == "Element2") {
      output[[id_add]] <- renderPlot({
        plot(data[,1],data[,4])
      })
    }

    ## Remove Elements ###
    # when the specific remove button is clicked, remove the larger UI element containing the plot and button
    observeEvent(input[[remove_id]], {
      removeUI(
        selector = paste0("#", ele_id)
      )
    })

  })
}

shinyApp(ui = ui, server = server)

