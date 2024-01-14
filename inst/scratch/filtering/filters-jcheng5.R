# An example of connected filtering by Joe Cheng, originally from 2017.
# Google Groups Post: https://groups.google.com/g/shiny-discuss/c/Q1ZvnjDCzUM/m/f9WOeXFeCQAJ
# gist: https://gist.github.com/jcheng5/76cffbbdd9db0b0e971fd34f575fa45b

library(shiny)

columnFilterUI <- function(id) {
  ns <- NS(id)
  uiOutput(ns("filter_container"))
}

columnFilter <- function(input, output, session, df, col_num, choice_filter) {
  # This renders a selectInput and only re-renders when the selected data
  # frame changes. (i.e. it doesn't re-render when filters change state.)
  output$filter_container <- renderUI({
    # Don't render if col_num is > actual number of cols
    req(col_num <= ncol(df()))
    
    freezeReactiveValue(input, "filter_value")
    selectInput(session$ns("filter_value"), names(df())[[col_num]],
                choices = sort(unique(df()[,col_num,drop=TRUE])),
                multiple = TRUE)
  })
  
  # When the other filters change, update this filter to remove rows that
  # are filtered out by the other filters' criteria. (We also add in the
  # currently selected values for this filter, so that changing other
  # filters does not cause this filter's selected values to be unselected;
  # while that behavior might make sense logically, it's a poor user
  # experience.)
  observeEvent(choice_filter(), {
    current_values <- input$filter_value
    
    updateSelectInput(session, "filter_value",
                      choices = sort(unique(c(current_values, df()[choice_filter(),col_num,drop=TRUE]))),
                      selected = current_values
    )
  })
  
  # Return a reactive that is a row index of selected rows, according to
  # just this filter. If this filter shouldn't be taken into account
  # because its col_num is too high, or if there are no values selected,
  # just return TRUE to accept all rows.
  reactive({
    if (col_num > ncol(df())) {
      TRUE
    } else if (!isTruthy(input$filter_value)) {
      TRUE
    } else {
      df()[,col_num,drop=TRUE] %in% input$filter_value
    }
  })
}

columnFilterSetUI <- function(id, maxcol, colwidth) {
  ns <- NS(id)
  
  fluidRow(
    lapply(1:maxcol, function(i) {
      column(colwidth, columnFilterUI(ns(paste0("col", i)))
      )
    })
  )
}

columnFilterSet <- function(input, output, session, df, maxcol) {
  # Each column filter needs to only display the choices that are
  # permitted after all the OTHER filters have had their say. But
  # each column filter must not take its own filter into account
  # (hence we do filter[-col], not filter, in the reactive below).
  create_choice_filter <- function(col) {
    reactive({
      filter_values <- lapply(filters[-col], do.call, args = list())
      Reduce(`&`, filter_values, TRUE)
    })
  }
  
  # filters is a list of reactive expressions, each of which is a
  # logical vector of rows to be selected.
  filters <- lapply(1:maxcol, function(i) {
    callModule(columnFilter, paste0("col", i), df, i, create_choice_filter(i))
  })
  
  reactive({
    # Unpack the list of reactive expressions to a list of logical vectors
    filter_values <- lapply(filters, do.call, args = list())
    # Combine all the logical vectors using & operator
    selected_rows <- Reduce(`&`, filter_values, TRUE)
    # Return the data frame, filtered by the selected rows
    df()[selected_rows,]
  })
}

ui <- fluidPage(
  selectInput("dataset", "Dataset", c("mtcars", "pressure", "cars"), selected = "mtcars"),
  columnFilterSetUI("filterset", maxcol = 4, colwidth = 3),
  DT::dataTableOutput("table")
)

server <- function(input, output, session) {
  selected_data <- reactive({
    get(input$dataset, "package:datasets")
  })
  filtered_data <- callModule(columnFilterSet, "filterset", df = selected_data, maxcol = 4)
  output$table <- DT::renderDataTable({ filtered_data() })
}

shinyApp(ui, server)
