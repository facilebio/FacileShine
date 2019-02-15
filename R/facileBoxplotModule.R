#' @export
#' @rdname facileBoxplot
facileBoxplot <- function(input, output, session, rfds, ...) {
  assert_class(rfds, "ReactiveFacileDataStore")

  aesthetx <- callModule(categoricalAestheticMap, "aes", rfds, ...)
  quant <- callModule(quantitativeTraitSelect, "quant", rfds, ...)

  rdat <- reactive({

  })
  output$boxplot <- renderPlotly({

  })
}

#' @export
#' @rdname facileBoxplot
facileBoxplotUI <- function(id, ...) {
  ns <- NS(id)

  tagList(
    fluidRow(
      column(12, categoricalAestheticMapUI(ns("aes")))),
    fluidRow(
      column(6, quantitativeTraitSelectUI(ns("quant")))),
    fluidRow(
      column(12, plotlyOutput(ns("boxplot")))))
}

# Scratch ======================================================================
if (FALSE) {
  # https://plotly-book.cpsievert.me/boxplots.html
  library(plotly)

  y1 <- c(0.75, 5.25, 5.5, 6, 6.2, 6.6, 6.80, 7.0, 7.2, 7.5, 7.5, 7.75, 8.15,
          8.15, 8.65, 8.93, 9.2, 9.5, 10, 10.25, 11.5, 12, 16, 20.90, 22.3, 23.25)
  y2 <- c(0.75, 5.25, 5.5, 6, 6.2, 6.6, 6.80, 7.0, 7.2, 7.5, 7.5, 7.75, 8.15,
          8.15, 8.65, 8.93, 9.2, 9.5, 10, 10.25, 11.5, 12, 16, 20.90, 22.3, 23.25)
  y3 <- c(0.75, 5.25, 5.5, 6, 6.2, 6.6, 6.80, 7.0, 7.2, 7.5, 7.5, 7.75, 8.15,
          8.15, 8.65, 8.93, 9.2, 9.5, 10, 10.25, 11.5, 12, 16, 20.90, 22.3, 23.25)
  y4 <- c(0.75, 5.25, 5.5, 6, 6.2, 6.6, 6.80, 7.0, 7.2, 7.5, 7.5, 7.75, 8.15,
          8.15, 8.65, 8.93, 9.2, 9.5, 10, 10.25, 11.5, 12, 16, 20.90, 22.3, 23.25)

  p <- plot_ly(type = 'box') %>%
    add_boxplot(y = y1, jitter = 0.3, pointpos = -1.8, boxpoints = 'all',
                marker = list(color = 'rgb(7,40,89)'),
                line = list(color = 'rgb(7,40,89)'),
                name = "All Points") %>%
    add_boxplot(y = y1, jitter = 0.3, pointpos = 0, boxpoints = 'all',
                marker = list(color = 'rgb(7,40,89)'),
                line = list(color = 'rgb(7,40,89)'),
                name = "All Points 2") %>%
    add_boxplot(y = y2, name = "Only Whiskers", boxpoints = FALSE,
                marker = list(color = 'rgb(9,56,125)'),
                line = list(color = 'rgb(9,56,125)')) %>%
    add_boxplot(y = y3, name = "Suspected Outlier", boxpoints = 'suspectedoutliers',
                marker = list(color = 'rgb(8,81,156)',
                              outliercolor = 'rgba(219, 64, 82, 0.6)',
                              line = list(outliercolor = 'rgba(219, 64, 82, 1.0)',
                                          outlierwidth = 2)),
                line = list(color = 'rgb(8,81,156)')) %>%
    add_boxplot(y = y4, name = "Whiskers and Outliers", boxpoints = 'outliers',
                marker = list(color = 'rgb(107,174,214)'),
                line = list(color = 'rgb(107,174,214)')) %>%
    layout(title = "Box Plot Styling Outliers")

  library(dplyr)
  dat <- tibble(
    y1 = y1, y2 = y2, y3 = y3
  )
}
