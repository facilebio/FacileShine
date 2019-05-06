#' Launch a workbench shiny app
#'
#'
#' @family workbench functions
#' @export
#' @importFrom shiny shinyApp
#' @importFrom shinydashboardPlus
#'   dashboardPagePlus
#' @importFrom yaml yaml.load_file
#' @param x The FacileDataStore
#' @param user the user accessing it (used for "custom_key" stuff in annotations)
#' @return shiny app
staticbenchplus <- function(config = NULL, user = Sys.getenv("USER"), ...) {
  if (is.null(config)) {
    config <- system.file("extdata", "workbench-config.yaml",
                          package = "FacileShine")
  }
  assert_file_exists(config, access = "r", extension = c("yaml", "yml"))
  assert_string(user)

  config <- yaml.load_file(config)

  options(
    facile.bs4dash = FALSE,
    facile.staticbench.config = config,
    facile.staticbench.user = user)

  shinyApp(
    ui = dashboardPagePlus(
      # skin = "yellow",
      # sidebar_background = "light",
      title = "Facile Workbench",
      header = .statibenchplus_header(),
      sidebar = .statibenchplus_left(),
      rightsidebar = .staticbenchplus_right(),
      footer = .staticbenchplus_footer(),
      body = .staticbenchplus_body()),

    server = .staticplus_server)
}


.dnli_logo_href <- "https://static1.squarespace.com/static/553936d6e4b07f1e049ab8b2/t/5548e9d3e4b06b185ec422eb/1521492790254/?format=1500w"

# Left Side Bar ----------------------------------------------------------------

#' @noRd
#' @importFrom shiny icon
#' @importFrom shinydashboard
#'   dashboardSidebar
#'   menuItem
#'   menuSubItem
#'   sidebarMenu
#'   tabItems
.statibenchplus_left <- function() {
  dashboardSidebar(
    width = 275,
    sidebarMenu(
      id = "leftsidebar",
      menuItem(
        text = "Dataset Management",
        icon = icon("database", lib = "font-awesome"),
        startExpanded = TRUE,
        menuSubItem(
          text = "Dataset Selection",
          tabName = "dataselecttab",
          icon = icon("object-group", lib = "font-awesome")),
        menuSubItem(
          text = "Sample Annotations",
          tabName = "sampleannotab",
          icon = icon("table", lib = "font-awesome"))),
      menuItem(
        text = "Statistical Testing",
        icon = icon("calculator", lib = "font-awesome"),
        startExpanded = FALSE,
        menuSubItem(
          text = "Differential Expression & GSEA",
          tabName = "dgetab",
          icon = icon("list-ol", lib = "font-awesome"))),
      menuItem(
        text = "Exploratory Data Analysis",
        icon = icon("binoculars", lib = "font-awesome"),
        startExpanded = FALSE,
        menuSubItem(
          text = "Dimensionality Reduction",
          tabName = "dimredtab",
          # compress-arrows-alt
          icon = icon("compress", lib = "font-awesome")),
        menuSubItem(
          text = "Scatter Plot",
          tabName = "scatterplottab",
          icon = icon("chart-line", lib = "font-awesome")),
        menuSubItem(
          text = "Box Plot",
          tabName = "boxplottab",
          icon = icon("chart-bar", lib = "font-awesome"))),
      menuItem(
        text = "Reporting",
        startExpanded = FALSE,
        icon = icon("file-contract", lib = "font-awesome"),
        menuSubItem(
          text = "Report Generation",
          tabName = "reportingtab",
          icon = icon("pencil-alt", lib = "font-awesome")))
    )
  )
}

# Body -------------------------------------------------------------------------

# @importFrom FacileAnalysis
#   fdgeAnalysisUI
# @importFrom FacileShine
#   facileScatterPlotUI
#   singleFilteredReactiveFacileDataStoreUI

#' @noRd
#' @importFrom shiny
#'   HTML
#'   tagList
#'   tags
#' @importFrom shinydashboard
#'   dashboardBody
#'   tabItem
#'   tabItems
.staticbenchplus_body <- function() {
  dashboardBody(
    tags$head(tags$style(HTML('
      .main-footer {
        background-color: #BFBFBF; color: white;
      }
    '))),
    tabItems(
      tabItem(
        "dataselecttab",
        datasetSelectUI("dselect"),
        singleFilteredReactiveFacileDataStoreUI("rfds")),
      tabItem("dimredtab", "Dimensionality Reduction"),
      tabItem("scatterplottab", facileScatterPlotUI("scatter")),
      tabItem("boxplottab", "Box Plot"),
      # tabItem("dgetab", fdgeAnalysisUI("fdgea")),
      tabItem("reportingtab", "Report Generation")))
}

# Server Function --------------------------------------------------------------
# Tie the module id's in here mostky to to the things in the body

# @importFrom FacileAnalysis
#   fdgeAnalysis
# @importFrom FacileShine
#   facileScatterPlot
#   singleFilteredReactiveFacileDataStore

#' @noRd
#' @importFrom shiny
#'   callModule
.staticplus_server <- function(input, output) {
  user <- getOption("facile.staticbench.user", NULL)
  config <- load_config(getOption("facile.staticbench.config", NULL))
  assert_string(user)

  dselect <- callModule(datasetSelect, "dselect", config = config, user = user)
  rfds <- callModule(singleFilteredReactiveFacileDataStore, "rfds",
                     dselect$path, user)
  # fdgea <- callModule(fdgeAnalysis, "fdgea", rfds)
  scatter <- callModule(facileScatterPlot, "scatter", rfds, ndim = 3)
}


# Footer -----------------------------------------------------------------------

#' @noRd
#' @importFrom shinydashboardPlus
#'   dashboardFooter
.staticbenchplus_footer <- function() {
  dashboardFooter(
    tags$a(
      href = "https://denalitherapeutics.com",
      target = "_blank",
      tags$img(src = .dnli_logo_href, style = "height: 1em",
               alt = "Denali Therapeutics")))
}

# Header -----------------------------------------------------------------------

#' @noRd
#' @importFrom shinydashboardPlus
#'   dashboardHeaderPlus
.statibenchplus_header <- function() {
  dashboardHeaderPlus(
    titleWidth = 275,
    title = "Facile Workbench",
    enable_rightsidebar = TRUE,
    rightSidebarIcon = "gears")
}

# Right Bar --------------------------------------------------------------------

#' @noRd
#' @importFrom shinydashboardPlus
#'   rightSidebar
#'   rightSidebarTabContent
.staticbenchplus_right <- function() {
  rightSidebar(
    width = 300,
    background = "light",
    rightSidebarTabContent(
      id = 1,
      icon = "clipboard",
      title = "Results",
      "Result Shelf"),
    rightSidebarTabContent(
      id = 2,
      icon = "table",
      "Sample Annotations"),
    rightSidebarTabContent(
      id = 3,
      icon = "list",
      "Gene Annotations"))
}


