#' Module to map categorical aesthetics to data.
#' 
#' @export
categoricalAestheticMapServer <- function(id, rfds, color = FALSE, 
                                          shape = FALSE, facet = FALSE,
                                          hover = FALSE,  group = FALSE, ...,
                                          with_none = TRUE, 
                                          exclude = reactive(NULL),
                                          debug = FALSE) {
  assert_class(rfds, "ReactiveFacileDataStore")
  
  moduleServer(id, function(input, output, session) {
    aes_covs <- .module_list(color = color, shape = shape, facet = facet,
                             hover = hover, group = group)
    all_aes_vars <- attr(aes_covs, "all_aes")
    
    aes_mods <- sapply(names(aes_covs), function(aes_name) {
      categoricalSampleCovariateSelectServer(
        aes_name, rfds, with_none = aes_name != "hover", include1 = FALSE,
        include_sample_id = aes_name == "hover")
    }, simplify = FALSE)
    
    aes_values <- reactive({
      sapply(all_aes_vars, function(aes_name) {
        amod <- aes_mods[[aes_name]]
        aval <- if (is.null(amod)) NULL else amod$selected()
        if (unselected(aval)) NULL else aval
      }, simplify = FALSE)
    }, label = "aes_values")
    
    if (debug) {
      output$debug <- shiny::renderText({
        avals <- aes_values()
        vals <- lapply(names(avals), function(aname) {
          value <- avals[[aname]]
          if (is.null(value)) {
            value <- "NULL"
          } else {
            value <- paste(value, collapse = ",")
          }
          paste0(aname, ": ", value)
        })
        vals$sep <- "\n"
        do.call(paste, vals)
      })
    }
    
    vals <- list(
      map = aes_values,
      modules = aes_mods,
      .ns = session$ns)
    class(vals) <- c("CategoricalAesMapModule", "AesMapModule")
    vals    
  })

}

#' @noRd
#' @export
categoricalAestheticMapInput <- function(id, color = FALSE, shape = FALSE,
                                         facet = FALSE, hover = FALSE, 
                                         group = FALSE, horizontal = TRUE,
                                         ..., debug = FALSE) {
  ns <- NS(id)
  aes_covs <- .module_list(color = color, shape = shape, facet = facet,
                           hover = hover, group = group)
  ncol <- floor(12 / length(aes_covs))
  
  aes.tags <- sapply(names(aes_covs), function(aes_name) {
    label <- tools::toTitleCase(aes_name)
    ui <- categoricalSampleCovariateSelectInput(
      ns(aes_name), label = label, multiple = aes_name == "hover", 
      include_sample_id = aes_name == "hover", ..., debug = debug)
    if (horizontal) ui <- shiny::column(ncol, ui)
    ui
  }, simplify = FALSE)

  if (horizontal) {
    aes.tags <- shiny::fluidRow(aes.tags)
  }

  if (debug) {
    aes.tags <- shiny::tagList(
      aes.tags,
      shiny::fluidRow(shiny::verbatimTextOutput(ns("debug")))
    )
  }
  
  aes.tags
}

#' Adds aesthetic covariates from a CategoricalAesMapModule to a facile_frame.
#' 
#' Note that this can only happen in a reactive context
#'
#' @export
#' @param aes the `CategoricalAesMapModule`
#' @param to the facile_frame to add it to
add_aesthetic_covariates <- function(aes, to, ...) {
  assert_class(aes, "AesMapModule")
  if (is(to, "reactive")) to <- req(to())
  req(is(to, "facile_frame"))
  
  # aes.map <- aes_mod$map()
  # aes.map <- aes.map[!is.null(aes.map)]
  # # The names of the aesthetics within the module need to be matched to the
  # # name of the aes_* parameters in the FacileViz::with_aesthetics function.
  # 
  # arename.all <- c(
  #   color = "color_aes",
  #   shape = "shape_aes",
  #   hover = "hover",
  #   size = "size_aes")
  
  amap <- aes$map()
  fetch <- setdiff(unlist(amap), colnames(to))
  if (length(fetch)) {
    to <- with_sample_covariates(to, fetch)
  }
  
  to
}

# Helper Functions =============================================================

#' @noRd
.module_list <- function(color = FALSE, shape = FALSE, facet = FALSE,
                         hover = FALSE, group = FALSE) {
  mod.include <- list(
    color = color, shape = shape, facet = facet, 
    hover = hover, group = group)
  out <- mod.include[unlist(mod.include)]
  attr(out, "all_aes") <- names(mod.include)
  out
}

