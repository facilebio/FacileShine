devtools::load_all(".")

ddir <- "/Users/steve/workspace/projects/revir/data/facileisr"

meta.fn <- file.path(ddir, "meta.yaml")

shiny::shinyApp(
  ui = shiny::fluidPage(
    FacileShine::facileDataSetSelectInput("fdslist"),
    DT::DTOutput("samples")),
  
  server = function(input, output) {
    fdslist <- FacileShine::facileDataSetSelectServer(
      "fdslist",
      datadir = shiny::reactive(ddir),
      metafn = meta.fn)
    
    rfds <- FacileShine::facileDataStoreServer(
      "rfds", fdslist$path,
      default_covariate = "group",
      user = "you")
    
    output$samples <- DT::renderDataTable({
      shiny::req(rfds$fds()) |> 
        FacileData::samples() |> 
        FacileData::with_sample_covariates()
    })
  })
