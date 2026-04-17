options(
  Ncpus = max(1L, parallel::detectCores(logical = FALSE)),
  repos = c(CRAN = "https://cloud.r-project.org")
)

if (!requireNamespace("BiocManager", quietly = TRUE)) {
  install.packages("BiocManager")
}

options(repos = BiocManager::repositories())

if (!requireNamespace("pak", quietly = TRUE)) {
  install.packages("pak")
}

pak::pkg_install(c(
  "github::facilebio/FacileBiocData",
  "github::facilebio/FacileData",
  "github::facilebio/FacileViz"
))

pak::pkg_install(c(
  "devtools",
  "httpgd",
  "knitr",
  "languageserver",
  "lintr",
  "pkgdown",
  "rmarkdown",
  "roxygen2",
  "styler",
  "testthat"
))

install.packages(
  "vscDebugger",
  repos = c(
    manuelhentschel = "https://manuelhentschel.r-universe.dev",
    getOption("repos")
  )
)

pak::local_install_deps(dependencies = TRUE)
