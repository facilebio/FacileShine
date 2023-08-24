.onLoad <- function(libname, pkgname) {
  pkg.opts <- list(
    FacileShine.spinner_type = "circle",
    FacileShine.spinner_color = "#FF7F00") # "#377EB8"

  opts <- options()
  toset <- !(names(pkg.opts) %in% names(opts))
  if (any(toset)) {
    options(pkg.opts[toset])
  }

  invisible()
}
