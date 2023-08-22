#' Convenience wrapper to require specified packages
#'
#' @noRd
#' @param pkg A character vector of packages to require
#' @param quietly defaults to true
#' @param ... passed into [requireNamespace()]
reqpkg <- function(pkg, quietly = TRUE, ...) {
  assert_character(pkg)
  for (p in pkg) {
    if (!requireNamespace(p, ..., quietly = quietly)) {
      stop("'", p, "' package required, please install it.", call. = FALSE)
    }
  }
}

#' Convenience funciton for tictoc::tic(..., quiet = TRUE)
toq <- function(log = FALSE, quiet = TRUE, func.toc = tictoc::toc.outmsg, ...,
                digits = 3) {
  out <- tictoc::toc(log = log, quiet = quiet, func.toc = func.toc, ...)
  out$seconds <- round(out$toc - out$tic, digits)
  out$ss <- paste0(out$seconds, "s")
  invisible(out)
}