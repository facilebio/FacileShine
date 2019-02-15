#' Test if we are running inside a reactive context
#'
#' @export
#' @rdname assert_reacting
#' @importFrom shiny getDefaultReactiveDomain
assert_reacting <- function() {
  rd <- getDefaultReactiveDomain()
  if (is.null(rd)) {
    stop("Not executing in a reactive context")
  }
  invisible(rd)
}

# Scratch ======================================================================
# #' @export
# #' @rdname check_reacting
# check_reacting <- function(...) {
#   rd <- getDefaultReactiveDomain()
#   if (is.null(rd)) {
#     "Not executing in a reactive context"
#   } else {
#     TRUE
#   }
# }
#
# #' @export
# #' @rdname test_reactive
# test_reacting <- function(...) {
#   identical(check_reacting(...), TRUE)
# }
