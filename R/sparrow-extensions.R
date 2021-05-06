#' The initialized generics are a facile thing, so this function can't be
#' officially exported as an S3 generic from sparrow.shiny
#'
#' sparrow.shiny defines initialized as an internal S3 generic, and we
#' grab its implementation from there and exported it here.
#'
#' @noRd
#' @export
initialized.ReactiveGeneSetDb <- sparrow.shiny:::initialized.ReactiveGeneSetDb

