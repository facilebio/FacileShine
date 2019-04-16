#' Checks if the return value from a selectInput looks like it's unselected
#'
#' For some reason, sometimes a selectInput returns `NULL` and other times
#' it returns `""`, so I'm just making this utility function to deal with that
#'
#' @export
#' @param value The (character) object returned from a `selectInput`
unselected <- function(value, ignore = c("---", "__initializing__", "")) {
  if (is.null(value)) return(TRUE)
  # Otherwise this is a character
  length(value) == 0L || nchar(value) == 0L || all(value %in% ignore)
}
