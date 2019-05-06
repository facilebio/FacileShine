#' Convenience function to launch the current version of the app
#'
#' @family workbench functions
#' @export
launch <- function(config = NULL, user = Sys.getenv("USER"),
                   ...,
                   .log_level_shine = "warn",
                   .log_level_data = "warn",
                   .log_level_analysis = "warn") {
  # logopts <- options(
  #   facile.log.level.fshine = .log_level_shine,
  #   facile.log.level.fdata = .log_level_data,
  #   facile.log.level.fanalysis = .log_level_analysis)
  # on.exit(options(logopts))
  staticbenchplus(config = config, user = user, ...)
}
