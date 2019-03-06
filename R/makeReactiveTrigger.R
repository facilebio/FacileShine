#' Create a reactive trigger to listen to and propagate application events
#'
#' A reactive trigger can be used when you want to be able to explicitly trigger
#' a reactive expression. You can think of it as being similar to an action
#' button, except instead of clicking on a button to trigger an expression, you
#' can programatically cause the trigger.
#'
#' This concept and code was created by Joe Cheng, and you can find more
#' references about it at these links:
#'
#' * https://github.com/daattali/advanced-shiny/tree/master/reactive-trigger,
#'   the documentation in this funciton is largely taken from this resource.
#' * https://github.com/MangoTheCat/dynshiny#triggering-ui-changes
#'
#' Reactive triggers are instantiated like so:
#' `myTrigger <- makeReactiveTrigger()`. To use it, put `myTrigger$depend()` in
#' any reactive code that should re-run when the trigger is fired, and call
#' `myTrigger$trigger()` to set off the trigger.
#'
#' @export
#' @return a list with `$depend()` and `$trigger()` to be used in reactive
#'   expressions across your app.
makeReactiveTrigger <- function() {
  rv <- reactiveValues(a = 0L)
  out <- list(
    depend = function() {
      rv$a
      invisible()
    },
    counter = function() isolate(rv$a), # for debugging purposes
    trigger = function() {
      rv$a <- isolate(rv$a + 1L)
    }
  )
  class(out) <- "ReactiveTrigger"
  out
}
