# Labeled acts as something of an interface to reactive modules.
#
# Modules that implement this interface must return `label` and `name` reactive
# elemengs within them.
#
# We use these when something (like a `quantitativeAssayDataSelect`) needs
# a "computer friendly" name for itself (`name()`), or a more human readable
# name (`label()`)

name <- function(x, ...) {
  UseMethod("name", x)
}

name.Labeled <- function(x, ...) {
  assert_reacting()
  out <- x[["name"]]
  if (!is(out, "reactive")) "unnamed" else out()
}

label <- function(x, ...) {
  UseMethod("label")
}

label.Labeled <- function(x, ...) {
  assert_reacting()
  out <- x[["label"]]
  if (!is(out, "reactive")) "unlabeled" else out()
}
