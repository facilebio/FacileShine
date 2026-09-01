test_that("multi-value selectize options include copy and paste defaults", {
  options <- multiValueSelectizeOptions()
  handler <- as.character(options$onInitialize)

  expect_identical(options$plugins, list("remove_button"))
  expect_true(inherits(options$onInitialize, "AsIs"))
  expect_match(handler, "addEventListener('paste'", fixed = TRUE)
  expect_match(handler, "navigator.clipboard.writeText", fixed = TRUE)
  expect_match(handler, "s.setValue(next, false)", fixed = TRUE)
})

test_that("caller options override multi-value selectize defaults", {
  custom_handler <- I("function() {}")
  options <- multiValueSelectizeOptions(list(
    onInitialize = custom_handler,
    maxItems = 10
  ))

  expect_identical(options$plugins, list("remove_button"))
  expect_identical(options$onInitialize, custom_handler)
  expect_identical(options$maxItems, 10)
})
