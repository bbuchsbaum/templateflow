test_that("tf_abort raises templateflow_error", {
  expect_error(
    tf_abort("test error"),
    class = "templateflow_error"
  )
})

test_that("tf_abort_network raises templateflow_network_error", {
  expect_error(
    tf_abort_network("network fail"),
    class = "templateflow_network_error"
  )
  # Also inherits from base class
  expect_error(
    tf_abort_network("network fail"),
    class = "templateflow_error"
  )
})

test_that("tf_abort_not_found raises templateflow_not_found", {
  expect_error(
    tf_abort_not_found("missing"),
    class = "templateflow_not_found"
  )
  expect_error(
    tf_abort_not_found("missing"),
    class = "templateflow_error"
  )
})

test_that("tf_abort_invalid_filter raises templateflow_invalid_filter", {
  expect_error(
    tf_abort_invalid_filter("bad filter"),
    class = "templateflow_invalid_filter"
  )
})

test_that("tf_abort_cache raises templateflow_cache_error", {
  expect_error(
    tf_abort_cache("cache broken"),
    class = "templateflow_cache_error"
  )
})

test_that("conditions can be caught with tryCatch", {
  result <- tryCatch(
    tf_abort_not_found("no file"),
    templateflow_not_found = function(e) "caught_not_found",
    templateflow_error = function(e) "caught_base"
  )
  expect_equal(result, "caught_not_found")

  result2 <- tryCatch(
    tf_abort_cache("cache issue"),
    templateflow_not_found = function(e) "caught_not_found",
    templateflow_error = function(e) "caught_base"
  )
  expect_equal(result2, "caught_base")
})
