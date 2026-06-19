test_that("tf_entities() with no args returns the global entity vocabulary", {
  ents <- tf_entities()
  expect_type(ents, "character")
  expect_true(all(c("suffix", "extension", "resolution", "desc") %in% ents))
  # values without template should error
  expect_error(tf_entities(values = TRUE), "values = TRUE")
})

test_that("tf_entities(template=) returns entities present in cache", {
  tmp <- file.path(tempdir(), "tfentities-cache")
  if (dir.exists(tmp)) unlink(tmp, recursive = TRUE, force = TRUE)
  client <- TemplateFlowClient(root = tmp)

  ents <- tf_entities(client, template = "MNI152Lin")
  expect_type(ents, "character")
  expect_true("suffix" %in% ents)

  vals <- tf_entities(client, template = "MNI152Lin", values = TRUE)
  expect_type(vals, "list")
  expect_true("suffix" %in% names(vals))
  expect_true(length(vals$suffix) > 0)
  # T1w is one of the canonical MNI152Lin suffixes
  expect_true("T1w" %in% vals$suffix)
})

test_that("tf_describe() returns a tf_description with template values", {
  tmp <- file.path(tempdir(), "tfdescribe-cache")
  if (dir.exists(tmp)) unlink(tmp, recursive = TRUE, force = TRUE)
  client <- TemplateFlowClient(root = tmp)

  d <- tf_describe(client, "MNI152Lin")
  expect_s3_class(d, "tf_description")
  expect_equal(d$template, "MNI152Lin")
  expect_true(is.list(d$metadata))
  expect_true(is.list(d$entities))
  expect_true("suffix" %in% names(d$entities))

  out <- utils::capture.output(print(d))
  expect_true(any(grepl("MNI152Lin", out)))
  expect_true(any(grepl("Entities present", out)))
})

test_that("tf_describe() rejects invalid template arguments", {
  expect_error(tf_describe(), "single template identifier")
  expect_error(tf_describe(template = c("a", "b")), "single template identifier")
})
