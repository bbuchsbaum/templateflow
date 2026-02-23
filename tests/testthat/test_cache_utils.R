test_that("tf_cache_stats returns correct structure", {
  tmp <- file.path(tempdir(), "tfcache-stats")
  if (dir.exists(tmp)) unlink(tmp, recursive = TRUE, force = TRUE)
  on.exit(unlink(tmp, recursive = TRUE, force = TRUE), add = TRUE)

  withr::with_options(
    list(templateflow.test.forcebundled = TRUE),
    {
      client <- TemplateFlowClient(root = tmp)
      stats <- tf_cache_stats(client$cache)
      expect_true(is.list(stats))
      expect_true("cache_files" %in% names(stats))
      expect_true("cache_bytes" %in% names(stats))
      expect_true("cache_size_human" %in% names(stats))
      expect_true(stats$cache_files > 0)
    }
  )
})

test_that("tf_cache_scan detects zero-byte files", {
  tmp <- file.path(tempdir(), "tfcache-scan")
  if (dir.exists(tmp)) unlink(tmp, recursive = TRUE, force = TRUE)
  on.exit(unlink(tmp, recursive = TRUE, force = TRUE), add = TRUE)

  withr::with_options(
    list(templateflow.test.forcebundled = TRUE),
    {
      client <- TemplateFlowClient(root = tmp)
      scan <- tf_cache_scan(client$cache)
      expect_true(is.list(scan))
      expect_true("zero" %in% names(scan))
      expect_true("xml" %in% names(scan))
      # Skeleton stubs are zero-byte
      expect_true(length(scan$zero) > 0)
    }
  )
})

test_that("tf_cache_refresh rebuilds layout", {
  tmp <- file.path(tempdir(), "tfcache-refresh")
  if (dir.exists(tmp)) unlink(tmp, recursive = TRUE, force = TRUE)
  on.exit(unlink(tmp, recursive = TRUE, force = TRUE), add = TRUE)

  withr::with_options(
    list(templateflow.test.forcebundled = TRUE),
    {
      client <- TemplateFlowClient(root = tmp)
      cache <- tf_cache_refresh(client$cache)
      expect_s3_class(cache$layout, "TemplateFlowLayout")
    }
  )
})

test_that("tf_human_size formats bytes correctly", {
  expect_equal(tf_human_size(0), "0 B")
  expect_equal(tf_human_size(500), "500.0 B")
  expect_equal(tf_human_size(1024), "1.0 KB")
  expect_equal(tf_human_size(1048576), "1.0 MB")
})

test_that("tf_is_xml_error detects S3 error responses", {
  tmpfile <- tempfile(fileext = ".xml")
  on.exit(unlink(tmpfile), add = TRUE)

  xml_path <- testthat::test_path("data", "error_response.xml")
  file.copy(xml_path, tmpfile, overwrite = TRUE)
  expect_true(tf_is_xml_error(tmpfile))

  # Normal file should not be flagged
  writeLines("hello world", tmpfile)
  expect_false(tf_is_xml_error(tmpfile))
})
