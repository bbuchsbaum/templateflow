test_that("tf_datalad_available returns logical", {
  result <- tf_datalad_available()
  expect_true(is.logical(result))
  expect_length(result, 1)
})

test_that("tf_datalad_install errors when datalad unavailable", {
  skip_if(tf_datalad_available(), "DataLad is installed")
  expect_error(
    tf_datalad_install("/tmp/test", source = "https://example.com"),
    class = "templateflow_error"
  )
})

test_that("tf_datalad_get errors when datalad unavailable", {
  skip_if(tf_datalad_available(), "DataLad is installed")
  expect_error(
    tf_datalad_get("file.txt", dataset = "/tmp"),
    class = "templateflow_error"
  )
})

test_that("tf_datalad_update errors when datalad unavailable", {
  skip_if(tf_datalad_available(), "DataLad is installed")
  expect_error(
    tf_datalad_update(dataset = "/tmp"),
    class = "templateflow_error"
  )
})

test_that("use_datalad config is parsed from env var", {
  withr::with_envvar(
    c(TEMPLATEFLOW_USE_DATALAD = "true"),
    {
      cfg <- tf_default_config(root = tempdir())
      expect_true(cfg$use_datalad)
    }
  )

  withr::with_envvar(
    c(TEMPLATEFLOW_USE_DATALAD = "false"),
    {
      cfg <- tf_default_config(root = tempdir())
      expect_false(cfg$use_datalad)
    }
  )
})

test_that("cache wipe is no-op for datalad backend", {
  tmp <- file.path(tempdir(), "tfdatalad-wipe")
  if (dir.exists(tmp)) unlink(tmp, recursive = TRUE, force = TRUE)
  dir.create(tmp, recursive = TRUE)
  writeLines("test", file.path(tmp, "testfile"))
  on.exit(unlink(tmp, recursive = TRUE, force = TRUE), add = TRUE)

  cfg <- tf_default_config(root = tmp, use_datalad = TRUE)
  cache <- TemplateFlowCache(config = cfg)

  suppressMessages(tf_cache_wipe(cache))
  # Directory should still exist since wipe is no-op for datalad
  expect_true(dir.exists(tmp))
  expect_true(file.exists(file.path(tmp, "testfile")))
})
