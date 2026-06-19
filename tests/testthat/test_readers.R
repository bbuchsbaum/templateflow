test_that("tf_file_extension handles compound extensions", {
  expect_equal(tf_file_extension("brain.nii.gz"), ".nii.gz")
  expect_equal(tf_file_extension("mesh.surf.gii"), ".surf.gii")
  expect_equal(tf_file_extension("data.tsv"), ".tsv")
  expect_equal(tf_file_extension("meta.json"), ".json")
  expect_equal(tf_file_extension("noext"), "")
})

test_that("tf_read_file reads JSON files", {
  tmp <- tempfile(fileext = ".json")
  on.exit(unlink(tmp), add = TRUE)
  writeLines('{"key": "value"}', tmp)

  result <- tf_read_file(tmp)
  expect_true(is.list(result))
  expect_equal(result$key, "value")
})

test_that("tf_read_file reads TSV files", {
  tmp <- tempfile(fileext = ".tsv")
  on.exit(unlink(tmp), add = TRUE)
  writeLines(c("col1\tcol2", "a\tb", "c\td"), tmp)

  result <- tf_read_file(tmp)
  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 2)
  expect_equal(names(result), c("col1", "col2"))
})

test_that("tf_read_file returns path for unknown extensions", {
  tmp <- tempfile(fileext = ".xyz")
  on.exit(unlink(tmp), add = TRUE)
  writeLines("data", tmp)

  result <- suppressMessages(tf_read_file(tmp))
  expect_equal(result, tmp)
})

test_that("tf_read_file returns path when NIfTI reader unavailable", {
  has_reader <- requireNamespace("RNifti", quietly = TRUE) ||
    requireNamespace("oro.nifti", quietly = TRUE)
  skip_if(has_reader, "NIfTI reader is available")

  tmp <- tempfile(fileext = ".nii.gz")
  on.exit(unlink(tmp), add = TRUE)
  writeBin(raw(10), tmp)

  result <- suppressMessages(tf_read_file(tmp))
  expect_equal(result, tmp)
})

test_that("tf_get with read=TRUE reads JSON files", {
  tmp <- file.path(tempdir(), "tfreader-get")
  if (dir.exists(tmp)) unlink(tmp, recursive = TRUE, force = TRUE)
  on.exit(unlink(tmp, recursive = TRUE, force = TRUE), add = TRUE)

  # Build a minimal cache with a real (non-zero) JSON file
  tpl_dir <- file.path(tmp, "tpl-TestTpl")
  dir.create(tpl_dir, recursive = TRUE)
  desc_path <- file.path(tpl_dir, "template_description.json")
  writeLines('{"Name": "TestTpl", "Identifier": "TestTpl"}', desc_path)

  withr::with_options(
    list(templateflow.test.forcebundled = TRUE),
    {
      cfg <- tf_default_config(root = tmp)
      cache <- TemplateFlowCache(config = cfg)
      cache$layout <- tf_build_layout(tmp)
      client <- list(cache = cache)
      class(client) <- "TemplateFlowClient"

      result <- tf_get(client, template = "TestTpl",
                       suffix = "template_description", extension = ".json",
                       read = TRUE)
      expect_true(is.list(result))
      expect_true("Name" %in% names(result))
      expect_equal(result$Name, "TestTpl")
    }
  )
})

test_that("tf_ls with as_df returns data frame", {
  tmp <- file.path(tempdir(), "tfls-asdf")
  if (dir.exists(tmp)) unlink(tmp, recursive = TRUE, force = TRUE)
  on.exit(unlink(tmp, recursive = TRUE, force = TRUE), add = TRUE)

  withr::with_options(
    list(templateflow.test.forcebundled = TRUE),
    {
      client <- TemplateFlowClient(root = tmp)
      df <- tf_ls(client, template = "MNI152Lin", as_df = TRUE)
      expect_true(is.data.frame(df))
      expect_true("template" %in% names(df))
      expect_true(nrow(df) > 0)
    }
  )
})
