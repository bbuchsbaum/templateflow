test_that("templates are discoverable", {
  tmp <- file.path(tempdir(), "tfclient-cache")
  if (dir.exists(tmp)) unlink(tmp, recursive = TRUE, force = TRUE)
  client <- TemplateFlowClient(root = tmp)
  tpls <- tf_templates(client)
  expect_true(length(tpls) > 0)
  expect_true("MNI152Lin" %in% tpls)
})

test_that("ls returns expected assets", {
  tmp <- file.path(tempdir(), "tfclient-cache-ls")
  if (dir.exists(tmp)) unlink(tmp, recursive = TRUE, force = TRUE)
  client <- TemplateFlowClient(root = tmp)
  paths <- tf_ls(client, template = "MNI152Lin", resolution = 1, suffix = "T1w")
  expect_true(length(paths) >= 1)
  expect_true(any(grepl("tpl-MNI152Lin_res-01_T1w", paths)))
})

test_that("metadata is readable", {
  tmp <- file.path(tempdir(), "tfclient-cache-meta")
  if (dir.exists(tmp)) unlink(tmp, recursive = TRUE, force = TRUE)
  client <- TemplateFlowClient(root = tmp)
  meta <- tf_get_metadata(client, "MNI152Lin")
  expect_true(is.list(meta))
  expect_true("Name" %in% names(meta))
})

test_that("citations match expected URLs and bibtex format", {
  skip_if_offline()
  tmp <- file.path(tempdir(), "tfclient-cache-cite")
  if (dir.exists(tmp)) unlink(tmp, recursive = TRUE, force = TRUE)
  client <- TemplateFlowClient(root = tmp)
  urls <- tf_get_citations(client, "MNI152NLin2009cAsym", bibtex = FALSE)
  expect_true(length(urls) >= 4)
  expect_setequal(
    urls,
    c(
      "https://doi.org/10.1016/j.neuroimage.2010.07.033",
      "https://doi.org/10.1016/S1053-8119(09)70884-5",
      "http://nist.mni.mcgill.ca/?p=904",
      "https://doi.org/10.1007/3-540-48714-X_16"
    )
  )

  bibs <- tf_get_citations(client, "MNI152NLin2009cAsym", bibtex = TRUE)
  expect_true(length(bibs) >= 1)
  first <- trimws(bibs[[1]])
  expect_true(grepl("^@\\w+\\{|^https?://", first))
})

test_that("tf_get downloads non-empty files", {
  skip_if_offline()
  tmp <- file.path(tempdir(), "tfclient-cache-get")
  if (dir.exists(tmp)) unlink(tmp, recursive = TRUE, force = TRUE)
  client <- TemplateFlowClient(root = tmp)
  path <- tf_get(client, template = "MNI152Lin", resolution = 1, suffix = "T1w")
  expect_true(file.exists(path))
  expect_gt(file.info(path)$size, 0)
})

test_that("truncate clears S3 error stubs", {
  tmpfile <- tempfile(fileext = ".xml")
  # copy canned error response from python tests
  xml_path <- testthat::test_path("data", "error_response.xml")
  file.copy(xml_path, tmpfile, overwrite = TRUE)
  expect_true(file.info(tmpfile)$size > 0)
  tf_truncate_s3_errors(tmpfile)
  expect_equal(file.info(tmpfile)$size, 0)
})

test_that("config respects TEMPLATEFLOW_HOME", {
  tmp <- file.path(tempdir(), "tfconfig-home")
  withr::with_envvar(
    c(TEMPLATEFLOW_HOME = tmp),
    {
      cfg <- tf_default_config()
      expect_equal(cfg$root, normalizePath(tmp, mustWork = FALSE))
      cache <- TemplateFlowCache(cfg)
      tf_cache_ensure(cache)
      expect_true(dir.exists(tmp))
      expect_true(length(list.files(tmp)) > 0)
    }
  )
})

test_that("autoupdate flag is parsed and stored", {
  tmp <- file.path(tempdir(), "tfconfig-autoupdate")
  cfg <- tf_default_config(root = tmp, autoupdate = FALSE)
  cache <- TemplateFlowCache(cfg)
  expect_false(cache$config$autoupdate)
})

test_that("remote skeleton fetch falls back to bundled when unreachable", {
  tmp <- file.path(tempdir(), "tfs3")
  dir.create(tmp, recursive = TRUE, showWarnings = FALSE)
  # Temporarily override fetchers to force bundled usage
  withr::with_options(
    list(templateflow.test.forcebundled = TRUE),
    {
      tf_update_skeleton(dest = tmp, overwrite = TRUE, silent = TRUE, timeout = 1)
      expect_true(length(list.files(tmp)) > 0)
    }
  )
})

test_that("version matches DESCRIPTION", {
  find_desc <- function(start = getwd()) {
    current <- normalizePath(start, mustWork = FALSE)
    for (i in 0:6) {
      candidate <- file.path(current, "DESCRIPTION")
      if (file.exists(candidate)) return(candidate)
      parent <- dirname(current)
      if (parent == current) break
      current <- parent
    }
    NULL
  }
  desc_path <- find_desc()
  testthat::skip_if(is.null(desc_path), "DESCRIPTION not found")
  desc <- unname(read.dcf(desc_path, fields = "Version")[1, 1])
  if (!requireNamespace("templateflow", quietly = TRUE)) {
    expect_true(nchar(desc) > 0)
  } else {
    expect_equal(as.character(utils::packageVersion("templateflow")), desc)
  }
})
