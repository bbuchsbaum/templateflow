# Additional CLI branches not covered by test_cli*.R --------------------------

test_that("tf_cli get lists matching files without downloading present assets", {
  tmp <- file.path(tempdir(), "tf-cli-get-branch")
  if (dir.exists(tmp)) unlink(tmp, recursive = TRUE, force = TRUE)
  on.exit(unlink(tmp, recursive = TRUE, force = TRUE), add = TRUE)

  # Seed a real non-empty JSON asset so get short-circuits before network I/O.
  tpl_dir <- file.path(tmp, "tpl-TestTpl")
  dir.create(tpl_dir, recursive = TRUE)
  asset <- file.path(tpl_dir, "tpl-TestTpl_desc-ref_template_description.json")
  writeLines('{"Name":"TestTpl"}', asset)

  withr::with_options(
    list(templateflow.test.forcebundled = TRUE),
    {
      cfg <- tf_default_config(root = tmp)
      cache <- TemplateFlowCache(config = cfg)
      cache$layout <- tf_build_layout(tmp)
      client <- structure(list(cache = cache), class = "TemplateFlowClient")

      testthat::local_mocked_bindings(
        tf_cli_make_client = function(root, autoupdate) client,
        .package = "templateflow"
      )

      output <- capture.output({
        paths <- tf_cli(c("--root", tmp, "get", "TestTpl", "--suffix", "template_description"))
      })
      expect_true(length(paths) >= 1)
      expect_true(any(grepl("template_description", paths)) ||
        any(grepl("template_description", output)))
    }
  )
})

test_that("tf_cli get errors without a template argument", {
  expect_error(tf_cli(c("get")), class = "tf_cli_error")
})

test_that("tf_cli cite and cite --bibtex print citations", {
  tmp <- file.path(tempdir(), "tf-cli-cite-branch")
  if (dir.exists(tmp)) unlink(tmp, recursive = TRUE, force = TRUE)
  on.exit(unlink(tmp, recursive = TRUE, force = TRUE), add = TRUE)

  out_plain <- capture.output(tf_cli(c("--root", tmp, "cite", "MNI152Lin")))
  expect_true(length(out_plain) >= 1)

  # Keep --bibtex offline: stub DOI conversion at the package boundary.
  testthat::local_mocked_bindings(
    tf_to_bibtex = function(doi, timeout) paste0("@article{stub,\n  url = {", doi, "}\n}"),
    .package = "templateflow"
  )
  out_bib <- capture.output(tf_cli(c("--root", tmp, "cite", "MNI152Lin", "--bibtex")))
  expect_true(any(grepl("@article|doi|MNI|template|http", out_bib, ignore.case = TRUE)))
})

test_that("tf_cli cite errors without a template argument", {
  expect_error(tf_cli(c("cite")), class = "tf_cli_error")
})

test_that("tf_cli meta errors for unknown fields and missing template", {
  tmp <- file.path(tempdir(), "tf-cli-meta-branch")
  if (dir.exists(tmp)) unlink(tmp, recursive = TRUE, force = TRUE)
  on.exit(unlink(tmp, recursive = TRUE, force = TRUE), add = TRUE)

  expect_error(tf_cli(c("meta")), class = "tf_cli_error")
  expect_error(
    tf_cli(c("--root", tmp, "meta", "MNI152Lin", "--field", "DefinitelyMissing")),
    class = "tf_cli_error"
  )
})

test_that("tf_cli doctor --fix removes zero-byte and XML error files", {
  tmp <- file.path(tempdir(), "tf-cli-doctor-fix")
  if (dir.exists(tmp)) unlink(tmp, recursive = TRUE, force = TRUE)
  on.exit(unlink(tmp, recursive = TRUE, force = TRUE), add = TRUE)

  client <- TemplateFlowClient(root = tmp)
  zero <- file.path(tmp, "tpl-MNI152Lin", "zero-stub.nii.gz")
  dir.create(dirname(zero), recursive = TRUE, showWarnings = FALSE)
  file.create(zero)

  xml <- file.path(tmp, "tpl-MNI152Lin", "bad.xml")
  file.copy(testthat::test_path("data", "error_response.xml"), xml, overwrite = TRUE)

  output <- capture.output(tf_cli(c("--root", tmp, "doctor", "--fix")))
  expect_true(any(grepl("Removed|zero-byte|XML", output, ignore.case = TRUE)))
  expect_false(file.exists(zero))
  expect_false(file.exists(xml))
})

test_that("tf_cli parses --autoupdate and rejects invalid values", {
  tmp <- file.path(tempdir(), "tf-cli-autoupdate")
  if (dir.exists(tmp)) unlink(tmp, recursive = TRUE, force = TRUE)
  on.exit(unlink(tmp, recursive = TRUE, force = TRUE), add = TRUE)

  expect_output(
    tf_cli(c("--root", tmp, "--autoupdate", "off", "config")),
    "TEMPLATEFLOW_AUTOUPDATE=off"
  )
  expect_error(
    tf_cli(c("--root", tmp, "--autoupdate", "maybe", "config")),
    class = "tf_cli_error"
  )
  expect_error(tf_cli(c("--root")), class = "tf_cli_error")
  expect_error(tf_cli(c("--autoupdate")), class = "tf_cli_error")
})

test_that("tf_cli update --help and refresh --help print usage", {
  expect_true(any(grepl("Usage:", capture.output(tf_cli(c("update", "--help"))))))
  expect_true(any(grepl("Usage:", capture.output(tf_cli(c("refresh", "--help"))))))
  expect_true(any(grepl("Usage:", capture.output(tf_cli(c("doctor", "--help"))))))
  expect_true(any(grepl("Usage:", capture.output(tf_cli(c("cite", "--help"))))))
  expect_true(any(grepl("Usage:", capture.output(tf_cli(c("get", "--help"))))))
})

test_that("tf_cli flag without value errors", {
  expect_error(
    tf_cli(c("--root", tempdir(), "ls", "MNI152Lin", "--suffix")),
    class = "tf_cli_error"
  )
})
