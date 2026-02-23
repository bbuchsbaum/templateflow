test_that("tf_cli --help prints usage", {
  output <- capture.output(tf_cli(c("--help")))
  expect_true(any(grepl("Usage:", output)))
  expect_true(any(grepl("config", output)))
})

test_that("tf_cli --version prints version", {
  output <- capture.output(tf_cli(c("--version")))
  expect_true(any(grepl("^[0-9]+\\.[0-9]+", output)))
})

test_that("tf_cli config prints settings", {
  tmp <- file.path(tempdir(), "tf-cli-config-ext")
  if (dir.exists(tmp)) unlink(tmp, recursive = TRUE, force = TRUE)
  on.exit(unlink(tmp, recursive = TRUE, force = TRUE), add = TRUE)

  output <- capture.output(tf_cli(c("--root", tmp, "config")))
  expect_true(any(grepl("TEMPLATEFLOW_HOME", output)))
})

test_that("tf_cli config --json returns JSON", {
  tmp <- file.path(tempdir(), "tf-cli-config-json")
  if (dir.exists(tmp)) unlink(tmp, recursive = TRUE, force = TRUE)
  on.exit(unlink(tmp, recursive = TRUE, force = TRUE), add = TRUE)

  output <- capture.output(tf_cli(c("--root", tmp, "config", "--json")))
  parsed <- jsonlite::fromJSON(paste(output, collapse = "\n"))
  expect_true("root" %in% names(parsed))
})

test_that("tf_cli config --show-cache displays stats", {
  tmp <- file.path(tempdir(), "tf-cli-config-cache")
  if (dir.exists(tmp)) unlink(tmp, recursive = TRUE, force = TRUE)
  on.exit(unlink(tmp, recursive = TRUE, force = TRUE), add = TRUE)

  output <- capture.output(tf_cli(c("--root", tmp, "config", "--show-cache")))
  expect_true(any(grepl("Cache files:", output)))
})

test_that("tf_cli ls lists assets for template", {
  tmp <- file.path(tempdir(), "tf-cli-ls-ext")
  if (dir.exists(tmp)) unlink(tmp, recursive = TRUE, force = TRUE)
  on.exit(unlink(tmp, recursive = TRUE, force = TRUE), add = TRUE)

  output <- capture.output({
    paths <- tf_cli(c("--root", tmp, "ls", "MNI152Lin", "--suffix", "T1w"))
  })
  expect_true(length(paths) >= 1)
  expect_true(any(grepl("tpl-MNI152Lin", output)))
})

test_that("tf_cli ls errors without template", {
  expect_error(tf_cli(c("ls")), class = "tf_cli_error")
})

test_that("tf_cli meta shows template metadata", {
  tmp <- file.path(tempdir(), "tf-cli-meta-ext")
  if (dir.exists(tmp)) unlink(tmp, recursive = TRUE, force = TRUE)
  on.exit(unlink(tmp, recursive = TRUE, force = TRUE), add = TRUE)

  output <- capture.output({
    meta <- tf_cli(c("--root", tmp, "meta", "MNI152Lin"))
  })
  expect_true(any(grepl("Name:", output)))
})

test_that("tf_cli meta --json returns JSON", {
  tmp <- file.path(tempdir(), "tf-cli-meta-json")
  if (dir.exists(tmp)) unlink(tmp, recursive = TRUE, force = TRUE)
  on.exit(unlink(tmp, recursive = TRUE, force = TRUE), add = TRUE)

  output <- capture.output({
    tf_cli(c("--root", tmp, "meta", "MNI152Lin", "--json"))
  })
  parsed <- jsonlite::fromJSON(paste(output, collapse = "\n"))
  expect_true("Name" %in% names(parsed))
})

test_that("tf_cli meta --field returns specific field", {
  tmp <- file.path(tempdir(), "tf-cli-meta-field")
  if (dir.exists(tmp)) unlink(tmp, recursive = TRUE, force = TRUE)
  on.exit(unlink(tmp, recursive = TRUE, force = TRUE), add = TRUE)

  output <- capture.output({
    tf_cli(c("--root", tmp, "meta", "MNI152Lin", "--field", "Name"))
  })
  expect_true(length(output) >= 1)
})

test_that("tf_cli refresh re-indexes cache", {
  tmp <- file.path(tempdir(), "tf-cli-refresh-ext")
  if (dir.exists(tmp)) unlink(tmp, recursive = TRUE, force = TRUE)
  on.exit(unlink(tmp, recursive = TRUE, force = TRUE), add = TRUE)

  # First create the cache
  client <- TemplateFlowClient(root = tmp)

  output <- capture.output(tf_cli(c("--root", tmp, "refresh")))
  expect_true(any(grepl("refreshed", output)))
})

test_that("tf_cli doctor scans cache", {
  tmp <- file.path(tempdir(), "tf-cli-doctor-ext")
  if (dir.exists(tmp)) unlink(tmp, recursive = TRUE, force = TRUE)
  on.exit(unlink(tmp, recursive = TRUE, force = TRUE), add = TRUE)

  client <- TemplateFlowClient(root = tmp)

  output <- capture.output(tf_cli(c("--root", tmp, "doctor")))
  # Should find zero-byte stubs or report clean
  expect_true(any(grepl("zero-byte|clean", output, ignore.case = TRUE)))
})

test_that("tf_cli wipe requires --force in non-interactive", {
  tmp <- file.path(tempdir(), "tf-cli-wipe-ext")
  if (dir.exists(tmp)) unlink(tmp, recursive = TRUE, force = TRUE)
  on.exit(unlink(tmp, recursive = TRUE, force = TRUE), add = TRUE)

  client <- TemplateFlowClient(root = tmp)
  expect_true(dir.exists(tmp))

  withr::local_options(rlang_interactive = FALSE)
  expect_error(tf_cli(c("--root", tmp, "wipe")), class = "tf_cli_error")
})

test_that("tf_cli wipe --force deletes cache", {
  tmp <- file.path(tempdir(), "tf-cli-wipe-force")
  if (dir.exists(tmp)) unlink(tmp, recursive = TRUE, force = TRUE)
  on.exit(unlink(tmp, recursive = TRUE, force = TRUE), add = TRUE)

  client <- TemplateFlowClient(root = tmp)
  expect_true(dir.exists(tmp))

  output <- capture.output(tf_cli(c("--root", tmp, "wipe", "--force")))
  expect_false(dir.exists(tmp))
})

test_that("tf_cli update --local updates from bundled skeleton", {
  tmp <- file.path(tempdir(), "tf-cli-update-ext")
  if (dir.exists(tmp)) unlink(tmp, recursive = TRUE, force = TRUE)
  on.exit(unlink(tmp, recursive = TRUE, force = TRUE), add = TRUE)

  dir.create(tmp, recursive = TRUE)
  # Capture messages since tf_update_skeleton logs via message()
  output <- capture.output(
    suppressMessages(tf_cli(c("--root", tmp, "update", "--local")))
  )
  expect_true(dir.exists(tmp))
  expect_true(length(list.files(tmp, recursive = TRUE)) > 0)
})

test_that("tf_cli subcommand --help prints help", {
  output <- capture.output(tf_cli(c("ls", "--help")))
  expect_true(any(grepl("Usage:", output)))
})

test_that("tf_cli unknown command errors", {
  expect_error(tf_cli(c("foobar")), class = "tf_cli_error")
})
