test_that("tf_cli lists assets for a template", {
  tmp <- file.path(tempdir(), "tf-cli-list")
  if (dir.exists(tmp)) unlink(tmp, recursive = TRUE, force = TRUE)
  on.exit(unlink(tmp, recursive = TRUE, force = TRUE), add = TRUE)

  output <- capture.output({
    paths <- tf_cli(c("--root", tmp, "ls", "MNI152Lin", "--suffix", "T1w"))
  })

  expect_true(length(paths) >= 1)
  expect_true(any(grepl("tpl-MNI152Lin", output)))
})

test_that("tf_cli config prints settings", {
  tmp <- file.path(tempdir(), "tf-cli-config")
  if (dir.exists(tmp)) unlink(tmp, recursive = TRUE, force = TRUE)
  on.exit(unlink(tmp, recursive = TRUE, force = TRUE), add = TRUE)

  expect_output(tf_cli(c("--root", tmp, "--autoupdate", "off", "config")), "TEMPLATEFLOW_HOME")
})

test_that("tf_cli errors when template is missing", {
  expect_error(tf_cli(c("ls")), "template")
})
