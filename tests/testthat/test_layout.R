test_that("config-driven entity parsing extracts expected values", {
  tmp <- file.path(tempdir(), "tflayout-test")
  if (dir.exists(tmp)) unlink(tmp, recursive = TRUE, force = TRUE)
  on.exit(unlink(tmp, recursive = TRUE, force = TRUE), add = TRUE)

  # Create a minimal template directory with a known file
  tpl_dir <- file.path(tmp, "tpl-MNI152Lin")
  dir.create(tpl_dir, recursive = TRUE)
  fname <- "tpl-MNI152Lin_res-01_T1w.nii.gz"
  writeLines("fake", file.path(tpl_dir, fname))

  row <- tf_parse_tpl_path(file.path(tpl_dir, fname), root = tmp)
  expect_equal(row$template, "MNI152Lin")
  expect_equal(row$resolution, 1L)
  expect_equal(row$suffix, "T1w")
  expect_equal(row$extension, ".nii.gz")
})

test_that("integer coercion applies to cohort and scale", {
  tmp <- file.path(tempdir(), "tflayout-int")
  if (dir.exists(tmp)) unlink(tmp, recursive = TRUE, force = TRUE)
  on.exit(unlink(tmp, recursive = TRUE, force = TRUE), add = TRUE)

  tpl_dir <- file.path(tmp, "tpl-MNIInfant", "cohort-01")
  dir.create(tpl_dir, recursive = TRUE)
  fname <- "tpl-MNIInfant_cohort-01_res-01_T1w.nii.gz"
  writeLines("fake", file.path(tpl_dir, fname))

  row <- tf_parse_tpl_path(file.path(tpl_dir, fname), root = tmp)
  expect_equal(row$cohort, 1L)
  expect_equal(row$resolution, 1L)
  expect_true(is.integer(row$cohort))
  expect_true(is.integer(row$resolution))
})

test_that("layout build indexes files correctly", {
  tmp <- file.path(tempdir(), "tflayout-build")
  if (dir.exists(tmp)) unlink(tmp, recursive = TRUE, force = TRUE)
  on.exit(unlink(tmp, recursive = TRUE, force = TRUE), add = TRUE)

  tpl_dir <- file.path(tmp, "tpl-MNI152Lin")
  dir.create(tpl_dir, recursive = TRUE)
  writeLines("fake", file.path(tpl_dir, "tpl-MNI152Lin_res-01_T1w.nii.gz"))
  writeLines("fake", file.path(tpl_dir, "tpl-MNI152Lin_res-02_T1w.nii.gz"))

  layout <- tf_build_layout(tmp)
  expect_s3_class(layout, "TemplateFlowLayout")
  expect_equal(nrow(layout$index), 2)

  # Query by resolution
  paths <- tf_layout_get(layout, return_type = "file", resolution = 1L)
  expect_length(paths, 1)
  expect_true(grepl("res-01", paths))
})

test_that("fuzzy matching suggests close entity names", {
  tmp <- file.path(tempdir(), "tflayout-fuzzy")
  if (dir.exists(tmp)) unlink(tmp, recursive = TRUE, force = TRUE)
  on.exit(unlink(tmp, recursive = TRUE, force = TRUE), add = TRUE)

  tpl_dir <- file.path(tmp, "tpl-MNI152Lin")
  dir.create(tpl_dir, recursive = TRUE)
  writeLines("fake", file.path(tpl_dir, "tpl-MNI152Lin_res-01_T1w.nii.gz"))

  layout <- tf_build_layout(tmp)
  expect_error(
    tf_layout_get(layout, return_type = "file", atlsa = "foo"),
    class = "templateflow_invalid_filter"
  )
})

test_that("template_description.json is parsed correctly", {
  tmp <- file.path(tempdir(), "tflayout-desc")
  if (dir.exists(tmp)) unlink(tmp, recursive = TRUE, force = TRUE)
  on.exit(unlink(tmp, recursive = TRUE, force = TRUE), add = TRUE)

  tpl_dir <- file.path(tmp, "tpl-MNI152Lin")
  dir.create(tpl_dir, recursive = TRUE)
  writeLines("{}", file.path(tpl_dir, "template_description.json"))

  row <- tf_parse_tpl_path(
    file.path(tpl_dir, "template_description.json"),
    root = tmp
  )
  expect_equal(row$suffix, "template_description")
  expect_equal(row$extension, ".json")
})
