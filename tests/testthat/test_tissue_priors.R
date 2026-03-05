make_prior_client <- function(label) {
  tmp <- file.path(tempdir(), paste0("tf-priors-", label))
  if (dir.exists(tmp)) unlink(tmp, recursive = TRUE, force = TRUE)
  withr::with_options(
    list(templateflow.test.forcebundled = TRUE),
    TemplateFlowClient(root = tmp)
  )
}

test_that("tissue label aliases are normalized", {
  client <- make_prior_client("aliases")

  expect_setequal(
    tf_ls(client, template = "MNI152NLin2009cAsym", resolution = 1, suffix = "probseg", label = "gray"),
    tf_ls(client, template = "MNI152NLin2009cAsym", resolution = 1, suffix = "probseg", label = "GM")
  )

  expect_setequal(
    tf_ls(client, template = "MNI152NLin2009cAsym", resolution = 1, suffix = "probseg", label = "white"),
    tf_ls(client, template = "MNI152NLin2009cAsym", resolution = 1, suffix = "probseg", label = "WM")
  )

  expect_setequal(
    tf_ls(client, template = "MNI152NLin2009cAsym", resolution = 1, suffix = "probseg", label = "csf"),
    tf_ls(client, template = "MNI152NLin2009cAsym", resolution = 1, suffix = "probseg", label = "CSF")
  )
})

test_that("tissue label queries fall back to desc when needed", {
  client <- make_prior_client("desc-fallback")

  csf_path <- tf_ls(
    client,
    template = "MouseIn",
    resolution = 1,
    suffix = "probseg",
    label = "csf"
  )
  expect_length(csf_path, 1)
  expect_match(basename(csf_path), "desc-CSF_probseg\\.nii\\.gz$")

  tissue_paths <- tf_ls(
    client,
    template = "MouseIn",
    resolution = 1,
    suffix = "probseg",
    label = c("gray", "white", "csf")
  )
  expect_length(tissue_paths, 3)
  expect_true(all(grepl("desc-(GM|WM|CSF)_probseg\\.nii\\.gz$", basename(tissue_paths))))
})

test_that("non-tissue label queries fall back to desc for mask and probseg", {
  client <- make_prior_client("non-tissue-desc-fallback")

  brain_mask <- tf_ls(
    client,
    template = "MNI152NLin2009cAsym",
    resolution = 1,
    suffix = "mask",
    label = "brain"
  )
  expect_length(brain_mask, 1)
  expect_match(basename(brain_mask), "desc-brain_mask\\.nii\\.gz$")

  whs_probseg <- tf_ls(
    client,
    template = "WHS",
    resolution = 2,
    suffix = "probseg",
    label = "brain"
  )
  expect_length(whs_probseg, 1)
  expect_match(basename(whs_probseg), "desc-brain_probseg\\.nii\\.gz$")
})
