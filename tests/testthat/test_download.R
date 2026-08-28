# Download helpers: error / retry / atomic-write with mocked HTTP --------------

local_force_https_fallback <- function(env = parent.frame()) {
  if (!requireNamespace("aws.s3", quietly = TRUE)) {
    return(invisible(NULL))
  }
  # Prefer the HTTPS path under test; do not hit S3 even if Suggests are present.
  testthat::local_mocked_bindings(
    save_object = function(...) stop("forced aws.s3 fallback for tests", call. = FALSE),
    .package = "aws.s3",
    .env = env
  )
}

local_mock_req_perform <- function(handler, env = parent.frame()) {
  testthat::local_mocked_bindings(
    req_perform = handler,
    .package = "httr2",
    .env = env
  )
}

test_that("tf_s3_url builds HTTPS object URLs from cache-relative paths", {
  root <- tempfile("tf-s3-url-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)

  cfg <- tf_default_config(root = root, s3_root = "https://example.test/tpl")
  fp <- file.path(root, "tpl-Demo", "tpl-Demo_T1w.nii.gz")
  expect_equal(
    tf_s3_url(cfg, fp),
    "https://example.test/tpl/tpl-Demo/tpl-Demo_T1w.nii.gz"
  )
})

test_that("tf_download_file atomically writes on mocked HTTPS success", {
  local_force_https_fallback()
  root <- tempfile("tf-dl-ok-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)

  cfg <- tf_default_config(root = root, timeout = 1)
  fp <- file.path(root, "tpl-Demo", "payload.bin")
  payload <- charToRaw("templateflow-bytes")

  local_mock_req_perform(function(req, path = NULL, ...) {
    expect_true(grepl("tpl-Demo/payload.bin$", req$url))
    if (!is.null(path)) writeBin(payload, path)
    httr2::response(status_code = 200L, url = req$url, body = payload)
  })

  out <- suppressMessages(tf_download_file(cfg, fp, max_retries = 1))
  expect_identical(out, fp)
  expect_true(file.exists(fp))
  expect_equal(readBin(fp, what = "raw", n = length(payload)), payload)
  leftovers <- list.files(dirname(fp), full.names = TRUE)
  expect_identical(normalizePath(leftovers), normalizePath(fp))
})

test_that("tf_download_file retries HTTP errors then succeeds", {
  local_force_https_fallback()
  root <- tempfile("tf-dl-retry-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)

  cfg <- tf_default_config(root = root, timeout = 1)
  fp <- file.path(root, "tpl-Demo", "retry.bin")
  payload <- charToRaw("after-retry")
  attempts <- 0L

  local_mock_req_perform(function(req, path = NULL, ...) {
    attempts <<- attempts + 1L
    if (attempts < 2L) {
      return(httr2::response(status_code = 503L, url = req$url, body = charToRaw("busy")))
    }
    if (!is.null(path)) writeBin(payload, path)
    httr2::response(status_code = 200L, url = req$url, body = payload)
  })

  # One backoff sleep (attempt 1 -> 2) is intentional to exercise the retry path.
  out <- suppressMessages(tf_download_file(cfg, fp, max_retries = 2))
  expect_identical(out, fp)
  expect_equal(attempts, 2L)
  expect_equal(readBin(fp, what = "raw", n = length(payload)), payload)
})

test_that("tf_download_file aborts after exhausting HTTP error retries", {
  local_force_https_fallback()
  root <- tempfile("tf-dl-http-fail-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)

  cfg <- tf_default_config(root = root, timeout = 1)
  fp <- file.path(root, "tpl-Demo", "missing.bin")
  attempts <- 0L

  local_mock_req_perform(function(req, path = NULL, ...) {
    attempts <<- attempts + 1L
    httr2::response(status_code = 404L, url = req$url, body = charToRaw("missing"))
  })

  # max_retries = 1 avoids backoff sleep while still covering the HTTP failure abort.
  expect_error(
    suppressMessages(tf_download_file(cfg, fp, max_retries = 1)),
    class = "templateflow_network_error"
  )
  expect_equal(attempts, 1L)
  expect_false(file.exists(fp))
})

test_that("tf_download_file aborts after transport errors", {
  local_force_https_fallback()
  root <- tempfile("tf-dl-net-fail-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)

  cfg <- tf_default_config(root = root, timeout = 1)
  fp <- file.path(root, "tpl-Demo", "offline.bin")
  attempts <- 0L

  local_mock_req_perform(function(req, path = NULL, ...) {
    attempts <<- attempts + 1L
    stop("connection refused", call. = FALSE)
  })

  expect_error(
    suppressMessages(tf_download_file(cfg, fp, max_retries = 1)),
    class = "templateflow_network_error"
  )
  expect_equal(attempts, 1L)
  expect_false(file.exists(fp))
})

test_that("tf_fetch_files no-ops for empty or already-present files", {
  root <- tempfile("tf-fetch-noop-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)

  cfg <- tf_default_config(root = root)
  cache <- TemplateFlowCache(config = cfg)

  expect_true(tf_fetch_files(cache, character()))

  present <- file.path(root, "already.bin")
  writeBin(charToRaw("cached"), present)
  expect_true(tf_fetch_files(cache, present))
})

test_that("tf_fetch_files downloads missing zero-byte stubs via mocked HTTPS", {
  local_force_https_fallback()
  root <- tempfile("tf-fetch-missing-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)

  cfg <- tf_default_config(root = root, timeout = 1, use_datalad = FALSE)
  cache <- TemplateFlowCache(config = cfg)

  missing <- c(
    file.path(root, "tpl-Demo", "a.bin"),
    file.path(root, "tpl-Demo", "b.bin")
  )
  dir.create(dirname(missing[[1]]), recursive = TRUE, showWarnings = FALSE)
  file.create(missing)

  local_mock_req_perform(function(req, path = NULL, ...) {
    if (!is.null(path)) writeBin(charToRaw("filled"), path)
    httr2::response(status_code = 200L, url = req$url, body = charToRaw("filled"))
  })

  expect_true(suppressMessages(tf_fetch_files(cache, missing)))
  expect_true(all(file.info(missing)$size > 0))
})

test_that("tf_fetch_files falls back to S3 when DataLad get fails", {
  local_force_https_fallback()
  root <- tempfile("tf-fetch-datalad-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)

  cfg <- tf_default_config(root = root, timeout = 1, use_datalad = TRUE)
  cache <- TemplateFlowCache(config = cfg)
  missing <- file.path(root, "tpl-Demo", "dl.bin")
  dir.create(dirname(missing), recursive = TRUE, showWarnings = FALSE)
  file.create(missing)

  testthat::local_mocked_bindings(
    tf_datalad_available = function() TRUE,
    tf_datalad_get = function(...) stop("datalad unavailable in test", call. = FALSE),
    .package = "templateflow"
  )
  local_mock_req_perform(function(req, path = NULL, ...) {
    if (!is.null(path)) writeBin(charToRaw("via-s3"), path)
    httr2::response(status_code = 200L, url = req$url, body = charToRaw("via-s3"))
  })

  expect_true(suppressMessages(tf_fetch_files(cache, missing)))
  expect_equal(rawToChar(readBin(missing, what = "raw", n = 16)), "via-s3")
})
