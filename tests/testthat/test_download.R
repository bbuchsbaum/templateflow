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

# Build cache-relative paths from the canonical config root. On macOS,
# tempfile() may yield /var/folders/... while tf_default_config() stores
# normalizePath() → /private/var/folders/...; tf_s3_url()/tf_relpath() require
# the filepath to live under cfg$root.
local_cfg_root <- function(prefix, ...) {
  staging <- tempfile(prefix)
  dir.create(staging)
  cfg <- tf_default_config(root = staging, ...)
  list(cfg = cfg, root = cfg$root)
}

test_that("tf_s3_url builds HTTPS object URLs from cache-relative paths", {
  setup <- local_cfg_root("tf-s3-url-", s3_root = "https://example.test/tpl")
  cfg <- setup$cfg
  on.exit(unlink(setup$root, recursive = TRUE, force = TRUE), add = TRUE)

  fp <- file.path(cfg$root, "tpl-Demo", "tpl-Demo_T1w.nii.gz")
  expect_equal(
    tf_s3_url(cfg, fp),
    "https://example.test/tpl/tpl-Demo/tpl-Demo_T1w.nii.gz"
  )
})

test_that("tf_download_file atomically writes on mocked HTTPS success", {
  local_force_https_fallback()
  setup <- local_cfg_root("tf-dl-ok-", timeout = 1)
  cfg <- setup$cfg
  on.exit(unlink(setup$root, recursive = TRUE, force = TRUE), add = TRUE)

  fp <- file.path(cfg$root, "tpl-Demo", "payload.bin")
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
  setup <- local_cfg_root("tf-dl-retry-", timeout = 1)
  cfg <- setup$cfg
  on.exit(unlink(setup$root, recursive = TRUE, force = TRUE), add = TRUE)

  fp <- file.path(cfg$root, "tpl-Demo", "retry.bin")
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
  setup <- local_cfg_root("tf-dl-http-fail-", timeout = 1)
  cfg <- setup$cfg
  on.exit(unlink(setup$root, recursive = TRUE, force = TRUE), add = TRUE)

  fp <- file.path(cfg$root, "tpl-Demo", "missing.bin")
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
  setup <- local_cfg_root("tf-dl-net-fail-", timeout = 1)
  cfg <- setup$cfg
  on.exit(unlink(setup$root, recursive = TRUE, force = TRUE), add = TRUE)

  fp <- file.path(cfg$root, "tpl-Demo", "offline.bin")
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
  setup <- local_cfg_root("tf-fetch-noop-")
  cfg <- setup$cfg
  on.exit(unlink(setup$root, recursive = TRUE, force = TRUE), add = TRUE)

  cache <- TemplateFlowCache(config = cfg)

  expect_true(tf_fetch_files(cache, character()))

  present <- file.path(cfg$root, "already.bin")
  writeBin(charToRaw("cached"), present)
  expect_true(tf_fetch_files(cache, present))
})

test_that("tf_fetch_files downloads missing zero-byte stubs via mocked HTTPS", {
  local_force_https_fallback()
  setup <- local_cfg_root("tf-fetch-missing-", timeout = 1, use_datalad = FALSE)
  cfg <- setup$cfg
  on.exit(unlink(setup$root, recursive = TRUE, force = TRUE), add = TRUE)

  cache <- TemplateFlowCache(config = cfg)

  missing <- c(
    file.path(cfg$root, "tpl-Demo", "a.bin"),
    file.path(cfg$root, "tpl-Demo", "b.bin")
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
  setup <- local_cfg_root("tf-fetch-datalad-", timeout = 1, use_datalad = TRUE)
  cfg <- setup$cfg
  on.exit(unlink(setup$root, recursive = TRUE, force = TRUE), add = TRUE)

  cache <- TemplateFlowCache(config = cfg)
  missing <- file.path(cfg$root, "tpl-Demo", "dl.bin")
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
