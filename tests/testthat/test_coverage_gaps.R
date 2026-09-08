# Meaningful coverage for exported behavior, boundaries, and error paths
# that remain thinly exercised after the main suites.

# --- DataLad success / failure with mocked CLI --------------------------------

local_mock_datalad_cli <- function(handler, env = parent.frame()) {
  testthat::local_mocked_bindings(
    tf_datalad_available = function() TRUE,
    .package = "templateflow",
    .env = env
  )
  testthat::local_mocked_bindings(
    system2 = handler,
    .package = "base",
    .env = env
  )
}

test_that("tf_datalad_install succeeds and supports recursive flag", {
  calls <- list()
  local_mock_datalad_cli(function(command, args = character(), ...) {
    calls[[length(calls) + 1]] <<- list(command = command, args = args)
    0L
  })

  expect_identical(
    tf_datalad_install("/tmp/tf-dl", source = "https://example.com/ds.git", recursive = TRUE),
    "/tmp/tf-dl"
  )
  expect_equal(calls[[1]]$command, "datalad")
  expect_true("-r" %in% calls[[1]]$args)
  expect_true("https://example.com/ds.git" %in% calls[[1]]$args)

  expect_identical(
    tf_datalad_install("/tmp/tf-dl2", source = "https://example.com/ds.git", recursive = FALSE),
    "/tmp/tf-dl2"
  )
  expect_false("-r" %in% calls[[2]]$args)
})

test_that("tf_datalad_install errors when the CLI returns non-zero", {
  local_mock_datalad_cli(function(...) 1L)
  expect_error(
    tf_datalad_install("/tmp/tf-dl", source = "https://example.com/ds.git"),
    class = "templateflow_network_error"
  )
})

test_that("tf_datalad_get succeeds and errors on CLI failure", {
  local_mock_datalad_cli(function(...) 0L)
  expect_identical(
    tf_datalad_get(c("a.txt", "b.txt"), dataset = "/tmp/ds"),
    c("a.txt", "b.txt")
  )

  local_mock_datalad_cli(function(...) 2L)
  expect_error(
    tf_datalad_get("a.txt", dataset = "/tmp/ds"),
    class = "templateflow_network_error"
  )
})

test_that("tf_datalad_update toggles recursive/merge flags and reports failures", {
  calls <- list()
  local_mock_datalad_cli(function(command, args = character(), ...) {
    calls[[length(calls) + 1]] <<- args
    0L
  })

  expect_true(tf_datalad_update("/tmp/ds", recursive = TRUE, merge = TRUE))
  expect_true("-r" %in% calls[[1]])
  expect_true("--merge" %in% calls[[1]])

  expect_true(tf_datalad_update("/tmp/ds", recursive = FALSE, merge = FALSE))
  expect_false("-r" %in% calls[[2]])
  expect_false("--merge" %in% calls[[2]])

  local_mock_datalad_cli(function(...) 1L)
  expect_error(
    tf_datalad_update("/tmp/ds"),
    class = "templateflow_network_error"
  )
})

# --- Client: dest copy, prefetch, print, citations/bibtex ---------------------

test_that("tf_copy_to_dest copies under tpl- subpaths and skips identical files", {
  root <- file.path(tempdir(), "tf-copy-root")
  dest <- file.path(tempdir(), "tf-copy-dest")
  if (dir.exists(root)) unlink(root, recursive = TRUE, force = TRUE)
  if (dir.exists(dest)) unlink(dest, recursive = TRUE, force = TRUE)
  on.exit(unlink(c(root, dest), recursive = TRUE, force = TRUE), add = TRUE)

  src <- file.path(root, "tpl-Demo", "tpl-Demo_T1w.nii.gz")
  dir.create(dirname(src), recursive = TRUE)
  writeBin(charToRaw("volume-bytes"), src)

  out <- tf_copy_to_dest(src, dest = dest, root = root)
  expect_true(file.exists(out))
  expect_true(startsWith(normalizePath(out), normalizePath(dest)))
  expect_true(grepl("tpl-Demo", out, fixed = TRUE))
  expect_equal(rawToChar(readBin(out, raw(), n = 32)), "volume-bytes")

  # Second copy with identical size should reuse the existing target.
  out2 <- tf_copy_to_dest(src, dest = dest, root = root)
  expect_equal(normalizePath(out2), normalizePath(out))

  # Paths outside the cache root fall back to basename placement.
  outside <- tempfile(fileext = ".bin")
  writeBin(charToRaw("out"), outside)
  on.exit(unlink(outside), add = TRUE)
  out3 <- tf_copy_to_dest(outside, dest = dest, root = root)
  expect_equal(basename(out3), basename(outside))
})

test_that("tf_copy_to_dest errors when the copy fails", {
  root <- file.path(tempdir(), "tf-copy-fail-root")
  dest <- file.path(tempdir(), "tf-copy-fail-dest")
  if (dir.exists(root)) unlink(root, recursive = TRUE, force = TRUE)
  if (dir.exists(dest)) unlink(dest, recursive = TRUE, force = TRUE)
  on.exit(unlink(c(root, dest), recursive = TRUE, force = TRUE), add = TRUE)

  src <- file.path(root, "tpl-Demo", "missing.bin")
  dir.create(dirname(src), recursive = TRUE)
  # Source path does not exist → file.copy returns FALSE.
  expect_error(
    tf_copy_to_dest(src, dest = dest, root = root),
    class = "templateflow_cache_error"
  )
})

test_that("tf_prefetch downloads stubs and counts successes", {
  tmp <- file.path(tempdir(), "tf-prefetch")
  if (dir.exists(tmp)) unlink(tmp, recursive = TRUE, force = TRUE)
  on.exit(unlink(tmp, recursive = TRUE, force = TRUE), add = TRUE)

  client <- TemplateFlowClient(root = tmp)
  downloaded <- character()

  testthat::local_mocked_bindings(
    tf_download_file = function(config, filepath, ...) {
      writeBin(charToRaw("prefetched"), filepath)
      downloaded <<- c(downloaded, filepath)
      invisible(filepath)
    },
    .package = "templateflow"
  )

  count <- suppressMessages(tf_prefetch(client, templates = "MNI152Lin", suffix = "T1w"))
  expect_true(count >= 1)
  expect_true(length(downloaded) >= 1)
  expect_true(all(file.info(downloaded)$size > 0))
})

test_that("tf_prefetch logs download failures without aborting", {
  tmp <- file.path(tempdir(), "tf-prefetch-fail")
  if (dir.exists(tmp)) unlink(tmp, recursive = TRUE, force = TRUE)
  on.exit(unlink(tmp, recursive = TRUE, force = TRUE), add = TRUE)

  client <- TemplateFlowClient(root = tmp)
  testthat::local_mocked_bindings(
    tf_download_file = function(...) stop("boom", call. = FALSE),
    .package = "templateflow"
  )

  expect_message(
    count <- tf_prefetch(client, templates = "MNI152Lin", suffix = "T1w"),
    "Downloaded"
  )
  expect_equal(count, 0L)
})

test_that("print.TemplateFlowClient reports backend mode", {
  tmp <- file.path(tempdir(), "tf-print-client")
  if (dir.exists(tmp)) unlink(tmp, recursive = TRUE, force = TRUE)
  on.exit(unlink(tmp, recursive = TRUE, force = TRUE), add = TRUE)

  client <- TemplateFlowClient(root = tmp, use_datalad = FALSE)
  out <- capture.output(print(client))
  expect_true(any(grepl("TemplateFlowClient\\[S3\\]", out)))

  cfg <- tf_default_config(root = tmp, use_datalad = TRUE)
  cache <- TemplateFlowCache(config = cfg)
  dl_client <- structure(list(cache = cache), class = "TemplateFlowClient")
  out_dl <- capture.output(print(dl_client))
  expect_true(any(grepl("DataLad", out_dl)))
})

test_that("TemplateFlowClient rejects cache combined with root/config", {
  tmp <- file.path(tempdir(), "tf-client-conflict")
  if (dir.exists(tmp)) unlink(tmp, recursive = TRUE, force = TRUE)
  on.exit(unlink(tmp, recursive = TRUE, force = TRUE), add = TRUE)

  cache <- TemplateFlowCache(tf_default_config(root = tmp))
  expect_error(
    TemplateFlowClient(root = tmp, cache = cache),
    class = "templateflow_error"
  )
  expect_error(
    tf_set_client(list()),
    class = "templateflow_error"
  )
})

test_that("tf_to_bibtex converts DOI responses and falls back on failure", {
  testthat::local_mocked_bindings(
    req_perform = function(req, ...) {
      httr2::response(
        status_code = 200L,
        url = req$url,
        body = charToRaw("@article{x,\n  url={http://dx.doi.org/10.1/abc}\n}")
      )
    },
    .package = "httr2"
  )
  bib <- tf_to_bibtex("https://doi.org/10.1/abc", timeout = 1)
  expect_true(grepl("@article", bib))
  expect_true(grepl("https://doi.org/", bib, fixed = TRUE))
  expect_false(grepl("http://dx.doi.org/", bib, fixed = TRUE))

  # Non-DOI URLs are returned unchanged.
  expect_equal(tf_to_bibtex("https://example.com/paper", timeout = 1), "https://example.com/paper")

  testthat::local_mocked_bindings(
    req_perform = function(...) stop("network down", call. = FALSE),
    .package = "httr2"
  )
  expect_message(
    fallback <- tf_to_bibtex("https://doi.org/10.1/abc", timeout = 1),
    "Failed to convert DOI"
  )
  expect_equal(fallback, "https://doi.org/10.1/abc")
})

test_that("tf_get_citations returns empty when metadata has no references", {
  tmp <- file.path(tempdir(), "tf-cite-empty")
  if (dir.exists(tmp)) unlink(tmp, recursive = TRUE, force = TRUE)
  on.exit(unlink(tmp, recursive = TRUE, force = TRUE), add = TRUE)

  client <- TemplateFlowClient(root = tmp)
  testthat::local_mocked_bindings(
    tf_get_metadata = function(...) list(Name = "NoRefs"),
    .package = "templateflow"
  )
  expect_equal(tf_get_citations(client, "NoRefs"), character(0))
})

test_that("tf_get raise_empty path reports unfetched files", {
  tmp <- file.path(tempdir(), "tf-get-unfetched")
  if (dir.exists(tmp)) unlink(tmp, recursive = TRUE, force = TRUE)
  on.exit(unlink(tmp, recursive = TRUE, force = TRUE), add = TRUE)

  client <- TemplateFlowClient(root = tmp)
  testthat::local_mocked_bindings(
    tf_fetch_files = function(...) invisible(TRUE),
    .package = "templateflow"
  )
  expect_error(
    tf_get(client, template = "MNI152Lin", resolution = 1, suffix = "T1w"),
    class = "templateflow_cache_error"
  )
})

# --- Readers: NIfTI / GIFTI dispatch ------------------------------------------

local_mock_requireNamespace <- function(denied = character(), env = parent.frame()) {
  # Avoid recursion into the mocked binding: resolve packages via find.package().
  testthat::local_mocked_bindings(
    requireNamespace = function(package, quietly = TRUE, ...) {
      if (package %in% denied) return(FALSE)
      !inherits(try(find.package(package, quiet = TRUE), silent = TRUE), "try-error")
    },
    .package = "base",
    .env = env
  )
}

test_that("tf_read_file reads NIfTI via RNifti when available", {
  skip_if_not_installed("RNifti")
  tmp <- tempfile(fileext = ".nii")
  on.exit(unlink(tmp), add = TRUE)
  RNifti::writeNifti(array(1:8, dim = c(2, 2, 2)), tmp)
  img <- tf_read_file(tmp)
  expect_true(inherits(img, "niftiImage") || is.array(img))
})

test_that("tf_read_file falls back to oro.nifti when RNifti is unavailable", {
  skip_if_not_installed("oro.nifti")
  skip_if_not_installed("RNifti")
  # Write a real NIfTI with RNifti; exercise the oro.nifti branch by denying RNifti.
  tmp <- tempfile(fileext = ".nii")
  on.exit(unlink(tmp), add = TRUE)
  RNifti::writeNifti(array(1:8, dim = c(2, 2, 2)), tmp)

  local_mock_requireNamespace(denied = "RNifti")
  result <- tf_read_file(tmp)
  expect_true(inherits(result, "nifti") || is.array(result))
})

test_that("tf_read_file returns path when GIFTI reader is unavailable", {
  tmp <- tempfile(fileext = ".surf.gii")
  on.exit(unlink(tmp), add = TRUE)
  writeLines("<GIFTI/>", tmp)

  local_mock_requireNamespace(denied = "gifti")
  result <- suppressMessages(tf_read_file(tmp))
  expect_equal(result, tmp)
})

test_that("tf_read_file returns path when no NIfTI reader is installed", {
  tmp <- tempfile(fileext = ".nii.gz")
  on.exit(unlink(tmp), add = TRUE)
  writeBin(raw(8), tmp)

  local_mock_requireNamespace(denied = c("RNifti", "oro.nifti"))
  result <- suppressMessages(tf_read_file(tmp))
  expect_equal(result, tmp)
})

# --- Config / utils / cache boundaries ----------------------------------------

test_that("tf_env_bool warns and falls back on unknown values", {
  withr::with_envvar(
    c(TEMPLATEFLOW_AUTOUPDATE = "sometimes"),
    expect_warning(
      expect_true(tf_env_bool("TEMPLATEFLOW_AUTOUPDATE", default = TRUE)),
      "unknown value"
    )
  )
  withr::with_envvar(
    c(TEMPLATEFLOW_USE_DATALAD = "maybe"),
    expect_warning(
      expect_false(tf_env_bool("TEMPLATEFLOW_USE_DATALAD", default = FALSE)),
      "unknown value"
    )
  )
})

test_that("tf_extdata_path errors for missing bundled files", {
  expect_error(tf_extdata_path("definitely-missing-file.zip"), class = "templateflow_cache_error")
})

test_that("tf_normalize helpers handle NULL and blank values", {
  expect_null(tf_normalize_ext(NULL))
  expect_equal(tf_normalize_ext(c("nii.gz", ".tsv", "")), c(".nii.gz", ".tsv", ""))
  expect_null(tf_normalize_tissue_label(NULL))
  expect_equal(tf_normalize_tissue_label(c("gm", "", NA_character_)), c("GM", "", NA_character_))
  expect_false(tf_suffix_allows_desc_fallback(NULL))
  expect_false(tf_is_xml_error(tempfile()))
})

test_that("tf_is_xml_error rejects empty content after open", {
  tmp <- tempfile()
  on.exit(unlink(tmp), add = TRUE)
  file.create(tmp)
  # Zero-byte files are rejected before reading.
  expect_false(tf_is_xml_error(tmp))
})

test_that("tf_cache_stats and tf_cache_scan handle missing roots and character paths", {
  missing <- file.path(tempdir(), "tf-missing-cache-root")
  if (dir.exists(missing)) unlink(missing, recursive = TRUE, force = TRUE)
  stats <- tf_cache_stats(missing)
  expect_equal(stats$cache_files, 0L)
  expect_equal(stats$cache_size_human, "0 B")
  scan <- tf_cache_scan(missing)
  expect_equal(scan$zero, character(0))
  expect_equal(scan$xml, character(0))
})

test_that("tf_cache_sync_skeleton records failure without wedging synced flag", {
  cache <- TemplateFlowCache(tf_default_config(root = tempfile("tf-sync-fail")))
  testthat::local_mocked_bindings(
    tf_update_skeleton = function(...) stop("sync exploded", call. = FALSE),
    .package = "templateflow"
  )
  expect_message(
    updated <- tf_cache_sync_skeleton(cache),
    "Skeleton sync failed"
  )
  expect_false(isTRUE(updated$skeleton_synced))
})

test_that("tf_latest_skeleton prefers remote zip when MD5 differs", {
  remote_zip <- tempfile(fileext = ".zip")
  on.exit(unlink(remote_zip), add = TRUE)
  scratch <- tempfile("skel-")
  dir.create(scratch)
  on.exit(unlink(scratch, recursive = TRUE, force = TRUE), add = TRUE)
  writeLines("x", file.path(scratch, "marker.txt"))
  old_wd <- setwd(scratch)
  on.exit(setwd(old_wd), add = TRUE)
  utils::zip(remote_zip, files = "marker.txt", flags = "-jq")
  setwd(old_wd)

  testthat::local_mocked_bindings(
    tf_skeleton_md5 = function() "bundled-md5",
    tf_fetch_remote_md5 = function(...) "remote-md5",
    tf_fetch_remote_zip = function(...) remote_zip,
    .package = "templateflow"
  )
  withr::with_options(
    list(templateflow.test.forcebundled = FALSE),
    {
      skel <- tf_latest_skeleton(timeout = 1, local = FALSE)
      expect_equal(skel$path, remote_zip)
      expect_true(skel$cleanup)
    }
  )
})

test_that("tf_fetch_remote_md5 and zip return NULL on HTTP failure", {
  testthat::local_mocked_bindings(
    req_perform = function(...) {
      httr2::response(status_code = 500L, url = "https://example.com", body = raw(0))
    },
    .package = "httr2"
  )
  expect_null(tf_fetch_remote_md5(timeout = 1))
  expect_null(tf_fetch_remote_zip(timeout = 1))
})

test_that("tf_update_skeleton reports up-to-date when overwrite is FALSE", {
  tmp <- file.path(tempdir(), "tf-skel-uptodate")
  if (dir.exists(tmp)) unlink(tmp, recursive = TRUE, force = TRUE)
  on.exit(unlink(tmp, recursive = TRUE, force = TRUE), add = TRUE)

  # Seed a full skeleton once.
  suppressMessages(tf_update_skeleton(tmp, overwrite = TRUE, silent = TRUE, local = TRUE))
  expect_message(
    again <- tf_update_skeleton(tmp, overwrite = FALSE, silent = FALSE, local = TRUE),
    "up to date"
  )
  expect_false(again)
})

# --- Describe print edge cases ------------------------------------------------

test_that("print.tf_description handles truncated entities and missing entities", {
  d <- structure(
    list(
      template = "Tiny",
      metadata = list(
        Name = "Tiny",
        Species = "Homo sapiens",
        ReferencesAndLinks = list("https://doi.org/10.0/1", "https://doi.org/10.0/2",
                                  "https://doi.org/10.0/3", "https://doi.org/10.0/4")
      ),
      entities = list(
        suffix = c("T1w", "T2w", "mask", "probseg", "dseg", "xfm", "bold",
                   "sphere", "surf", "label", "extra")
      )
    ),
    class = "tf_description"
  )
  out <- capture.output(print(d, max_values = 3L))
  expect_true(any(grepl("References: 4", out)))
  expect_true(any(grepl("more", out)))

  empty <- structure(
    list(template = "Empty", metadata = list(), entities = list()),
    class = "tf_description"
  )
  out_empty <- capture.output(print(empty))
  expect_true(any(grepl("no entity values", out_empty)))
})

# --- CLI remaining branches ---------------------------------------------------

test_that("tf_cli wipe --help and interactive abort work", {
  expect_true(any(grepl("Usage:", capture.output(tf_cli(c("wipe", "--help"))))))
  expect_true(any(grepl("Usage:", capture.output(tf_cli(c("config", "--help"))))))
  expect_true(any(grepl("Usage:", capture.output(tf_cli(c("meta", "--help"))))))

  tmp <- file.path(tempdir(), "tf-cli-wipe-interactive")
  if (dir.exists(tmp)) unlink(tmp, recursive = TRUE, force = TRUE)
  on.exit(unlink(tmp, recursive = TRUE, force = TRUE), add = TRUE)
  TemplateFlowClient(root = tmp)

  testthat::local_mocked_bindings(
    is_interactive = function() TRUE,
    .package = "rlang"
  )
  testthat::local_mocked_bindings(
    readline = function(...) "no",
    .package = "base"
  )
  out <- capture.output(res <- tf_cli(c("--root", tmp, "wipe")))
  expect_false(res)
  expect_true(any(grepl("Aborted", out)))
  expect_true(dir.exists(tmp))
})

test_that("tf_cli rejects unrecognized filter args and prints help when empty", {
  expect_true(any(grepl("Usage:", capture.output(tf_cli(character())))))

  expect_error(
    tf_cli(c("--root", tempdir(), "ls", "MNI152Lin", "--not-a-real-flag", "1")),
    class = "tf_cli_error"
  )
})

test_that("tf_coerce_entities converts integer-typed CLI filters", {
  coerced <- tf_coerce_entities(list(resolution = "1", density = "01", suffix = "T1w"))
  expect_type(coerced$resolution, "integer")
  expect_equal(coerced$resolution, 1L)
  expect_equal(coerced$suffix, "T1w")
})

test_that("tf_validate_template suggests close matches", {
  tmp <- file.path(tempdir(), "tf-validate-tpl")
  if (dir.exists(tmp)) unlink(tmp, recursive = TRUE, force = TRUE)
  on.exit(unlink(tmp, recursive = TRUE, force = TRUE), add = TRUE)
  client <- TemplateFlowClient(root = tmp)

  expect_error(
    tf_validate_template("MNI152LinX", client$cache),
    class = "templateflow_error"
  )
})

test_that("tf_cli_help_command reports missing help text", {
  out <- capture.output(tf_cli_help_command("not-a-command"))
  expect_true(any(grepl("No help available", out)))
})

test_that("tf_fetch_files succeeds after DataLad get fills stubs", {
  setup_root <- tempfile("tf-fetch-dl-ok-")
  dir.create(setup_root)
  cfg <- tf_default_config(root = setup_root, use_datalad = TRUE, timeout = 1)
  on.exit(unlink(cfg$root, recursive = TRUE, force = TRUE), add = TRUE)
  cache <- TemplateFlowCache(config = cfg)
  missing <- file.path(cfg$root, "tpl-Demo", "filled.bin")
  dir.create(dirname(missing), recursive = TRUE)
  file.create(missing)

  testthat::local_mocked_bindings(
    tf_datalad_available = function() TRUE,
    tf_datalad_get = function(files, dataset) {
      writeBin(charToRaw("from-datalad"), missing)
      invisible(files)
    },
    .package = "templateflow"
  )
  expect_true(suppressMessages(tf_fetch_files(cache, missing)))
  expect_equal(rawToChar(readBin(missing, raw(), n = 32)), "from-datalad")
})
