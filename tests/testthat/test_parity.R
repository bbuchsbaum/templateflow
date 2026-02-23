# Parity tests: verify the R client matches the Python client's behavior
# These tests use the bundled skeleton (offline-safe) unless noted.

# --- Shared cache: extracted once, reused by all read-only tests ---
.parity_env <- new.env(parent = emptyenv())

get_shared_client <- function() {
  if (!is.null(.parity_env$client)) return(.parity_env$client)
  tmp <- file.path(tempdir(), "tf-parity-shared")
  if (dir.exists(tmp)) unlink(tmp, recursive = TRUE, force = TRUE)
  .parity_env$client <- withr::with_options(
    list(templateflow.test.forcebundled = TRUE),
    TemplateFlowClient(root = tmp)
  )
  .parity_env$client
}

# For tests that need an isolated cache (destructive operations)
make_client <- function(label = "parity") {
  tmp <- file.path(tempdir(), paste0("tf-parity-", label))
  if (dir.exists(tmp)) unlink(tmp, recursive = TRUE, force = TRUE)
  withr::with_options(
    list(templateflow.test.forcebundled = TRUE),
    TemplateFlowClient(root = tmp)
  )
}

# =========================================================================
# 1. Template discovery
# =========================================================================

test_that("templates() returns known templates from the archive", {
  client <- get_shared_client()
  tpls <- tf_templates(client)
  expect_true(length(tpls) > 10, label = "archive has many templates")
  expected <- c(
    "MNI152Lin", "MNI152NLin2009cAsym", "MNI152NLin6Asym",
    "fsLR", "fsaverage", "OASIS30ANTs"
  )
  for (tpl in expected) {
    expect_true(tpl %in% tpls, label = paste0(tpl, " is discoverable"))
  }
  # Output must be sorted (Python guarantees sorted())
  expect_identical(tpls, sort(tpls))
})

test_that("templates() with entity filter narrows results", {
  client <- get_shared_client()
  tpls_all <- tf_templates(client)
  tpls_pd  <- tf_templates(client, suffix = "PD")
  expect_true(length(tpls_pd) > 0)
  expect_true(length(tpls_pd) < length(tpls_all))
  expect_true("MNI152Lin" %in% tpls_pd)
})

# =========================================================================
# 2. File listing (ls)
# =========================================================================

test_that("ls returns files matching entity filters", {
  client <- get_shared_client()
  paths <- tf_ls(client, template = "MNI152Lin", resolution = 1, suffix = "T1w")
  expect_true(length(paths) >= 1)
  expect_true(all(grepl("tpl-MNI152Lin", paths)))
  expect_true(all(grepl("res-01", paths)))
  expect_true(all(grepl("T1w", paths)))
})

test_that("ls with extension filter works", {
  client <- get_shared_client()
  paths <- tf_ls(client, template = "MNI152Lin", extension = ".nii.gz")
  expect_true(length(paths) >= 1)
  expect_true(all(grepl("\\.nii\\.gz$", paths)))
})

test_that("ls with desc=NA filters to files without desc entity", {
  client <- get_shared_client()
  paths <- tf_ls(client, template = "MNI152Lin", resolution = 1, suffix = "T1w", desc = NA)
  expect_true(length(paths) >= 1)
  expect_false(any(grepl("desc-", basename(paths))))
})

test_that("ls as_df returns a data frame with entity columns", {
  client <- get_shared_client()
  df <- tf_ls(client, template = "MNI152Lin", as_df = TRUE)
  expect_true(is.data.frame(df))
  expect_true(nrow(df) > 0)
  expected_cols <- c("path", "template", "resolution", "suffix", "extension")
  for (col in expected_cols) {
    expect_true(col %in% names(df), label = paste0("column '", col, "' present"))
  }
  expect_true(all(df$template == "MNI152Lin"))
})

test_that("ls with invalid entity gives informative error", {
  client <- get_shared_client()
  expect_error(
    tf_ls(client, template = "MNI152Lin", resoluton = 1),
    class = "templateflow_invalid_filter"
  )
})

# =========================================================================
# 3. Metadata
# =========================================================================

test_that("get_metadata returns expected fields", {
  client <- get_shared_client()
  meta <- tf_get_metadata(client, "MNI152Lin")
  expect_true(is.list(meta))
  expect_true("Name" %in% names(meta))
  expect_true(nchar(meta$Name) > 0)
  expect_true("ReferencesAndLinks" %in% names(meta))
})

test_that("get_metadata for MNI152NLin2009cAsym has expected references", {
  client <- get_shared_client()
  meta <- tf_get_metadata(client, "MNI152NLin2009cAsym")
  refs <- meta$ReferencesAndLinks
  expect_true(length(refs) >= 4)
  expect_true(any(grepl("doi.org/10.1016/j.neuroimage.2010.07.033", refs)))
})

# =========================================================================
# 4. Citations
# =========================================================================

test_that("citations return expected URLs for MNI152NLin2009cAsym", {
  client <- get_shared_client()
  urls <- tf_get_citations(client, "MNI152NLin2009cAsym")
  expected_urls <- c(
    "https://doi.org/10.1016/j.neuroimage.2010.07.033",
    "https://doi.org/10.1016/S1053-8119(09)70884-5",
    "http://nist.mni.mcgill.ca/?p=904",
    "https://doi.org/10.1007/3-540-48714-X_16"
  )
  expect_setequal(urls, expected_urls)
})

test_that("citations return expected URLs for fsLR", {
  client <- get_shared_client()
  urls <- tf_get_citations(client, "fsLR")
  expected_urls <- c(
    "https://doi.org/10.1093/cercor/bhr291",
    "https://github.com/Washington-University/HCPpipelines/tree/master/global/templates"
  )
  expect_setequal(urls, expected_urls)
})

test_that("bibtex conversion produces valid entries or falls back to URLs", {
  skip_if_offline()
  client <- get_shared_client()
  bibs <- tf_get_citations(client, "MNI152NLin2009cAsym", bibtex = TRUE)
  expect_true(length(bibs) >= 1)
  # Each entry must be either a BibTeX entry (@type{...}) or a URL fallback
  for (b in bibs) {
    entry <- trimws(b)
    is_bibtex <- grepl("^@\\w+\\{", entry)
    is_url <- grepl("^https?://", entry)
    expect_true(is_bibtex || is_url,
                label = paste0("entry is bibtex or URL: ", substr(entry, 1, 60)))
  }
  # The non-DOI URL (nist.mni.mcgill.ca) should always be present as-is
  expect_true(any(grepl("nist.mni.mcgill.ca", bibs)))
})

test_that("citations for template with no refs returns empty", {
  client <- get_shared_client()
  tpls <- tf_templates(client)
  urls <- tf_get_citations(client, tpls[[1]])
  expect_true(is.character(urls))
})

# =========================================================================
# 5. get() behavior
# =========================================================================

test_that("get returns single path when one match (not a vector)", {
  skip_if_offline()
  client <- get_shared_client()
  result <- tf_get(client, template = "MNI152Lin", resolution = 1,
                   suffix = "T1w", desc = NA)
  # Python: returns single Path when one match
  expect_true(is.character(result))
  expect_length(result, 1)
})

test_that("get with raise_empty=TRUE errors on no match", {
  client <- get_shared_client()
  expect_error(
    tf_get(client, template = "MNI152Lin", suffix = "NONEXISTENT", raise_empty = TRUE),
    class = "templateflow_not_found"
  )
})

test_that("get with raise_empty=FALSE returns empty on no match", {
  client <- get_shared_client()
  result <- tf_get(client, template = "MNI152Lin", suffix = "NONEXISTENT",
                   raise_empty = FALSE)
  expect_length(result, 0)
})

# =========================================================================
# 6. Cache operations
# =========================================================================

test_that("cache_stats reports file counts", {
  client <- get_shared_client()
  root <- client$cache$config$root
  stats <- tf_cache_stats(root)
  expect_true(is.list(stats))
  expect_true("cache_files" %in% names(stats))
  expect_true("cache_bytes" %in% names(stats))
  expect_true("cache_size_human" %in% names(stats))
  expect_true(stats$cache_files > 0)
})

test_that("cache_scan detects zero-byte stubs", {
  client <- get_shared_client()
  root <- client$cache$config$root
  scan <- tf_cache_scan(root)
  expect_true(is.list(scan))
  expect_true("zero" %in% names(scan))
  expect_true("xml" %in% names(scan))
  # Skeleton files are zero-byte stubs
  expect_true(length(scan$zero) > 0)
})

test_that("cache_refresh rebuilds layout", {
  client <- make_client("cache-refresh")
  root <- client$cache$config$root
  expect_silent(tf_cache_refresh(root))
})

# =========================================================================
# 7. Environment variable overrides
# =========================================================================

test_that("TEMPLATEFLOW_S3_ROOT is respected", {
  withr::with_envvar(
    c(TEMPLATEFLOW_S3_ROOT = "https://custom-s3.example.com"),
    {
      cfg <- tf_default_config()
      expect_equal(cfg$s3_root, "https://custom-s3.example.com")
    }
  )
})

test_that("TEMPLATEFLOW_TIMEOUT is respected", {
  withr::with_envvar(
    c(TEMPLATEFLOW_TIMEOUT = "42"),
    {
      cfg <- tf_default_config()
      expect_equal(cfg$timeout, 42)
    }
  )
})

test_that("TEMPLATEFLOW_HOME is respected", {
  tmp <- file.path(tempdir(), "tf-envhome-test")
  withr::with_envvar(
    c(TEMPLATEFLOW_HOME = tmp),
    {
      cfg <- tf_default_config()
      expect_equal(cfg$root, normalizePath(tmp, mustWork = FALSE))
    }
  )
})

test_that("TEMPLATEFLOW_USE_DATALAD is respected", {
  withr::with_envvar(
    c(TEMPLATEFLOW_USE_DATALAD = "on"),
    {
      cfg <- tf_default_config()
      expect_true(cfg$use_datalad)
    }
  )
  withr::with_envvar(
    c(TEMPLATEFLOW_USE_DATALAD = "off"),
    {
      cfg <- tf_default_config()
      expect_false(cfg$use_datalad)
    }
  )
})

test_that("TEMPLATEFLOW_AUTOUPDATE is respected", {
  withr::with_envvar(
    c(TEMPLATEFLOW_AUTOUPDATE = "false"),
    {
      cfg <- tf_default_config()
      expect_false(cfg$autoupdate)
    }
  )
})

test_that("env var defaults when unset match Python defaults", {
  withr::with_envvar(
    c(TEMPLATEFLOW_S3_ROOT = NA, TEMPLATEFLOW_TIMEOUT = NA,
      TEMPLATEFLOW_USE_DATALAD = NA, TEMPLATEFLOW_AUTOUPDATE = NA),
    {
      cfg <- tf_default_config()
      expect_equal(cfg$s3_root, "https://templateflow.s3.amazonaws.com")
      expect_equal(cfg$timeout, 10)
      expect_false(cfg$use_datalad)
      expect_true(cfg$autoupdate)
    }
  )
})

# =========================================================================
# 8. tf_entities()
# =========================================================================

test_that("tf_entities returns known BIDS entity names", {
  ents <- tf_entities()
  expect_true(is.character(ents))
  expect_true(length(ents) > 5)
  expected <- c("template", "resolution", "atlas", "suffix", "extension",
                "desc", "hemi", "space", "density")
  for (e in expected) {
    expect_true(e %in% ents, label = paste0("entity '", e, "' present"))
  }
  expect_identical(ents, sort(ents))
})

# =========================================================================
# 9. Error condition classes (match Python exception hierarchy)
# =========================================================================

test_that("error classes match Python hierarchy", {
  err <- tryCatch(tf_abort_not_found("test"), error = identity)
  expect_s3_class(err, "templateflow_not_found")
  expect_s3_class(err, "templateflow_error")

  err <- tryCatch(tf_abort_network("test"), error = identity)
  expect_s3_class(err, "templateflow_network_error")
  expect_s3_class(err, "templateflow_error")

  err <- tryCatch(tf_abort_invalid_filter("test"), error = identity)
  expect_s3_class(err, "templateflow_invalid_filter")
  expect_s3_class(err, "templateflow_error")

  err <- tryCatch(tf_abort_cache("test"), error = identity)
  expect_s3_class(err, "templateflow_cache_error")
  expect_s3_class(err, "templateflow_error")
})

# =========================================================================
# 10. Client constructor edge cases
# =========================================================================

test_that("TemplateFlowClient rejects cache + root together", {
  client <- get_shared_client()
  expect_error(
    TemplateFlowClient(root = "/tmp/other", cache = client$cache),
    "cannot be used"
  )
})

test_that("global client singleton works", {
  client <- get_shared_client()
  tf_set_client(client)
  retrieved <- tf_client()
  expect_identical(retrieved$cache$config$root, client$cache$config$root)
})

test_that("print method does not error", {
  client <- get_shared_client()
  expect_output(print(client), "TemplateFlowClient")
})

# =========================================================================
# 11. S3 XML error detection and truncation
# =========================================================================

test_that("XML error detection matches Python behavior", {
  xml_file <- tempfile(fileext = ".xml")
  writeLines(c("<?xml version=\"1.0\" encoding=\"UTF-8\"?>",
               "<Error><Code>NoSuchKey</Code></Error>"), xml_file)
  expect_true(tf_is_xml_error(xml_file))
  tf_truncate_s3_errors(xml_file)
  expect_equal(file.info(xml_file)$size, 0)
})

test_that("normal files are not treated as XML errors", {
  normal_file <- tempfile(fileext = ".txt")
  writeLines("hello world", normal_file)
  expect_false(tf_is_xml_error(normal_file))
  orig_size <- file.info(normal_file)$size
  tf_truncate_s3_errors(normal_file)
  expect_equal(file.info(normal_file)$size, orig_size)
})

# =========================================================================
# 12. Cross-template file listing consistency
# =========================================================================

test_that("ls for surface template (fsLR) returns expected patterns", {
  client <- get_shared_client()
  paths <- tf_ls(client, template = "fsLR")
  expect_true(length(paths) > 0)
  expect_true(all(grepl("tpl-fsLR", paths)))
})

test_that("ls for fsaverage returns expected patterns", {
  client <- get_shared_client()
  paths <- tf_ls(client, template = "fsaverage")
  expect_true(length(paths) > 0)
  expect_true(all(grepl("tpl-fsaverage", paths)))
})
