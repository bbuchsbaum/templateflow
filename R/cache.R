# Cache management -------------------------------------------------------------

TemplateFlowCache <- function(config = tf_default_config()) {
  cache <- list(
    config = config,
    precached = tf_cached(config$root),
    layout = NULL
  )
  class(cache) <- "TemplateFlowCache"
  cache
}

tf_cached <- function(root) {
  file.exists(root) && length(list.files(root, all.files = TRUE, no.. = TRUE)) > 0
}

tf_cache_ensure <- function(cache) {
  if (!inherits(cache, "TemplateFlowCache")) {
    tf_abort("cache must be a TemplateFlowCache object")
  }
  if (!tf_cached(cache$config$root)) {
    dir.create(cache$config$root, recursive = TRUE, showWarnings = FALSE)
    tf_update_skeleton(cache$config$root, overwrite = TRUE, silent = FALSE, timeout = cache$config$timeout)
  }
  cache$layout <- cache$layout %||% tf_build_layout(cache$config$root)
  cache
}

#' Update template cache
#'
#' Re-extract the skeleton archive, optionally fetching the latest version from
#' the remote repository.
#' @param cache A `TemplateFlowCache` object (optional; uses default client cache).
#' @param local If `TRUE`, only use the bundled skeleton (no network).
#' @param overwrite If `TRUE`, overwrite existing stub files.
#' @param silent If `TRUE`, suppress progress messages.
#' @return The updated cache (invisibly).
#' @examples
#' \dontrun{
#' tf_cache_update()
#' tf_cache_update(local = TRUE)
#' }
#' @export
tf_cache_update <- function(cache = NULL, local = FALSE, overwrite = TRUE, silent = FALSE) {
  cache <- cache %||% tf_client()$cache
  if (local) {
    withr::with_options(
      list(templateflow.test.forcebundled = TRUE),
      tf_update_skeleton(cache$config$root, overwrite = overwrite, silent = silent, timeout = cache$config$timeout)
    )
  } else {
    tf_update_skeleton(cache$config$root, overwrite = overwrite, silent = silent, timeout = cache$config$timeout)
  }
  cache$layout <- NULL
  invisible(cache)
}

#' Wipe the template cache
#'
#' Delete the entire local template cache directory.  Not supported when using
#' the DataLad backend (use `datalad drop` instead).
#' @param cache A `TemplateFlowCache` object (optional; uses default client cache).
#' @return The cache object (invisibly).
#' @examples
#' \dontrun{
#' tf_cache_wipe()
#' }
#' @export
tf_cache_wipe <- function(cache = NULL) {
  cache <- cache %||% tf_client()$cache
  if (isTRUE(cache$config$use_datalad)) {
    tf_log("Cache wipe is not supported with DataLad backend. Use datalad drop instead.")
    return(invisible(cache))
  }
  cache$layout <- NULL
  if (dir.exists(cache$config$root)) {
    unlink(cache$config$root, recursive = TRUE, force = TRUE)
  }
  invisible(cache)
}

# Cache utilities --------------------------------------------------------------

#' Cache statistics
#'
#' Report the number and total size of cached files.
#' @param cache A `TemplateFlowCache` object or a character path to the cache
#'   root (optional; uses default client cache).
#' @return A list with `cache_files`, `cache_bytes`, and `cache_size_human`.
#' @examples
#' \dontrun{
#' tf_cache_stats()
#' }
#' @export
tf_cache_stats <- function(cache = NULL) {
  if (is.character(cache)) {
    root <- cache
  } else {
    cache <- cache %||% tf_client()$cache
    root <- cache$config$root
  }
  if (!dir.exists(root)) {
    return(list(cache_files = 0L, cache_bytes = 0L, cache_size_human = "0 B"))
  }
  files <- list.files(root, recursive = TRUE, full.names = TRUE)
  info <- file.info(files)
  sizes <- info$size[!is.na(info$size)]
  list(
    cache_files = length(files),
    cache_bytes = sum(sizes),
    cache_size_human = tf_human_size(sum(sizes))
  )
}

#' Scan cache for problematic files
#'
#' Find zero-byte stubs and S3 XML error responses in the cache.
#' @param cache A `TemplateFlowCache` object or a character path to the cache
#'   root (optional; uses default client cache).
#' @return A list with `zero` (zero-byte paths) and `xml` (XML error paths).
#' @examples
#' \dontrun{
#' tf_cache_scan()
#' }
#' @export
tf_cache_scan <- function(cache = NULL) {
  if (is.character(cache)) {
    root <- cache
  } else {
    cache <- cache %||% tf_client()$cache
    root <- cache$config$root
  }
  if (!dir.exists(root)) {
    return(list(zero = character(0), xml = character(0)))
  }
  files <- list.files(root, recursive = TRUE, full.names = TRUE)
  info <- file.info(files)
  zero <- files[!is.na(info$size) & info$size == 0]
  xml <- files[vapply(files, tf_is_xml_error, logical(1))]
  list(zero = zero, xml = xml)
}

#' Refresh cache layout
#'
#' Rebuild the layout index without downloading any files.
#' @param cache A `TemplateFlowCache` object or a character path to the cache
#'   root (optional; uses default client cache).
#' @return The updated cache (invisibly), or a `TemplateFlowLayout` if a
#'   character path was supplied.
#' @examples
#' \dontrun{
#' tf_cache_refresh()
#' }
#' @export
tf_cache_refresh <- function(cache = NULL) {
  if (is.character(cache)) {
    root <- cache
    layout <- tf_build_layout(root)
    return(invisible(layout))
  }
  cache <- cache %||% tf_client()$cache
  cache$layout <- tf_build_layout(cache$config$root)
  invisible(cache)
}

# Skeleton handling ------------------------------------------------------------

tf_skeleton_zip <- function() {
  tf_extdata_path("templateflow-skel.zip")
}

tf_skeleton_md5 <- function() {
  md5file <- try(tf_extdata_path("templateflow-skel.md5"), silent = TRUE)
  if (inherits(md5file, "try-error") || !file.exists(md5file)) return(NULL)
  trimws(readChar(md5file, nchars = file.info(md5file)$size))
}

tf_fetch_remote_md5 <- function(timeout) {
  url <- "https://raw.githubusercontent.com/templateflow/python-client/master/templateflow/conf/templateflow-skel.md5"
  resp <- tryCatch(
    httr2::request(url) |>
      httr2::req_timeout(timeout) |>
      httr2::req_error(is_error = function(resp) FALSE) |>
      httr2::req_perform(),
    error = function(e) NULL
  )
  if (is.null(resp) || httr2::resp_status(resp) >= 400) {
    return(NULL)
  }
  trimws(rawToChar(httr2::resp_body_raw(resp)))
}

tf_fetch_remote_zip <- function(timeout) {
  url <- "https://raw.githubusercontent.com/templateflow/python-client/master/templateflow/conf/templateflow-skel.zip"
  tmp <- tempfile(fileext = ".zip")
  resp <- tryCatch(
    httr2::request(url) |>
      httr2::req_timeout(timeout) |>
      httr2::req_error(is_error = function(resp) FALSE) |>
      httr2::req_perform(),
    error = function(e) NULL
  )
  if (is.null(resp) || httr2::resp_status(resp) >= 400) {
    return(NULL)
  }
  writeBin(httr2::resp_body_raw(resp), con = tmp)
  tmp
}

tf_latest_skeleton <- function(timeout) {
  # Allow tests to force bundled without hitting network
  if (isTRUE(getOption("templateflow.test.forcebundled", FALSE))) {
    return(list(path = tf_skeleton_zip(), cleanup = FALSE))
  }
  bundled_md5 <- tf_skeleton_md5()
  remote_md5 <- tf_fetch_remote_md5(timeout)

  if (!is.null(remote_md5) && !is.null(bundled_md5) && !identical(remote_md5, bundled_md5)) {
    remote_zip <- tf_fetch_remote_zip(timeout)
    if (!is.null(remote_zip)) {
      return(list(path = remote_zip, cleanup = TRUE))
    }
  }
  list(path = tf_skeleton_zip(), cleanup = FALSE)
}

tf_update_skeleton <- function(dest, overwrite = TRUE, silent = FALSE, timeout = 10) {
  skel <- tf_latest_skeleton(timeout)
  dest <- normalizePath(dest, mustWork = FALSE)
  dir.create(dest, recursive = TRUE, showWarnings = FALSE)

  contents <- utils::unzip(skel$path, list = TRUE)
  entries <- contents$Name

  if (!overwrite) {
    existing <- file.exists(file.path(dest, entries))
    entries <- entries[!existing]
  }

  if (!length(entries)) {
    if (!silent) tf_log("TEMPLATEFLOW_HOME is up to date at ", dest)
    if (skel$cleanup) file.remove(skel$path)
    return(FALSE)
  }

  if (!silent) tf_log("Updating TEMPLATEFLOW_HOME at ", dest)
  utils::unzip(skel$path, files = entries, exdir = dest, overwrite = overwrite, junkpaths = FALSE)
  if (skel$cleanup) file.remove(skel$path)
  TRUE
}
