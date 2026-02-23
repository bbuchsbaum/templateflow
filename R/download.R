# S3 download helpers ---------------------------------------------------------

tf_relpath <- function(path, root) {
  path <- normalizePath(path, mustWork = FALSE)
  root <- normalizePath(root, mustWork = FALSE)
  sub(paste0("^", root, "/?"), "", path)
}

tf_s3_url <- function(config, filepath) {
  rel <- gsub("\\\\", "/", tf_relpath(filepath, config$root))
  paste0(rtrim_slash(config$s3_root), "/", rel)
}

tf_download_file <- function(config, filepath, max_retries = 3) {
  url <- tf_s3_url(config, filepath)
  dir.create(dirname(filepath), recursive = TRUE, showWarnings = FALSE)
  tf_log("Downloading ", url)

  if (requireNamespace("aws.s3", quietly = TRUE)) {
    ok <- try(
      aws.s3::save_object(
        object = tf_relpath(filepath, config$root),
        bucket = "templateflow",
        file = filepath,
        base_url = "s3.amazonaws.com",
        use_https = TRUE,
        check_region = FALSE
      ),
      silent = TRUE
    )
    if (!inherits(ok, "try-error")) {
      return(invisible(filepath))
    }
    tf_log("aws.s3 download failed, falling back to HTTPS for ", url)
  }

  # Atomic write: download to temp file, then rename on success
  tmp <- tempfile(tmpdir = dirname(filepath))
  on.exit(unlink(tmp, force = TRUE), add = TRUE)

  last_err <- NULL
  for (attempt in seq_len(max_retries)) {
    result <- tryCatch(
      {
        httr2::request(url) |>
          httr2::req_timeout(config$timeout) |>
          httr2::req_error(is_error = function(resp) FALSE) |>
          httr2::req_perform(path = tmp)
      },
      error = function(e) e
    )

    if (inherits(result, "error")) {
      last_err <- result
      if (attempt < max_retries) {
        Sys.sleep(1 + attempt)
        next
      }
      tf_abort_network(paste0("Failed to download ", url, ": ", conditionMessage(last_err)))
    }

    status <- httr2::resp_status(result)
    if (status >= 400) {
      last_err <- paste0("HTTP ", status)
      if (attempt < max_retries) {
        Sys.sleep(1 + attempt)
        next
      }
      tf_abort_network(paste0("Failed to download ", url, " (status ", status, ")"))
    }

    # Success — atomic rename
    file.rename(tmp, filepath)
    return(invisible(filepath))
  }

  tf_abort_network(paste0("Failed to download ", url, " after ", max_retries, " attempts"))
}

tf_fetch_files <- function(cache, files) {
  if (!length(files)) return(invisible(TRUE))
  info <- file.info(files)
  missing <- files[is.na(info$size) | info$size == 0]
  if (!length(missing)) return(invisible(TRUE))

  # Try DataLad first when configured
  if (isTRUE(cache$config$use_datalad) && tf_datalad_available()) {
    rel_paths <- vapply(missing, tf_relpath, character(1), root = cache$config$root)
    result <- tryCatch(
      {
        tf_datalad_get(rel_paths, dataset = cache$config$root)
        TRUE
      },
      error = function(e) {
        tf_log("DataLad get failed, falling back to S3: ", conditionMessage(e))
        FALSE
      }
    )
    if (result) {
      # Re-check which files are still missing after datalad
      info2 <- file.info(missing)
      missing <- missing[is.na(info2$size) | info2$size == 0]
    }
  }

  if (length(missing) > 1 && requireNamespace("cli", quietly = TRUE)) {
    cli::cli_progress_bar(
      format = "Downloading {cli::pb_current}/{cli::pb_total} [{cli::pb_bar}] {cli::pb_eta}",
      total = length(missing)
    )
    for (fp in missing) {
      tf_download_file(cache$config, fp)
      cli::cli_progress_update()
    }
    cli::cli_progress_done()
  } else {
    for (fp in missing) {
      tf_download_file(cache$config, fp)
    }
  }
  invisible(TRUE)
}
