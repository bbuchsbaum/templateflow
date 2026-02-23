# TemplateFlow client ----------------------------------------------------------

#' TemplateFlow R client
#'
#' Create a client for querying the TemplateFlow archive.
#' @param root Optional cache root (defaults to `~/.cache/templateflow` or `TEMPLATEFLOW_HOME`).
#' @param cache Optional pre-built `TemplateFlowCache`.
#' @param ... Additional config passed to `tf_default_config()`.
#' @return A `TemplateFlowClient` object.
#' @examples
#' \dontrun{
#' client <- TemplateFlowClient()
#' client <- TemplateFlowClient(root = "/tmp/tf-cache")
#' }
#' @export
TemplateFlowClient <- function(root = NULL, cache = NULL, ...) {
  if (is.null(cache)) {
    config <- tf_default_config(root = root, ...)
    cache <- TemplateFlowCache(config = config)
  } else if (!is.null(root) || length(list(...))) {
    tf_abort("If `cache` is provided, `root` and other config arguments cannot be used.")
  }
  cache <- tf_cache_ensure(cache)
  client <- list(cache = cache)
  class(client) <- "TemplateFlowClient"
  client
}

.tf_state <- new.env(parent = emptyenv())

#' Get or create the default global TemplateFlow client
#' @return A `TemplateFlowClient`.
#' @export
tf_client <- function() {
  if (!exists("client", envir = .tf_state, inherits = FALSE)) {
    .tf_state$client <- TemplateFlowClient()
  }
  .tf_state$client
}

#' Set global client
#' @param client A `TemplateFlowClient`.
#' @return The client (invisibly).
#' @examples
#' \dontrun{
#' client <- TemplateFlowClient()
#' tf_set_client(client)
#' }
#' @export
tf_set_client <- function(client) {
  if (!inherits(client, "TemplateFlowClient")) tf_abort("client must be a TemplateFlowClient")
  .tf_state$client <- client
  invisible(client)
}

#' Print a TemplateFlowClient
#' @param x A `TemplateFlowClient` object.
#' @param ... Additional arguments (ignored).
#' @return `x`, invisibly.
#' @exportS3Method print TemplateFlowClient
print.TemplateFlowClient <- function(x, ...) {
  cache <- x$cache
  mode <- if (isTRUE(cache$config$use_datalad)) "DataLad" else "S3"
  cat(sprintf("<TemplateFlowClient[%s] cache=\"%s\">\n", mode, cache$config$root))
  invisible(x)
}

#' List template assets
#'
#' Query the TemplateFlow cache for files matching a template and optional
#' entity filters.
#'
#' @details
#' Entity filters are passed via `...`.
#' Use `NULL` to *exclude* an entity — for example, `desc = NULL` returns only
#' files that do **not** have a `desc-` key in their filename.  This matches the
#' Python client's behaviour where `desc=None` filters out files with a
#' description tag.
#'
#' @param client A `TemplateFlowClient` (optional; default global client).
#' @param template Template identifier (e.g., `"MNI152Lin"`).
#' @param as_df If `TRUE`, return a data frame instead of file paths.
#' @param ... Entity filters (resolution, suffix, atlas, desc, hemi, space,
#'   density, label, segmentation, cohort, scale, roi, extension, etc.).
#'   Pass `NULL` to exclude files containing that entity.
#' @return Character vector of file paths, or a data frame if `as_df = TRUE`.
#' @examples
#' \dontrun{
#' tf_ls(template = "MNI152Lin", resolution = 1, suffix = "T1w")
#' tf_ls(template = "MNI152Lin", extension = ".nii.gz")
#' tf_ls(template = "MNI152Lin", as_df = TRUE)
#' }
#' @export
tf_ls <- function(client = NULL, template, as_df = FALSE, ...) {
  default_client <- is.null(client)
  client <- client %||% tf_client()
  if (!inherits(client, "TemplateFlowClient")) tf_abort("client must be a TemplateFlowClient")
  cache <- tf_cache_ensure(client$cache)
  client$cache <- cache
  if (default_client) tf_set_client(client)
  filters <- list(...)
  if (!missing(template)) {
    filters$template <- template %||% NULL
  }
  if ("extension" %in% names(filters)) {
    filters$extension <- tf_normalize_ext(filters$extension)
  }
  if (isTRUE(as_df)) {
    args <- c(list(layout = cache$layout, return_type = "data.frame"), filters)
    df <- do.call(tf_layout_get, args)
    if (requireNamespace("tibble", quietly = TRUE)) {
      return(tibble::as_tibble(df))
    }
    return(df)
  }
  args <- c(list(layout = cache$layout, return_type = "file"), filters)
  paths <- do.call(tf_layout_get, args)
  normalizePath(paths, winslash = "/", mustWork = FALSE)
}

#' Fetch template assets (downloading if needed)
#'
#' Download template files from the TemplateFlow archive and optionally read
#' them into R objects.  Like [tf_ls()], entity filters support `NULL` values
#' to exclude files containing that entity.
#'
#' @inheritParams tf_ls
#' @param raise_empty If `TRUE`, error when no files match the query.
#' @param read If `TRUE`, read downloaded files into R objects using appropriate
#'   readers (NIfTI via RNifti, GIFTI via gifti, JSON, TSV). Requires optional
#'   packages for neuroimaging formats.
#' @return Path string, character vector, or R object(s) if `read = TRUE`.
#'   When a single file matches, a scalar path (or single R object) is returned;
#'   when multiple files match, a character vector (or list of R objects).
#' @examples
#' \dontrun{
#' path <- tf_get(template = "MNI152Lin", resolution = 1, suffix = "T1w")
#' img <- tf_get(template = "MNI152Lin", resolution = 1, suffix = "T1w", read = TRUE)
#' }
#' @export
tf_get <- function(client = NULL, template, raise_empty = FALSE, read = FALSE, ...) {
  paths <- tf_ls(client, template, ...)
  if (raise_empty && !length(paths)) {
    tf_abort_not_found("No results found")
  }
  tf_truncate_s3_errors(paths)
  client <- client %||% tf_client()
  tf_fetch_files(client$cache, paths)
  missing <- paths[!file.exists(paths) | file.info(paths)$size == 0]
  if (length(missing)) {
    tf_abort_cache(paste0("Could not fetch template files: ", paste(missing, collapse = ", ")))
  }
  if (isTRUE(read)) {
    objects <- lapply(paths, tf_read_file)
    if (length(objects) == 1) return(objects[[1]])
    return(objects)
  }
  if (length(paths) == 1) return(paths[[1]])
  paths
}

#' Batch prefetch template files
#'
#' Downloads all zero-byte skeleton stubs matching the given filters.
#' @param client A `TemplateFlowClient` (optional; default global client).
#' @param templates Character vector of template names to prefetch. If NULL,
#'   prefetches all templates.
#' @param ... Entity filters passed to `tf_ls()`.
#' @return Invisible count of files downloaded.
#' @examples
#' \dontrun{
#' tf_prefetch(templates = "MNI152Lin")
#' tf_prefetch()
#' }
#' @export
tf_prefetch <- function(client = NULL, templates = NULL, ...) {
  client <- client %||% tf_client()
  cache <- tf_cache_ensure(client$cache)

  if (is.null(templates)) {
    templates <- tf_templates(client)
  }

  count <- 0L
  for (tpl in templates) {
    paths <- tf_ls(client, template = tpl, ...)
    info <- file.info(paths)
    stubs <- paths[is.na(info$size) | info$size == 0]
    if (length(stubs)) {
      if (requireNamespace("cli", quietly = TRUE)) {
        cli::cli_progress_bar(
          format = paste0("Downloading {tpl}: {cli::pb_current}/{cli::pb_total}"),
          total = length(stubs)
        )
      }
      for (fp in stubs) {
        tryCatch(
          {
            tf_download_file(cache$config, fp)
            count <- count + 1L
          },
          error = function(e) {
            tf_log("Failed to download: ", fp, " (", conditionMessage(e), ")")
          }
        )
        if (requireNamespace("cli", quietly = TRUE)) {
          cli::cli_progress_update()
        }
      }
      if (requireNamespace("cli", quietly = TRUE)) {
        cli::cli_progress_done()
      }
    }
  }

  tf_log("Downloaded ", count, " file(s).")
  invisible(count)
}

#' List available templates
#' @inheritParams tf_ls
#' @return Character vector of template IDs.
#' @examples
#' \dontrun{
#' tf_templates()
#' }
#' @export
tf_templates <- function(client = NULL, ...) {
  default_client <- is.null(client)
  client <- client %||% tf_client()
  cache <- tf_cache_ensure(client$cache)
  client$cache <- cache
  if (default_client) tf_set_client(client)
  tf_layout_templates(cache$layout, ...)
}

#' Read template metadata
#' @inheritParams tf_ls
#' @return List parsed from `template_description.json`.
#' @examples
#' \dontrun{
#' meta <- tf_get_metadata(template = "MNI152Lin")
#' meta$Name
#' }
#' @export
tf_get_metadata <- function(client = NULL, template) {
  default_client <- is.null(client)
  client <- client %||% tf_client()
  cache <- tf_cache_ensure(client$cache)
  client$cache <- cache
  if (default_client) tf_set_client(client)
  path <- file.path(cache$config$root, sprintf("tpl-%s", template), "template_description.json")
  tf_fetch_files(cache, path)
  jsonlite::fromJSON(path)
}

#' Retrieve template citations
#' @inheritParams tf_ls
#' @param bibtex If TRUE, fetch citations in BibTeX format.
#' @return Character vector of citation URLs or BibTeX entries.
#' @examples
#' \dontrun{
#' tf_get_citations(template = "MNI152NLin2009cAsym")
#' tf_get_citations(template = "MNI152NLin2009cAsym", bibtex = TRUE)
#' }
#' @export
tf_get_citations <- function(client = NULL, template, bibtex = FALSE) {
  data <- tf_get_metadata(client, template)
  refs <- data$ReferencesAndLinks
  if (is.null(refs)) return(character(0))
  if (is.list(refs)) refs <- unlist(refs, use.names = FALSE)
  if (!bibtex) return(refs)
  default_client <- is.null(client)
  client <- client %||% tf_client()
  vapply(refs, tf_to_bibtex, character(1), timeout = client$cache$config$timeout)
}

tf_to_bibtex <- function(doi, timeout) {
  if (!grepl("doi.org", doi)) return(doi)
  resp <- tryCatch(
    httr2::request(doi) |>
      httr2::req_method("POST") |>
      httr2::req_headers(Accept = "application/x-bibtex; charset=utf-8") |>
      httr2::req_timeout(timeout) |>
      httr2::req_error(is_error = function(resp) FALSE) |>
      httr2::req_perform(),
    error = function(e) NULL
  )
  if (is.null(resp) || httr2::resp_status(resp) >= 400) {
    tf_log("Failed to convert DOI <", doi, "> to bibtex, returning URL.")
    return(doi)
  }
  bib <- rawToChar(httr2::resp_body_raw(resp))
  sub("http://dx.doi.org/", "https://doi.org/", bib, fixed = TRUE)
}
