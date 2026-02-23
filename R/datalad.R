# DataLad backend support ------------------------------------------------------

#' Check if DataLad is available
#'
#' Tests whether the `datalad` command-line tool is installed and accessible.
#' @return TRUE if datalad is available, FALSE otherwise.
tf_datalad_available <- function() {
  result <- tryCatch(
    system2("datalad", "--version", stdout = TRUE, stderr = TRUE),
    error = function(e) NULL,
    warning = function(w) NULL
  )
  !is.null(result) && length(result) > 0
}

#' Install a DataLad dataset
#'
#' Clone/install a DataLad dataset to a local path.
#' @param path Local destination path.
#' @param source Dataset source URL or path.
#' @param recursive If TRUE, install subdatasets recursively.
#' @return Invisible path.
#' @examples
#' \dontrun{
#' tf_datalad_install("~/.cache/templateflow",
#'   source = "https://github.com/templateflow/templateflow.git")
#' }
#' @export
tf_datalad_install <- function(path, source, recursive = TRUE) {
  if (!tf_datalad_available()) {
    tf_abort("DataLad is not installed or not found on PATH")
  }
  args <- c("install", "-s", source)
  if (recursive) args <- c(args, "-r")
  args <- c(args, path)
  status <- system2("datalad", args, stdout = "", stderr = "")
  if (status != 0) {
    tf_abort_network(paste0("DataLad install failed for source: ", source))
  }
  invisible(path)
}

#' Get files from a DataLad dataset
#'
#' Retrieve (download) specific files from a DataLad dataset.
#' @param files Character vector of file paths to retrieve.
#' @param dataset Path to the DataLad dataset root.
#' @return Invisible files vector.
#' @examples
#' \dontrun{
#' tf_datalad_get(c("tpl-MNI152Lin/tpl-MNI152Lin_res-01_T1w.nii.gz"),
#'   dataset = "~/.cache/templateflow")
#' }
#' @export
tf_datalad_get <- function(files, dataset) {
  if (!tf_datalad_available()) {
    tf_abort("DataLad is not installed or not found on PATH")
  }
  args <- c("get", "-d", dataset, files)
  status <- system2("datalad", args, stdout = "", stderr = "")
  if (status != 0) {
    tf_abort_network(paste0("DataLad get failed for dataset: ", dataset))
  }
  invisible(files)
}

#' Update a DataLad dataset
#'
#' Fetch updates from the remote and optionally merge.
#' @param dataset Path to the DataLad dataset root.
#' @param recursive If TRUE, update subdatasets recursively.
#' @param merge If TRUE, merge fetched changes.
#' @return Invisible TRUE.
#' @examples
#' \dontrun{
#' tf_datalad_update("~/.cache/templateflow", recursive = TRUE, merge = TRUE)
#' }
#' @export
tf_datalad_update <- function(dataset, recursive = TRUE, merge = TRUE) {
  if (!tf_datalad_available()) {
    tf_abort("DataLad is not installed or not found on PATH")
  }
  args <- c("update", "-d", dataset)
  if (recursive) args <- c(args, "-r")
  if (merge) args <- c(args, "--merge")
  status <- system2("datalad", args, stdout = "", stderr = "")
  if (status != 0) {
    tf_abort_network(paste0("DataLad update failed for dataset: ", dataset))
  }
  invisible(TRUE)
}
