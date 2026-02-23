# Neuroimaging file readers ----------------------------------------------------

#' Read a template file into an R object
#'
#' Dispatches by file extension to the appropriate reader. NIfTI files use
#' `RNifti` or `oro.nifti` (soft deps), GIFTI files use `gifti`, JSON files
#' use `jsonlite`, and TSV files use `utils::read.delim()`.
#' @param path Path to the file.
#' @return An R object appropriate for the file type, or the path if no reader
#'   is available.
tf_read_file <- function(path) {
  ext <- tf_file_extension(path)

  if (ext %in% c(".nii.gz", ".nii")) {
    if (requireNamespace("RNifti", quietly = TRUE)) {
      return(RNifti::readNifti(path))
    }
    if (requireNamespace("oro.nifti", quietly = TRUE)) {
      return(oro.nifti::readNIfTI(path, reorient = FALSE))
    }
    tf_log("No NIfTI reader available (install RNifti or oro.nifti). Returning path.")
    return(path)
  }

  if (ext %in% c(".gii", ".surf.gii", ".func.gii", ".shape.gii", ".label.gii")) {
    if (requireNamespace("gifti", quietly = TRUE)) {
      return(gifti::read_gifti(path))
    }
    tf_log("No GIFTI reader available (install gifti). Returning path.")
    return(path)
  }

  if (ext == ".json") {
    return(jsonlite::fromJSON(path))
  }

  if (ext == ".tsv") {
    return(utils::read.delim(path, stringsAsFactors = FALSE))
  }

  tf_log("No reader for extension '", ext, "'. Returning path.")
  path
}

#' Extract compound file extension
#'
#' Handles compound extensions like `.nii.gz`, `.surf.gii`, etc.
#' @param path File path.
#' @return The file extension including leading dot.
tf_file_extension <- function(path) {
  fname <- basename(path)
  # Check compound extensions first
  compound <- c(".nii.gz", ".surf.gii", ".func.gii", ".shape.gii", ".label.gii")
  for (ext in compound) {
    if (endsWith(fname, ext)) return(ext)
  }
  # Simple extension
  dot_pos <- regexpr("\\.[^.]+$", fname)
  if (dot_pos > 0) {
    return(substring(fname, dot_pos))
  }
  ""
}
