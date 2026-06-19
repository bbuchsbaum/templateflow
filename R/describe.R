# Template description / discovery --------------------------------------------

#' Describe a template
#'
#' Summarise what a template offers: name, identifier, citations, and the
#' BIDS entities (with their unique values) that are available for it. This
#' is the discovery counterpart to [tf_get()] — use it when you want to know
#' which `desc`, `label`, `resolution`, etc. values are valid before
#' constructing a query.
#'
#' @param client A `TemplateFlowClient` (optional; default global client).
#' @param template Template identifier (e.g., `"MNI152Lin"`).
#' @return An object of class `tf_description` (a list with `template`,
#'   `metadata`, and `entities`). It has a `print` method that renders a
#'   human-readable summary.
#' @examples
#' \dontrun{
#' tf_describe("MNI152Lin")
#' tf_describe("MNI152NLin2009cAsym")$entities$desc
#' }
#' @export
tf_describe <- function(client = NULL, template) {
  if (missing(template) || !is.character(template) || length(template) != 1) {
    tf_abort("`template` must be a single template identifier")
  }
  meta <- tf_get_metadata(client, template)
  ents <- tf_entities(client, template = template, values = TRUE)
  structure(
    list(template = template, metadata = meta, entities = ents),
    class = "tf_description"
  )
}

#' @exportS3Method print tf_description
print.tf_description <- function(x, max_values = 10L, ...) {
  cat(sprintf("<TemplateFlow description: %s>\n", x$template))
  meta <- x$metadata
  if (!is.null(meta$Name)) {
    cat("  Name: ", meta$Name, "\n", sep = "")
  }
  if (!is.null(meta$Species)) {
    cat("  Species: ", meta$Species, "\n", sep = "")
  }
  refs <- meta$ReferencesAndLinks
  if (!is.null(refs)) {
    if (is.list(refs)) refs <- unlist(refs, use.names = FALSE)
    cat("  References: ", length(refs), " entry(ies)\n", sep = "")
    for (r in refs[seq_len(min(3L, length(refs)))]) {
      cat("    - ", r, "\n", sep = "")
    }
    if (length(refs) > 3L) cat("    ... (", length(refs) - 3L, " more)\n", sep = "")
  }
  if (length(x$entities)) {
    cat("\n  Entities present:\n")
    nm_width <- max(nchar(names(x$entities)))
    for (nm in names(x$entities)) {
      vals <- x$entities[[nm]]
      shown <- vals
      more <- 0L
      if (length(shown) > max_values) {
        more <- length(shown) - max_values
        shown <- shown[seq_len(max_values)]
      }
      pad <- formatC(nm, width = nm_width, flag = "-")
      tail <- if (more > 0) sprintf(", ... (+%d more)", more) else ""
      cat("    ", pad, " : ", paste(shown, collapse = ", "), tail, "\n", sep = "")
    }
  } else {
    cat("\n  (no entity values found in cache for this template)\n")
  }
  invisible(x)
}
