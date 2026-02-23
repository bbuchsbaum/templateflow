# Custom condition classes for TemplateFlow errors -------------------------

#' @importFrom rlang abort
tf_abort <- function(message, class = NULL, ..., call = NULL) {
  cls <- c(class, "templateflow_error")
  rlang::abort(message, class = cls, ..., call = call)
}

tf_abort_network <- function(message, ..., call = NULL) {
  tf_abort(message, class = "templateflow_network_error", ..., call = call)
}

tf_abort_not_found <- function(message, ..., call = NULL) {
  tf_abort(message, class = "templateflow_not_found", ..., call = call)
}

tf_abort_invalid_filter <- function(message, ..., call = NULL) {
  tf_abort(message, class = "templateflow_invalid_filter", ..., call = call)
}

tf_abort_cache <- function(message, ..., call = NULL) {
  tf_abort(message, class = "templateflow_cache_error", ..., call = call)
}
