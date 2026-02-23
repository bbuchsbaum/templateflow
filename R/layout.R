# Layout indexing and query helpers -------------------------------------------

# Module-level cache for config entities
.layout_env <- new.env(parent = emptyenv())

tf_load_config <- function() {
  if (!is.null(.layout_env$config)) return(.layout_env$config)
  config_path <- tf_extdata_path("config.json")
  .layout_env$config <- jsonlite::fromJSON(config_path, simplifyVector = FALSE)
  .layout_env$config
}

tf_get_entities <- function() {
  if (!is.null(.layout_env$entities)) return(.layout_env$entities)
  cfg <- tf_load_config()
  ents <- cfg$entities
  # Filter out meta-entities (suffix, extension, description) — handled separately
  skip <- c("suffix", "extension", "description")
  ents <- Filter(function(e) !e$name %in% skip, ents)
  .layout_env$entities <- ents
  ents
}

#' List available BIDS entity names
#'
#' Returns the entity names recognised by TemplateFlow for filtering
#' template files (e.g. `resolution`, `atlas`, `suffix`).
#' @return A character vector of entity names sorted alphabetically.
#' @examples
#' tf_entities()
#' @export
tf_entities <- function() {
  ents <- tf_get_entities()
  nms <- vapply(ents, function(e) e$name, character(1))
  sort(unique(c(nms, "suffix", "extension")))
}

tf_build_layout <- function(root) {
  files <- list.files(root, recursive = TRUE, full.names = TRUE, all.files = FALSE, no.. = TRUE)
  if (!length(files)) {
    return(structure(list(root = root, index = data.frame()), class = "TemplateFlowLayout"))
  }
  files <- files[!grepl("templateflow-skel\\.zip$", files)]
  files <- files[!grepl("/scripts/", files)]
  info <- file.info(files)
  files <- files[!info$isdir]
  idx <- lapply(files, tf_parse_tpl_path, root = root)
  # Union all columns across rows (some may have different columns)
  all_names <- unique(unlist(lapply(idx, names)))
  idx <- lapply(idx, function(row) {
    missing <- setdiff(all_names, names(row))
    if (length(missing)) {
      for (nm in missing) row[[nm]] <- NA
    }
    row[all_names]
  })
  idx <- do.call(rbind, idx)
  structure(list(root = root, index = idx), class = "TemplateFlowLayout")
}

tf_parse_tpl_path <- function(path, root) {
  norm_root <- normalizePath(root, mustWork = FALSE)
  norm_path <- normalizePath(path, mustWork = FALSE)
  # Remove root prefix using fixed string matching
  rel <- norm_path
  if (startsWith(rel, norm_root)) {
    rel <- substring(rel, nchar(norm_root) + 1)
  }
  rel <- gsub("^/+", "", rel)
  fname <- basename(path)

  bidser_suffix <- NA_character_
  if (requireNamespace("bidser", quietly = TRUE)) {
    parsed <- try(bidser::encode(fname), silent = TRUE)
    if (!inherits(parsed, "try-error") && !is.null(parsed) && !is.null(parsed$suffix)) {
      bidser_suffix <- parsed$suffix
    }
  }

  # Dynamic entity extraction from config.json
  entities <- tf_get_entities()
  row <- list(path = path, relpath = rel)

  # Prepend / so patterns requiring a leading separator always match
  match_str <- paste0("/", rel)

  for (ent in entities) {
    pat <- ent$pattern
    m <- regmatches(match_str, regexpr(pat, match_str, perl = TRUE))
    val <- NA_character_
    if (length(m) && nchar(m) > 0) {
      cap <- regmatches(match_str, regexec(pat, match_str, perl = TRUE))[[1]]
      if (length(cap) >= 2) val <- cap[2]
    }
    # Apply dtype coercion
    if (!is.na(val) && !is.null(ent$dtype) && ent$dtype == "int") {
      row[[ent$name]] <- as.integer(val)
    } else if (!is.na(val)) {
      row[[ent$name]] <- val
    } else {
      # Preserve correct NA type
      if (!is.null(ent$dtype) && ent$dtype == "int") {
        row[[ent$name]] <- NA_integer_
      } else {
        row[[ent$name]] <- NA_character_
      }
    }
  }

  # Suffix and extension — special-case template_description.json first
  dot_pos <- regexpr("\\.", fname)[1]
  extension <- if (dot_pos > 0) substr(fname, dot_pos, nchar(fname)) else NA_character_

  suffix <- NA_character_
  if (grepl("template_description\\.json$", fname)) {
    suffix <- "template_description"
    extension <- ".json"
  } else if (dot_pos > 0) {
    prefix <- substr(fname, 1, dot_pos - 1)
    us_pos <- max(gregexpr("_", prefix)[[1]])
    if (us_pos > 0) {
      suffix <- substr(prefix, us_pos + 1, nchar(prefix))
    } else {
      suffix <- prefix
    }
  }

  if (is.na(suffix) && !is.na(bidser_suffix)) {
    suffix <- bidser_suffix
  }

  row$suffix <- suffix
  row$extension <- extension

  as.data.frame(row, stringsAsFactors = FALSE)
}

tf_layout_get <- function(layout, return_type = c("file", "data.frame"), ...) {
  if (!inherits(layout, "TemplateFlowLayout")) tf_abort("layout must be a TemplateFlowLayout")
  return_type <- match.arg(return_type)
  filters <- list(...)
  idx <- layout$index
  if (!length(idx) || nrow(idx) == 0) return(if (return_type == "file") character(0) else idx)

  for (name in names(filters)) {
    val <- filters[[name]]
    if (is.null(val)) next
    if (!name %in% names(idx)) {
      # Fuzzy match suggestion
      valid_names <- setdiff(names(idx), c("path", "relpath"))
      close <- agrep(name, valid_names, max.distance = 0.3, value = TRUE)
      hint <- if (length(close)) paste0(". Did you mean: ", paste(close, collapse = ", "), "?") else ""
      tf_abort_invalid_filter(paste0("Unknown query field: ", name, hint))
    }
    column <- idx[[name]]
    if (length(val) == 1 && is.na(val)) {
      keep <- is.na(column) | column == ""
    } else {
      keep <- column %in% val
    }
    idx <- idx[keep, , drop = FALSE]
  }

  if (return_type == "data.frame") {
    return(idx)
  }
  idx$path
}

tf_layout_templates <- function(layout, ...) {
  if (!inherits(layout, "TemplateFlowLayout")) tf_abort("layout must be a TemplateFlowLayout")
  idx <- tf_layout_get(layout, return_type = "data.frame", ...)
  sort(unique(stats::na.omit(idx$template)))
}
