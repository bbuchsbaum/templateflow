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
#'
#' When `template` is supplied, returns only the entities that are actually
#' present for that template (useful for discovering which filters make
#' sense). When `values = TRUE`, returns a named list of the unique values
#' observed for each entity instead of just the names.
#'
#' @param client A `TemplateFlowClient` (optional; default global client). Only
#'   used when `template` is given.
#' @param template Optional template identifier. If supplied, restricts the
#'   result to entities present for that template.
#' @param values If `TRUE`, return a named list of unique entity values
#'   instead of a character vector of names. Requires `template`.
#' @return A character vector of entity names, or (when `values = TRUE`) a
#'   named list of unique values per entity.
#' @examples
#' tf_entities()
#' \dontrun{
#' tf_entities(template = "MNI152Lin")
#' tf_entities(template = "MNI152Lin", values = TRUE)$desc
#' }
#' @export
tf_entities <- function(client = NULL, template = NULL, values = FALSE) {
  if (is.null(template)) {
    if (isTRUE(values)) {
      tf_abort("`values = TRUE` requires `template` to be supplied")
    }
    ents <- tf_get_entities()
    nms <- vapply(ents, function(e) e$name, character(1))
    return(sort(unique(c(nms, "suffix", "extension"))))
  }

  client <- client %||% tf_client()
  if (!inherits(client, "TemplateFlowClient")) tf_abort("client must be a TemplateFlowClient")
  cache <- tf_cache_ensure(client$cache)
  tf_assert_layout_nonempty(cache)

  df <- tf_layout_get(cache$layout, return_type = "data.frame", template = template)
  meta_cols <- c("path", "relpath", "template")
  cand <- setdiff(names(df), meta_cols)

  present_mask <- vapply(cand, function(nm) {
    col <- df[[nm]]
    any(!is.na(col) & nzchar(as.character(col)))
  }, logical(1))
  present <- cand[present_mask]

  if (!isTRUE(values)) {
    return(sort(unique(present)))
  }

  out <- lapply(present, function(nm) {
    col <- df[[nm]]
    col <- col[!is.na(col)]
    if (is.character(col)) col <- col[nzchar(col)]
    sort(unique(col))
  })
  names(out) <- present
  out[order(names(out))]
}

tf_build_layout <- function(root) {
  # Normalize the root up front so the paths returned by list.files() carry the
  # exact prefix tf_parse_tpl_path() strips. config$root may have been
  # normalized before the cache directory existed (mustWork = FALSE leaves an
  # unresolved path), so it can differ from the resolved path now that the
  # directory is present -- which would force a per-file normalizePath()
  # fallback on every one of the ~2.5k entries.
  root <- normalizePath(root, mustWork = FALSE)
  files <- list.files(root, recursive = TRUE, full.names = TRUE, all.files = FALSE, no.. = TRUE)
  if (!length(files)) {
    return(structure(list(root = root, index = data.frame()), class = "TemplateFlowLayout"))
  }
  files <- files[!grepl("templateflow-skel\\.zip$", files)]
  files <- files[!grepl("/scripts/", files)]
  # list.files(recursive = TRUE) already excludes directory entries, so every
  # path here is a file -- no need to stat each one to filter out directories.
  # Reuse the normalized root and entity table for every file instead of
  # recomputing per call (normalizePath() is a filesystem stat).
  entities <- tf_get_entities()
  idx <- lapply(
    files, tf_parse_tpl_path,
    root = root, norm_root = root, entities = entities
  )
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

tf_parse_tpl_path <- function(path, root, norm_root = NULL, entities = NULL) {
  if (is.null(norm_root)) norm_root <- normalizePath(root, mustWork = FALSE)
  # Strip the (normalized) root prefix to get the relative path. Paths coming
  # from list.files(root) already carry this prefix, so we avoid a per-file
  # normalizePath() (a filesystem stat on the hot path) and fall back to
  # normalizing only when the prefix does not already match.
  rel <- path
  if (!startsWith(rel, norm_root)) {
    rel <- normalizePath(path, mustWork = FALSE)
  }
  if (startsWith(rel, norm_root)) {
    rel <- substring(rel, nchar(norm_root) + 1)
  }
  rel <- gsub("^/+", "", rel)
  fname <- basename(path)

  # Dynamic entity extraction from config.json
  if (is.null(entities)) entities <- tf_get_entities()
  row <- list(path = path, relpath = rel)

  # Prepend / so patterns requiring a leading separator always match
  match_str <- paste0("/", rel)

  for (ent in entities) {
    # A single regexec() yields both the match test and the capture group,
    # avoiding a redundant regexpr() pass per entity per file.
    cap <- regmatches(match_str, regexec(ent$pattern, match_str, perl = TRUE))[[1]]
    val <- if (length(cap) >= 2) cap[2] else NA_character_
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

  # Fall back to bidser's BIDS parser only when the regex above couldn't
  # determine a suffix -- avoids calling bidser::encode() on every file.
  if (is.na(suffix) && requireNamespace("bidser", quietly = TRUE)) {
    parsed <- try(bidser::encode(fname), silent = TRUE)
    if (!inherits(parsed, "try-error") && !is.null(parsed) && !is.null(parsed$suffix)) {
      suffix <- parsed$suffix
    }
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
