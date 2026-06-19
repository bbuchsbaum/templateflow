# CLI helper functions ----------------------------------------------------------

tf_cli_entity_defs <- function() {
  cfg <- tf_load_config()
  skip <- c("suffix", "extension", "description")
  Filter(function(e) !e$name %in% skip, cfg$entities)
}

tf_cli_flag_map <- function() {
  list(
    "--res"         = list(name = "resolution", dtype = "int"),
    "--resolution"  = list(name = "resolution", dtype = "int"),
    "--den"         = list(name = "density"),
    "--density"     = list(name = "density"),
    "--atlas"       = list(name = "atlas"),
    "-a"            = list(name = "atlas"),
    "--suffix"      = list(name = "suffix"),
    "-s"            = list(name = "suffix"),
    "--desc"        = list(name = "desc"),
    "-d"            = list(name = "desc"),
    "--description" = list(name = "desc"),
    "--ext"         = list(name = "extension"),
    "--extension"   = list(name = "extension"),
    "--label"       = list(name = "label"),
    "-l"            = list(name = "label"),
    "--hemi"        = list(name = "hemi"),
    "--space"       = list(name = "space"),
    "--cohort"      = list(name = "cohort", dtype = "int"),
    "--seg"         = list(name = "segmentation"),
    "--segmentation" = list(name = "segmentation"),
    "--scale"       = list(name = "scale", dtype = "int"),
    "--roi"         = list(name = "roi"),
    "--from"        = list(name = "from"),
    "--to"          = list(name = "to"),
    "--mode"        = list(name = "mode")
  )
}

tf_coerce_entities <- function(filters) {
  flag_map <- tf_cli_flag_map()
  int_entities <- vapply(flag_map, function(x) identical(x$dtype, "int"), logical(1))
  int_names <- unique(vapply(flag_map[int_entities], function(x) x$name, character(1)))

  for (nm in names(filters)) {
    if (nm %in% int_names) {
      filters[[nm]] <- as.integer(filters[[nm]])
    }
  }
  filters
}

tf_available_templates <- function(cache = NULL) {
  cache <- cache %||% tryCatch(tf_client()$cache, error = function(e) NULL)
  if (!is.null(cache) && dir.exists(cache$config$root)) {
    dirs <- list.dirs(cache$config$root, full.names = FALSE, recursive = FALSE)
    tpls <- sub("^tpl-", "", dirs[grepl("^tpl-", dirs)])
    return(sort(tpls))
  }
  character(0)
}

tf_validate_template <- function(template, cache = NULL) {
  avail <- tf_available_templates(cache)
  if (length(avail) && !template %in% avail) {
    close <- agrep(template, avail, max.distance = 0.3, value = TRUE)
    hint <- if (length(close)) paste0(". Did you mean: ", paste(close, collapse = ", "), "?") else ""
    tf_abort(paste0("Unknown template: ", template, hint))
  }
  invisible(template)
}

tf_cli_error <- function(message, exit_code = 1L) {
  cond <- structure(
    class = c("tf_cli_error", "templateflow_error", "error", "condition"),
    list(message = message, exit_code = exit_code)
  )
  stop(cond)
}

tf_cli_parse_entity_flags <- function(args, i) {
  flag_map <- tf_cli_flag_map()
  arg <- args[[i]]
  if (!arg %in% names(flag_map)) return(NULL)
  spec <- flag_map[[arg]]
  if (i >= length(args)) {
    tf_cli_error(paste0("Flag ", arg, " requires a value"))
  }
  val <- args[[i + 1]]
  if (!is.null(spec$dtype) && spec$dtype == "int") {
    val <- as.integer(val)
  }
  list(name = spec$name, value = val, consumed = 2L)
}

tf_cli_help_main <- function() {
  cat("Usage: templateflow <command> [options]\n\n")
  cat("Commands:\n")
  cat("  config   Show current settings\n")
  cat("  ls       List matching template files\n")
  cat("  get      Download and list matching files\n")
  cat("  wipe     Delete the template cache\n")
  cat("  update   Update the skeleton archive\n")
  cat("  meta     Show template metadata\n")
  cat("  cite     Print template citations\n")
  cat("  refresh  Re-index the cache layout\n")
  cat("  doctor   Scan and fix problematic cache files\n")
  cat("\nOptions:\n")
  cat("  --root <path>    Set cache root directory\n")
  cat("  --help, -h       Show this help message\n")
  cat("  --version        Show package version\n")
  invisible(NULL)
}

tf_cli_help_command <- function(cmd) {
  help_text <- list(
    config = "Usage: templateflow config [--json] [--show-cache]\n\nShow current TemplateFlow settings.",
    ls = paste0(
      "Usage: templateflow ls <template> [entity flags]\n\n",
      "List files matching the given template and entity filters."
    ),
    get = paste0(
      "Usage: templateflow get <template> [entity flags]\n\n",
      "Download (if needed) and list files matching the template and filters."
    ),
    wipe = paste0(
      "Usage: templateflow wipe [--force]\n\n",
      "Delete the entire template cache. Requires confirmation unless --force is used."
    ),
    update = paste0(
      "Usage: templateflow update [--local] [--overwrite|--no-overwrite]\n\n",
      "Update the skeleton archive from remote or bundled source."
    ),
    meta = "Usage: templateflow meta <template> [--field <name>] [--json]\n\nDisplay metadata for the given template.",
    cite = "Usage: templateflow cite <template> [--bibtex]\n\nPrint citation references for the given template.",
    refresh = "Usage: templateflow refresh\n\nRe-index the cache layout without downloading.",
    doctor = paste0(
      "Usage: templateflow doctor [--fix]\n\n",
      "Scan cache for zero-byte and XML error files. Use --fix to remove them."
    )
  )
  msg <- help_text[[cmd]]
  if (is.null(msg)) {
    cat("No help available for: ", cmd, "\n")
  } else {
    cat(msg, "\n")
  }
  invisible(NULL)
}
