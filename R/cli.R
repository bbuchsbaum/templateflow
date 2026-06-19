#' TemplateFlow CLI
#'
#' Command-line interface for the TemplateFlow archive. Supports subcommands:
#' `config`, `ls`, `get`, `wipe`, `update`, `meta`, `cite`, `refresh`, `doctor`.
#' @param args Character vector of CLI-style arguments (defaults to commandArgs).
#' @return Invisible result of the requested command.
#' @examples
#' \dontrun{
#' tf_cli(c("ls", "MNI152Lin", "--suffix", "T1w"))
#' tf_cli(c("config"))
#' tf_cli(c("meta", "MNI152Lin"))
#' tf_cli(c("doctor", "--fix"))
#' }
#' @export
tf_cli <- function(args = commandArgs(trailingOnly = TRUE)) {
  if (length(args) == 0 || args[1] %in% c("--help", "-h")) {
    tf_cli_help_main()
    return(invisible(NULL))
  }

  if (args[1] == "--version") {
    cat(as.character(utils::packageVersion("templateflow")), "\n")
    return(invisible(NULL))
  }

  # Parse global options and find command
  valid_cmds <- c("config", "ls", "get", "wipe", "update", "meta", "cite", "refresh", "doctor")
  root <- NULL
  autoupdate <- NULL
  cmd <- NULL
  cmd_args <- character(0)

  i <- 1
  while (i <= length(args)) {
    arg <- args[[i]]
    if (is.null(cmd) && arg == "--root") {
      if (i >= length(args)) tf_cli_error("--root requires a value")
      root <- args[[i + 1]]
      i <- i + 2
      next
    }
    if (is.null(cmd) && arg == "--autoupdate") {
      if (i >= length(args)) tf_cli_error("--autoupdate requires a value")
      autoupdate <- args[[i + 1]]
      i <- i + 2
      next
    }
    if (is.null(cmd) && arg %in% valid_cmds) {
      cmd <- arg
      cmd_args <- args[seq_along(args)[(i + 1):length(args)]]
      if (i >= length(args)) cmd_args <- character(0)
      break
    }
    if (is.null(cmd)) {
      tf_cli_error(paste0("Unknown argument or command: ", arg))
    }
    i <- i + 1
  }

  if (is.null(cmd)) {
    tf_cli_error("No command supplied (expected one of: config, ls, get, wipe, update, meta, cite, refresh, doctor)")
  }

  # Parse autoupdate
  if (!is.null(autoupdate)) {
    val <- tolower(as.character(autoupdate))
    autoupdate <- if (val %in% c("true", "on", "1", "yes", "y")) {
      TRUE
    } else if (val %in% c("false", "off", "0", "no", "n")) {
      FALSE
    } else {
      tf_cli_error(paste0("Invalid value for --autoupdate: ", autoupdate))
    }
  }

  # Dispatch
  handlers <- list(
    config  = tf_cli_config,
    ls      = tf_cli_ls,
    get     = tf_cli_get,
    wipe    = tf_cli_wipe,
    update  = tf_cli_update,
    meta    = tf_cli_meta,
    cite    = tf_cli_cite,
    refresh = tf_cli_refresh,
    doctor  = tf_cli_doctor
  )

  handler <- handlers[[cmd]]

  # When running as a standalone script (non-interactive, no testthat),
  # catch errors and exit with appropriate status codes.
  if (!interactive() && !identical(Sys.getenv("TESTTHAT"), "true")) {
    tryCatch(
      handler(cmd_args, root = root, autoupdate = autoupdate),
      tf_cli_error = function(e) {
        message("Error: ", conditionMessage(e))
        code <- if (!is.null(e$exit_code)) e$exit_code else 1L
        quit(save = "no", status = code)
      },
      templateflow_network_error = function(e) {
        message("Network error: ", conditionMessage(e))
        quit(save = "no", status = 2L)
      },
      templateflow_not_found = function(e) {
        message("Error: ", conditionMessage(e))
        quit(save = "no", status = 3L)
      },
      templateflow_cache_error = function(e) {
        message("Cache error: ", conditionMessage(e))
        quit(save = "no", status = 4L)
      },
      templateflow_error = function(e) {
        message("Error: ", conditionMessage(e))
        quit(save = "no", status = 1L)
      }
    )
  } else {
    handler(cmd_args, root = root, autoupdate = autoupdate)
  }
}

# --- CLI command implementations ---

tf_cli_make_client <- function(root, autoupdate) {
  TemplateFlowClient(
    root = root,
    autoupdate = autoupdate %||% tf_env_bool("TEMPLATEFLOW_AUTOUPDATE", TRUE)
  )
}

tf_cli_parse_template_filters <- function(args, cmd_name) {
  if (length(args) && args[1] %in% c("--help", "-h")) {
    tf_cli_help_command(cmd_name)
    return(NULL)
  }
  template <- NULL
  filters <- list()
  i <- 1
  while (i <= length(args)) {
    arg <- args[[i]]
    # Entity flag?
    parsed <- tf_cli_parse_entity_flags(args, i)
    if (!is.null(parsed)) {
      filters[[parsed$name]] <- c(filters[[parsed$name]], parsed$value)
      i <- i + parsed$consumed
      next
    }
    # A non-option argument is treated as the template
    if (is.null(template) && !startsWith(arg, "-")) {
      template <- arg
      i <- i + 1
      next
    }
    tf_cli_error(paste0("Unrecognized argument for ", cmd_name, ": ", arg))
  }
  list(template = template, filters = filters)
}

tf_cli_config <- function(args, root = NULL, autoupdate = NULL) {
  show_json <- "--json" %in% args
  show_cache <- "--show-cache" %in% args

  if (length(args) && args[1] %in% c("--help", "-h")) {
    tf_cli_help_command("config")
    return(invisible(NULL))
  }

  client <- tf_cli_make_client(root, autoupdate)
  cfg <- client$cache$config

  if (show_json) {
    cat(jsonlite::toJSON(cfg, auto_unbox = TRUE, pretty = TRUE), "\n")
  } else {
    cat("Current TemplateFlow settings:\n\n")
    cat("    TEMPLATEFLOW_HOME=", cfg$root, "\n", sep = "")
    cat("    TEMPLATEFLOW_USE_DATALAD=", if (cfg$use_datalad) "on" else "off", "\n", sep = "")
    cat("    TEMPLATEFLOW_AUTOUPDATE=", if (cfg$autoupdate) "on" else "off", "\n", sep = "")
    cat("    TEMPLATEFLOW_S3_ROOT=", cfg$s3_root, "\n", sep = "")
    cat("    TEMPLATEFLOW_TIMEOUT=", cfg$timeout, "s\n", sep = "")
  }

  if (show_cache) {
    stats <- tf_cache_stats(client$cache)
    cat("\n    Cache files: ", stats$cache_files, "\n", sep = "")
    cat("    Cache size:  ", stats$cache_size_human, "\n", sep = "")
  }

  invisible(cfg)
}

tf_cli_ls <- function(args, root = NULL, autoupdate = NULL) {
  parsed <- tf_cli_parse_template_filters(args, "ls")
  if (is.null(parsed)) return(invisible(NULL))

  if (is.null(parsed$template)) {
    tf_cli_error("template argument is required for ls")
  }

  client <- tf_cli_make_client(root, autoupdate)
  tf_validate_template(parsed$template, client$cache)
  res <- do.call(tf_ls, c(list(client = client, template = parsed$template), parsed$filters))
  cat(paste(res, collapse = "\n"), "\n", sep = "")
  invisible(res)
}

tf_cli_get <- function(args, root = NULL, autoupdate = NULL) {
  parsed <- tf_cli_parse_template_filters(args, "get")
  if (is.null(parsed)) return(invisible(NULL))

  if (is.null(parsed$template)) {
    tf_cli_error("template argument is required for get")
  }

  client <- tf_cli_make_client(root, autoupdate)
  tf_validate_template(parsed$template, client$cache)
  res <- do.call(tf_get, c(list(client = client, template = parsed$template), parsed$filters))
  if (is.character(res)) cat(paste(res, collapse = "\n"), "\n", sep = "")
  invisible(res)
}

tf_cli_wipe <- function(args, root = NULL, autoupdate = NULL) {
  if (length(args) && args[1] %in% c("--help", "-h")) {
    tf_cli_help_command("wipe")
    return(invisible(NULL))
  }

  force <- "--force" %in% args
  client <- tf_cli_make_client(root, autoupdate)

  if (!force && rlang::is_interactive()) {
    ans <- readline("Are you sure you want to delete the cache? (yes/no): ")
    if (!tolower(ans) %in% c("yes", "y")) {
      cat("Aborted.\n")
      return(invisible(FALSE))
    }
  } else if (!force) {
    tf_cli_error("Use --force to wipe cache in non-interactive mode")
  }

  tf_cache_wipe(client$cache)
  cat("Cache deleted.\n")
  invisible(TRUE)
}

tf_cli_update <- function(args, root = NULL, autoupdate = NULL) {
  if (length(args) && args[1] %in% c("--help", "-h")) {
    tf_cli_help_command("update")
    return(invisible(NULL))
  }

  local <- "--local" %in% args
  overwrite <- if ("--no-overwrite" %in% args) FALSE else TRUE

  client <- tf_cli_make_client(root, autoupdate)
  tf_cache_update(client$cache, local = local, overwrite = overwrite, silent = FALSE)
  cat("Successfully updated local TemplateFlow Archive.\n")
  invisible(TRUE)
}

tf_cli_meta <- function(args, root = NULL, autoupdate = NULL) {
  if (length(args) && args[1] %in% c("--help", "-h")) {
    tf_cli_help_command("meta")
    return(invisible(NULL))
  }

  template <- NULL
  field <- NULL
  show_json <- FALSE
  i <- 1
  while (i <= length(args)) {
    arg <- args[[i]]
    if (arg == "--field") {
      field <- args[[i + 1]]
      i <- i + 2
      next
    }
    if (arg == "--json") {
      show_json <- TRUE
      i <- i + 1
      next
    }
    if (is.null(template) && !startsWith(arg, "-")) {
      template <- arg
      i <- i + 1
      next
    }
    tf_cli_error(paste0("Unrecognized argument for meta: ", arg))
  }

  if (is.null(template)) tf_cli_error("template argument is required for meta")

  client <- tf_cli_make_client(root, autoupdate)
  tf_validate_template(template, client$cache)
  meta <- tf_get_metadata(client, template)

  if (!is.null(field)) {
    val <- meta[[field]]
    if (is.null(val)) {
      tf_cli_error(paste0("Field '", field, "' not found in metadata"))
    }
    if (show_json) {
      cat(jsonlite::toJSON(val, auto_unbox = TRUE, pretty = TRUE), "\n")
    } else {
      cat(paste(val, collapse = "\n"), "\n")
    }
    return(invisible(val))
  }

  if (show_json) {
    cat(jsonlite::toJSON(meta, auto_unbox = TRUE, pretty = TRUE), "\n")
  } else {
    for (nm in names(meta)) {
      val <- meta[[nm]]
      if (is.list(val) || length(val) > 1) {
        val <- paste(unlist(val), collapse = ", ")
      }
      cat(sprintf("  %s: %s\n", nm, val))
    }
  }
  invisible(meta)
}

tf_cli_cite <- function(args, root = NULL, autoupdate = NULL) {
  if (length(args) && args[1] %in% c("--help", "-h")) {
    tf_cli_help_command("cite")
    return(invisible(NULL))
  }

  template <- NULL
  bibtex <- FALSE
  i <- 1
  while (i <= length(args)) {
    arg <- args[[i]]
    if (arg == "--bibtex") {
      bibtex <- TRUE
      i <- i + 1
      next
    }
    if (is.null(template) && !startsWith(arg, "-")) {
      template <- arg
      i <- i + 1
      next
    }
    tf_cli_error(paste0("Unrecognized argument for cite: ", arg))
  }

  if (is.null(template)) tf_cli_error("template argument is required for cite")

  client <- tf_cli_make_client(root, autoupdate)
  tf_validate_template(template, client$cache)
  refs <- tf_get_citations(client, template, bibtex = bibtex)
  cat(paste(refs, collapse = "\n\n"), "\n")
  invisible(refs)
}

tf_cli_refresh <- function(args, root = NULL, autoupdate = NULL) {
  if (length(args) && args[1] %in% c("--help", "-h")) {
    tf_cli_help_command("refresh")
    return(invisible(NULL))
  }

  client <- tf_cli_make_client(root, autoupdate)
  tf_cache_refresh(client$cache)
  cat("Cache layout refreshed.\n")
  invisible(TRUE)
}

tf_cli_doctor <- function(args, root = NULL, autoupdate = NULL) {
  if (length(args) && args[1] %in% c("--help", "-h")) {
    tf_cli_help_command("doctor")
    return(invisible(NULL))
  }

  fix <- "--fix" %in% args
  client <- tf_cli_make_client(root, autoupdate)
  scan <- tf_cache_scan(client$cache)

  n_zero <- length(scan$zero)
  n_xml <- length(scan$xml)

  if (n_zero == 0 && n_xml == 0) {
    cat("Cache is clean. No issues found.\n")
    return(invisible(scan))
  }

  if (n_zero > 0) {
    cat(sprintf("Found %d zero-byte file(s).\n", n_zero))
  }
  if (n_xml > 0) {
    cat(sprintf("Found %d S3 XML error file(s).\n", n_xml))
  }

  if (fix) {
    # Remove both zero-byte stubs and XML error files, then reindex
    all_bad <- unique(c(scan$zero, scan$xml))
    removed <- 0L
    for (fp in all_bad) {
      tryCatch(
        {
          file.remove(fp)
          removed <- removed + 1L
        },
        error = function(e) NULL
      )
    }
    cat(sprintf("Removed %d problematic file(s).\n", removed))
    tf_cache_refresh(client$cache)
    cat("Cache index refreshed.\n")
  } else {
    cat("Use --fix to remove bad files and refresh the index.\n")
  }

  invisible(scan)
}
