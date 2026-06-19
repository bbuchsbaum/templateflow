# Configuration helpers for TemplateFlow

tf_env_bool <- function(var, default = FALSE) {
  val <- Sys.getenv(var, unset = NA_character_)
  if (is.na(val) || val == "") {
    return(isTRUE(default))
  }
  val_low <- tolower(val)
  if (val_low %in% c("1", "true", "on", "yes", "y")) {
    return(TRUE)
  }
  if (val_low %in% c("0", "false", "off", "no", "n")) {
    return(FALSE)
  }
  warning(sprintf("%s is set to unknown value <%s>; falling back to default <%s>", var, val, default))
  isTRUE(default)
}

tf_home <- function() {
  default_home <- rappdirs::user_cache_dir("templateflow")
  root <- Sys.getenv("TEMPLATEFLOW_HOME", unset = default_home)
  normalizePath(root, mustWork = FALSE)
}

tf_extdata_path <- function(name) {
  path <- system.file("extdata", name, package = "templateflow", mustWork = FALSE)
  if (path != "" && file.exists(path)) {
    return(path)
  }
  # Walk up a few parent levels to find inst/extdata when running uninstalled tests
  cwd <- getwd()
  for (i in 0:4) {
    candidate_root <- cwd
    if (i > 0) {
      for (j in seq_len(i)) candidate_root <- dirname(candidate_root)
    }
    local <- file.path(candidate_root, "inst", "extdata", name)
    if (file.exists(local)) {
      return(normalizePath(local, mustWork = FALSE))
    }
  }
  tf_abort_cache(paste0("Bundled file not found: ", name))
}

tf_default_config <- function(
    root = NULL,
    s3_root = Sys.getenv("TEMPLATEFLOW_S3_ROOT", unset = "https://templateflow.s3.amazonaws.com"),
    autoupdate = tf_env_bool("TEMPLATEFLOW_AUTOUPDATE", TRUE),
    timeout = as.numeric(Sys.getenv("TEMPLATEFLOW_TIMEOUT", unset = "10")),
    use_datalad = tf_env_bool("TEMPLATEFLOW_USE_DATALAD", FALSE),
    origin = Sys.getenv("TEMPLATEFLOW_ORIGIN", unset = "https://github.com/templateflow/templateflow.git")) {
  list(
    root = normalizePath(root %||% tf_home(), mustWork = FALSE),
    s3_root = s3_root,
    autoupdate = autoupdate,
    timeout = timeout,
    use_datalad = use_datalad,
    origin = origin
  )
}

tf_log <- function(...) {
  msg <- paste0(...)
  message("[templateflow] ", msg)
}

`%||%` <- function(x, y) if (is.null(x)) y else x
