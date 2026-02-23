rtrim_slash <- function(x) sub("/+$", "", x)

tf_normalize_ext <- function(x) {
  if (is.null(x)) return(NULL)
  vapply(x, function(val) {
    if (is.na(val) || val == "") return(val)
    if (startsWith(val, ".")) val else paste0(".", val)
  }, character(1), USE.NAMES = FALSE)
}

tf_is_xml_error <- function(path) {
  if (!file.exists(path)) return(FALSE)
  info <- file.info(path)
  if (is.na(info$size) || info$size == 0 || info$size >= 1024) return(FALSE)
  con <- file(path, "rb")
  on.exit(close(con), add = TRUE)
  content <- readBin(con, what = "raw", n = 100)
  if (!length(content)) return(FALSE)
  # Skip binary files containing NUL bytes
  if (any(content == as.raw(0L))) return(FALSE)
  txt <- rawToChar(content)
  grepl("^<\\?xml", txt) && grepl("<Error><Code>", txt)
}

tf_truncate_s3_errors <- function(paths) {
  for (fp in paths) {
    if (tf_is_xml_error(fp)) {
      file.create(fp, showWarnings = FALSE)
    }
  }
  invisible(TRUE)
}

tf_human_size <- function(num) {
  if (is.na(num) || num == 0) return("0 B")
  units <- c("B", "KB", "MB", "GB", "TB")
  exp <- min(floor(log(num, 1024)), length(units) - 1)
  val <- num / (1024^exp)
  sprintf("%.1f %s", val, units[exp + 1])
}
