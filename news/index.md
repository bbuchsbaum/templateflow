# Changelog

## templateflow 0.1.0

### Breaking changes

- Cache directory now uses platform-appropriate location via `rappdirs`:
  macOS `~/Library/Caches/templateflow`, Windows `AppData/Local`. Set
  `TEMPLATEFLOW_HOME` to override.

### New features

- Custom error conditions (`templateflow_error`,
  `templateflow_network_error`, `templateflow_not_found`,
  `templateflow_invalid_filter`, `templateflow_cache_error`) for
  structured error handling with
  [`tryCatch()`](https://rdrr.io/r/base/conditions.html).
- Migrated HTTP backend from `httr` to `httr2` with automatic retry and
  exponential backoff on transient failures.
- Atomic file writes prevent partial/corrupt downloads.
- Config-driven layout parsing from `config.json` instead of hardcoded
  entities.
- [`tf_ls()`](https://bbuchsbaum.github.io/templateflow/reference/tf_ls.md)
  gains `as_df` parameter to return results as a data frame.
- [`tf_get()`](https://bbuchsbaum.github.io/templateflow/reference/tf_get.md)
  gains `read` parameter to return neuroimaging objects directly using
  `RNifti`, `oro.nifti`, or `gifti` when available.
- [`tf_prefetch()`](https://bbuchsbaum.github.io/templateflow/reference/tf_prefetch.md)
  batch-downloads all zero-byte skeleton stubs.
- [`tf_cache_stats()`](https://bbuchsbaum.github.io/templateflow/reference/tf_cache_stats.md),
  [`tf_cache_scan()`](https://bbuchsbaum.github.io/templateflow/reference/tf_cache_scan.md),
  [`tf_cache_refresh()`](https://bbuchsbaum.github.io/templateflow/reference/tf_cache_refresh.md)
  cache utilities.
- Fuzzy entity name matching with suggestions on typos.
- Full CLI rewrite with 9 subcommands: `config`, `ls`, `get`, `wipe`,
  `update`, `meta`, `cite`, `refresh`, `doctor`.
- DataLad backend support via `TEMPLATEFLOW_USE_DATALAD` environment
  variable.
- Neuroimaging file reader dispatch
  ([`tf_read_file()`](https://bbuchsbaum.github.io/templateflow/reference/tf_read_file.md))
  for NIfTI, GIFTI, JSON, TSV.

### Bug fixes

- `print.TemplateFlowClient` now correctly returns `invisible(x)`.
- Roxygen annotation for `print.TemplateFlowClient` moved to proper
  location.

## templateflow 0.0.1

- Initial release with S3-backed template querying and caching.
