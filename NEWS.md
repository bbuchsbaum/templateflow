# templateflow (development)

## Breaking changes

* `tf_get()` now defaults to `raise_empty = TRUE` so that queries with no
  matching files error loudly instead of silently returning `character(0)`.
  Pass `raise_empty = FALSE` to restore the previous lenient behaviour.

## New features

* `tf_get(dest = ...)` copies fetched files into a user-supplied directory,
  preserving the `tpl-<id>/...` BIDS sub-path. The cache remains the canonical
  store; `dest` receives a snapshot copy.
* `tf_describe(template)` summarises a template: name, citations, and the
  BIDS entities (with their unique values) actually available for it. Useful
  for discovering valid `desc`, `label`, `resolution`, ... values before
  building a query.
* `tf_entities(template = ...)` (new arguments) returns only the entities
  present for a given template. Combined with `values = TRUE`, returns a
  named list of unique values per entity.
* When the local skeleton index is missing or unreadable, `tf_ls()` and
  `tf_get()` now raise an actionable cache error pointing the user at
  `tf_cache_update()` instead of silently returning empty results.
* The first-run skeleton sync no longer leaves the cache in a "synced but
  empty" state when extraction fails: a subsequent call retries.

# templateflow 0.1.0

## Breaking changes

* Cache directory now uses platform-appropriate location via `rappdirs`:
  macOS `~/Library/Caches/templateflow`, Windows `AppData/Local`.
  Set `TEMPLATEFLOW_HOME` to override.

## New features
* Custom error conditions (`templateflow_error`, `templateflow_network_error`,
  `templateflow_not_found`, `templateflow_invalid_filter`, `templateflow_cache_error`)
  for structured error handling with `tryCatch()`.
* Migrated HTTP backend from `httr` to `httr2` with automatic retry and
  exponential backoff on transient failures.
* Atomic file writes prevent partial/corrupt downloads.
* Config-driven layout parsing from `config.json` instead of hardcoded entities.
* `tf_ls()` gains `as_df` parameter to return results as a data frame.
* `tf_get()` gains `read` parameter to return neuroimaging objects directly
  using `RNifti`, `oro.nifti`, or `gifti` when available.
* `tf_prefetch()` batch-downloads all zero-byte skeleton stubs.
* `tf_cache_stats()`, `tf_cache_scan()`, `tf_cache_refresh()` cache utilities.
* Fuzzy entity name matching with suggestions on typos.
* Full CLI rewrite with 9 subcommands: `config`, `ls`, `get`, `wipe`, `update`,
 `meta`, `cite`, `refresh`, `doctor`.
* DataLad backend support via `TEMPLATEFLOW_USE_DATALAD` environment variable.
* Neuroimaging file reader dispatch (`tf_read_file()`) for NIfTI, GIFTI, JSON, TSV.

## Bug fixes
* `print.TemplateFlowClient` now correctly returns `invisible(x)`.
* Roxygen annotation for `print.TemplateFlowClient` moved to proper location.

# templateflow 0.0.1

* Initial release with S3-backed template querying and caching.
