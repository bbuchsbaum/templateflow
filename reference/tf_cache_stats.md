# Cache statistics

Report the number and total size of cached files.

## Usage

``` r
tf_cache_stats(cache = NULL)
```

## Arguments

- cache:

  A \`TemplateFlowCache\` object or a character path to the cache root
  (optional; uses default client cache).

## Value

A list with \`cache_files\`, \`cache_bytes\`, and \`cache_size_human\`.

## Examples

``` r
if (FALSE) { # \dontrun{
tf_cache_stats()
} # }
```
