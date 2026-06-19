# Scan cache for problematic files

Find zero-byte stubs and S3 XML error responses in the cache.

## Usage

``` r
tf_cache_scan(cache = NULL)
```

## Arguments

- cache:

  A \`TemplateFlowCache\` object or a character path to the cache root
  (optional; uses default client cache).

## Value

A list with \`zero\` (zero-byte paths) and \`xml\` (XML error paths).

## Examples

``` r
if (FALSE) { # \dontrun{
tf_cache_scan()
} # }
```
