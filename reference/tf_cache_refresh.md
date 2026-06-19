# Refresh cache layout

Rebuild the layout index without downloading any files.

## Usage

``` r
tf_cache_refresh(cache = NULL)
```

## Arguments

- cache:

  A \`TemplateFlowCache\` object or a character path to the cache root
  (optional; uses default client cache).

## Value

The updated cache (invisibly), or a \`TemplateFlowLayout\` if a
character path was supplied.

## Examples

``` r
if (FALSE) { # \dontrun{
tf_cache_refresh()
} # }
```
