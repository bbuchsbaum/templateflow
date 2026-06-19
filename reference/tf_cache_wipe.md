# Wipe the template cache

Delete the entire local template cache directory. Not supported when
using the DataLad backend (use \`datalad drop\` instead).

## Usage

``` r
tf_cache_wipe(cache = NULL)
```

## Arguments

- cache:

  A \`TemplateFlowCache\` object (optional; uses default client cache).

## Value

The cache object (invisibly).

## Examples

``` r
if (FALSE) { # \dontrun{
tf_cache_wipe()
} # }
```
