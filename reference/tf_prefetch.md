# Batch prefetch template files

Downloads all zero-byte skeleton stubs matching the given filters.

## Usage

``` r
tf_prefetch(client = NULL, templates = NULL, ...)
```

## Arguments

- client:

  A \`TemplateFlowClient\` (optional; default global client).

- templates:

  Character vector of template names to prefetch. If NULL, prefetches
  all templates.

- ...:

  Entity filters passed to \`tf_ls()\`.

## Value

Invisible count of files downloaded.

## Examples

``` r
if (FALSE) { # \dontrun{
tf_prefetch(templates = "MNI152Lin")
tf_prefetch()
} # }
```
