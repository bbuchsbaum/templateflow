# Update template cache

Re-extract the skeleton archive, optionally fetching the latest version
from the remote repository.

## Usage

``` r
tf_cache_update(cache = NULL, local = FALSE, overwrite = TRUE, silent = FALSE)
```

## Arguments

- cache:

  A \`TemplateFlowCache\` object (optional; uses default client cache).

- local:

  If \`TRUE\`, only use the bundled skeleton (no network).

- overwrite:

  If \`TRUE\`, overwrite existing stub files.

- silent:

  If \`TRUE\`, suppress progress messages.

## Value

The updated cache (invisibly).

## Examples

``` r
if (FALSE) { # \dontrun{
tf_cache_update()
tf_cache_update(local = TRUE)
} # }
```
