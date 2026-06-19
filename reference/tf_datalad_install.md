# Install a DataLad dataset

Clone/install a DataLad dataset to a local path.

## Usage

``` r
tf_datalad_install(path, source, recursive = TRUE)
```

## Arguments

- path:

  Local destination path.

- source:

  Dataset source URL or path.

- recursive:

  If TRUE, install subdatasets recursively.

## Value

Invisible path.

## Examples

``` r
if (FALSE) { # \dontrun{
tf_datalad_install("~/.cache/templateflow",
  source = "https://github.com/templateflow/templateflow.git")
} # }
```
