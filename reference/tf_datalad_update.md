# Update a DataLad dataset

Fetch updates from the remote and optionally merge.

## Usage

``` r
tf_datalad_update(dataset, recursive = TRUE, merge = TRUE)
```

## Arguments

- dataset:

  Path to the DataLad dataset root.

- recursive:

  If TRUE, update subdatasets recursively.

- merge:

  If TRUE, merge fetched changes.

## Value

Invisible TRUE.

## Examples

``` r
if (FALSE) { # \dontrun{
tf_datalad_update("~/.cache/templateflow", recursive = TRUE, merge = TRUE)
} # }
```
