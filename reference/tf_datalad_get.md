# Get files from a DataLad dataset

Retrieve (download) specific files from a DataLad dataset.

## Usage

``` r
tf_datalad_get(files, dataset)
```

## Arguments

- files:

  Character vector of file paths to retrieve.

- dataset:

  Path to the DataLad dataset root.

## Value

Invisible files vector.

## Examples

``` r
if (FALSE) { # \dontrun{
tf_datalad_get(c("tpl-MNI152Lin/tpl-MNI152Lin_res-01_T1w.nii.gz"),
  dataset = "~/.cache/templateflow")
} # }
```
