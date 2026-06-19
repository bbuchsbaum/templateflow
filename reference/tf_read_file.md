# Read a template file into an R object

Dispatches by file extension to the appropriate reader. NIfTI files use
\`RNifti\` or \`oro.nifti\` (soft deps), GIFTI files use \`gifti\`, JSON
files use \`jsonlite\`, and TSV files use \`utils::read.delim()\`.

## Usage

``` r
tf_read_file(path)
```

## Arguments

- path:

  Path to the file.

## Value

An R object appropriate for the file type, or the path if no reader is
available.
