# Fetch template assets (downloading if needed)

Download template files from the TemplateFlow archive and optionally
read them into R objects. Like \[tf_ls()\], entity filters support
\`NULL\` values to exclude files containing that entity.

## Usage

``` r
tf_get(
  client = NULL,
  template,
  raise_empty = TRUE,
  read = FALSE,
  dest = NULL,
  ...
)
```

## Arguments

- client:

  A \`TemplateFlowClient\` (optional; default global client).

- template:

  Template identifier (e.g., \`"MNI152Lin"\`).

- raise_empty:

  If \`TRUE\` (the default), error when no files match the query. Set to
  \`FALSE\` to silently return an empty character vector. Defaulted to
  \`TRUE\` so that typo'd queries fail loudly rather than producing
  surprising empty results.

- read:

  If \`TRUE\`, read downloaded files into R objects using appropriate
  readers (NIfTI via RNifti, GIFTI via gifti, JSON, TSV). Requires
  optional packages for neuroimaging formats.

- dest:

  Optional path to a directory. When supplied, fetched files are copied
  under \`dest\` preserving their \`tpl-\<id\>/...\` BIDS sub-path, and
  the returned paths point inside \`dest\`. The cache remains the
  canonical store; \`dest\` receives a snapshot copy.

- ...:

  Entity filters (resolution, suffix, atlas, desc, hemi, space, density,
  label, segmentation, cohort, scale, roi, extension, etc.). Pass
  \`NULL\` to exclude files containing that entity.

## Value

Path string, character vector, or R object(s) if \`read = TRUE\`. When a
single file matches, a scalar path (or single R object) is returned;
when multiple files match, a character vector (or list of R objects).

## Examples

``` r
if (FALSE) { # \dontrun{
path <- tf_get(template = "MNI152Lin", resolution = 1, suffix = "T1w")
img <- tf_get(template = "MNI152Lin", resolution = 1, suffix = "T1w", read = TRUE)
tf_get(template = "MNI152Lin", resolution = 1, suffix = "T1w", dest = "./templates")
} # }
```
