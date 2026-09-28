# templateflow

<!-- badges: start -->
[![R-CMD-check](https://github.com/bbuchsbaum/templateflow/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/bbuchsbaum/templateflow/actions/workflows/R-CMD-check.yaml)
[![pkgdown](https://github.com/bbuchsbaum/templateflow/actions/workflows/pkgdown.yaml/badge.svg)](https://bbuchsbaum.github.io/templateflow/)
[![Codecov test coverage](https://codecov.io/gh/bbuchsbaum/templateflow/branch/main/graph/badge.svg)](https://app.codecov.io/gh/bbuchsbaum/templateflow?branch=main)
<!-- badges: end -->

**templateflow** is an R client for the [TemplateFlow](https://www.templateflow.org/)
archive of neuroimaging templates. List BIDS-named template assets, download
missing files into a local cache backed by the public TemplateFlow S3 bucket,
read template metadata, and retrieve citations — with no Python or reticulate
dependency.

[Documentation](https://bbuchsbaum.github.io/templateflow/) ·
[Getting started](https://bbuchsbaum.github.io/templateflow/articles/templateflow.html) ·
[API reference](https://bbuchsbaum.github.io/templateflow/reference/) ·
[Changelog](NEWS.md)

## Installation

```r
# install.packages("remotes")
remotes::install_github("bbuchsbaum/templateflow")
```

Requires R >= 4.1.0. Optional Suggests packages `RNifti` (or `oro.nifti`) and
`gifti` are needed only when calling `tf_get(..., read = TRUE)`.

## Quick start

```r
library(templateflow)

# List available templates
tf_templates()

# Summarise one template (name, citations, entity values)
tf_describe("MNI152Lin")

# List files for a specific template
tf_ls(template = "MNI152Lin", resolution = 1, suffix = "T1w")

# Download and return the local path
path <- tf_get(template = "MNI152Lin", resolution = 1, suffix = "T1w")

# Read directly as a NIfTI object (requires RNifti or oro.nifti)
img <- tf_get(template = "MNI152Lin", resolution = 1, suffix = "T1w", read = TRUE)

# Return listing results as a data frame
df <- tf_ls(template = "MNI152Lin", as_df = TRUE)

# Template metadata and citations
tf_get_metadata(template = "MNI152Lin")
tf_get_citations(template = "MNI152NLin2009cAsym", bibtex = TRUE)
```

Related entry points include `tf_entities()`, `tf_prefetch()`, cache helpers
(`tf_cache_stats()`, `tf_cache_update()`, `tf_cache_wipe()`), and `tf_cli()`
for shell use. See the vignette linked above for worked examples.

## Configuration

| Variable | Default | Description |
|----------|---------|-------------|
| `TEMPLATEFLOW_HOME` | Platform cache dir (`rappdirs`) | Override cache location |
| `TEMPLATEFLOW_AUTOUPDATE` | `on` | Auto-update skeleton stubs |
| `TEMPLATEFLOW_TIMEOUT` | `10` (seconds) | Network timeout for downloads |
| `TEMPLATEFLOW_USE_DATALAD` | `off` | Use DataLad backend |

## CLI

A command-line interface is available via `Rscript`:

```bash
Rscript -e 'templateflow::tf_cli()' ls MNI152Lin --suffix T1w
Rscript -e 'templateflow::tf_cli()' config
Rscript -e 'templateflow::tf_cli()' get MNI152Lin --res 1 --suffix T1w
```

## License

Apache License 2.0

<!-- albersdown:theme-note:start -->
## Albers theme
This package uses the albersdown theme. Existing vignette theme hooks are replaced so `albers.css` and local `albers.js` render consistently on CRAN and GitHub Pages. The palette family is provided via `params$family` (default 'teal'). The pkgdown site uses `template: { package: albersdown }`.
<!-- albersdown:theme-note:end -->
