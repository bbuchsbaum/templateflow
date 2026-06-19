# templateflow

<!-- badges: start -->
[![R-CMD-check](https://github.com/bbuchsbaum/templateflow/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/bbuchsbaum/templateflow/actions/workflows/R-CMD-check.yaml)
[![pkgdown](https://github.com/bbuchsbaum/templateflow/actions/workflows/pkgdown.yaml/badge.svg)](https://bbuchsbaum.github.io/templateflow/)
<!-- badges: end -->

R client for the [TemplateFlow](https://www.templateflow.org/) archive of neuroimaging templates.

## Installation

```r
# Install from GitHub:
remotes::install_github("bbuchsbaum/templateflow")
```

## Quick start

```r
library(templateflow)

# List available templates
tf_templates()

# List files for a specific template
tf_ls(template = "MNI152Lin", resolution = 1, suffix = "T1w")

# Download and return path
path <- tf_get(template = "MNI152Lin", resolution = 1, suffix = "T1w")

# Read directly as NIfTI object (requires RNifti or oro.nifti)
img <- tf_get(template = "MNI152Lin", resolution = 1, suffix = "T1w", read = TRUE)

# Return results as a data frame
df <- tf_ls(template = "MNI152Lin", as_df = TRUE)

# Template metadata and citations
tf_get_metadata(template = "MNI152Lin")
tf_get_citations(template = "MNI152NLin2009cAsym", bibtex = TRUE)
```

## Configuration

| Variable | Default | Description |
|----------|---------|-------------|
| `TEMPLATEFLOW_HOME` | Platform cache dir | Override cache location |
| `TEMPLATEFLOW_AUTOUPDATE` | `on` | Auto-update skeleton stubs |
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
