# Retrieve template citations

Retrieve template citations

## Usage

``` r
tf_get_citations(client = NULL, template, bibtex = FALSE)
```

## Arguments

- client:

  A \`TemplateFlowClient\` (optional; default global client).

- template:

  Template identifier (e.g., \`"MNI152Lin"\`).

- bibtex:

  If TRUE, fetch citations in BibTeX format.

## Value

Character vector of citation URLs or BibTeX entries.

## Examples

``` r
if (FALSE) { # \dontrun{
tf_get_citations(template = "MNI152NLin2009cAsym")
tf_get_citations(template = "MNI152NLin2009cAsym", bibtex = TRUE)
} # }
```
