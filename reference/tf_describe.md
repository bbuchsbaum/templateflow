# Describe a template

Summarise what a template offers: name, identifier, citations, and the
BIDS entities (with their unique values) that are available for it. This
is the discovery counterpart to \[tf_get()\] — use it when you want to
know which \`desc\`, \`label\`, \`resolution\`, etc. values are valid
before constructing a query.

## Usage

``` r
tf_describe(client = NULL, template)
```

## Arguments

- client:

  A \`TemplateFlowClient\` (optional; default global client).

- template:

  Template identifier (e.g., \`"MNI152Lin"\`).

## Value

An object of class \`tf_description\` (a list with \`template\`,
\`metadata\`, and \`entities\`). It has a \`print\` method that renders
a human-readable summary.

## Examples

``` r
if (FALSE) { # \dontrun{
tf_describe("MNI152Lin")
tf_describe("MNI152NLin2009cAsym")$entities$desc
} # }
```
