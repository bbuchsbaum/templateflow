# TemplateFlow R client

Create a client for querying the TemplateFlow archive.

## Usage

``` r
TemplateFlowClient(root = NULL, cache = NULL, ...)
```

## Arguments

- root:

  Optional cache root (defaults to \`~/.cache/templateflow\` or
  \`TEMPLATEFLOW_HOME\`).

- cache:

  Optional pre-built \`TemplateFlowCache\`.

- ...:

  Additional config passed to \`tf_default_config()\`.

## Value

A \`TemplateFlowClient\` object.

## Examples

``` r
if (FALSE) { # \dontrun{
client <- TemplateFlowClient()
client <- TemplateFlowClient(root = "/tmp/tf-cache")
} # }
```
