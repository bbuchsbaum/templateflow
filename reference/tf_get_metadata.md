# Read template metadata

Read template metadata

## Usage

``` r
tf_get_metadata(client = NULL, template)
```

## Arguments

- client:

  A \`TemplateFlowClient\` (optional; default global client).

- template:

  Template identifier (e.g., \`"MNI152Lin"\`).

## Value

List parsed from \`template_description.json\`.

## Examples

``` r
if (FALSE) { # \dontrun{
meta <- tf_get_metadata(template = "MNI152Lin")
meta$Name
} # }
```
