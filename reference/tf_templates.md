# List available templates

List available templates

## Usage

``` r
tf_templates(client = NULL, ...)
```

## Arguments

- client:

  A \`TemplateFlowClient\` (optional; default global client).

- ...:

  Entity filters (resolution, suffix, atlas, desc, hemi, space, density,
  label, segmentation, cohort, scale, roi, extension, etc.). Pass
  \`NULL\` to exclude files containing that entity.

## Value

Character vector of template IDs.

## Examples

``` r
if (FALSE) { # \dontrun{
tf_templates()
} # }
```
