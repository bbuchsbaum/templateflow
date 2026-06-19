# List available BIDS entity names

Returns the entity names recognised by TemplateFlow for filtering
template files (e.g. \`resolution\`, \`atlas\`, \`suffix\`).

## Usage

``` r
tf_entities(client = NULL, template = NULL, values = FALSE)
```

## Arguments

- client:

  A \`TemplateFlowClient\` (optional; default global client). Only used
  when \`template\` is given.

- template:

  Optional template identifier. If supplied, restricts the result to
  entities present for that template.

- values:

  If \`TRUE\`, return a named list of unique entity values instead of a
  character vector of names. Requires \`template\`.

## Value

A character vector of entity names, or (when \`values = TRUE\`) a named
list of unique values per entity.

## Details

When \`template\` is supplied, returns only the entities that are
actually present for that template (useful for discovering which filters
make sense). When \`values = TRUE\`, returns a named list of the unique
values observed for each entity instead of just the names.

## Examples

``` r
tf_entities()
#>  [1] "atlas"        "cohort"       "density"      "desc"         "extension"   
#>  [6] "from"         "hemi"         "label"        "mode"         "resolution"  
#> [11] "roi"          "scale"        "segmentation" "space"        "suffix"      
#> [16] "template"     "to"          
if (FALSE) { # \dontrun{
tf_entities(template = "MNI152Lin")
tf_entities(template = "MNI152Lin", values = TRUE)$desc
} # }
```
