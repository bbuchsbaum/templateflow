# TemplateFlow CLI

Command-line interface for the TemplateFlow archive. Supports
subcommands: \`config\`, \`ls\`, \`get\`, \`wipe\`, \`update\`,
\`meta\`, \`cite\`, \`refresh\`, \`doctor\`.

## Usage

``` r
tf_cli(args = commandArgs(trailingOnly = TRUE))
```

## Arguments

- args:

  Character vector of CLI-style arguments (defaults to commandArgs).

## Value

Invisible result of the requested command.

## Examples

``` r
if (FALSE) { # \dontrun{
tf_cli(c("ls", "MNI152Lin", "--suffix", "T1w"))
tf_cli(c("config"))
tf_cli(c("meta", "MNI152Lin"))
tf_cli(c("doctor", "--fix"))
} # }
```
