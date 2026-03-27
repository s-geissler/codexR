# codexR

`codexR` is a small R package that lets you call the local Codex CLI from an open `.R` file in RStudio or RStudio Server.

## Install

From a local source checkout:

```r
install.packages(".", repos = NULL, type = "source")
```

From GitHub via remotes:

```r
remotes::install_github("s-geissler/codexR")
```

## Basic use

Load the package once in your session:

```r
library(codexR)
codex_init(quiet = TRUE)
```

Then place a line like this in an open `.R` file and source that line:

```r
codex("Explain why this function is failing and suggest a fix.")
```

By default the package sends:

- your prompt
- the active document path
- the active cursor position
- the full active document contents
- a small amount of session context

When called from RStudio or RStudio Server, `codex()` also rewrites the active editor buffer by default:

- the invoked `codex("...")` line is converted into an R comment
- the Codex response is inserted directly below that commented line
- the response is inserted only if it parses as valid R code

If Codex returns non-R text or syntactically invalid R, `codex()` stops with an error and does not modify the editor.

## Object Metadata

You can provide object names from the calling environment:

```r
codex("Iterate over df_data", objects = "df_data")
```

The package sends metadata only. It does not send raw values, rows, vector contents, or list contents to Codex. For example, for a data frame it includes schema-style information such as:

- class
- type
- dimensions
- column names
- column types

For lists, matrices, arrays, and vectors it only includes structural metadata such as length, dimensions, names, and element classes.

The metadata exporter is allowlist-based. Each supported object class has an explicit set of permitted fields, and unknown classes fall back to a very small structural summary only.

## Variants

Use only the current selection:

```r
codex("Refactor this block for clarity.", scope = "selection")
```

Disable editor write-back and only print the response:

```r
codex("Refactor this block for clarity.", write_back = FALSE)
```

Select a specific Codex binary and set session defaults:

```r
codex_init(
  codex_bin = "/home/youruser/.local/bin/codex",
  quiet = TRUE,
  write_back = TRUE
)
```

Write the final Codex response to a file instead of printing it:

```r
path <- codex(
  "Summarize this script and suggest tests.",
  output = "file",
  write_back = FALSE
)
cat(readLines(path), sep = "\n")
```

Pass extra flags directly to the Codex CLI:

```r
codex(
  "Review this script for bugs.",
  codex_args = c("--model", "gpt-5.4", "--full-auto")
)
```

## Requirements

- `codex` must be installed and available on `PATH`
- `rstudioapi` must be installed
- for best results, run from an open file in RStudio or RStudio Server

## Development

Run a local package check:

```r
devtools::check()
```

The repository also includes a GitHub Actions workflow for `R CMD check`.
