\name{claude}
\alias{claude}
\title{Call Claude from RStudio or RStudio Server}
\usage{
claude(
  prompt,
  scope = c("document", "selection", "none"),
  model = NULL,
  output = c("console", "file"),
  output_file = NULL,
  include_session = NULL,
  objects = NULL,
  write_back = NULL,
  quiet = NULL,
  claude_bin = NULL,
  claude_args = character()
)
}
\arguments{
  \item{prompt}{A single prompt string to send to Claude.}

  \item{scope}{How much editor context to include.}

  \item{model}{Optional Claude model name passed to \code{--model}.}

  \item{output}{Whether to print the final response to the console or write it to a file.}

  \item{output_file}{Optional path for the response output file.}

  \item{include_session}{Whether to include small amounts of R session context.
  Defaults to the current \code{claude_init()} setting.}

  \item{objects}{Optional character vector of object names to summarize from the
  calling environment. Only metadata is sent, never raw object values.}

  \item{write_back}{Whether to comment out the active \code{claude(...)} call in the
  editor and insert the Claude response directly below it. Defaults to the
  current \code{claude_init()} setting.}

  \item{quiet}{Whether to suppress printing the returned code to the console when
  \code{output = "console"}, and whether to suppress normal Claude CLI
  stderr output during successful runs. Defaults to the current
  \code{claude_init()} setting.}

  \item{claude_bin}{Optional explicit Claude binary path or command name. Defaults
  to the current \code{claude_init()} setting, then \code{PATH}.}

  \item{claude_args}{Additional command-line flags passed to the Claude CLI.}
}
\value{
If \code{output = "console"}, returns the final response text invisibly.
If \code{output = "file"}, returns the output file path invisibly.
}
\description{
Calls the local Claude CLI from R, using RStudio editor context when available.
This is designed to support a workflow where a user writes \code{claude("...")}
inside an open \code{.R} file and sources that line in RStudio Server.
By default, the active call line is turned into a comment and the response is
inserted directly below it.
The returned text must parse as valid R code before it is written back.
When \code{objects} is supplied, only metadata about those objects is included
in the Claude prompt. The exported metadata is restricted to an explicit
allowlist of schema-style fields.
}
\examples{
\dontrun{
library(codexR)
claude_init(quiet = TRUE)
claude("Explain this script.")
claude("Iterate over df_data", objects = "df_data")
claude("Refactor this block.", scope = "selection")
claude("Only print the response.", write_back = FALSE)
}
}
