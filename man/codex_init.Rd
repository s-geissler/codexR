\name{codex_init}
\alias{codex_init}
\title{Initialize Session Defaults for codexR}
\usage{
codex_init(codex_bin = NULL, include_session = TRUE, write_back = TRUE, quiet = TRUE)
}
\arguments{
  \item{codex_bin}{Optional explicit Codex binary path or command name.}

  \item{include_session}{Default for whether \code{codex()} includes R session metadata.}

  \item{write_back}{Default for whether \code{codex()} comments the call line and inserts the response into the editor.}

  \item{quiet}{Default for whether \code{codex()} suppresses console output.}
}
\value{
An invisible list of the active session defaults.
}
\description{
Sets session-scoped defaults used by \code{codex()}. This is useful on
multi-user systems where you want to force a specific Codex binary, and when
you want to suppress console output by default.
}
\examples{
\dontrun{
codex_init(
  codex_bin = "/home/youruser/.local/bin/codex",
  quiet = TRUE
)
}
}
