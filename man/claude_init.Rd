\name{claude_init}
\alias{claude_init}
\title{Initialize Session Defaults for Claude}
\usage{
claude_init(claude_bin = NULL, include_session = TRUE, write_back = TRUE, quiet = TRUE)
}
\arguments{
  \item{claude_bin}{Optional explicit Claude binary path or command name.}

  \item{include_session}{Default for whether \code{claude()} includes R session metadata.}

  \item{write_back}{Default for whether \code{claude()} comments the call line and inserts the response into the editor.}

  \item{quiet}{Default for whether \code{claude()} suppresses its own console
  output and the normal stderr output of the Claude CLI during successful runs.}
}
\value{
An invisible list of the active session defaults.
}
\description{
Sets session-scoped defaults used by \code{claude()}. This is useful on
multi-user systems where you want to force a specific Claude binary, and when
you want to suppress console output by default.
}
\examples{
\dontrun{
claude_init(
  claude_bin = "/home/youruser/.local/bin/claude",
  quiet = TRUE
)
}
}
