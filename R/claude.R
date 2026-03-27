.claude_last_response_file <- local({
  path <- NULL

  function(set = NULL) {
    if (!is.null(set)) {
      path <<- set
    }

    path
  }
})

.claude_config <- local({
  env <- new.env(parent = emptyenv())
  env$claude_bin <- NULL
  env$include_session <- TRUE
  env$write_back <- TRUE
  env$quiet <- FALSE

  env
})

#' Initialise session-level defaults for claude()
#'
#' @param claude_bin Path to the Claude CLI binary. `NULL` means resolve from
#'   PATH.
#' @param include_session Include session metadata (search path, global objects)
#'   in the context sent to Claude. Defaults to `TRUE`.
#' @param write_back Automatically insert the Claude response into the active
#'   RStudio document. Defaults to `TRUE`.
#' @param quiet Suppress console output from the Claude CLI. Defaults to
#'   `TRUE`.
#'
#' @return Invisibly, a named list of the current settings.
#' @export
claude_init <- function(claude_bin = NULL,
                        include_session = TRUE,
                        write_back = TRUE,
                        quiet = TRUE) {
  if (!is.null(claude_bin)) {
    if (!is.character(claude_bin) || length(claude_bin) != 1L || !nzchar(claude_bin)) {
      stop("`claude_bin` must be NULL or a single non-empty string.", call. = FALSE)
    }
  }

  .claude_config$claude_bin <- claude_bin
  .claude_config$include_session <- isTRUE(include_session)
  .claude_config$write_back <- isTRUE(write_back)
  .claude_config$quiet <- isTRUE(quiet)

  invisible(list(
    claude_bin = .claude_config$claude_bin,
    include_session = .claude_config$include_session,
    write_back = .claude_config$write_back,
    quiet = .claude_config$quiet
  ))
}

#' Query the current session-level claude() configuration
#'
#' @return A named list with the current session defaults.
#' @export
claude_config <- function() {
  list(
    claude_bin = .claude_config$claude_bin,
    include_session = .claude_config$include_session,
    write_back = .claude_config$write_back,
    quiet = .claude_config$quiet
  )
}

#' Check whether the Claude CLI is available
#'
#' @return `TRUE` if the Claude CLI binary can be resolved, `FALSE` otherwise.
#' @export
claude_available <- function() {
  nzchar(resolve_claude_bin())
}

#' Return the path to the last Claude response file
#'
#' @return A character string with the file path, or `NULL` if no response has
#'   been written yet.
#' @export
claude_last_file <- function() {
  .claude_last_response_file()
}

#' Call the Claude CLI from an RStudio editor
#'
#' Captures the current editor context (document contents, cursor position,
#' selection) and sends it together with `prompt` to the local Claude CLI.
#' When `write_back = TRUE` and the response is valid R code, it is inserted
#' back into the active document and the original `claude()` call is commented
#' out.
#'
#' @param prompt A single non-empty string describing what you want Claude to
#'   do.
#' @param scope One of `"document"` (default), `"selection"`, or `"none"`.
#'   Controls how much of the active editor file is included in the context.
#' @param model Model name to pass to `--model`. `NULL` uses the CLI default.
#' @param output One of `"console"` (default) or `"file"`. With `"file"` the
#'   function returns the path to the saved response instead of printing it.
#' @param output_file Path where the response should be saved. Generated
#'   automatically when `NULL`.
#' @param include_session Include session metadata in the context. Inherits
#'   from [claude_init()] when `NULL`.
#' @param objects Character vector of object names whose metadata should be
#'   included in the context. Raw values are never sent; only structural
#'   information (dimensions, column names, types, etc.) is included.
#' @param write_back Insert the response into the editor. Inherits from
#'   [claude_init()] when `NULL`.
#' @param quiet Suppress CLI output on the console. Inherits from
#'   [claude_init()] when `NULL`.
#' @param claude_bin Path to the Claude CLI binary. Inherits from
#'   [claude_init()] when `NULL`.
#' @param claude_args Additional character arguments passed verbatim to the
#'   Claude CLI.
#'
#' @return Invisibly, the response text (when `output = "console"`) or the
#'   path to the response file (when `output = "file"`).
#' @export
claude <- function(prompt,
                   scope = c("document", "selection", "none"),
                   model = NULL,
                   output = c("console", "file"),
                   output_file = NULL,
                   include_session = NULL,
                   objects = NULL,
                   write_back = NULL,
                   quiet = NULL,
                   claude_bin = NULL,
                   claude_args = character()) {
  scope <- match.arg(scope)
  output <- match.arg(output)

  if (!is.character(prompt) || length(prompt) != 1L || !nzchar(prompt)) {
    stop("`prompt` must be a single non-empty string.", call. = FALSE)
  }

  if (is.null(include_session)) {
    include_session <- .claude_config$include_session
  }

  if (is.null(write_back)) {
    write_back <- .claude_config$write_back
  }

  if (is.null(quiet)) {
    quiet <- .claude_config$quiet
  }

  claude_bin <- resolve_claude_bin(claude_bin)
  if (!nzchar(claude_bin)) {
    stop("Claude CLI not found on PATH.", call. = FALSE)
  }

  call_text <- paste(deparse(match.call()), collapse = " ")
  write_target <- claude_write_target(call_text = call_text, prompt = prompt)
  object_metadata <- codex_object_metadata(objects = objects, env = parent.frame())
  context <- codex_context(
    scope = scope,
    include_session = include_session,
    object_metadata = object_metadata
  )
  prompt_text <- claude_prompt(prompt = prompt, context = context)

  args <- c(
    "-p", prompt_text,
    "--output-format", "text",
    "--bare",
    "--no-session-persistence"
  )

  if (!is.null(model) && nzchar(model)) {
    args <- c(args, "--model", model)
  }

  if (length(claude_args) > 0L) {
    args <- c(args, claude_args)
  }

  cli_log <- tempfile("claude-cli-", fileext = ".log")
  stderr_target <- if (isTRUE(quiet)) cli_log else ""

  stdout_lines <- system2(
    command = claude_bin,
    args = args,
    stdout = TRUE,
    stderr = stderr_target
  )

  status <- attr(stdout_lines, "status")
  if (is.null(status)) {
    status <- 0L
  }

  if (!identical(as.integer(status), 0L)) {
    stop(format_claude_error(status, cli_log), call. = FALSE)
  }

  response_text <- normalize_codex_response(paste(stdout_lines, collapse = "\n"))
  validate_claude_r_code(response_text)

  if (is.null(output_file)) {
    output_file <- tempfile("claude-response-", fileext = ".md")
  }
  writeLines(response_text, output_file)
  .claude_last_response_file(output_file)

  if (isTRUE(write_back)) {
    claude_write_back(response_text, write_target = write_target)
  }

  if (identical(output, "console") && !isTRUE(quiet)) {
    cat(response_text, sep = "\n")
    invisible(response_text)
  } else if (identical(output, "console")) {
    invisible(response_text)
  } else {
    invisible(output_file)
  }
}

resolve_claude_bin <- function(claude_bin = NULL) {
  candidate <- claude_bin
  if (is.null(candidate)) {
    candidate <- .claude_config$claude_bin
  }

  if (!is.null(candidate)) {
    if (!is.character(candidate) || length(candidate) != 1L || !nzchar(candidate)) {
      stop("`claude_bin` must be NULL or a single non-empty string.", call. = FALSE)
    }

    if (file.exists(candidate)) {
      return(normalizePath(candidate, winslash = "/", mustWork = FALSE))
    }

    resolved <- Sys.which(candidate)
    return(unname(resolved[[1]]))
  }

  unname(Sys.which("claude")[[1]])
}

format_claude_error <- function(status, cli_log) {
  if (!file.exists(cli_log)) {
    return(sprintf("Claude CLI exited with status %s.", status))
  }

  log_lines <- readLines(cli_log, warn = FALSE, encoding = "UTF-8")
  log_lines <- log_lines[nzchar(trimws(log_lines))]
  if (length(log_lines) == 0L) {
    return(sprintf("Claude CLI exited with status %s.", status))
  }

  snippet <- utils::tail(log_lines, 10L)
  sprintf(
    "Claude CLI exited with status %s. Last output:\n%s",
    status,
    paste(snippet, collapse = "\n")
  )
}

claude_prompt <- function(prompt, context) {
  sections <- c(
    "You are assisting from an R session launched from RStudio or RStudio Server.",
    "Use the provided editor context as the source of truth when relevant.",
    "Reply for the R user who invoked claude() from their script.",
    "Return only R code.",
    "Do not describe what the code would do.",
    "Do not show example output or simulated results.",
    "Do not wrap the response in Markdown fences.",
    "If the user asks for an action like listing files, write R code that performs that action.",
    "Any provided object information contains metadata only, not raw data values.",
    "Do not assume access to object contents beyond the supplied metadata.",
    "",
    "User request to implement as R code:",
    prompt,
    "",
    "R context:"
  )

  context_lines <- format_context(context)
  paste(c(sections, context_lines), collapse = "\n")
}

claude_write_target <- function(call_text, prompt) {
  if (!rstudioapi::isAvailable()) {
    return(NULL)
  }

  document <- rstudioapi::getActiveDocumentContext()
  if (!nzchar(document$id) || length(document$selection) == 0L) {
    return(NULL)
  }

  target <- document$selection[[1]]$range
  start_row <- target$start[["row"]]
  end_row <- target$end[["row"]]

  if (is.null(start_row) || is.null(end_row)) {
    return(NULL)
  }

  resolved_rows <- resolve_claude_rows(
    contents = document$contents,
    start_row = start_row,
    end_row = end_row,
    call_text = call_text,
    prompt = prompt
  )

  list(
    id = document$id,
    start_row = resolved_rows$start_row,
    end_row = resolved_rows$end_row
  )
}

claude_write_back <- function(response_text, write_target = NULL) {
  if (!rstudioapi::isAvailable() || is.null(write_target)) {
    return(invisible(FALSE))
  }

  document <- rstudioapi::getSourceEditorContext(id = write_target$id)
  if (!nzchar(document$id)) {
    return(invisible(FALSE))
  }

  contents <- document$contents
  if (length(contents) == 0L) {
    return(invisible(FALSE))
  }

  line_count <- length(contents)
  start_row <- max(1L, min(write_target$start_row, line_count))
  end_row <- max(start_row, min(write_target$end_row, line_count))

  original_lines <- contents[start_row:end_row]
  replacement_lines <- c(comment_lines(original_lines), split_lines(response_text))
  replacement_text <- paste(replacement_lines, collapse = "\n")

  last_line <- contents[[end_row]]
  replace_range <- rstudioapi::document_range(
    rstudioapi::document_position(start_row, 1),
    rstudioapi::document_position(end_row, nchar(last_line, type = "chars") + 1L)
  )

  rstudioapi::modifyRange(
    location = replace_range,
    text = replacement_text,
    id = write_target$id
  )

  invisible(TRUE)
}

resolve_claude_rows <- function(contents, start_row, end_row, call_text, prompt) {
  line_count <- length(contents)
  if (line_count == 0L) {
    return(list(start_row = start_row, end_row = end_row))
  }

  start_row <- max(1L, min(start_row, line_count))
  end_row <- max(start_row, min(end_row, line_count))

  target_lines <- contents[start_row:end_row]
  if (all(grepl("\\bclaude\\s*\\(", target_lines))) {
    return(list(start_row = start_row, end_row = end_row))
  }

  normalized_call <- normalize_line(call_text)
  normalized_prompt <- normalize_line(prompt)
  normalized_contents <- vapply(contents, normalize_line, character(1))

  exact_match <- which(normalized_contents == normalized_call)
  if (length(exact_match) > 0L) {
    row <- exact_match[[1]]
    return(list(start_row = row, end_row = row))
  }

  prompt_match <- which(
    grepl("\\bclaude\\s*\\(", contents) &
      grepl(normalized_prompt, normalized_contents, fixed = TRUE)
  )
  if (length(prompt_match) > 0L) {
    row <- prompt_match[[length(prompt_match)]]
    return(list(start_row = row, end_row = row))
  }

  claude_rows <- which(grepl("\\bclaude\\s*\\(", contents))
  if (length(claude_rows) > 0L) {
    prior_rows <- claude_rows[claude_rows <= start_row]
    if (length(prior_rows) > 0L) {
      row <- prior_rows[[length(prior_rows)]]
      return(list(start_row = row, end_row = row))
    }

    row <- claude_rows[[1]]
    return(list(start_row = row, end_row = row))
  }

  list(start_row = start_row, end_row = end_row)
}

validate_claude_r_code <- function(text) {
  if (!nzchar(trimws(text))) {
    stop("Claude returned an empty response instead of R code.", call. = FALSE)
  }

  parse_result <- tryCatch(
    parse(text = text, keep.source = FALSE),
    error = identity
  )

  if (inherits(parse_result, "error")) {
    message <- conditionMessage(parse_result)
    stop(
      sprintf("Claude returned text that is not valid R code: %s", message),
      call. = FALSE
    )
  }

  invisible(TRUE)
}
