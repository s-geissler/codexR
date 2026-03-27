.codex_last_response_file <- local({
  path <- NULL

  function(set = NULL) {
    if (!is.null(set)) {
      path <<- set
    }

    path
  }
})

.codex_config <- local({
  env <- new.env(parent = emptyenv())
  env$codex_bin <- NULL
  env$include_session <- TRUE
  env$write_back <- TRUE
  env$quiet <- FALSE

  env
})

codex_init <- function(codex_bin = NULL,
                       include_session = TRUE,
                       write_back = TRUE,
                       quiet = TRUE) {
  if (!is.null(codex_bin)) {
    if (!is.character(codex_bin) || length(codex_bin) != 1L || !nzchar(codex_bin)) {
      stop("`codex_bin` must be NULL or a single non-empty string.", call. = FALSE)
    }
  }

  .codex_config$codex_bin <- codex_bin
  .codex_config$include_session <- isTRUE(include_session)
  .codex_config$write_back <- isTRUE(write_back)
  .codex_config$quiet <- isTRUE(quiet)

  invisible(list(
    codex_bin = .codex_config$codex_bin,
    include_session = .codex_config$include_session,
    write_back = .codex_config$write_back,
    quiet = .codex_config$quiet
  ))
}

codex_config <- function() {
  list(
    codex_bin = .codex_config$codex_bin,
    include_session = .codex_config$include_session,
    write_back = .codex_config$write_back,
    quiet = .codex_config$quiet
  )
}

codex_available <- function() {
  nzchar(resolve_codex_bin())
}

codex_last_file <- function() {
  .codex_last_response_file()
}

codex <- function(prompt,
                  scope = c("document", "selection", "none"),
                  model = NULL,
                  output = c("console", "file"),
                  output_file = NULL,
                  include_session = NULL,
                  objects = NULL,
                  write_back = NULL,
                  quiet = NULL,
                  codex_bin = NULL,
                  codex_args = character()) {
  scope <- match.arg(scope)
  output <- match.arg(output)

  if (!is.character(prompt) || length(prompt) != 1L || !nzchar(prompt)) {
    stop("`prompt` must be a single non-empty string.", call. = FALSE)
  }

  if (is.null(include_session)) {
    include_session <- .codex_config$include_session
  }

  if (is.null(write_back)) {
    write_back <- .codex_config$write_back
  }

  if (is.null(quiet)) {
    quiet <- .codex_config$quiet
  }

  codex_bin <- resolve_codex_bin(codex_bin)
  if (!nzchar(codex_bin)) {
    stop("Codex CLI not found on PATH.", call. = FALSE)
  }

  call_text <- paste(deparse(match.call()), collapse = " ")
  write_target <- codex_write_target(call_text = call_text, prompt = prompt)
  object_metadata <- codex_object_metadata(objects = objects, env = parent.frame())
  context <- codex_context(
    scope = scope,
    include_session = include_session,
    object_metadata = object_metadata
  )
  prompt_text <- codex_prompt(prompt = prompt, context = context)

  if (is.null(output_file)) {
    output_file <- tempfile("codex-response-", fileext = ".md")
  }

  args <- c(
    "exec",
    "--skip-git-repo-check",
    "--color", "never",
    "--output-last-message", output_file
  )

  if (!is.null(model) && nzchar(model)) {
    args <- c(args, "--model", model)
  }

  if (length(codex_args) > 0L) {
    args <- c(args, codex_args)
  }

  cli_log <- tempfile("codex-cli-", fileext = ".log")
  stdout_target <- if (isTRUE(quiet)) cli_log else ""
  stderr_target <- if (isTRUE(quiet)) cli_log else ""

  status <- system2(
    command = codex_bin,
    args = args,
    input = prompt_text,
    stdout = stdout_target,
    stderr = stderr_target
  )

  .codex_last_response_file(output_file)

  if (!identical(status, 0L)) {
    stop(format_codex_error(status, cli_log), call. = FALSE)
  }

  response <- readLines(output_file, warn = FALSE, encoding = "UTF-8")
  response_text <- normalize_codex_response(paste(response, collapse = "\n"))
  validate_r_code(response_text)

  if (isTRUE(write_back)) {
    codex_write_back(response_text, write_target = write_target)
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

resolve_codex_bin <- function(codex_bin = NULL) {
  candidate <- codex_bin
  if (is.null(candidate)) {
    candidate <- .codex_config$codex_bin
  }

  if (!is.null(candidate)) {
    if (!is.character(candidate) || length(candidate) != 1L || !nzchar(candidate)) {
      stop("`codex_bin` must be NULL or a single non-empty string.", call. = FALSE)
    }

    if (file.exists(candidate)) {
      return(normalizePath(candidate, winslash = "/", mustWork = FALSE))
    }

    resolved <- Sys.which(candidate)
    return(unname(resolved[[1]]))
  }

  unname(Sys.which("codex")[[1]])
}

format_codex_error <- function(status, cli_log) {
  if (!file.exists(cli_log)) {
    return(sprintf("Codex CLI exited with status %s.", status))
  }

  log_lines <- readLines(cli_log, warn = FALSE, encoding = "UTF-8")
  log_lines <- log_lines[nzchar(trimws(log_lines))]
  if (length(log_lines) == 0L) {
    return(sprintf("Codex CLI exited with status %s.", status))
  }

  snippet <- utils::tail(log_lines, 10L)
  sprintf(
    "Codex CLI exited with status %s. Last output:\n%s",
    status,
    paste(snippet, collapse = "\n")
  )
}

codex_context <- function(scope = c("document", "selection", "none"),
                          include_session = TRUE,
                          object_metadata = NULL) {
  scope <- match.arg(scope)

  context <- list(
    source = "R",
    working_directory = normalizePath(getwd(), winslash = "/", mustWork = FALSE),
    timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")
  )

  if (isTRUE(include_session)) {
    context$search_path <- search()
    context$objects <- utils::head(ls(.GlobalEnv, all.names = TRUE), 100L)
  }

  if (!is.null(object_metadata) && length(object_metadata) > 0L) {
    context$object_metadata <- object_metadata
  }

  if (!rstudioapi::isAvailable()) {
    return(context)
  }

  document <- rstudioapi::getActiveDocumentContext()
  if (!nzchar(document$id)) {
    return(context)
  }

  path <- document$path
  if (!nzchar(path)) {
    path <- "<unsaved>"
  } else {
    path <- normalizePath(path, winslash = "/", mustWork = FALSE)
  }

  selection_items <- document$selection
  if (length(selection_items) > 0L) {
    selection <- paste(vapply(selection_items, `[[`, "", "text"), collapse = "\n")
    cursor <- selection_items[[1]]$range
    line <- cursor$start[["row"]]
    column <- cursor$start[["column"]]
  } else {
    selection <- ""
    line <- NA_integer_
    column <- NA_integer_
  }

  context$active_document <- list(
    path = path,
    line = line,
    column = column,
    contents = paste(document$contents, collapse = "\n")
  )

  if (nzchar(selection)) {
    context$selection <- selection
  }

  if (identical(scope, "selection") && !nzchar(selection)) {
    warning("No active selection found; falling back to document scope.", call. = FALSE)
    scope <- "document"
  }

  if (identical(scope, "none")) {
    context$active_document$contents <- NULL
    context$selection <- NULL
  }

  if (identical(scope, "selection")) {
    context$active_document$contents <- NULL
  }

  context
}

codex_prompt <- function(prompt, context) {
  sections <- c(
    "You are assisting from an R session launched from RStudio or RStudio Server.",
    "Use the provided editor context as the source of truth when relevant.",
    "Reply for the R user who invoked codex() from their script.",
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

codex_write_target <- function(call_text, prompt) {
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

  resolved_rows <- resolve_codex_rows(
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

codex_write_back <- function(response_text, write_target = NULL) {
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

resolve_codex_rows <- function(contents, start_row, end_row, call_text, prompt) {
  line_count <- length(contents)
  if (line_count == 0L) {
    return(list(start_row = start_row, end_row = end_row))
  }

  start_row <- max(1L, min(start_row, line_count))
  end_row <- max(start_row, min(end_row, line_count))

  target_lines <- contents[start_row:end_row]
  if (all(grepl("\\bcodex\\s*\\(", target_lines))) {
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
    grepl("\\bcodex\\s*\\(", contents) &
      grepl(normalized_prompt, normalized_contents, fixed = TRUE)
  )
  if (length(prompt_match) > 0L) {
    row <- prompt_match[[length(prompt_match)]]
    return(list(start_row = row, end_row = row))
  }

  codex_rows <- which(grepl("\\bcodex\\s*\\(", contents))
  if (length(codex_rows) > 0L) {
    prior_rows <- codex_rows[codex_rows <= start_row]
    if (length(prior_rows) > 0L) {
      row <- prior_rows[[length(prior_rows)]]
      return(list(start_row = row, end_row = row))
    }

    row <- codex_rows[[1]]
    return(list(start_row = row, end_row = row))
  }

  list(start_row = start_row, end_row = end_row)
}

normalize_codex_response <- function(text) {
  if (!nzchar(text)) {
    return(text)
  }

  lines <- strsplit(text, "\n", fixed = TRUE)[[1]]
  if (length(lines) >= 2L &&
      startsWith(lines[[1]], "```") &&
      startsWith(lines[[length(lines)]], "```")) {
    lines <- lines[2:(length(lines) - 1L)]
  }

  paste(lines, collapse = "\n")
}

validate_r_code <- function(text) {
  if (!nzchar(trimws(text))) {
    stop("Codex returned an empty response instead of R code.", call. = FALSE)
  }

  parse_result <- tryCatch(
    parse(text = text, keep.source = FALSE),
    error = identity
  )

  if (inherits(parse_result, "error")) {
    message <- conditionMessage(parse_result)
    stop(
      sprintf("Codex returned text that is not valid R code: %s", message),
      call. = FALSE
    )
  }

  invisible(TRUE)
}

normalize_line <- function(text) {
  gsub("\\s+", " ", trimws(text))
}

codex_object_metadata <- function(objects, env) {
  if (is.null(objects)) {
    return(NULL)
  }

  if (is.symbol(substitute(objects))) {
    objects <- as.character(substitute(objects))
  }

  if (!is.character(objects) || length(objects) == 0L || any(!nzchar(objects))) {
    stop("`objects` must be NULL or a character vector of object names.", call. = FALSE)
  }

  unique_objects <- unique(objects)
  metadata <- stats::setNames(vector("list", length(unique_objects)), unique_objects)

  for (name in unique_objects) {
    if (!exists(name, envir = env, inherits = TRUE)) {
      metadata[[name]] <- list(error = "Object not found in calling environment.")
      next
    }

    object <- get(name, envir = env, inherits = TRUE)
    metadata[[name]] <- summarize_object_metadata(object)
  }

  metadata
}

summarize_object_metadata <- function(object) {
  if (is.data.frame(object)) {
    return(list(
      class = class(object),
      typeof = typeof(object),
      dimensions = as.integer(dim(object)),
      name_count = length(names(object)),
      data_frame = list(
        nrow = nrow(object),
        ncol = ncol(object),
        column_names = names(object),
        column_types = vapply(object, column_type_string, character(1))
      )
    ))
  }

  if (is.matrix(object)) {
    result <- list(
      class = class(object),
      typeof = typeof(object),
      dimensions = as.integer(dim(object)),
      matrix = list(
        nrow = nrow(object),
        ncol = ncol(object),
        element_type = typeof(object),
        has_rownames = !is.null(rownames(object)),
        has_colnames = !is.null(colnames(object))
      )
    )

    if (!is.null(colnames(object))) {
      result$matrix$column_names <- utils::head(colnames(object), 100L)
    }

    return(result)
  }

  if (is.array(object)) {
    return(list(
      class = class(object),
      typeof = typeof(object),
      dimensions = as.integer(dim(object)),
      array = list(
        rank = length(dim(object)),
        element_type = typeof(object),
        has_dimnames = !is.null(dimnames(object))
      )
    ))
  }

  if (is.list(object)) {
    result <- list(
      class = class(object),
      typeof = typeof(object),
      length = length(object),
      list = list(
        length = length(object),
        named = !is.null(names(object)),
        element_classes = utils::head(vapply(object, list_element_class, character(1)), 50L)
      )
    )

    if (!is.null(names(object))) {
      result$list$names <- utils::head(names(object), 100L)
      result$list$name_count <- length(names(object))
    }

    return(result)
  }

  if (is.factor(object)) {
    return(list(
      class = class(object),
      typeof = typeof(object),
      length = length(object),
      factor = list(
        levels_count = length(levels(object)),
        ordered = is.ordered(object)
      )
    ))
  }

  if (inherits(object, "Date") || inherits(object, "POSIXt")) {
    return(list(
      class = class(object),
      typeof = typeof(object),
      length = length(object),
      temporal = list(
        classes = class(object),
        timezone = timezone_or_na(object)
      )
    ))
  }

  if (is.atomic(object)) {
    result <- list(
      class = class(object),
      typeof = typeof(object),
      length = length(object),
      atomic = list(
        mode = mode(object)
      )
    )

    object_names <- names(object)
    if (!is.null(object_names)) {
      result$atomic$named <- TRUE
      result$atomic$name_count <- length(object_names)
      result$atomic$names <- utils::head(object_names, 100L)
    }

    return(result)
  }

  list(
    class = class(object),
    typeof = typeof(object),
    fallback = list(
      length = safe_length(object),
      has_dim = !is.null(dim(object)),
      has_names = !is.null(names(object))
    )
  )
}

timezone_or_na <- function(object) {
  tz <- attr(object, "tzone", exact = TRUE)
  if (is.null(tz) || length(tz) == 0L || !nzchar(tz[[1]])) {
    return(NA_character_)
  }

  tz[[1]]
}

safe_length <- function(object) {
  value <- tryCatch(length(object), error = function(...) NA_integer_)
  as.integer(value)
}

column_type_string <- function(x) {
  paste(class(x), collapse = "/")
}

list_element_class <- function(x) {
  paste(class(x), collapse = "/")
}

comment_lines <- function(lines) {
  vapply(
    lines,
    FUN.VALUE = character(1),
    FUN = function(line) {
      if (!nzchar(line)) {
        return("#")
      }

      if (startsWith(line, "#")) {
        return(line)
      }

      paste0("# ", line)
    }
  )
}

split_lines <- function(text) {
  if (!nzchar(text)) {
    return(character())
  }

  strsplit(text, "\n", fixed = TRUE)[[1]]
}

format_context <- function(x, indent = 0L) {
  prefix <- paste(rep("  ", indent), collapse = "")
  lines <- character()

  for (name in names(x)) {
    value <- x[[name]]

    if (is.null(value) || length(value) == 0L) {
      next
    }

    if (is.list(value)) {
      lines <- c(lines, sprintf("%s%s:", prefix, name))
      lines <- c(lines, format_context(value, indent = indent + 1L))
      next
    }

    if (length(value) > 1L) {
      lines <- c(lines, sprintf("%s%s:", prefix, name))
      lines <- c(
        lines,
        sprintf("%s- %s", paste(rep("  ", indent + 1L), collapse = ""), as.character(value))
      )
      next
    }

    text <- as.character(value)
    if (grepl("\n", text, fixed = TRUE)) {
      lines <- c(lines, sprintf("%s%s:", prefix, name))
      block_prefix <- paste(rep("  ", indent + 1L), collapse = "")
      block <- paste(block_prefix, strsplit(text, "\n", fixed = TRUE)[[1]], sep = "")
      lines <- c(lines, block)
    } else {
      lines <- c(lines, sprintf("%s%s: %s", prefix, name, text))
    }
  }

  lines
}
