test_that("resolve_claude_bin falls back to PATH", {
  expect_identical(
    codexR:::resolve_claude_bin("claude"),
    unname(Sys.which("claude")[[1]])
  )
})

test_that("resolve_claude_bin returns empty string for missing command", {
  expect_identical(codexR:::resolve_claude_bin("definitely-not-a-real-claude-bin"), "")
})

test_that("validate_claude_r_code rejects prose", {
  expect_error(
    codexR:::validate_claude_r_code("Here are your files:"),
    "not valid R code"
  )
})

test_that("validate_claude_r_code rejects empty response", {
  expect_error(
    codexR:::validate_claude_r_code("   "),
    "empty response"
  )
})

test_that("claude_prompt returns single-line instruction and multi-line context", {
  ctx <- list(source = "R", working_directory = "/tmp")
  parts <- codexR:::claude_prompt("list files", ctx)
  expect_named(parts, c("instruction", "context"))
  expect_false(grepl("\n", parts$instruction, fixed = TRUE))
  expect_true(grepl("\n", parts$context, fixed = TRUE))
  expect_false(grepl("codex()", parts$instruction, fixed = TRUE))
})

test_that("resolve_claude_rows finds claude() call lines", {
  contents <- c(
    "library(codexR)",
    'claude("list files in the project")',
    "x <- 1"
  )
  result <- codexR:::resolve_claude_rows(
    contents = contents,
    start_row = 2L,
    end_row = 2L,
    call_text = 'claude("list files in the project")',
    prompt = "list files in the project"
  )
  expect_equal(result$start_row, 2L)
  expect_equal(result$end_row, 2L)
})
