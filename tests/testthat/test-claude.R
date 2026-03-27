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

test_that("claude_prompt references claude() not codex()", {
  ctx <- list(source = "R", working_directory = "/tmp")
  prompt_text <- codexR:::claude_prompt("list files", ctx)
  expect_true(grepl("claude()", prompt_text, fixed = TRUE))
  expect_false(grepl("codex()", prompt_text, fixed = TRUE))
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
