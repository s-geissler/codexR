test_that("resolve_codex_bin falls back to PATH", {
  expect_true(nzchar(codexR:::resolve_codex_bin("codex")))
})

test_that("object metadata for data frames is schema-only", {
  df_data <- data.frame(
    id = 1:3,
    name = c("alice", "bob", "charlie"),
    score = c(1.1, 2.2, 3.3)
  )

  metadata <- codexR:::codex_object_metadata("df_data", environment())

  expect_named(metadata, "df_data")
  expect_equal(metadata$df_data$data_frame$column_names, c("id", "name", "score"))
  expect_equal(unname(metadata$df_data$data_frame$column_types), c("integer", "character", "numeric"))
  expect_false(any(grepl("alice|bob|charlie", capture.output(str(metadata)), ignore.case = TRUE)))
})

test_that("normalize_codex_response strips fenced code blocks", {
  text <- "```r\nfiles <- list.files(\".\")\nprint(files)\n```"
  expect_equal(
    codexR:::normalize_codex_response(text),
    "files <- list.files(\".\")\nprint(files)"
  )
})

test_that("validate_r_code rejects prose", {
  expect_error(
    codexR:::validate_r_code("Here are your files:"),
    "not valid R code"
  )
})
