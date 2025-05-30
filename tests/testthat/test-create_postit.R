test_that("create_postit returns a ggplot object", {
  p <- create_postit(text = "Test label", plot = FALSE)
  expect_s3_class(p, "ggplot")
})

test_that("Text is abbreviated", {
  txt <- process_postit_text(
    "Single Cell Workflow",
    wrap = FALSE,
    abbreviate_text = TRUE,
    abbr_minlength = 3
  )
  expect_match(txt, "Sng|Cll|Wrk", ignore.case = TRUE)
})

test_that("Text is truncated when exceeding max_lines", {
  txt <- process_postit_text(
    "This text should be truncated into one line only",
    wrap = TRUE,
    text_width = 10,
    max_lines = 1,
    truncate = TRUE
  )
  expect_true(grepl("\\.\\.\\.$", txt))
})

test_that("No wrapping truncates long single word", {
  txt <- process_postit_text(
    "ThisTextIsWayTooLong",
    wrap = FALSE,
    text_width = 5,
    max_lines = 1,
    truncate = TRUE
  )
  expect_equal(txt, "ThisT...")
})

test_that("Empty string for max_lines = 0", {
  txt <- process_postit_text("Some input text", wrap = TRUE, max_lines = 0)
  expect_equal(txt, "")
})

test_that("Abbreviation without minlength defaults reasonably", {
  txt <- process_postit_text(
    "Label LongLabel",
    abbreviate_text = TRUE,
    abbr_minlength = NULL,
    text_width = 10
  )
  words <- strsplit(gsub("<br>", "", txt), " ")[[1]]
  expect_true(all(nchar(words) <= 10))
})

test_that("create_postit generates expected plot", {
  skip_if_not_installed("vdiffr")
  vdiffr::expect_doppelganger(
  title = "create_postit_plot",
  fig = function() {
    p <- create_postit("Post-IT", plot = FALSE)
    print(p)
  }
)
})
