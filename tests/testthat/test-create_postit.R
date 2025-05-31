
test_that("create_postit returns a ggplot object by default", {
  result <- create_postit(text = "Hello, world!", plot = FALSE)
  expect_s3_class(result, "ggplot")
})

test_that("abbreviation shortens words correctly", {
  processed <- process_postit_text(
    text = "Differential Expression Analysis",
    text_size = 200,
    abbreviate_text = TRUE,
    abbr_minlength = 2
  )
  expect_false(grepl("Differential", processed))
})

test_that("truncation applies when lines exceed space", {
  processed <- process_postit_text(
    text = paste(rep("word", 100), collapse = " "),
    text_size = 200,
    wrap = TRUE,
    truncate = TRUE
  )
  expect_true(grepl("\\.\\.\\.$", processed))
})


test_that("empty text with too small npc area returns empty string", {
  processed <- process_postit_text(
    text = "Too Big Text",
    width_npc = 0.001,
    height_npc = 0.001,
    text_size = 500
  )
  expect_equal(processed, "")
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
