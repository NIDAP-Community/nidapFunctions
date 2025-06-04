
test_that("create_postit runs without error with default settings", {
  expect_invisible(suppressMessages(
    create_postit("hello world")))
})

test_that("create_postit handles custom colors and font", {
  expect_invisible(suppressMessages(
    create_postit(
    text_string = "custom~color~test",
    custom_fill = "#ABCDEF",
    custom_text_color = "#123456",
    font_face = "bold"
  )))
})

test_that("create_postit handles relative padding", {
  expect_invisible(suppressMessages(
    create_postit(
    text_string = "padded~note",
    use_relative_padding = TRUE,
    padding_width = 0.1,
    padding_height = 0.1
  )))
})

test_that("create_postit displays multiline layout", {
  expect_invisible(suppressMessages(
    create_postit("This~is~a~test // New~line~block")))
})

test_that("create_postit generates expected grid plot", {
  skip_if_not_installed("vdiffr")
  
  vdiffr::expect_doppelganger(
    title = "postit_grid_output",
    fig = function() {
      # We just call the function, which internally draws via grid
      suppressMessages(
        create_postit(
        "\u270D Post-IT",
        device_width = 5, device_height = 2,
      ))
    }
  )
})

test_that("create_postit() returns patchwork object silently", {
  out <- suppressMessages(
    create_postit("A ~ B // C", output = "object")
  )
  
  expect_s3_class(out, "gg")
})
