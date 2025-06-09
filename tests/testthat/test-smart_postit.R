test_that("smart_postit handles custom color overrides", {
  grDevices::pdf(NULL)  # open invisible graphics device
  on.exit(grDevices::dev.off(), add = TRUE)
  expect_invisible(
    smart_postit(
        text_string = "Color~override test",
        custom_fill_color = "#ABCDEF",
        custom_text_color = "#123456",
        custom_border_color = "#654321",
        font_face = "bold",
        new_page = FALSE,
        output = "object",
        verbose = FALSE
      )
    )
})

test_that("smart_postit supports absolute padding mode", {
  grDevices::pdf(NULL)  # open invisible graphics device
  on.exit(grDevices::dev.off(), add = TRUE)
  expect_invisible(
    smart_postit(
      text_string = "Absolute padding test",
      use_relative_padding = FALSE,
      padding_width = 1,
      padding_height = 0.5,
      new_page = FALSE,
      output = "object",
      verbose = FALSE
    )
  )
})

test_that("smart_postit returns patchwork object silently", {
  grDevices::pdf(NULL)  # open invisible graphics device
  on.exit(grDevices::dev.off(), add = TRUE)
  obj <- smart_postit(
    "This is an~object", output = "object", new_page = FALSE, verbose = FALSE)
  expect_s3_class(obj, "gg")
})
test_that("smart_postit works with different font faces", {
  grDevices::pdf(NULL)  # open invisible graphics device
  on.exit(grDevices::dev.off(), add = TRUE)
  expect_invisible(
    smart_postit(
      text_string = "Font~style~test",
      font_family = "sans",
      font_face = "italic",
      new_page = TRUE,
      output = "plot",
      verbose = FALSE
    )
  )
})

test_that("smart_postit generates expected grid plot snapshot", {
  skip_if_not_installed("vdiffr")
  vdiffr::expect_doppelganger(
    title = "postit_snapshot",
    fig = function() {
      smart_postit(
        "\u270D~Post-IT",
        device_width = 5,
        device_height = 3,
        new_page = TRUE,
        verbose = FALSE
      )
    }
  )
})



